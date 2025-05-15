/**
 * ********** DATA COLLECTION *************
 * [] FETCH_SILC_CSV : IMPORT CSV FILES PROVIDED BY NSI
 * ****************************************
 */

%MACRO FETCH_SILC_CSV (F=D,VER=2,PREV=N);/*import CSV file provided by the NSI*/

%LOCAL DIR FNAME FVER _VARLIST_ _FORMAT_ _INFORMAT_ _LENGTH_;

DATA _NULL_;
   CALL SYMPUTX('FVER','0000_V'||PUT(&VER,Z4.));
RUN;

/*declare files names*/
%LET DIR=%SYSFUNC(PATHNAME(CSV));
    %LET FPROD=SILC_&ss.&F._A_%UPCASE(&cc)_&yyyy._&FVER..CSV;
    %LET FPROD2=SILC_&ss.&F._A_%UPCASE(&cc)_&yyyy._&FVER..csv;

    %LET FNAME=&FPROD;/*if CSV in capital letter*/
    %IF NOT %SYSFUNC(FILEEXIST(&DIR&_dirsp_.&FPROD)) AND %SYSFUNC(FILEEXIST(&DIR&_dirsp_.&FPROD2)) %THEN %DO;
    %LET FNAME=&FPROD2;/*if csv in lowercase letter*/
    %END;

%IF %SYSFUNC(FILEEXIST(&DIR&_dirsp_.&FNAME)) %THEN %DO;
        %PUT *I* XML_COLLECT: HANDLING SILC DATAFILE &DIR&_dirsp_.&FNAME;
		/*1st import to get the list of variables*/
		OPTIONS OBS=1;
		filename csvfile "&DIR&_dirsp_.&FNAME";
		proc import dbms=csv datafile=csvfile OUT=FILE_&F.TEMP REPLACE;
		getnames=no ;
		run;
		OPTIONS OBS=MAX;
		proc transpose out=LIST_VAR_CSV;var _all_;run;
		data LIST_VAR_CSV;set LIST_VAR_CSV;keep variable varnum;variable=upcase(col1);varnum=tranwrd (_name_,"VAR","")+0;run;
		proc sort data=LIST_VAR_CSV; by variable;run;
		data LIST_VAR_CSV;set LIST_VAR_CSV;if variable=lag(variable) then dupl=1;run;
		data LIST_VAR_CSV;set LIST_VAR_CSV;drop dupl;if dupl=1 then variable=compress(variable!!"dupl"," ");run;



		/*setting the expected variables with their expected format and length +2 */
		/*core variables, taken in sval_ante_ if exists because more complete, in sval otherwise*/
		DATA SVAL_2;
			%IF %SYSFUNC(EXIST(SVAL_ANTE_)) %THEN %DO;
				SET SVAL_ANTE_;
			%END;
			%ELSE %DO;
				SET SVAL;
			%END;
			IF SUBSTR(VARIABLE,1,1)="&F";
			keep VARIABLE FORMAT_VAR INFORMAT_VAR FORMAT_VAR2;
			if substr(format,1,1)="$" then do;
				FORMAT_VAR2="$";
				FORMAT_VAR="format " || TRIM(VARIABLE) || " " || compress("$"||put(floor(TRANWRD(FORMAT,"$","")+2),best8.)||"."," ");
				INFORMAT_VAR="informat " || TRIM(VARIABLE) || " " || compress("$"||put(floor(TRANWRD(FORMAT,"$","")+2),best8.)||"."," ");
			end;
			else do;
				FORMAT_VAR="format " || TRIM(VARIABLE) || " best12.";
				INFORMAT_VAR="informat " || TRIM(VARIABLE) || " best32.";
			end;
		RUN;
		/*flags variables*/
		DATA SVAL_FLAGS2;
			SET SVAL_FLAGS;
			keep VAR_F FORMAT_VAR INFORMAT_VAR;
			RENAME VAR_F=VARIABLE;
			IF SUBSTR(VARIABLE,1,1)="&F";
			VAR_F=compress(VARIABLE||"_F"," ");
			FORMAT_VAR="format " || TRIM(VAR_F) || " best12.";
			INFORMAT_VAR="informat " || TRIM(VAR_F) || " best32.";
		RUN;
		/*imputation factors variables*/
		DATA SVAL_IMPUTE2;
			SET SVAL;
			RENAME VAR_IF=VARIABLE;
			VAR_IF=compress(VARIABLE||"_IF"," ");
			keep VAR_IF FORMAT_VAR FORMAT_VAR;
			IF SUBSTR(VARIABLE,1,1)="&F" AND INCOME="Y";
			FORMAT_VAR="format " || TRIM(VAR_IF) || " best12.";
			INFORMAT_VAR="informat " || TRIM(VAR_IF) || " best32.";
		RUN;
		/*all the variables*/
		DATA SVAL_TOT_2;
		SET SVAL_2 SVAL_FLAGS2 SVAL_IMPUTE2 ;
		RUN;
		
		/*merging the expected variables with the actual ones, and keeping only the latters*/
		PROC SQL;
		CREATE TABLE SVAL_TOT_2 AS SELECT a.*, FORMAT_VAR, INFORMAT_VAR, FORMAT_VAR2
		from LIST_VAR_CSV as a left join SVAL_TOT_2 as b
		on a.variable=b.variable;
		quit;
		run;
		DATA SVAL_TOT_2;set SVAL_TOT_2;
		IF substr(FORMAT_VAR2,1,1)="$" THEN VARIABLE=TRIM(VARIABLE) || " $";
		IF FORMAT_VAR="" THEN FORMAT_VAR="format " || TRIM(VARIABLE) || " best12.";
		IF INFORMAT_VAR="" THEN INFORMAT_VAR="informat " || TRIM(VARIABLE) || " best32.";
		RUN;
		PROC SORT NODUPKEY;BY VARNUM;RUN;
		
		/*creating macro variables for the infile step*/
		DATA SVAL_TOT_&F;
		SET SVAL_TOT_2 END=EOF;
		LENGTH _VARLIST_ _FORMAT_ _INFORMAT_ $32767;
		RETAIN _VARLIST_ _FORMAT_ _INFORMAT_ ;
		_VARLIST_ = TRIM(_VARLIST_) || ' ' || TRIM(VARIABLE);
		_FORMAT_ = TRIM(_FORMAT_) || ' ' ||TRIM(FORMAT_VAR);
		_INFORMAT_ = TRIM(_INFORMAT_) || ' ' ||TRIM(INFORMAT_VAR);
		IF NOT EOF THEN DO;
			_VARLIST_ = TRIM(_VARLIST_) || ' ';
			_FORMAT_ = TRIM(_FORMAT_) || '%STR(;) ';
			_INFORMAT_ = TRIM(_INFORMAT_) || '%STR(;) ';
		END;
		ELSE DO;
			CALL SYMPUT('_VARLIST_',%str(TRIM(_VARLIST_)));
			CALL SYMPUT('_FORMAT_',%str(TRIM(_FORMAT_)));
			CALL SYMPUT('_INFORMAT_',%str(TRIM(_INFORMAT_)));
		END;
		RUN;

		/*the 2nd import with the full data*/
		%IF &PREV = Y %THEN %DO;data FILE_&F&PVER;%END;
		%ELSE %DO;data FILE_&F;%END;
		    infile "&DIR&_dirsp_.&FNAME"
			delimiter = ","
			truncover 
			dsd
			firstobs=2
			lrecl=32767;			 
			%unquote(&_INFORMAT_);		 
			%unquote(&_FORMAT_);
		 
			input &_VARLIST_;
		run;


        %IF &PREV NE Y %THEN %DO;
	        DATA RAW.&_SPLIT_MODE_&ss&cc&YY&F;
	        SET  FILE_&F;
	        RUN;
        %END;

%END;
%ELSE %DO;
    %LET _IOERR_=1;
        %PUT ERROR: DATAFILE &DIR&_dirsp_.&FNAME NOT FOUND
        ;
        %PUT ERROR: DATAFILE &DIR&_dirsp_.&FPROD2 NOT FOUND
        ;
%END;

%MEND FETCH_SILC_CSV;


