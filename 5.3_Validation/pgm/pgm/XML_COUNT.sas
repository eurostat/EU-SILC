/**
 * Revision History
 * - NOV 29th 2023
 */

%macro count_setup;
%GLOBAL HH_MEMBERSHIP_MANDATORY HH_MEMBERSHIP_MANDATORY_F;
%LOCAL INDIR INFNAME MAPDIR MAPNAME;

%LET INDIR=%SYSFUNC(PATHNAME(XMLCFG))&_dirsp_%quote(COMP);
%LET INFNAME=COMP-R-&YYYY..xml;

/*Import list of variables to be counted*/
LIBNAME IN_XML XMLV2 "&INDIR&_dirsp_%quote(&INFNAME)" XMLMAP="%SYSFUNC(PATHNAME(MAPDIR))&_dirsp_%quote(COMP.map)";

DATA COUNT_CFG;
SET  IN_XML.COMP;
PROC SORT NODUPKEY;
  BY ID;
RUN;

/*initialize counting tables*/
DATA cont_STATS;
  FORMAT FILE $16. VARIABLE $12. YEAR Z4. TOT NMISS NA N NEG ZERO POS COMMAX21.2 PCT_MISS PCT_NEG PCT_ZERO PCT_POS PERCENT9.2;
  STOP;
RUN;

DATA disc_STATS;
  FORMAT FILE $16. VARIABLE $12. YEAR Z4. TOT N NMISS N NA COMMAX21.2 PCT_MISS PCT_NEG PCT_ZERO PCT_POS PERCENT9.2;
  STOP;
RUN;


DATA flag_STATS;
  FORMAT FILE $16. VARIABLE $12. REQFM2 REQFM3 REQFM4 REQFM5 $1.  YEAR Z4. TOT N NMISS NEG8 NEG7 NEG6 NEG5 NEG4 NEG3 NEG2 NEG1 POS COMMAX21.2 
		PCT_MISS PCT_POS PCT_NEG8 PCT_NEG7 PCT_NEG6 PCT_NEG5 PCT_NEG4 PCT_NEG3 PCT_NEG2 PCT_NEG1 PERCENT9.2;
  STOP;
RUN;

/*selecting mandatory variables for non household members and putting them in a macro var*/
DATA COUNT_HH_MEMBER_MANDATORY ;
SET  SVAL_HH_MEMBER_MANDATORY;
RUN;
PROC SORT NODUPKEY;BY ID;RUN;
DATA _NULL_;
SET COUNT_HH_MEMBER_MANDATORY END=EOF;
	LENGTH VARS $32767 FLAGS $32767;
	RETAIN VARS '' FLAGS '';
	VARS = TRIM(VARS) || ' "' || TRIM(ID) || '"' ;
	FLAGS = TRIM(FLAGS) || ' "' || TRIM(ID) || '_F"' ;
	IF NOT EOF THEN DO;
		VARS = TRIM(VARS);
		FLAGS = TRIM(FLAGS);
	END;
	ELSE DO;
		CALL SYMPUT('HH_MEMBERSHIP_MANDATORY',TRIM(VARS));
		CALL SYMPUT('HH_MEMBERSHIP_MANDATORY_F',TRIM(FLAGS));
	END;
RUN;

%mend count_setup;




%macro run_count(F, hh_memb=);
	%LET DS_1=RAW.&ss&cc&YY.&F;
	/*SELECTING HH-MEMBERS OR NON-HH-MEMBERS FOR R FILE*/
	%IF &F=R %THEN %DO;
		DATA &ss&cc&YY.&F;
		SET &DS_1;
			%IF &hh_memb=N %THEN %DO;IF RB245 NOT IN (1,2,3,4);%END;
			%ELSE %DO;IF RB245 IN (1,2,3,4);%END;
		RUN;
		%LET DS_1=&ss&cc&YY.&F;
	%END;

	/*selecting variables present both in config files and in data files and putting them in macro var lists*/
	OPTIONS VALIDVARNAME=UPCASE;
	PROC CONTENTS NOPRINT DATA=&DS_1 OUT=MEMBERS_1;
	OPTIONS VALIDVARNAME=ANY;
	PROC SQL; 
		CREATE TABLE COUNT_CFG_&F AS SELECT A.* 
		FROM COUNT_CFG A INNER JOIN MEMBERS_1 B ON A.ID=B.NAME;
		QUIT;
	RUN;
	PROC SQL NOPRINT;
	  SELECT ID INTO :VARS_&F._C SEPARATED BY ' ' FROM COUNT_CFG_&F WHERE SUBSTR(ID,1,1)="&F" & TYPE="C" & desc="Y";
	  SELECT ID INTO :VARS_&F._D SEPARATED BY ' ' FROM COUNT_CFG_&F WHERE SUBSTR(ID,1,1)="&F" & TYPE="D" & desc="Y";
	  SELECT COMPRESS(ID||"_F"," ") INTO :VARS_&F._C_F SEPARATED BY ' ' FROM COUNT_CFG_&F WHERE SUBSTR(ID,1,1)="&F" & TYPE="C" & desc="Y";
	  SELECT COMPRESS(ID||"_F"," ") INTO :VARS_&F._D_F SEPARATED BY ' ' FROM COUNT_CFG_&F WHERE SUBSTR(ID,1,1)="&F" & TYPE="D" & desc="Y";
	  SELECT COMPRESS(ID||"_F"," ") INTO :VARS_&F._F SEPARATED BY ' ' FROM COUNT_CFG_&F WHERE SUBSTR(ID,1,1)="&F" & TYPE="D" /*& desc="Y"*/;
	  SELECT W INTO :VARS_&F._W FROM COUNT_CFG_&F WHERE SUBSTR(ID,1,1)="&F" ;
	QUIT;
	RUN;


/*CONTINUOUS VARIABLES COUNTING*/

%if %symexist(VARS_&F._C) %then %do;
	DATA COUNT_&F._C_NA;SET &DS_1 (keep=&&VARS_&F._C &&VARS_&F._C_F &F.B010 where=(&F.B010=&YYYY));
	ARRAY X &&VARS_&F._C;
	ARRAY Y &&VARS_&F._C_F;
	DO I=1 TO DIM(X);IF X(I)=. AND Y(I) not in (-8,-1,.) THEN X(I)=1;ELSE X(I)=0;
	END; 
	DATA COUNT_&F._C_MISS;SET &DS_1 (keep=&&VARS_&F._C &&VARS_&F._C_F &F.B010 where=(&F.B010=&YYYY));
	ARRAY X &&VARS_&F._C;
	ARRAY Y &&VARS_&F._C_F;
	DO I=1 TO DIM(X);IF X(I)=. AND Y(I) in (-8,-1,.) THEN X(I)=1;ELSE X(I)=0;
	END; 
	DATA COUNT_&F._C_NEG;SET &DS_1 (keep=&&VARS_&F._C &&VARS_&F._C_F &F.B010 where=(&F.B010=&YYYY));
	ARRAY X &&VARS_&F._C;
	ARRAY Y &&VARS_&F._C_F;
	DO I=1 TO DIM(X);IF X(I) ne . AND X(I)<0 THEN X(I)=1;ELSE X(I)=0;
	END; 
	DATA COUNT_&F._C_ZERO;SET &DS_1 (keep=&&VARS_&F._C &&VARS_&F._C_F &F.B010 where=(&F.B010=&YYYY));
	ARRAY X &&VARS_&F._C;
	ARRAY Y &&VARS_&F._C_F;
	DO I=1 TO DIM(X);IF X(I) ne . AND X(I)=0 THEN X(I)=1;ELSE X(I)=0;
	END; 
	DATA COUNT_&F._C_POS;SET &DS_1 (keep=&&VARS_&F._C &&VARS_&F._C_F &F.B010 where=(&F.B010=&YYYY));
	ARRAY X &&VARS_&F._C;
	ARRAY Y &&VARS_&F._C_F;
	DO I=1 TO DIM(X);IF X(I) ne . AND X(I)>0 THEN X(I)=1;ELSE X(I)=0;
	END; 

	PROC MEANS DATA=COUNT_&F._C_NA NOPRINT;VAR &&VARS_&F._C;OUTPUT OUT=COUNT_&F._C_NA (drop=_type_ _freq_) SUM=;
	PROC TRANSPOSE OUT=COUNT_&F._C_NA (rename=(_name_=VARIABLE col1=NA));RUN;
	PROC MEANS DATA=COUNT_&F._C_MISS NOPRINT;VAR &&VARS_&F._C;OUTPUT OUT=COUNT_&F._C_MISS (drop=_type_ _freq_) SUM=;
	PROC TRANSPOSE OUT=COUNT_&F._C_MISS (rename=(_name_=VARIABLE col1=NMISS));RUN;
	PROC MEANS DATA=COUNT_&F._C_NEG NOPRINT;VAR &&VARS_&F._C;OUTPUT OUT=COUNT_&F._C_NEG (drop=_type_ _freq_) SUM=;
	PROC TRANSPOSE OUT=COUNT_&F._C_NEG (rename=(_name_=VARIABLE col1=NEG));RUN;
	PROC MEANS DATA=COUNT_&F._C_ZERO NOPRINT;VAR &&VARS_&F._C;OUTPUT OUT=COUNT_&F._C_ZERO (drop=_type_ _freq_) SUM=;
	PROC TRANSPOSE OUT=COUNT_&F._C_ZERO (rename=(_name_=VARIABLE col1=ZERO));RUN;
	PROC MEANS DATA=COUNT_&F._C_POS NOPRINT;VAR &&VARS_&F._C;OUTPUT OUT=COUNT_&F._C_POS (drop=_type_ _freq_) SUM=;
	PROC TRANSPOSE OUT=COUNT_&F._C_POS (rename=(_name_=VARIABLE col1=POS));RUN;

	PROC SQL;
		CREATE TABLE CONT_STATS_&F&hh_memb
		AS SELECT A.VARIABLE, SUM(NA, NMISS, NEG, ZERO, POS,0) as TOT, NA, NMISS, SUM(NEG, ZERO, POS,0) as N, NEG, ZERO, POS,
		NMISS / sum(NEG,ZERO,POS,NMISS,0) AS PCT_MISS format=PERCENT9.2, 
		NEG/sum(NEG,ZERO,POS,0) AS PCT_NEG format=PERCENT9.2, 
		ZERO/sum(NEG,ZERO,POS,0) AS PCT_ZERO format=PERCENT9.2, 
		POS/sum(NEG,ZERO,POS,0) AS PCT_POS format=PERCENT9.2,
		"&F.-file&hh_memb" as FILE
		FROM COUNT_&F._C_NA A
		FULL JOIN COUNT_&F._C_MISS B ON A.VARIABLE=B.VARIABLE
		FULL JOIN COUNT_&F._C_NEG C  ON A.VARIABLE=C.VARIABLE
		FULL JOIN COUNT_&F._C_ZERO D ON A.VARIABLE=D.VARIABLE
		FULL JOIN COUNT_&F._C_POS E ON A.VARIABLE=E.VARIABLE
		;
		QUIT;
	RUN;
	PROC APPEND FORCE DATA=cont_STATS_&F&hh_memb BASE=cont_STATS;RUN;	
%end;

/*DISCRETE VARIABLES COUNTING*/

%if %symexist(VARS_&F._D) %then %do;
	DATA COUNT_&F._D_NA;SET &DS_1 (keep=&&VARS_&F._D &&VARS_&F._D_F &F.B010 where=(&F.B010=&YYYY));
	ARRAY X &&VARS_&F._D;
	ARRAY Y &&VARS_&F._D_F;
	DO I=1 TO DIM(X);IF X(I)=. AND Y(I) not in (-8,-1,.) THEN X(I)=1;ELSE X(I)=0;
	END; 
	DATA COUNT_&F._D_MISS;SET &DS_1 (keep=&&VARS_&F._D &&VARS_&F._D_F &F.B010 where=(&F.B010=&YYYY));
	ARRAY X &&VARS_&F._D;
	ARRAY Y &&VARS_&F._D_F;
	DO I=1 TO DIM(X);IF X(I)=. AND Y(I) in (-8,-1,.) THEN X(I)=1;ELSE X(I)=0;
	END; 
	DATA COUNT_&F._D_N;SET &DS_1 (keep=&&VARS_&F._D &&VARS_&F._D_F &F.B010 where=(&F.B010=&YYYY));
	ARRAY X &&VARS_&F._D;
	ARRAY Y &&VARS_&F._D_F;
	DO I=1 TO DIM(X);IF X(I) ne . THEN X(I)=1;ELSE X(I)=0;
	END; 

	PROC MEANS DATA=COUNT_&F._D_NA NOPRINT;VAR &&VARS_&F._D;OUTPUT OUT=COUNT_&F._D_NA (drop=_type_ _freq_) SUM=;
	PROC TRANSPOSE OUT=COUNT_&F._D_NA (rename=(_name_=VARIABLE col1=NA));RUN;
	PROC MEANS DATA=COUNT_&F._D_MISS NOPRINT;VAR &&VARS_&F._D;OUTPUT OUT=COUNT_&F._D_MISS (drop=_type_ _freq_) SUM=;
	PROC TRANSPOSE OUT=COUNT_&F._D_MISS (rename=(_name_=VARIABLE col1=NMISS));RUN;
	PROC MEANS DATA=COUNT_&F._D_N NOPRINT;VAR &&VARS_&F._D;OUTPUT OUT=COUNT_&F._D_N (drop=_type_ _freq_) SUM=;
	PROC TRANSPOSE OUT=COUNT_&F._D_N (rename=(_name_=VARIABLE col1=N));RUN;

	PROC SQL;
		CREATE TABLE DISC_STATS_&F&hh_memb 
		AS SELECT A.VARIABLE, SUM(NA, NMISS, N,0) as TOT, NA, NMISS, N,
		NMISS / sum(N,NMISS,0) AS PCT_MISS format=PERCENT9.2,
		"&F.-file&hh_memb" as FILE
		FROM COUNT_&F._D_NA A
		FULL JOIN COUNT_&F._D_MISS B ON A.VARIABLE=B.VARIABLE
		FULL JOIN COUNT_&F._D_N C  ON A.VARIABLE=C.VARIABLE
		;
		QUIT;
	RUN;
	PROC APPEND FORCE DATA=disc_STATS_&F&hh_memb BASE=disc_STATS;RUN;		
%end;

/*FLAG VARIABLES COUNTING*/

%if %symexist(VARS_&F._F) %then %do;
	DATA COUNT_&F._F_MISS;SET &DS_1 (keep=&&VARS_&F._F &F.B010 where=(&F.B010=&YYYY));
	ARRAY X &&VARS_&F._F;
	DO I=1 TO DIM(X);IF X(I) in (.,0) THEN X(I)=1;ELSE X(I)=0;
	END; 
	DATA COUNT_&F._F_POS;SET &DS_1 (keep=&&VARS_&F._F &F.B010 where=(&F.B010=&YYYY));
	ARRAY X &&VARS_&F._F;
	DO I=1 TO DIM(X);IF X(I) ne . & X(i)>0 THEN X(I)=1;ELSE X(I)=0;
	END; 
	PROC MEANS DATA=COUNT_&F._F_MISS NOPRINT;VAR &&VARS_&F._F;OUTPUT OUT=COUNT_&F._F_MISS (drop=_type_ _freq_) SUM=;
	PROC TRANSPOSE OUT=COUNT_&F._F_MISS (rename=(_name_=VARIABLE col1=NMISS));RUN;
	PROC MEANS DATA=COUNT_&F._F_POS NOPRINT;VAR &&VARS_&F._F;OUTPUT OUT=COUNT_&F._F_POS (drop=_type_ _freq_) SUM=;
	PROC TRANSPOSE OUT=COUNT_&F._F_POS (rename=(_name_=VARIABLE col1=POS));RUN;
	%DO J=1 %TO 8; 
		DATA COUNT_&F._F_NEG&J;SET &DS_1 (keep=&&&VARS_&F._F &F.B010 where=(&F.B010=&YYYY));
		ARRAY X &&&VARS_&F._F;
		DO I=1 TO DIM(X);IF X(I) ne . and X(I)=-&J THEN X(I)=1;ELSE X(I)=0;END;
		RUN; 
		
	PROC MEANS DATA=COUNT_&F._F_NEG&J NOPRINT;VAR &&&VARS_&F._F;OUTPUT OUT=COUNT_&F._F_NEG&J (drop=_type_ _freq_) SUM=;
	PROC TRANSPOSE OUT=COUNT_&F._F_NEG&J (rename=(_name_=VARIABLE col1=NEG&J));RUN;
	%END;
	PROC SQL;
		CREATE TABLE FLAG_STATS_&F&hh_memb 
		AS SELECT A.VARIABLE, REQFM2, REQFM3, REQFM4, REQFM5, NMISS,SUM(NEG8,NEG7,NEG6,NEG5,NEG4,NEG3,NEG2,NEG1,POS,0) as N,
		NEG8,NEG7,NEG6,NEG5,NEG4,NEG3,NEG2,NEG1,POS,
		NEG8  / sum(NEG8,NEG7,NEG6,NEG5,NEG4,NEG3,NEG2,NEG1,POS,0) AS PCT_NEG8 format=PERCENT9.2, 
		NEG7  / sum(NEG8,NEG7,NEG6,NEG5,NEG4,NEG3,NEG2,NEG1,POS,0) AS PCT_NEG7 format=PERCENT9.2, 
		NEG6  / sum(NEG8,NEG7,NEG6,NEG5,NEG4,NEG3,NEG2,NEG1,POS,0) AS PCT_NEG6 format=PERCENT9.2, 
		NEG5  / sum(NEG8,NEG7,NEG6,NEG5,NEG4,NEG3,NEG2,NEG1,POS,0) AS PCT_NEG5 format=PERCENT9.2, 
		NEG4  / sum(NEG8,NEG7,NEG6,NEG5,NEG4,NEG3,NEG2,NEG1,POS,0) AS PCT_NEG4 format=PERCENT9.2, 
		NEG3  / sum(NEG8,NEG7,NEG6,NEG5,NEG4,NEG3,NEG2,NEG1,POS,0) AS PCT_NEG3 format=PERCENT9.2, 
		NEG2  / sum(NEG8,NEG7,NEG6,NEG5,NEG4,NEG3,NEG2,NEG1,POS,0) AS PCT_NEG2 format=PERCENT9.2, 
		NEG1  / sum(NEG8,NEG7,NEG6,NEG5,NEG4,NEG3,NEG2,NEG1,POS,0) AS PCT_NEG1 format=PERCENT9.2, 
		POS  / sum(NEG8,NEG7,NEG6,NEG5,NEG4,NEG3,NEG2,NEG1,POS,0) AS PCT_POS format=PERCENT9.2, 
		"&F.-file&hh_memb" as FILE
		FROM COUNT_&F._F_MISS A
		FULL JOIN COUNT_&F._F_POS C  ON A.VARIABLE=C.VARIABLE
		FULL JOIN COUNT_&F._F_NEG8 D  ON A.VARIABLE=D.VARIABLE
		FULL JOIN COUNT_&F._F_NEG7 E  ON A.VARIABLE=E.VARIABLE
		FULL JOIN COUNT_&F._F_NEG6 F  ON A.VARIABLE=F.VARIABLE
		FULL JOIN COUNT_&F._F_NEG5 G  ON A.VARIABLE=G.VARIABLE
		FULL JOIN COUNT_&F._F_NEG4 H  ON A.VARIABLE=H.VARIABLE
		FULL JOIN COUNT_&F._F_NEG3 I  ON A.VARIABLE=I.VARIABLE
		FULL JOIN COUNT_&F._F_NEG2 J  ON A.VARIABLE=J.VARIABLE
		FULL JOIN COUNT_&F._F_NEG1 K  ON A.VARIABLE=K.VARIABLE
		LEFT JOIN COUNT_CFG L  ON A.VARIABLE=COMPRESS(L.ID!!"_F"," ")
		ORDER BY VARIABLE
		;
		QUIT;
	RUN;
	PROC APPEND FORCE DATA=flag_STATS_&F&hh_memb BASE=flag_STATS;RUN;	
%end;

%mend run_count;

%macro check_count_loop;
/*loop on 4 files*/
%run_count(D);
%run_count(H);
%run_count(P);
%run_count(R);/*HH-MEMBERS SELECTED BY DEFAULT*/
%run_count(R, hh_memb=N);/*NON HH-MEMBERS*/
/*delete temporary tables*/
proc datasets nodetails nolist;
delete count_:;
run;
DATA COUNT_CFG;
SET  IN_XML.COMP;
PROC SORT NODUPKEY;
  BY ID;
RUN;
/*adding order for grid variables*/;
DATA cont_STATS;
	SET cont_STATS;
	IF FILE = "R-fileN" THEN FILE = "R-non HH-members";
RUN;
DATA disc_STATS;
	SET disc_STATS;
	ORDER=0;
	IF SUBSTR(VARIABLE,1,2) = "RG" AND length(VARIABLE)=6 THEN ORDER=substr(VARIABLE,4,1)+0;
	IF SUBSTR(VARIABLE,1,2) = "RG" AND length(VARIABLE)=7 THEN ORDER=substr(VARIABLE,4,2)+0;
	IF FILE = "R-fileN" THEN FILE = "R-non HH-members";
RUN;
PROC SORT;BY FILE ORDER VARIABLE;RUN;
DATA flag_STATS;
	SET flag_STATS;
	ORDER=0;
	IF SUBSTR(VARIABLE,1,2) = "RG" AND length(VARIABLE)=6 THEN ORDER=substr(VARIABLE,4,1)+0;
	IF SUBSTR(VARIABLE,1,2) = "RG" AND length(VARIABLE)=7 THEN ORDER=substr(VARIABLE,4,2)+0;
	IF FILE = "R-fileN" THEN FILE = "R-non HH-members";
RUN;
PROC SORT;BY FILE ORDER VARIABLE;RUN;
DATA flag_STATS;
	SET flag_STATS;
	NEG5_L=lag(NEG5);
	NEG4_L=lag(NEG4);
	NEG2_L=lag(NEG2);
	NEG1_L=lag(NEG1);
	POS_L=lag(POS);
	VAR_L=lag(VARIABLE);
RUN;
%mend check_count_loop;

%macro rep_count;
%LOCAL SELRESP_CTR;

/*****************
 * CONTINUOUS VALUES     *
 *****************/

OPTIONS NODATE NONUMBER ORIENTATION=LANDSCAPE;
ODS &OUTPUTFORMAT FILE = "&OUT&_dirsp_%quote(&CC&YY.-Counting.&EXTENSION)" &OUTOPTION ;

TITLE1 "&CC - &YYYY / TRANSMISSION=&SS";
TITLE2 "NUMERIC VARIABLE COUNTINGS";
TITLE3 "YEAR &YYYY";

PROC REPORT DATA=cont_STATS;
COLUMNS FILE VARIABLE TOT NA NMISS N NEG ZERO POS PCT_MISS PCT_NEG PCT_ZERO PCT_POS;
	define FILE /display "File" format=$16. left; 
	define VARIABLE /display "Flag" format=$VARLABEL. left;
	define TOT /display "Total" format=8.0 ;
	define NA /display "Non-appl." format=8.0 ;
	define NMISS /display "Missing" format=8.0 ;
	define N /display "Filled" format=8.0 ;
	define NEG /display "Negative" format=8.0 ;
	define ZERO /display "Zero" format=8.0 ;
	define POS /display "Positive" format=8.0 ;
	define PCT_MISS /display "Missing" format=PERCENTN7.1 style=[background=Lightgrey];
	define PCT_NEG /display "Negative" format=PERCENTN7.1 style=[background=Lightgrey] ;
	define PCT_ZERO /display "Zero" format=PERCENTN7.1 style=[background=Lightgrey] ;
	define PCT_POS /display "Positive" format=PERCENTN7.1 style=[background=Lightgrey] ;
	compute file;
		if file="D-file" then call define (_col_,'style','style=[background=Lightblue');
		if file="H-file" then call define (_col_,'style','style=[background=Lightgreen');
		if file="R-file" then call define (_col_,'style','style=[background=Salmon');
		if file="R-non HH-members" then call define (_col_,'style','style=[background=LightSalmon');
		if file="P-file" then call define (_col_,'style','style=[background=Lightyellow');
	endcomp;
	compute VARIABLE;
		if file="D-file" then call define (_col_,'style','style=[background=Lightblue');
		if file="H-file" then call define (_col_,'style','style=[background=Lightgreen');
		if file="R-file" then call define (_col_,'style','style=[background=Salmon');
		if file="R-non HH-members" then call define (_col_,'style','style=[background=LightSalmon');
		if file="P-file" then call define (_col_,'style','style=[background=Lightyellow');
	endcomp;
	compute PCT_MISS;
		if PCT_MISS>=0.1 then call define (_col_,'style','style=[background=yellow');
		if PCT_MISS>=0.2 then call define (_col_,'style','style=[background=orange');
		if PCT_MISS>=0.5 then call define (_col_,'style','style=[background=red');
		if PCT_MISS>=1   then call define (_col_,'style','style=[background=crimson');
		if PCT_MISS in (1,.)   and FILE ="R-non HH-members" and VARIABLE NOT IN (&HH_MEMBERSHIP_MANDATORY) then call define (_col_,'style','style=[background=lightgreen');
		if PCT_MISS not in (1,.)   and FILE ="R-non HH-members" and VARIABLE NOT IN (&HH_MEMBERSHIP_MANDATORY) then call define (_col_,'style','style=[background=crimson');
	endcomp;
run;


/*****************
 * DISCRETE VALUES    *
 *****************/
TITLE1 "NOMINAL VARIABLE COUNTINGS";
TITLE2 "&CC - &YYYY / TRANSMISSION=&SS";
TITLE3 "YEAR &YYYY";

PROC REPORT DATA=disc_STATS;
COLUMNS FILE VARIABLE TOT NA NMISS N PCT_MISS;
	define FILE /display "File" format=$16. left; 
	define VARIABLE /display "Flag" format=$VARLABEL. left;
	define TOT /display "Total" format=8.0 ;
	define NA /display "Non-appl." format=8.0 ;
	define NMISS /display "Missing" format=8.0 ;
	define N /display "Filled" format=8.0 ;
	define PCT_MISS /display "Missing" format=PERCENTN7.1 style=[background=Lightgrey];
	compute file;
		if file="D-file" then call define (_col_,'style','style=[background=Lightblue');
		if file="H-file" then call define (_col_,'style','style=[background=Lightgreen');
		if file="R-file" then call define (_col_,'style','style=[background=Salmon');
		if file="R-non HH-members" then call define (_col_,'style','style=[background=LightSalmon');
		if file="P-file" then call define (_col_,'style','style=[background=Lightyellow');
	endcomp;
	compute VARIABLE;
		if file="D-file" then call define (_col_,'style','style=[background=Lightblue');
		if file="H-file" then call define (_col_,'style','style=[background=Lightgreen');
		if file="R-file" then call define (_col_,'style','style=[background=Salmon');
		if file="R-non HH-members" then call define (_col_,'style','style=[background=LightSalmon');
		if file="P-file" then call define (_col_,'style','style=[background=Lightyellow');
	endcomp;
	compute PCT_MISS;
		if PCT_MISS>=0.1 then call define (_col_,'style','style=[background=yellow');
		if PCT_MISS>=0.2 then call define (_col_,'style','style=[background=orange');
		if PCT_MISS>=0.5 then call define (_col_,'style','style=[background=red');
		if PCT_MISS>=1   then call define (_col_,'style','style=[background=crimson');
		if PCT_MISS in (1,.)   and FILE ="R-non HH-members" and VARIABLE NOT IN (&HH_MEMBERSHIP_MANDATORY) then call define (_col_,'style','style=[background=lightgreen');
		if PCT_MISS not in (1,.)   and FILE ="R-non HH-members" and VARIABLE NOT IN (&HH_MEMBERSHIP_MANDATORY) then call define (_col_,'style','style=[background=crimson');
	endcomp;
run;


TITLE1;
TITLE2;
TITLE3;

/*****************
 *     FLAGS     *
 *****************/
TITLE1 "FLAG COUNTINGS";
TITLE2 "&CC - &YYYY / TRANSMISSION=&SS";
TITLE3 "YEAR &YYYY";


	/*TO SEE IF COUNTRY HAS SELECTED RESPONDANTS*/
	%LET SELRESP_CTR=0;
	DATA _NULL_;
		SET  SVAL_SELRESP_CNTRY_LST (WHERE=(CNTRY="&CC")) NOBS=NOBS;
		CALL SYMPUTX('SELRESP_CTR',NOBS);
	RUN;

	/*COMPUTE MESSAGE*/
	DATA flag_STATS;set flag_STATS;
		length MESSAGE $250;
		if FILE ="R-non HH-members" and (NMISS=0 or N>0) and VARIABLE NOT IN (&HH_MEMBERSHIP_MANDATORY_F) then MESSAGE="Flags must be missing for non-HH members";
		if FILE ne "R-non HH-members" then do;
			IF SUBSTR(VARIABLE,1,2) = "RG" THEN DO;
				if NMISS>0 then MESSAGE=COMPBL(MESSAGE!!"|Flags can not have a missing value|");

				if VARIABLE = "RG_1_F" and POS =0 then MESSAGE=COMPBL(MESSAGE!!"| '>0' can not be 0 for RG_1_F |");
				if VARIABLE NE "RG_1_F" and POS >  POS_L then MESSAGE=COMPBL(MESSAGE!!"| '>0' must decrease |");
				if NEG2=0 and VARIABLE = "RG_1_F" then MESSAGE=COMPBL(MESSAGE!!"| '-2' can not be 0 for RG_1_F |");
				if NEG2 ne POS_L+NEG1_L-POS-NEG1 and VARIABLE NE "RG_1_F" 
					then MESSAGE=COMPBL(MESSAGE!!"| Flags('-2' + '-1' + '>0') of "!!VARIABLE!!"must be equal to flags('>0' + '-1') of "!!VAR_L!!" |");
				if NEG4>0 and VARIABLE = "RG_1_F" then MESSAGE=COMPBL(MESSAGE!!"| '-4' must be 0 for RG_1_F |");
				if NEG4+NEG5 ne NEG5_L+NEG4_L+NEG2_L and VARIABLE NE "RG_1_F" 
					then MESSAGE=COMPBL(MESSAGE!!"| Flags('-4' + '-5') of "!!VARIABLE!!" must be equal to flags('-2' + '-4' + '-5') of "!!VAR_L!!" |");
				if NEG5>0 and VARIABLE = "RG_1_F" then MESSAGE=COMPBL(MESSAGE!!"| '-5' must be 0 for RG_1_F |");
			END;
			ELSE DO;
				if NMISS>0 then MESSAGE=COMPBL(MESSAGE!!"| Flags can not have a missing value |");
				if NEG2=0 and REQFM2 ="Y" then MESSAGE=COMPBL(MESSAGE!!"| '-2' can not be 0 for "!!VARIABLE!!" |");
				%IF &SELRESP_CTR >0 %THEN %DO;
				if NEG3=0 and REQFM3 in ("S","Y") then MESSAGE=COMPBL(MESSAGE!!"| '-3' can not be 0 for "!!VARIABLE!!" |");
				if NEG3>0 and REQFM3 not in ("S","Y") then MESSAGE=COMPBL(MESSAGE!!"| '-3' must be 0 for "!!VARIABLE!!" |");
				%END;
				if NEG4=0 and REQFM4 ="Y" then MESSAGE=COMPBL(MESSAGE!!" '-4' can not be 0 for "!!VARIABLE!!" |");
				if NEG5>0 and NEG5<N and REQFM5 ="Y" then MESSAGE=COMPBL(MESSAGE!!"| '-5' must be equal to 0 or to N for "!!VARIABLE!!" |");
			END;
		end;
		IF MESSAGE="" THEN MESSAGE=" ";
	run;

	%IF &SELRESP_CTR >0 %THEN %DO;
	PROC REPORT DATA=flag_STATS (WHERE=(SUBSTR(VARIABLE,1,2) NE "RG" AND REQFM3 NE "S"));
		COLUMNS FILE VARIABLE REQFM2 REQFM3 REQFM4 REQFM5 N NMISS NMISS_1 N_1 NEG8 NEG7 NEG6 NEG5 NEG4 NEG3 NEG2 NEG1 POS MESSAGE;
	%END;
	%ELSE %DO;
	PROC REPORT DATA=flag_STATS (WHERE=(SUBSTR(VARIABLE,1,2) NE "RG"));
		COLUMNS FILE VARIABLE REQFM2 REQFM4 REQFM5 N NMISS NMISS_1 N_1 NEG8 NEG7 NEG6 NEG5 NEG4 NEG3 NEG2 NEG1 POS MESSAGE;
	%END;
		define FILE /display "File" format=$16. left; 
		define REQFM2 / noprint; 
		define REQFM4 / noprint; 
		define REQFM5 / noprint; 
		define VARIABLE /display "Flag" format=$VARLABEL. left; 
		define N /analysis noprint ;
		define NMISS /analysis noprint ;
		define NMISS_1 /computed "Missing" format=8.0 ;
		define N_1 /computed "Filled" format=8.0 ;
		define NEG8 /display '-8'  format=8.0 ; 
		define NEG7 /display '-7'  format=8.0 ; 
		define NEG6 /display '-6'  format=8.0 ; 
		define NEG5 /display '-5'  format=8.0 ; 
		define NEG4 /display '-4'  format=8.0 ; 
		define NEG2 /display '-2'  format=8.0 ; 
		define NEG1 /display '-1'  format=8.0 ; 
		define POS /display '>0'  format=8.0 ; 
		define MESSAGE /display 'MESSAGE__________'  format=$200. ; 
		%IF &SELRESP_CTR >0 %THEN %DO;
		define REQFM3 / noprint; 
		define NEG3 /display '-3' format=8.0 ; 
		%END;
		compute file;
			if file="D-file" then call define (_col_,'style','style=[background=Lightblue');
			if file="H-file" then call define (_col_,'style','style=[background=Lightgreen');
			if file="R-file" then call define (_col_,'style','style=[background=Salmon');
			if file="R-non HH-members" then call define (_col_,'style','style=[background=LightSalmon');
			if file="P-file" then call define (_col_,'style','style=[background=Lightyellow');
		endcomp;
		compute VARIABLE;
			if file="D-file" then call define (_col_,'style','style=[background=Lightblue');
			if file="H-file" then call define (_col_,'style','style=[background=Lightgreen');
			if file="R-file" then call define (_col_,'style','style=[background=Salmon');
			if file="R-non HH-members" then call define (_col_,'style','style=[background=LightSalmon');
			if file="P-file" then call define (_col_,'style','style=[background=Lightyellow');
		endcomp;
		compute NMISS_1;
			NMISS_1=NMISS.sum;
			if NMISS_1>0 then call define (_col_,'style','style=[background=red');
			if NMISS_1>0 and N.sum=0 and FILE ="R-non HH-members" and VARIABLE NOT IN (&HH_MEMBERSHIP_MANDATORY_F) then call define (_col_,'style','style=[background=lightgreen');
		endcomp;
		compute N_1;
			N_1=N.sum;
			if N_1>0 and FILE ="R-non HH-members" and VARIABLE NOT IN (&HH_MEMBERSHIP_MANDATORY_F) then call define (_col_,'style','style=[background=red');
		endcomp;
		compute NEG1;
			if NEG1>0 and FILE ="R-non HH-members" and VARIABLE NOT IN (&HH_MEMBERSHIP_MANDATORY_F) then call define (_col_,'style','style=[background=red');
		endcomp;
		compute NEG2;
			if file NE "R-non HH-members" then do;
				if NEG2=0 and REQFM2 ="Y" then call define (_col_,'style','style=[background=red');
				if NEG2>0 and REQFM2 ="Y" then call define (_col_,'style','style=[background=lightgreen');
			end;
			if NEG2>0 and FILE ="R-non HH-members" and VARIABLE NOT IN (&HH_MEMBERSHIP_MANDATORY_F) then call define (_col_,'style','style=[background=red');
		endcomp;
		%IF &SELRESP_CTR >0 %THEN %DO;
		compute NEG3;
			if file NE "R-non HH-members" then do;
				if NEG3=0 and REQFM3 in ("S","Y") then call define (_col_,'style','style=[background=red');
				if NEG3>0 and REQFM3 in ("S","Y") then call define (_col_,'style','style=[background=lightgreen');
				if NEG3>0 and REQFM3 not in ("S","Y") then call define (_col_,'style','style=[background=red');
			end;
		endcomp;
		%END;
		compute NEG4;
			if file NE "R-non HH-members" then do;
				if NEG4=0 and REQFM4 ="Y" then call define (_col_,'style','style=[background=red');
				if NEG4>0 and REQFM4 ="Y" then call define (_col_,'style','style=[background=lightgreen');
			end;
			if NEG4>0 and FILE ="R-non HH-members" and VARIABLE NOT IN (&HH_MEMBERSHIP_MANDATORY_F) then call define (_col_,'style','style=[background=red');
		endcomp;
		compute NEG5;
			if file NE "R-non HH-members" then do;
				if NEG5>0 and NEG5<N.sum and REQFM5 ="Y" then call define (_col_,'style','style=[background=red');
				if (NEG5=0 or NEG5=N.sum) and REQFM5 ="Y" then call define (_col_,'style','style=[background=lightgreen');
			end;
			if NEG5>0 and FILE ="R-non HH-members" and VARIABLE NOT IN (&HH_MEMBERSHIP_MANDATORY_F) then call define (_col_,'style','style=[background=red');
		endcomp;
		compute NEG6;
			if NEG6>0 and FILE ="R-non HH-members" and VARIABLE NOT IN (&HH_MEMBERSHIP_MANDATORY_F) then call define (_col_,'style','style=[background=red');
		endcomp;
		compute NEG7;
			if NEG7>0 and FILE ="R-non HH-members" and VARIABLE NOT IN (&HH_MEMBERSHIP_MANDATORY_F) then call define (_col_,'style','style=[background=red');
		endcomp;
		compute NEG8;
			if NEG8>0 and FILE ="R-non HH-members" and VARIABLE NOT IN (&HH_MEMBERSHIP_MANDATORY_F) then call define (_col_,'style','style=[background=red');
		endcomp;
	run;


	PROC REPORT DATA=flag_STATS (WHERE=(SUBSTR(VARIABLE,1,2) = "RG"));
		COLUMNS FILE VARIABLE 
		NEG5_L NEG4_L NEG2_L NEG1_L POS_L 
		N NMISS NEG8 NEG7 NEG6 NEG5 NEG4 NEG2 NEG1 POS 
		NMISS_1 N_1 NEG8_1 NEG7_1 NEG6_1 NEG5_1 NEG4_1 NEG2_1 NEG1_1 POS_1 MESSAGE;

		define FILE /display "File" format=$16. left; 
		define VARIABLE /display "Flag" format=$VARLABEL. left; 
		define NEG5_L /analysis noprint ; 
		define NEG4_L /analysis noprint ; 
		define NEG2_L /analysis noprint ; 
		define NEG1_L /analysis noprint ; 
		define POS_L /analysis noprint ; 

		define NMISS /analysis noprint ;
		define N /analysis noprint ;
		define NEG8 /analysis noprint  ; 
		define NEG7 /analysis noprint  ; 
		define NEG6 /analysis noprint  ; 
		define NEG5 /analysis noprint  ; 
		define NEG4 /analysis noprint ; 
		define NEG2 /analysis noprint ; 
		define NEG1 /analysis noprint ; 
		define POS /analysis noprint; 

		/*we define all these var to be sure values are known because declared before*/
		define NMISS_1 /computed "Missing" format=8.0 ;
		define N_1 /computed "Filled" format=8.0 ;
		define NEG8_1 /computed '-8'  format=8.0 ; 
		define NEG7_1 /computed '-7'  format=8.0 ; 
		define NEG6_1 /computed '-6'  format=8.0 ; 
		define NEG5_1 /computed '-5'  format=8.0 ; 
		define NEG4_1 /computed '-4'  format=8.0 ; 
		define NEG2_1 /computed '-2'  format=8.0 ; 
		define NEG1_1 /computed '-1'  format=8.0 ; 
		define POS_1 /computed '> 0'  format=8.0 ; 

		define MESSAGE /display 'MESSAGE__________'  format=$200. ; 

		compute file;
			if file="D-file" then call define (_col_,'style','style=[background=Lightblue');
			if file="H-file" then call define (_col_,'style','style=[background=Lightgreen');
			if file="R-file" then call define (_col_,'style','style=[background=Salmon');
			if file="R-non HH-members" then call define (_col_,'style','style=[background=LightSalmon');
			if file="P-file" then call define (_col_,'style','style=[background=Lightyellow');
		endcomp;
		compute VARIABLE;
			if file="D-file" then call define (_col_,'style','style=[background=Lightblue');
			if file="H-file" then call define (_col_,'style','style=[background=Lightgreen');
			if file="R-file" then call define (_col_,'style','style=[background=Salmon');
			if file="R-non HH-members" then call define (_col_,'style','style=[background=LightSalmon');
			if file="P-file" then call define (_col_,'style','style=[background=Lightyellow');
		endcomp;
		compute NMISS_1;
			NMISS_1=NMISS.sum;
			if NMISS_1>0 then call define (_col_,'style','style=[background=red');
			if NMISS_1>0 and N.sum=0 and FILE ="R-non HH-members" and VARIABLE NOT IN (&HH_MEMBERSHIP_MANDATORY_F) then call define (_col_,'style','style=[background=lightgreen');
		endcomp;
		compute N_1;
			N_1=N.sum;
		endcomp;
		compute NEG8_1;
			NEG8_1=NEG8.sum;
		endcomp;
		compute NEG7_1;
			NEG7_1=NEG7.sum;
		endcomp;
		compute NEG6_1;
			NEG6_1=NEG6.sum;
		endcomp;
		compute NEG5_1;
			NEG5_1=NEG5.sum;
			if file NE "R-non HH-members" then do;
				if NEG5_1>0 and VARIABLE = "RG_1_F" then call define (_col_,'style','style=[background=red');
				if NEG5_1=0 and VARIABLE = "RG_1_F" then call define (_col_,'style','style=[background=lightgreen');
				if NEG5_1 ne NEG5_L.sum+NEG4_L.sum+NEG2_L.sum-NEG4.sum and VARIABLE NE "RG_1_F" then call define (_col_,'style','style=[background=red');
				if NEG5_1 =  NEG5_L.sum+NEG4_L.sum+NEG2_L.sum-NEG4.sum and VARIABLE NE "RG_1_F" then call define (_col_,'style','style=[background=lightgreen');
			end;
		endcomp;
		compute NEG4_1;
			NEG4_1=NEG4.sum;
			if file NE "R-non HH-members" then do;
				if NEG4_1>0 and VARIABLE = "RG_1_F" then call define (_col_,'style','style=[background=red');
				if NEG4_1=0 and VARIABLE = "RG_1_F" then call define (_col_,'style','style=[background=lightgreen');
				if NEG4_1 ne NEG5_L.sum+NEG4_L.sum+NEG2_L.sum-NEG5.sum and VARIABLE NE "RG_1_F" then call define (_col_,'style','style=[background=red');
				if NEG4_1 =  NEG5_L.sum+NEG4_L.sum+NEG2_L.sum-NEG5.sum and VARIABLE NE "RG_1_F" then call define (_col_,'style','style=[background=lightgreen');
			end;
		endcomp;
		compute NEG2_1;
			NEG2_1=NEG2.sum;
			if file NE "R-non HH-members" then do;
				if NEG2_1=0 and VARIABLE = "RG_1_F" then call define (_col_,'style','style=[background=red');
				if NEG2_1>0 and VARIABLE = "RG_1_F" then call define (_col_,'style','style=[background=lightgreen');
				if NEG2_1 ne POS_L.sum+NEG1_L.sum-POS.sum-NEG1.sum and VARIABLE NE "RG_1_F" then call define (_col_,'style','style=[background=red');
				if NEG2_1 =  POS_L.sum+NEG1_L.sum-POS.sum-NEG1.sum and VARIABLE NE "RG_1_F" then call define (_col_,'style','style=[background=lightgreen');
			end;
		endcomp;
		compute NEG1_1;
			NEG1_1=NEG1.sum;
		endcomp;
		compute POS_1;
			POS_1=POS.sum;
			if file NE "R-non HH-members" then do;
				if VARIABLE NE "RG_1_F" and POS_1 >  POS_L.sum then call define (_col_,'style','style=[background=red');
				if VARIABLE NE "RG_1_F" and POS_1 <= POS_L.sum then call define (_col_,'style','style=[background=lightgreen');
				if VARIABLE = "RG_1_F" and POS_1 =0 then call define (_col_,'style','style=[background=red');
				if VARIABLE = "RG_1_F" and POS_1 >0 then call define (_col_,'style','style=[background=lightgreen');
			end;
		endcomp;
	run;



TITLE1;
TITLE2;
TITLE3;

DATA HEADER (KEEP=MSG);
  LENGTH MSG T $125;
  LABEL MSG = 'VALIDATION STATUS';
  T = PUT(DATETIME(),DATETIME20.);
  MSG = "VALUE & FLAG COUNTINGS ANALYSIS ACCOMPLISHED ON " || COMPRESS(T);
RUN;

PROC REPORT DATA=HEADER;
  COLUMN MSG;
RUN;

ODS &OUTPUTFORMAT CLOSE;


%mend rep_count;
