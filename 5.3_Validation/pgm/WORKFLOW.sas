/**
 * Revision History
 * - November 8th 2022 
 */
%LET _SILC_IOERR_=0;
%LET _IOERR_=0;
%LET OUT=%SYSFUNC(PATHNAME(OUT));
options compress=no;



%MACRO VALID_INCLUDE;
/*only executed with sas foundation, not with sas EG*/
%IF %SYMEXIST(SRC) %THEN %DO;
    %INCLUDE "&SRC&_dirsp_%quote(XML_DEBUG.sas)";
    %INCLUDE "&SRC&_dirsp_%quote(XML_VERSION_COMP.sas)";
    %INCLUDE "&SRC&_dirsp_%quote(XML_COLLECT.sas)";
    %INCLUDE "&SRC&_dirsp_%quote(XML_CHECK.sas)";
    %INCLUDE "&SRC&_dirsp_%quote(XML_INCOME.sas)";
    %INCLUDE "&SRC&_dirsp_%quote(XML_GRID.sas)";
    %INCLUDE "&SRC&_dirsp_%quote(XML_SVAL.sas)";
    %INCLUDE "&SRC&_dirsp_%quote(XML_SCL.sas)";
    %INCLUDE "&SRC&_dirsp_%quote(XML_DERIVED.sas)";
    %INCLUDE "&SRC&_dirsp_%quote(XML_LVAL_PRE.sas)";
    %INCLUDE "&SRC&_dirsp_%quote(XML_LVAL.sas)";
    %INCLUDE "&SRC&_dirsp_%quote(XML_SAMP_SIZE.sas)";
    %INCLUDE "&SRC&_dirsp_%quote(XML_WEIGHT.sas)";
    %INCLUDE "&SRC&_dirsp_%quote(XML_OUTL.sas)";
    %INCLUDE "&SRC&_dirsp_%quote(XML_COMPARISON.sas)";
    %INCLUDE "&SRC&_dirsp_%quote(XML_COUNT.sas)";
    %INCLUDE "&SRC&_dirsp_%quote(XML_SPLIT.sas)";
    %INCLUDE "&SRC&_dirsp_%quote(XML_SUMMARY.sas)";
%END;

%MEND VALID_INCLUDE;

%MACRO VALID_WORKFLOW(F=);

%LET _doGRID_=0;

OPTIONS FmtErr;*NOSOURCE NOMPRINT NOMLOGIC;

/*** SILC-FILE-SPECIFIC HANDLING */
%IF &F EQ D OR &F EQ R OR &F EQ H OR &F EQ P %THEN %DO;
    /*** COLLECT */
    %IF &_SPLIT_MODE_ EQ _ANTE_ %THEN %DO;
            %getSVAL (YR=&YYYY,MODE=_POST_);
            %getSVAL (YR=&_SPLIT_CUT_,MODE=_ANTE_);
            %svalxml_SPLITS;
    %END;
    %ELSE %DO;
            %getSVAL (YR=&YYYY);
    %END;

   /*** EXTERNAL CODE-LISTs HANDLING*/
    %getSCL_GEO;
    %getSCL_NACE;
    %getSCL_ISCO;
    %getSCL_NUTS;

    /** VARIABLE LABLELS FORMATS*/
    %IF &_SPLIT_MODE_ NE _POST_ %THEN %DO;
	    DATA VARLABELS (KEEP=START END LABEL FMTNAME TYPE);
	    SET  SVAL;
		    START = VARIABLE; 
			END = START; 
			FMTNAME = 'VARLABEL'; 
			TYPE = 'C';
	    	LABEL = TRIM(VARIABLE) || ' - ' || TRIM(LABEL);
	    RUN;

	    PROC FORMAT CNTLIN=VARLABELS LIBRARY=WORK;RUN;
    %END;

    /*** XML_COLLECT*/
    %LET _IOERR_=0;
	    %FETCH_SILC_CSV(F=&F,VER=&FVER);
    %LET _SILC_IOERR_=%SYSFUNC(MAX(&_SILC_IOERR_,&_IOERR_));

    /*
	%IF &_SPLIT_MODE_ NE %THEN %DO;%purge_SPLITS (F=&F);%END;
	*/
    %purge_SPLITS (F=&F);

    %IF &_IOERR_ EQ 0 %THEN %DO;
        /*** XML_CHECK*/
        %getSVAL_F(F=&F);
        %chkEXIST(F=&F);
        %purgeVAR(F=&F);
        %chkCOMPL(F=&F);
		%chkTRANS_LIST(F=&F);
        %chkFLAG(F=&F);
        %chkFMT(F=&F);
        %purgeVAR(F=&F,STEP=2);
        /*** XML_INCOME*/
        %SPLIT_INCOME_XML(F=&F,IMPUTE=N);
        %SPLIT_INCOME_XML(F=&F,IMPUTE=Y); /* new-style vars */
    %END;

	/*** XML_SVAL*/
    %IF &_IOERR_ EQ 0 %THEN %DO;
        %check_VARIABLES(F=&F);                                                                                                 
        %check_VARIABLES(F=&F,MODE=LIST);                                                                                       
        %check_BRK_LOV (F=&F);                                                                                                  
        %check_FLAGS(F=&F);                                                                                                     
        %check_BRK_FLAG (F=&F);                                                                                                 
        %check_FLAG_LENGTHS(F=&F);                                                                                              
        %check_V_FLAGS(F=&F);
/*		%IF &F EQ H or &F EQ P %THEN %DO;%check_IMPUT_FACTORS(F=&F);%END;*/
		%IF &_SPLIT_MODE_ NE _ANTE_ %THEN %DO;
			%check_IMPUT_FACTORS(F=&F);
        %END;

         /*** R + P MERGE*/
        %IF &F EQ P %THEN %DO;
            %IF %SYSFUNC(EXIST(RAW.&_SPLIT_MODE_&ss&cc&YY.R)) %THEN %DO;
                %PUT *I* XML_DERIVED: RECONCILIATION (P vs R) ...;

				DATA SELRES (KEEP=RB010 RB030 RB245 SELRES AGE RHID RENAME=(RB010=PB010 RB030=PB030 RHID=PHID));
                SET RAW.&_SPLIT_MODE_&ss&cc&YY.R (where=(RB110 lt 5));
                    SELRES = RB245;

                PROC SORT NODUPKEY OUT=SELRES (INDEX=(PK=(PB010 PB030)/UNIQUE));
                  BY PB010 PB030;
                RUN;

                DATA RAW.&_SPLIT_MODE_&ss&cc&YY.P;
                SET  RAW.&_SPLIT_MODE_&ss&cc&YY.P;
                SET  SELRES KEY=PK/UNIQUE;
                        _ERROR_ = 0;
                RUN;
            %END;
        %END;
        %check_MINUS7(F=&F,MOD=ADHOC);    
        %check_MINUS7(F=&F,MOD=RECUR3);
        %check_MINUS7(F=&F,MOD=RECUR6);
        %check_MINUS7(F=&F);                                                                                                
        %check_NA(F=&F);
        %check_CHKSUM(F=&F);
        %check_CHILDNULL(F=&F);
        %check_PARENT(F=&F);
        %check_UNIQUE(F=&F);

        PROC SQL NOPRINT;
          SELECT MIN(COUNT(*),1) INTO :_doGRID_
          FROM   SVAL_&F
          WHERE  TYPE = 'HGRID';
        QUIT;

        %PUT *I* doGRID: &_doGRID_ ;
        %IF &_doGRID_ EQ 1 AND &F EQ R %THEN %DO;%check_GRID(F=&F);%END;
        %check_MISS(F=&F);
        %IF &F EQ R %THEN %DO;
        %purge_NON_HH_MEMBER(F=&F);
        %END;
        %count_MISS(F=&F);
		%purge_FALSE_ANTE_ERRORS_24_25(F=&F);
        %rep_SVAL(F=&F);
        %PUT *I* STRUCTURAL VALIDATION ACCOMPLISHED ON &F
        ;
    %END;
%END;

/*** OVERALL SILC DATA HANDLING*/
%ELSE %DO;
    /*** DERIVED VARIABLES*/
    %IF &_SILC_IOERR_ EQ 0 %THEN %DO;
	    %PUT *I* DERIVED VARIABLE COMPUTATION FOR SILC;
	    %comp_DERIVED;
    %END;
%END;

%MEND VALID_WORKFLOW;

%MACRO VALID_MAIN;
%global MODE _SPLIT_MODE_ _SPLIT_CUT_;

%LET _IOERR_=0;
%LET PROMPT_CHECK=1;

%IF &PROMPT_CHECK EQ 1 %THEN %DO;
    /**************************/
    /*** GET FILES VERSION    */
    /**************************/
	%get_VERSION;

    /**************************/
    /*** STRUCTURAL VALIDATION*/
    /**************************/
    /*** For 2023 onwards : no split year*/
	%if &YYYY>=2024 %then %do;
		%let MODE=S;%let _SPLIT_MODE_=;%let _SPLIT_CUT_=2020;
		    %VALID_WORKFLOW(F=D);
		    %VALID_WORKFLOW(F=R);
		    %VALID_WORKFLOW(F=H);
		    %VALID_WORKFLOW(F=P);
	%end;
	%else %do;
    /*** Before 2023  : split year = 2020 and structural checks run twice : _ANTE_ (2020 and before) and _POST_ (after 2020)*/
		%let MODE=S;%let _SPLIT_MODE_=_ANTE_;%let _SPLIT_CUT_=2020;
		    %VALID_WORKFLOW(F=D);
		    %VALID_WORKFLOW(F=R);
		    %VALID_WORKFLOW(F=H);
		    %VALID_WORKFLOW(F=P);
		%let MODE=S;%let _SPLIT_MODE_=_POST_;%let _SPLIT_CUT_=2020;
	        %VALID_WORKFLOW(F=D);
	        %VALID_WORKFLOW(F=R);
	        %VALID_WORKFLOW(F=H);
	        %VALID_WORKFLOW(F=P);
	%end;

    /**************************/
    /*** LOGICAL VALIDATION   */
    /**************************/
	%let MODE=L;%let _SPLIT_MODE_=;%let _SPLIT_CUT_=;

    /*** 2-SPLIT  RECONCILIATION (_SPLIT_MODE_ = _POST_) (useless after 2023)*/
    %PUT *I* MERGING SPLITS, IF ANY ... ;
    %mrg_SPLITS(F=D);
    %mrg_SPLITS(F=R);
    %mrg_SPLITS(F=H);
    %mrg_SPLITS(F=P);

    /*** DERIVED VARIABLE COMPUTATION*/
    %VALID_WORKFLOW;

    /*** PURGING OF THE TEMPORARY VARIABLES*/
    %clean_DS(F=D);
    %clean_DS(F=R);
    %clean_DS(F=H);
    %clean_DS(F=P);

    /*** LOGICAL VALIDATION*/
    %getSVAL (YR=&YYYY); /* SVAL XML MUST BE FETCHED AGAIN IF THE ELABORATION STARTS HERE */
    %getLVAL;
    %getSQL;
    %purgeSQL;
    %check_SQL;

    /*** SAMP SIZE CHECK*/
    %check_SAMP_SIZE;

    /*** WEIGHT VALIDATION*/
    %check_WEIGHT;

    /*** OUTLIER DETECTION*/
    %getOUTLCFG;
    %check_OUTL;
    %rep_OUTL;

    /*** COMPARATIVE ANALYSIS vs PP*/
    %comp_setup;
    %check_comparison_loop;
    %rep_comparison;
    /*** COUNT ANALYSIS*/
    %count_setup;
    %check_count_loop;
    %rep_count;

	%rep_VERSION;
	%rep_SUMMARY;


    %PUT *I* WRITING SILC RAW DATASETS ... ;
    LIBNAME RAW_DB "&eusilc&_dirsp_%quote(main)&_dirsp_%LOWCASE(&CC)&_dirsp_%LOWCASE(&SS&YY)"  compress=yes;
    DATA RAW_DB.&ss&cc&YY.D;SET RAW.&ss&cc&YY.D;RUN;
	DATA RAW_DB.&ss&cc&YY.R;SET RAW.&ss&cc&YY.R;RUN;
	DATA RAW_DB.&ss&cc&YY.H;SET RAW.&ss&cc&YY.H;RUN;
	DATA RAW_DB.&ss&cc&YY.P;SET RAW.&ss&cc&YY.P;RUN;

%END;

%MEND VALID_MAIN;

%MACRO clean_DS(F=P);

%LOCAL VARS;

PROC CONTENTS DATA=RAW.&ss&cc&YY.&F OUT=MEMBERS NOPRINT;
RUN;

PROC SQL NOPRINT;
  SELECT TRIM(NAME) INTO :VARS SEPARATED BY ' '
  FROM   MEMBERS
  WHERE  UPCASE(NAME) LIKE '%_INPUT' OR UPCASE(NAME) = 'I'
  ;
QUIT;

%IF &VARS NE %THEN %DO;
    DATA RAW.&ss&cc&YY.&F;
    SET RAW.&ss&cc&YY.&F (DROP=&VARS);
    RUN;
%END;

%MEND clean_DS;

%PUT _USER_;

%VALID_INCLUDE;
%VALID_MAIN;
