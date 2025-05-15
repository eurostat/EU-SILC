/**
 * Revision History
 * - APR 28th 2022
 */


%macro comp_setup;

%LOCAL INDIR INFNAME MAPDIR MAPNAME;

%LET INDIR=%SYSFUNC(PATHNAME(XMLCFG))&_dirsp_%quote(COMP);
%LET INFNAME=COMP-R-&YYYY..xml;

/*Import list of variables to be compared*/
LIBNAME IN_XML XMLV2 "&INDIR&_dirsp_%quote(&INFNAME)" XMLMAP="%SYSFUNC(PATHNAME(MAPDIR))&_dirsp_%quote(COMP.map)";

DATA COMP_THRES_in;
SET  IN_XML.COMP_THRES;
PROC SORT NODUPKEY;
  BY CONTEXT TYPE;
RUN;

PROC TRANSPOSE OUT=COMP_THRES;
  BY CONTEXT;
  ID TYPE;
  VAR TOL; 
RUN;

DATA COMP_CFG;
SET  IN_XML.COMP;
PROC SORT NODUPKEY;
  BY ID;
RUN;

/*declare parameters for previous year*/
%IF &YY GT 10 %THEN %LET YY_0=%eval(&YY-1);
%IF &YY LT 11 %THEN %LET YY_0=0%eval(&YY-1);

/*initialize comparison tables*/

data disc_diff;
FORMAT COUNTRY $2.
	FILE $6. 
	VARIABLE $12. 
	TOL 4.1 
	VALUE 4.1 
	VAL_diff_percent 4.0
	SILC_&yy._percent 4.1
	SILC_&yy_0._percent 4.1
	N_obs_&yy 8.0
	N_obs_&yy_0 8.0
	N_Obs_diff 4.0
;
STOP;
run;

data flag_diff ;
FORMAT COUNTRY $2. 
	FILE $6. 
	VARIABLE $12. 
	TOL 4.1 
	MODALITY 4.1 
	VAL_diff_percent 4.0
	SILC_&yy._percent 4.1
	SILC_&yy_0._percent 4.1
	N_obs_&yy 8.0
	N_obs_&yy_0 8.0
	N_Obs_diff_percent 4.0
;
STOP;
run;

DATA cont_diff;
FORMAT COUNTRY $2. 
	FILE $6. 
	VARIABLE $12. 
	TOL 4.1 
	PERCENTILE $8.
	VAL_diff_percent 4.0
	SILC_&yy._value 12.
	SILC_&yy_0._value 12.
	N_obs_&yy 8.0
	N_obs_&yy_0 8.0
	N_Obs_diff_percent 4.0
;
STOP;
RUN;

%mend comp_setup;




%macro run_comparison(F);

/**
 * PARAMETERS
 */
%LET TH_SIZ=100; /* DEFAULT=100 */
%LET TH_PCTL=50;
%LET TH_RATE=1;

%IF &YY GT 10 %THEN %LET YY_0=%eval(&YY-1);
%IF &YY LT 11 %THEN %LET YY_0=0%eval(&YY-1);
LIBNAME RAW_0 "&eusilc&_dirsp_%quote(main)&_dirsp_%LOWCASE(&CC)&_dirsp_%LOWCASE(&SS&YY_0)"; 


%LET DS_1=RAW.&ss&cc&YY.&F;
%LET DS_0=RAW_0.&ss&cc&YY_0.&F;

/*tests if previous year data exists*/
%LET EXIST_PAST_TBL=0;
%IF %SYSFUNC(EXIST(&DS_0)) %THEN %DO;
    %LET EXIST_PAST_TBL=1;
%END;

%IF &EXIST_PAST_TBL EQ 1 %THEN %DO;

	/*selecting variables present in config files and in data files for bot years and putting them in macro var lists*/

	OPTIONS VALIDVARNAME=UPCASE;
		PROC CONTENTS NOPRINT DATA=&DS_1 OUT=MEMBERS_1;
		PROC CONTENTS NOPRINT DATA=&DS_0 OUT=MEMBERS_0;
	OPTIONS VALIDVARNAME=ANY;
	PROC SQL; 
		CREATE TABLE COMP_CFG_&F AS SELECT A.* , D.YTY
		FROM COMP_CFG A 
		INNER JOIN MEMBERS_1 B ON A.ID=B.NAME
		INNER JOIN MEMBERS_0 C ON A.ID=C.NAME
		INNER JOIN COMP_THRES D ON A.CONTEXT=D.CONTEXT;
		QUIT;
	RUN;
	PROC SQL NOPRINT;
	  SELECT ID INTO :VARS_&F._C SEPARATED BY ' ' FROM COMP_CFG_&F WHERE SUBSTR(ID,1,1)="&F" & TYPE="C" & desc="Y";
	  SELECT ID INTO :VARS_&F._D SEPARATED BY ' ' FROM COMP_CFG_&F WHERE SUBSTR(ID,1,1)="&F" & TYPE="D" & desc="Y";
	  SELECT COMPRESS(ID||"_F"," ") INTO :VARS_&F._C_F SEPARATED BY ' ' FROM COMP_CFG_&F WHERE SUBSTR(ID,1,1)="&F" & TYPE="C" & desc="Y";
	  SELECT COMPRESS(ID||"_F"," ") INTO :VARS_&F._D_F SEPARATED BY ' ' FROM COMP_CFG_&F WHERE SUBSTR(ID,1,1)="&F" & TYPE="D" & desc="Y";
	  SELECT W INTO :VARS_&F._W FROM COMP_CFG_&F WHERE SUBSTR(ID,1,1)="&F" ;
	QUIT;
	RUN;
	%LET FW=%SUBSTR(&&VARS_&F._W,1,1);

	%LET DS_1W=RAW.&ss&cc&YY.&FW;
	%LET DS_0W=RAW_0.&ss&cc&YY_0.&FW;

	/*merging H file with D file to get the weight variable and selecting the years*/
	%IF &F = H %THEN %DO;
		PROC SQL;
			CREATE TABLE DS_0 AS SELECT A.*, B.&&VARS_&F._W FROM &&DS_0 A LEFT JOIN &&DS_0W B ON A.HB010=B.DB010 & A.HB020=B.DB020 & A.HB030=B.DB030 WHERE HB010=%eval(&YYYY-1);
			CREATE TABLE DS_1 AS SELECT A.*, B.&&VARS_&F._W FROM &&DS_1 A LEFT JOIN &&DS_1W B ON A.HB010=B.DB010 & A.HB020=B.DB020 & A.HB030=B.DB030 WHERE HB010=&YYYY;
		QUIT;
		RUN;
	%END;
	%ELSE %DO;
		DATA DS_0;SET &&DS_0;IF &F.B010=%eval(&YYYY-1);RUN;
		DATA DS_1;SET &&DS_1;IF &F.B010=&YYYY;RUN;
	%END;

	/*CONTINUOUS VARIABLES COMPARISON*/

	%if %symexist(VARS_&F._C) %then %do;
		%DO k=0 %TO 1;
			DATA DS_&k;SET DS_&k;ARRAY X &&&VARS_&F._C;DO OVER X;IF X<=0 THEN X=.;END;RUN;
			PROC MEANS DATA=DS_&k NOPRINT;
				VAR &&&VARS_&F._C;
				WEIGHT &&&VARS_&F._W;
				OUTPUT OUT=COMP_&F._C_P50_&k. (DROP=_TYPE_ _FREQ_) P50=;
			RUN;
			PROC TRANSPOSE OUT=COMP_&F._C_P50_&k (RENAME=(COL1=P50_&k _NAME_=VARIABLE));RUN;
			PROC MEANS DATA=DS_&k NOPRINT;
				VAR &&&VARS_&F._C;
				WEIGHT &&&VARS_&F._W;
				OUTPUT OUT=COMP_&F._C_N_&k (DROP=_TYPE_ _FREQ_) N=;
			RUN;
			PROC TRANSPOSE OUT=COMP_&F._C_N_&k (RENAME=(COL1=N_&k _NAME_=VARIABLE));RUN;
		%END;

		PROC SQL;
			CREATE TABLE CONT_DIFF_&F 
			AS SELECT "&CC" AS COUNTRY,(CASE WHEN A.VARIABLE="" THEN B.VARIABLE ELSE A.VARIABLE END) AS VARIABLE, "P50" AS PERCENTILE, 
				N_0 as N_obs_&yy_0, N_1 as N_obs_&yy, 
				P50_0 as SILC_&yy_0._value, P50_1 as SILC_&yy._value, 
				MAX(TOL,YTY,0) AS TOL  format=4.2,
				(N_1 - N_0) / N_0 AS N_Obs_diff_percent format=4.1, 
				(P50_1 - P50_0) / P50_0 AS VAL_diff_percent format=4.1, 
				"&F.-file" as FILE
			FROM COMP_&F._C_N_0 A
			FULL JOIN COMP_&F._C_N_1 B  ON A.VARIABLE=B.VARIABLE
			FULL JOIN COMP_&F._C_P50_0 C ON A.VARIABLE=C.VARIABLE
			FULL JOIN COMP_&F._C_P50_1 D ON A.VARIABLE=D.VARIABLE
			LEFT JOIN COMP_CFG_&F E ON A.VARIABLE=E.ID
			HAVING ABS(CALCULATED VAL_diff_percent) >= CALCULATED TOL AND MIN(N_0, N_1) >= &TH_SIZ
			ORDER BY VARIABLE
			;
			QUIT;
		RUN;
		PROC APPEND FORCE DATA=CONT_DIFF_&F BASE=CONT_DIFF;RUN;	
	%end;

	/*DISCRETE VARIABLES COMPARISON*/

	%if %symexist(VARS_&F._D) %then %do;
		%DO k=0 %TO 1;
			DATA DS_&k;SET DS_&k;ARRAY X &&&VARS_&F._D;DO OVER X;IF X=. THEN X=-1000;END;RUN;

			PROC MEANS DATA=DS_&k NOPRINT;
				VAR &F.B010;
				CLASS &&&VARS_&F._D;
				TYPES &&&VARS_&F._D;
				WEIGHT &&&VARS_&F._W;
				OUTPUT OUT=COMP_&F._D_W_&k (DROP=_TYPE_ _FREQ_) SUMWGT=;
			RUN;
			DATA COMP_&F._D_W_&k;
				SET COMP_&F._D_W_&k (RENAME=(&F.B010=W_&K));KEEP VARIABLE VALUE W_&K;
				ARRAY X &&&VARS_&F._D;
				DO I=1 TO DIM(X) ;IF X(I) NE . THEN DO;VARIABLE= SCAN("&&&VARS_&F._D",I);VALUE=X(I);END;END;
				IF VALUE=-1000 THEN DELETE;
			RUN;
			PROC SQL;
				CREATE TABLE COMP_&F._D_W_&k AS SELECT *, SUM(W_&k) as TOTW_&k, W_&k/SUM(W_&k) AS PCTW_&k
				FROM COMP_&F._D_W_&k 
				GROUP BY VARIABLE;
			QUIT;RUN;
			PROC MEANS DATA=DS_&k NOPRINT;
				VAR &F.B010;
				CLASS &&&VARS_&F._D;
				TYPES &&&VARS_&F._D;
				WEIGHT &&&VARS_&F._W;
				OUTPUT OUT=COMP_&F._D_N_&k (DROP=_TYPE_ _FREQ_) N=;
			RUN;
			DATA COMP_&F._D_N_&k;
				SET COMP_&F._D_N_&k (RENAME=(&F.B010=N_&K));KEEP VARIABLE VALUE N_&K;
				ARRAY X &&&VARS_&F._D;
				DO I=1 TO DIM(X) ;IF X(I) NE . THEN DO;VARIABLE= SCAN("&&&VARS_&F._D",I);VALUE=X(I);END;END;
				IF VALUE=-1000 THEN DELETE;
			RUN;
		%END;
		PROC SQL;
			CREATE TABLE DISC_DIFF_&F 
			AS SELECT "&CC" AS COUNTRY,
				(CASE WHEN A.VARIABLE="" THEN B.VARIABLE ELSE A.VARIABLE END) AS VARIABLE, 
				(CASE WHEN A.VALUE=. THEN B.VALUE ELSE A.VALUE END) AS VALUE,
				MAX(PCTW_0,0) as SILC_&yy_0._percent format=4.1, 
				MAX(PCTW_1,0) as SILC_&yy._percent format=4.1, 
				SUM(PCTW_1, - PCTW_0) AS VAL_diff_percent format=4.0, 
				MAX(N_0,0) as N_obs_&yy_0, 
				MAX(N_1,0) as N_obs_&yy, 
				SUM(N_1, - N_0) AS N_Obs_diff , 
				(CASE WHEN MAX(E.TOL,F.TOL) =. THEN MAX(E.YTY,F.YTY) ELSE MAX(E.TOL,F.TOL) END) AS TOL  format=4.2, 
				"&F.-file" as FILE
			FROM COMP_&F._D_W_0 A 
			FULL JOIN COMP_&F._D_W_1 B ON A.VARIABLE=B.VARIABLE AND A.VALUE=B.VALUE
			FULL JOIN COMP_&F._D_N_0 C ON A.VARIABLE=C.VARIABLE AND A.VALUE=C.VALUE
			FULL JOIN COMP_&F._D_N_1 D ON B.VARIABLE=D.VARIABLE AND B.VALUE=D.VALUE
			LEFT JOIN COMP_CFG_&F E ON A.VARIABLE=E.ID
			LEFT JOIN COMP_CFG_&F F ON B.VARIABLE=F.ID
			HAVING ABS(MAX(PCTW_1,0) - MAX(PCTW_0,0)) >= CALCULATED TOL AND MAX(N_0,N_1) >= &TH_SIZ 
			ORDER BY VARIABLE, VALUE
		;
		QUIT;
		RUN;
		PROC APPEND FORCE DATA=DISC_DIFF_&F BASE=DISC_DIFF;RUN;	
	%end;

	/*FLAG VARIABLES COMPARISON*/

	%if %symexist(VARS_&F._C_F) or %symexist(VARS_&F._D_F) %then %do;
		%LET VARS_&F._F =;
		%IF %symexist(VARS_&F._C_F) %THEN %DO;%LET VARS_&F._F=&&VARS_&F._F &VARS_&F._C_F;%END; 
		%IF %symexist(VARS_&F._D_F) %THEN %DO;%LET VARS_&F._F=&&VARS_&F._F &VARS_&F._D_F;%END;
		%DO k=0 %TO 1;
			DATA DS_&k;SET DS_&k;ARRAY X &&&VARS_&F._F;DO OVER X;IF X=. THEN X=-1000;END;RUN;

			PROC MEANS DATA=DS_&k NOPRINT;
				VAR &F.B010;
				CLASS &&&VARS_&F._F;
				TYPES &&&VARS_&F._F;
				WEIGHT &&&VARS_&F._W;
				OUTPUT OUT=COMP_&F._F_W_&k (DROP=_TYPE_ _FREQ_) SUMWGT=;
			RUN;
			DATA COMP_&F._F_W_&k;
				SET COMP_&F._F_W_&k (RENAME=(&F.B010=W_&K));KEEP VARIABLE MODALITY W_&K;
				ARRAY X &&&VARS_&F._F;
				DO I=1 TO DIM(X) ;IF X(I) NE . THEN DO;VARIABLE= SCAN("&&&VARS_&F._F",I);MODALITY=X(I);END;END;
				IF MODALITY=-1000 THEN DELETE;
			RUN;
			PROC SQL;
				CREATE TABLE COMP_&F._F_W_&k AS SELECT *, SUM(W_&k) as TOTW_&k, W_&k/SUM(W_&k) AS PCTW_&k
				FROM COMP_&F._F_W_&k 
				GROUP BY VARIABLE;
			QUIT;RUN;
			PROC MEANS DATA=DS_&k NOPRINT;
				VAR &F.B010;
				CLASS &&&VARS_&F._F;
				TYPES &&&VARS_&F._F;
				WEIGHT &&&VARS_&F._W;
				OUTPUT OUT=COMP_&F._F_N_&k (DROP=_TYPE_ _FREQ_) N=;
			RUN;
			DATA COMP_&F._F_N_&k;
				SET COMP_&F._F_N_&k (RENAME=(&F.B010=N_&K));KEEP VARIABLE MODALITY N_&K;
				ARRAY X &&&VARS_&F._F;
				DO I=1 TO DIM(X) ;IF X(I) NE . THEN DO;VARIABLE= SCAN("&&&VARS_&F._F",I);MODALITY=X(I);END;END;
				IF MODALITY=-1000 THEN DELETE;
			RUN;
		%END;
		PROC SQL;
			CREATE TABLE FLAG_DIFF_&F 
			AS SELECT "&CC" AS COUNTRY, 
				(CASE WHEN A.VARIABLE="" THEN B.VARIABLE ELSE A.VARIABLE END) AS VARIABLE, 
				(CASE WHEN A.MODALITY=. THEN B.MODALITY ELSE A.MODALITY END) AS MODALITY,
				MAX(PCTW_0,0) as SILC_&yy_0._percent format=4.1, 
				MAX(PCTW_1,0) as SILC_&yy._percent format=4.1, 
				SUM(PCTW_1, - PCTW_0) AS VAL_diff_percent format=4.0, 
				MAX(N_0,0) as N_obs_&yy_0, 
				MAX(N_1,0) as N_obs_&yy, 
				SUM(N_1, - N_0) / N_0 AS N_Obs_diff_percent , 
				MAX(TOL,YTY,0) AS TOL  format=4.1,
				"&F.-file" as FILE
			FROM COMP_&F._F_W_0 A 
			FULL JOIN COMP_&F._F_W_1 B ON A.VARIABLE=B.VARIABLE AND A.MODALITY=B.MODALITY
			FULL JOIN COMP_&F._F_N_0 C ON A.VARIABLE=C.VARIABLE AND A.MODALITY=C.MODALITY
			FULL JOIN COMP_&F._F_N_1 D ON A.VARIABLE=D.VARIABLE AND A.MODALITY=D.MODALITY
			LEFT JOIN COMP_CFG_&F E ON COMPRESS(A.VARIABLE,"_F")=COMPRESS(E.ID,"_")
			HAVING ABS(SUM(PCTW_1, - PCTW_0)) >= CALCULATED TOL AND MAX(N_0,N_1) >= &TH_SIZ
			ORDER BY VARIABLE, MODALITY
		;
		QUIT;
		RUN;
		PROC APPEND FORCE DATA=FLAG_DIFF_&F BASE=FLAG_DIFF;RUN;	
	%end;
%END;
%MEND run_comparison;




%macro check_comparison_loop;
%run_comparison(D);
%run_comparison(H);
%run_comparison(P);
%run_comparison(R);

proc datasets nodetails nolist;
delete comp_:;
run;

DATA COMP_THRES_in;
SET  IN_XML.COMP_THRES;
PROC SORT NODUPKEY;
  BY CONTEXT TYPE;
RUN;

PROC TRANSPOSE OUT=COMP_THRES;
  BY CONTEXT;
  ID TYPE;
  VAR TOL; 
RUN;

DATA COUNT_CFG;
SET  IN_XML.COMP;
PROC SORT NODUPKEY;
  BY ID;
RUN;

%mend check_comparison_loop;

%macro rep_comparisonold;
%IF &YY GT 10 %THEN %LET YY_0=%eval(&YY-1);
%IF &YY LT 11 %THEN %LET YY_0=0%eval(&YY-1);

PROC FORMAT;
  VALUE PCT
    LOW-<-0.20  = 'RED'
  -0.20-<-0.15 = 'SALMON'
  -0.15-<-0.10 = 'ORANGE'
  -0.10-<-0.05 = 'YELLOW'
  -0.05-<-0.00 = 'WHITE'
   0.00-<+0.05 = 'WHITE'
   0.05-<+0.10 = 'YELLOW'
   0.10-<+0.15 = 'ORANGE'
   0.15-<+0.20 = 'SALMON'
   0.20-HIGH  = 'RED'
  ;
RUN;


OPTIONS NODATE NONUMBER ORIENTATION=LANDSCAPE;
ODS &OUTPUTFORMAT FILE = "&OUT&_dirsp_%quote(&CC&YY.-Comparisonold.&EXTENSION)" &OUTOPTION ;

/*****************
 * CONTINUOUS VALUES     *
 *****************/

TITLE1 "&CC - &YYYY / TRANSMISSION=&SS";
TITLE2 "NUMERIC VARIABLE COMPARATIVE ANALYSIS YR &YY vs &YY_0";
TITLE3 "WEIGHTED FIGURES";

DATA cont_diff_REP (KEEP=SILC_FILE VARIABLE PERCENTILE YR VALUE CNT);
SET  cont_diff;
  FORMAT YR $8.;
  SILC_FILE = SUBSTR(VARIABLE,1,1);
  YR = "&YY"; 	VALUE = SILC_&YY._VALUE; 	CNT = N_OBS_&YY; 		    OUTPUT; 
  YR = "&YY_0"; VALUE = SILC_&YY_0._VALUE; 	CNT = N_OBS_&YY_0; 	OUTPUT;
  YR = "DELTA[%]"; VALUE = VAL_DIFF_PERCENT; CNT = N_OBS_DIFF_PERCENT; OUTPUT;
PROC SORT;
  BY SILC_FILE;
RUN;

OPTIONS ORIENTATION=LANDSCAPE;
PROC TABULATE DATA=cont_diff_REP;
  BY SILC_FILE;
  CLASS VARIABLE PERCENTILE YR;
  FORMAT VARIABLE $VARLABEL.;
  VAR VALUE CNT;
  TABLE VARIABLE*PERCENTILE, YR*(VALUE*F=7. CNT*F=7.);
  LABEL YR = 'YEAR';
  KEYLABEL SUM=' ';
RUN;

/*****************
 * DISCRETE VALUES    *
 *****************/
TITLE1 "NOMINAL VARIABLE COMPARATIVE ANALYSIS YR &YY vs &YY_0";
TITLE2 "&CC - &YYYY / TRANSMISSION=&SS";
TITLE3 "PCT = WEIGHTED FIGURE | CNT = RAW FIGURE (NON-WEIGHTED)";

DATA disc_diff_REP (KEEP=SILC_FILE VARIABLE VALUE YR PERCENT CNT);
SET  disc_diff;
  FORMAT YR $8.;
  SILC_FILE = SUBSTR(VARIABLE,1,1);
  YR = "&YY"; 	PERCENT = SILC_&YY._PERCENT / 100;   CNT = N_OBS_&YY; OUTPUT; 
  YR = "&YY_0"; PERCENT = SILC_&YY_0._PERCENT / 100; CNT = N_OBS_&YY_0; OUTPUT;
  YR = "DELTA"; PERCENT = VAL_DIFF_PERCENT / 100; CNT = N_OBS_DIFF; OUTPUT;
PROC SORT;
  BY SILC_FILE;
RUN;

OPTIONS ORIENTATION=LANDSCAPE;
PROC TABULATE DATA=disc_diff_REP;
  BY SILC_FILE;
  CLASS VARIABLE VALUE YR;
  FORMAT VARIABLE $VARLABEL.;
  FORMAT VALUE 8.;
  VAR PERCENT CNT;
  TABLE VARIABLE*VALUE, YR*(PERCENT*F=PERCENT7. CNT*F=7.);
  LABEL YR = 'YEAR';
  KEYLABEL SUM=' ';
RUN;

/*****************
 *     FLAGS     *
 *****************/
TITLE1 "FLAG COMPARATIVE ANALYSIS YR &YY vs &YY_0";
TITLE2 "&CC - &YYYY / TRANSMISSION=&SS";
TITLE3 "PCT = WEIGHTED FIGURE | CNT = RAW FIGURE (NON-WEIGHTED)";

DATA flag_diff_REP (KEEP=SILC_FILE VARIABLE MODALITY YR PERCENT CNT);
SET  flag_diff;
  FORMAT YR $8.;
  SILC_FILE = SUBSTR(VARIABLE,1,1);
  YR = "&YY"; 	PERCENT = SILC_&YY._PERCENT; 	CNT = N_OBS_&YY;	OUTPUT; 
  YR = "&YY_0"; PERCENT = SILC_&YY_0._PERCENT; 	CNT = N_OBS_&YY_0; 	OUTPUT;
  YR = "DELTA[%]"; PERCENT = VAL_DIFF_PERCENT;  CNT = N_OBS_DIFF_PERCENT; OUTPUT;
PROC SORT;
  BY SILC_FILE;
RUN;

PROC TABULATE DATA=flag_diff_REP;
  BY SILC_FILE;
  CLASS VARIABLE MODALITY YR;
  FORMAT MODALITY 8.;
  VAR PERCENT CNT;
  TABLE VARIABLE*MODALITY, YR*(PERCENT*F=7. CNT*F=7.);
  LABEL YR = 'YEAR';
  KEYLABEL SUM=' ';
RUN;


TITLE1;
TITLE2;
TITLE3;

DATA HEADER (KEEP=MSG);
  LENGTH MSG T $125;
  LABEL MSG = 'VALIDATION STATUS';
  T = PUT(DATETIME(),DATETIME20.);
  MSG = "VALUES AND FLAG COMPARATIVE ANALYSIS (Y2Y) ACCOMPLISHED ON " || COMPRESS(T);
RUN;

PROC REPORT DATA=HEADER;
  COLUMN MSG;
RUN;


ODS &OUTPUTFORMAT CLOSE;

%IF &CSV=YES %THEN %DO;
	PROC EXPORT DATA=disc_diff FILE="&OUT&_dirsp_%quote(&CC&YY.-Comp_discrete_values.csv)" REPLACE;	RUN;
	PROC EXPORT DATA=cont_diff FILE="&OUT&_dirsp_%quote(&CC&YY.-Comp_cont_values.csv)" REPLACE;RUN;
	PROC EXPORT DATA=flag_diff FILE="&OUT&_dirsp_%quote(&CC&YY.-Comp_flags.csv)" REPLACE;RUN;
%END;

%mend rep_comparisonold;

%macro rep_comparison;
%IF &YY GT 10 %THEN %LET YY_0=%eval(&YY-1);
%IF &YY LT 11 %THEN %LET YY_0=0%eval(&YY-1);
%LET YYYC_0=%eval(&YYYY-1);


OPTIONS NODATE NONUMBER ORIENTATION=LANDSCAPE;
ODS &OUTPUTFORMAT FILE = "&OUT&_dirsp_%quote(&CC&YY.-Comparison.&EXTENSION)" &OUTOPTION ;

/*****************
 * CONTINUOUS VALUES     *
 *****************/

TITLE1 "&CC - &YYYY / TRANSMISSION=&SS";
TITLE2 "NUMERIC VARIABLE COMPARATIVE ANALYSIS YR &YY vs &YY_0";
TITLE3 "VALUE = WEIGHTED FIGURE | COUNT = RAW FIGURE (NON-WEIGHTED)";

PROC REPORT DATA=cont_diff;
COLUMNS FILE  VARIABLE  PERCENTILE TOL
("&YYYC_0" SILC_&yy_0._value N_obs_&yy_0) ("&YYYY" SILC_&yy._value N_obs_&yy) ("DELTA" VAL_diff_percent N_Obs_diff_percent);
	define FILE /display "File" format=$6. left; 
	define VARIABLE /display "Variable" format=$VARLABEL. left;
	define PERCENTILE /display "Percentile" format=$8. left;
	define TOL /display "Tolerance" format=PERCENTN7.1 ;
	define SILC_&yy_0._value /display "Value" format=12. ;
	define N_obs_&yy_0 /display "Count" format=8.0 ;
	define SILC_&yy._value /display "Value" format=12. ;
	define N_obs_&yy /display "Count" format=8.0 ;
	define VAL_diff_percent /display "Value" format=PERCENTN7.1 ;
	define N_Obs_diff_percent /display "Count" format=PERCENTN7.1 ;
	compute file;
		if file="D-file" then call define (_col_,'style','style=[background=Lightblue');
		if file="H-file" then call define (_col_,'style','style=[background=Lightgreen');
		if file="R-file" then call define (_col_,'style','style=[background=Salmon');
		if file="P-file" then call define (_col_,'style','style=[background=Lightyellow');
	endcomp;
	compute VARIABLE;
		if file="D-file" then call define (_col_,'style','style=[background=Lightblue');
		if file="H-file" then call define (_col_,'style','style=[background=Lightgreen');
		if file="R-file" then call define (_col_,'style','style=[background=Salmon');
		if file="P-file" then call define (_col_,'style','style=[background=Lightyellow');
	endcomp;
	compute PERCENTILE;
		if file="D-file" then call define (_col_,'style','style=[background=Lightblue');
		if file="H-file" then call define (_col_,'style','style=[background=Lightgreen');
		if file="R-file" then call define (_col_,'style','style=[background=Salmon');
		if file="P-file" then call define (_col_,'style','style=[background=Lightyellow');
	endcomp;
	compute TOL;
		if file="D-file" then call define (_col_,'style','style=[background=Lightblue');
		if file="H-file" then call define (_col_,'style','style=[background=Lightgreen');
		if file="R-file" then call define (_col_,'style','style=[background=Salmon');
		if file="P-file" then call define (_col_,'style','style=[background=Lightyellow');
	endcomp;
	compute VAL_diff_percent;
		if VAL_diff_percent<0.2 & VAL_diff_percent>-0.1 then call define (_col_,'style','style=[background=yellow');
		if VAL_diff_percent>=0.2 or VAL_diff_percent<=-0.1 then call define (_col_,'style','style=[background=orange');
		if VAL_diff_percent>=0.5 or VAL_diff_percent<=-0.2 then call define (_col_,'style','style=[background=red');
		if VAL_diff_percent>=1 or VAL_diff_percent<=-0.5 then call define (_col_,'style','style=[background=crimson');
	endcomp;
run;


/*****************
 * DISCRETE VALUES    *
 *****************/
TITLE1 "NOMINAL VARIABLE COMPARATIVE ANALYSIS YR &YY vs &YY_0";
TITLE2 "&CC - &YYYY / TRANSMISSION=&SS";
TITLE3 "PERCENT = WEIGHTED FIGURE | COUNT = RAW FIGURE (NON-WEIGHTED)";

PROC REPORT DATA=disc_diff;
COLUMNS FILE VARIABLE VALUE TOL
("&YYYC_0" SILC_&yy_0._percent N_obs_&yy_0) ("&YYYY" SILC_&yy._percent N_obs_&yy) ("DELTA" VAL_diff_percent N_Obs_diff);
	define FILE /display "File" format=$6. left; 
	define VARIABLE /display "Variable" format=$VARLABEL. left;
	define VALUE /display "Value" format=4.0 center;
	define TOL /display "Tolerance" format=PERCENTN7.1 ;
	define SILC_&yy_0._percent /display "Percent" format=PERCENTN7.1 ;
	define N_obs_&yy_0 /display "Count" format=8.0 ;
	define SILC_&yy._percent /display "Percent" format=PERCENTN7.1 ;
	define N_obs_&yy /display "Count" format=8.0 ;
	define VAL_diff_percent /display "Percent" format=PERCENTN7.1 ;
	define N_Obs_diff /display "Count" format=4.0 ;
	compute file;
		if file="D-file" then call define (_col_,'style','style=[background=Lightblue');
		if file="H-file" then call define (_col_,'style','style=[background=Lightgreen');
		if file="R-file" then call define (_col_,'style','style=[background=Salmon');
		if file="P-file" then call define (_col_,'style','style=[background=Lightyellow');
	endcomp;
	compute VARIABLE;
		if file="D-file" then call define (_col_,'style','style=[background=Lightblue');
		if file="H-file" then call define (_col_,'style','style=[background=Lightgreen');
		if file="R-file" then call define (_col_,'style','style=[background=Salmon');
		if file="P-file" then call define (_col_,'style','style=[background=Lightyellow');
	endcomp;
	compute VALUE;
		if file="D-file" then call define (_col_,'style','style=[background=Lightblue');
		if file="H-file" then call define (_col_,'style','style=[background=Lightgreen');
		if file="R-file" then call define (_col_,'style','style=[background=Salmon');
		if file="P-file" then call define (_col_,'style','style=[background=Lightyellow');
	endcomp;
	compute TOL;
		if file="D-file" then call define (_col_,'style','style=[background=Lightblue');
		if file="H-file" then call define (_col_,'style','style=[background=Lightgreen');
		if file="R-file" then call define (_col_,'style','style=[background=Salmon');
		if file="P-file" then call define (_col_,'style','style=[background=Lightyellow');
	endcomp;
	compute VAL_diff_percent;
		if VAL_diff_percent<0.2 & VAL_diff_percent>-0.1 then call define (_col_,'style','style=[background=yellow');
		if VAL_diff_percent>=0.2 or VAL_diff_percent<=-0.1 then call define (_col_,'style','style=[background=orange');
		if VAL_diff_percent>=0.5 or VAL_diff_percent<=-0.2 then call define (_col_,'style','style=[background=red');
		if VAL_diff_percent>=1 or VAL_diff_percent<=-0.5 then call define (_col_,'style','style=[background=crimson');
	endcomp;
run;

/*****************
 *     FLAGS     *
 *****************/
TITLE1 "FLAG COMPARATIVE ANALYSIS YR &YY vs &YY_0";
TITLE2 "&CC - &YYYY / TRANSMISSION=&SS";
TITLE3 "PERCENT = WEIGHTED FIGURE | COUNT = RAW FIGURE (NON-WEIGHTED)";

PROC REPORT DATA=flag_diff;
COLUMNS FILE VARIABLE MODALITY TOL
("&YYYC_0" SILC_&yy_0._percent N_obs_&yy_0) ("&YYYY" SILC_&yy._percent N_obs_&yy) ("DELTA" VAL_diff_percent N_Obs_diff_percent);
	define FILE /display "File" format=$6. left; 
	define VARIABLE /display "Flag" format=$VARLABEL. left;
	define MODALITY /display "Value" format=4.0 center;
	define TOL /display "Tolerance" format=PERCENTN7.1 ;
	define SILC_&yy_0._percent /display "Percent" format=PERCENTN7.1 ;
	define N_obs_&yy_0 /display "Count" format=8.0 ;
	define SILC_&yy._percent /display "Percent" format=PERCENTN7.1 ;
	define N_obs_&yy /display "Count" format=8.0 ;
	define VAL_diff_percent /display "Percent" format=PERCENTN7.1 ;
	define N_Obs_diff_percent /display "Count" format=PERCENTN7.1  ;
	compute file;
		if file="D-file" then call define (_col_,'style','style=[background=Lightblue');
		if file="H-file" then call define (_col_,'style','style=[background=Lightgreen');
		if file="R-file" then call define (_col_,'style','style=[background=Salmon');
		if file="P-file" then call define (_col_,'style','style=[background=Lightyellow');
	endcomp;
	compute VARIABLE;
		if file="D-file" then call define (_col_,'style','style=[background=Lightblue');
		if file="H-file" then call define (_col_,'style','style=[background=Lightgreen');
		if file="R-file" then call define (_col_,'style','style=[background=Salmon');
		if file="P-file" then call define (_col_,'style','style=[background=Lightyellow');
	endcomp;
	compute MODALITY;
		if file="D-file" then call define (_col_,'style','style=[background=Lightblue');
		if file="H-file" then call define (_col_,'style','style=[background=Lightgreen');
		if file="R-file" then call define (_col_,'style','style=[background=Salmon');
		if file="P-file" then call define (_col_,'style','style=[background=Lightyellow');
	endcomp;
	compute TOL;
		if file="D-file" then call define (_col_,'style','style=[background=Lightblue');
		if file="H-file" then call define (_col_,'style','style=[background=Lightgreen');
		if file="R-file" then call define (_col_,'style','style=[background=Salmon');
		if file="P-file" then call define (_col_,'style','style=[background=Lightyellow');
	endcomp;
	compute VAL_diff_percent;
		if VAL_diff_percent<0.2 & VAL_diff_percent>-0.1 then call define (_col_,'style','style=[background=yellow');
		if VAL_diff_percent>=0.2 or VAL_diff_percent<=-0.1 then call define (_col_,'style','style=[background=orange');
		if VAL_diff_percent>=0.5 or VAL_diff_percent<=-0.2 then call define (_col_,'style','style=[background=red');
		if VAL_diff_percent>=1 or VAL_diff_percent<=-0.5 then call define (_col_,'style','style=[background=crimson');
	endcomp;
run;

TITLE1;
TITLE2;
TITLE3;

DATA HEADER (KEEP=MSG);
  LENGTH MSG T $125;
  LABEL MSG = 'VALIDATION STATUS';
  T = PUT(DATETIME(),DATETIME20.);
  MSG = "VALUES AND FLAG COMPARATIVE ANALYSIS (Y2Y) ACCOMPLISHED ON " || COMPRESS(T);
RUN;

PROC REPORT DATA=HEADER;
  COLUMN MSG;
RUN;


ODS &OUTPUTFORMAT CLOSE;

%IF &CSV=YES %THEN %DO;
	filename compcsv "&OUT&_dirsp_%quote(&CC&YY.-Comp_discrete_values.csv)";
	PROC EXPORT DATA=disc_diff FILE=compcsv REPLACE DBMS=csv;	RUN;
	filename compcsv clear;
	filename compcsv "&OUT&_dirsp_%quote(&CC&YY.-Comp_cont_values.csv)";
	PROC EXPORT DATA=cont_diff FILE=compcsv REPLACE DBMS=csv;RUN;
	filename compcsv clear;
	filename compcsv "&OUT&_dirsp_%quote(&CC&YY.-Comp_flags.csv)";
	PROC EXPORT DATA=flag_diff FILE=compcsv REPLACE DBMS=csv;RUN;
	filename compcsv clear;

%END;

%mend rep_comparison;

