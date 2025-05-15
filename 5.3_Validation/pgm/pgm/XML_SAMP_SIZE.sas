/**
 * Revision History
 * - Mar 31st 2022
 */

/**
 * rno19.ANALYSIS.txt
 * ---------------------------------------
 * DB135 -> Household interview acceptance
 * [] 1 -> INTERVIEW ACCEPTED
 * [] 2 -> INTERVIEW REJECTED
 * DB110 -> HOUSEHOLD STATUS
 * [] 1 -> AT THE SAME ADDRESS AS LAST INTERVIEW
 * [] 2 -> CHANGED
 * DB120 -> HOUSEHOLD CONTACT (ADDRESS)
 * [] 1 -> CONTACTED
 * [] 2+-> NOT CONTACTED (ISSUES)
 * RB245 -> RESPONDENT STATUS
 * [] 1 ->
 * [] 2 ->
 * [] 3 ->
 * RB250 -> DATA STATUS
 * [] 1 -> INFO COMPLETED FROM INTERIEWS
 * [] 2 -> INFO COMPLETED FROM REGISTERS
 * [] 3 -> INFO COMPLETED FROM BOTH
 * [] 4 -> IMPUTATION
 * [] 5 -> NO DATA
 */

%macro get_size_data (YR=);
%LET INDIR=%SYSFUNC(PATHNAME(XMLCFG))&_dirsp_%quote(SIZE);
%LET INFNAME=SIZE-&YR..xml;

LIBNAME IN_XML XMLV2 "&INDIR&_dirsp_%quote(&INFNAME)" XMLMAP="%SYSFUNC(PATHNAME(MAPDIR))&_dirsp_%quote(SIZE.map)";

DATA SAMP_SIZE_TARGET_by_HH (KEEP=CNTRY NUTS HH_C HH_L RENAME=(HH_C=TARGET_C HH_L=TARGET_L));
SET IN_XML.SIZE_TARGET;
 IF UPCASE(CNTRY) = "&cc";
 *IF SAMPLING='';
RUN;

PROC SORT;BY NUTS;RUN;

%LET NOBS=0;
DATA _NULL_;
	SET  SAMP_SIZE_TARGET_by_HH NOBS=NOBS;
	CALL SYMPUTX('NOBS',NOBS);
RUN;
%PUT NOBS &NOBS;

%IF &NOBS=3 %THEN %DO;
	/*DELETE NUTS1 AND NUTS2 WHEN THE COUNTRY HAS ONLY 1 NUTS */
	DATA SAMP_SIZE_TARGET_by_HH ;
	SET SAMP_SIZE_TARGET_by_HH ;
	IF LENGTH(NUTS)=2;
	RUN;
%END;
%ELSE %DO;
	/*SELECT NUTS2 WHICH ARE TOO SMALL FOR SAMPLE SIZE REQUIREMENTS (POP<500000)*/
	DATA SAMP_SIZE_NUTS2_UNAPP;SET SAMP_SIZE_TARGET_by_HH;KEEP NUTS KEEP_NUTS1;
	IF LENGTH (NUTS)= 4 and TARGET_C=0;
	NUTS=SUBSTR(NUTS,1,3);
	KEEP_NUTS1=1;
	RUN;
	PROC SORT NODUPKEY;BY NUTS;RUN;

	%LET NOBS=0;
	DATA _NULL_;
		SET  SAMP_SIZE_NUTS2_UNAPP NOBS=NOBS;
		CALL SYMPUTX('NOBS',NOBS);
	RUN;

	/*KEEP NUTS1 ONLY WHEN NUTS2 TOO SMALL*/
	%IF &NOBS GT 0 %THEN %DO;
		DATA SAMP_SIZE_TARGET_by_HH ;MERGE SAMP_SIZE_TARGET_by_HH SAMP_SIZE_NUTS2_UNAPP;BY NUTS;RUN;
	%END;
	%ELSE %DO;
		DATA SAMP_SIZE_TARGET_by_HH ;SET SAMP_SIZE_TARGET_by_HH;KEEP_NUTS1=.;RUN;

	%END;
	DATA SAMP_SIZE_TARGET_by_HH;SET SAMP_SIZE_TARGET_by_HH ;DROP KEEP_NUTS1;IF LENGTH(NUTS)=3 and KEEP_NUTS1 =. THEN DELETE;RUN;
%END;
%mend get_size_data;


%macro check_SSIZE;

%LOCAL IS_SELRESP;
%get_size_data (YR=&YYYY);

DATA SAMP_SIZE_STAT ;
  LENGTH MSG00 MSG01 MSG2 MSG3 $125;
  FORMAT TARGET ACTUAL best8.;
	STOP;
RUN;

/** SAMPLE SIZE
 * HH LEVEL */
PROC SQL;
  CREATE TABLE SAMP_SIZE_CALC_NUTS0 AS
  SELECT "&CC" as NUTS, 0 as LEVEL, COUNT(DB135) AS CNT_DB135
  FROM RAW.&ss&cc&YY.D
  WHERE DB135 = 1 AND DB010 = &YYYY
 ;
QUIT;
PROC SQL;
  CREATE TABLE SAMP_SIZE_CALC_NUTS1 AS
  SELECT SUBSTR(DB040,1,3) AS NUTS, 1 as LEVEL, COUNT(DB135) AS CNT_DB135
  FROM RAW.&ss&cc&YY.D
  WHERE DB135 = 1 AND DB010 = &YYYY
  GROUP BY CALCULATED NUTS
 ;
QUIT;
PROC SQL;
  CREATE TABLE SAMP_SIZE_CALC_NUTS2 AS
  SELECT DB040 as NUTS, 2 as LEVEL, COUNT(DB135) AS CNT_DB135
  FROM RAW.&ss&cc&YY.D
  WHERE DB135 = 1 AND DB010 = &YYYY
  GROUP BY DB040
 ;
QUIT;

PROC SQL;
  CREATE TABLE SAMP_SIZE_CALC_LONG AS
  SELECT "&CC" as NUTS, 6 as LEVEL, COUNT(A.DB135) AS CNT_DB135
  FROM RAW.&ss&cc&YY.D A 
	INNER JOIN RAW.&ss&cc&YY.D B ON A.DB030=B.DB030
	INNER JOIN RAW.&ss&cc&YY.D C ON A.DB030=C.DB030
	INNER JOIN RAW.&ss&cc&YY.D D ON A.DB030=D.DB030
  WHERE A.DB135 = 1 AND A.DB010 = &YYYY AND B.DB010 = %EVAL(&YYYY-1) AND C.DB010 = %EVAL(&YYYY-2) AND D.DB010 = %EVAL(&YYYY-3)
 ;
QUIT;


DATA SAMP_SIZE_CALC_by_HH;LENGTH NUTS $4;SET SAMP_SIZE_CALC_NUTS0 SAMP_SIZE_CALC_NUTS1 SAMP_SIZE_CALC_NUTS2 SAMP_SIZE_CALC_LONG;RUN;
PROC SORT;BY NUTS;RUN;


DATA MSG;
	MERGE SAMP_SIZE_CALC_by_HH  SAMP_SIZE_TARGET_by_HH ;BY NUTS;
	LENGTH MSG00 MSG01 MSG2 MSG3 $125;
	IF LEVEL=6 THEN TARGET = TARGET_L;ELSE TARGET = TARGET_C;
	IF TARGET NE .; 
	ACTUAL = FLOOR(CNT_DB135);
	DELTA = CNT_DB135 - TARGET;
	IF LEVEL=0 THEN DO;
		MSG00 = "1. Sample Size &CC cross-sectional";
		MSG01 = " {DB135=1} >= (2600 + 900*SQRT(NB households in millions) )";
	END;
	ELSE IF LEVEL=6 THEN DO; 
		MSG00 = "3. Sample Size &CC longitudinal (4 years)";
		MSG01 = " {DB135=1} >= (1000 + 350*SQRT(NB households in millions) )";
	END;
	ELSE DO; 
		MSG00 = "2. Sample Size cross-sectional NUTS =" || trim(nuts);
		MSG01 = " {DB135=1} >= (600 * SQRT(NB households in millions) )";
	END;
	IF ACTUAL < TARGET THEN DO;
		MSG2 = "Warning : theoritical sample size not achieved";
	END;
	ELSE IF (ACTUAL >= TARGET AND TARGET>0) THEN DO;
		MSG2 = "Good : theoritical sample size achieved";
	END;
	ELSE IF (ACTUAL >= TARGET AND TARGET=0) THEN DO;
		MSG2 = "No sample size target for this region (population < 0.5m)";
	END;
	put " ";
RUN;

PROC APPEND BASE=SAMP_SIZE_STAT DATA=MSG FORCE;
RUN;

PROC SORT; BY MSG00;RUN;



/** NON-RESPONSE ADJUSTMENT 
 * HH LEVEL */
PROC SQL;
 CREATE TABLE SAMP_ADJ_DEN_by_HH AS 
 SELECT COUNT(DB010) AS DEN
 FROM RAW.&ss&cc&YY.D
 %IF &ss = R or &ss = E %THEN %DO;
 WHERE (DB120 in (11,21,22,23) OR DB110 = 1) 
 AND DB010 = &YYYY
 %END;
 %ELSE %DO;
 WHERE (DB120 in (11,21,22,23)) 
 AND DB010 = &YYYY
 %END;
 ;
QUIT;

PROC SQL;
 CREATE TABLE SAMP_ADJ_NUM_by_HH AS 
 SELECT COUNT(DB135) AS NUM
 FROM   RAW.&ss&cc&YY.D
 WHERE  DB135 = 1 
 AND DB010 = &YYYY
 ;
QUIT;

DATA MSG;
MERGE SAMP_ADJ_DEN_by_HH SAMP_ADJ_NUM_by_HH  ;
  LENGTH MSG00 MSG01 MSG2 MSG3 $125;
  ACTUAL = FLOOR(100 - 100*(NUM/DEN));
  TARGET = 40;
  MSG00 =  "4. Household non-response rate (%)";
  MSG01 =  "   (1 - ({DB135=1} / {DB120 in (11,21,22,23)})) * 100 =< 40%";
  if ACTUAL > TARGET then do;
	MSG2 = "Bad : Household non-response rate out of target"; 
  end;
  else do;
	MSG2 = "Good : Household non-response rate in target"; 
  end;
  MSG3 = "N(DB120 in (11,21,22) = " || compress(PUT(DEN,BEST.)) || "/ N(DB135=1) = " || 
  compress(PUT(NUM,BEST.)); 
  put " ";
RUN;

PROC APPEND BASE=SAMP_SIZE_STAT DATA=MSG FORCE;
RUN;

/**
 * NON-RESPONSE ADJUSTMENT
 * P16 LEVEL */

PROC SQL;
  CREATE TABLE SAMP_ADJ_DEN_by_P16 AS 
  SELECT COUNT(RB245) AS DEN
  FROM   RAW.&ss&cc&YY.R
  WHERE  RB245 IN (1,2,3) 
  AND RB010 = &YYYY
  ;
QUIT;

PROC SQL;
  CREATE TABLE SAMP_ADJ_NUM_by_P16 AS 
  SELECT COUNT(RB250) AS NUM
  FROM   RAW.&ss&cc&YY.R
  %IF &ss = R %THEN %DO;
  WHERE RB250 in (11,12,13,14) 
  AND RB110 < 5 
  AND RB010 = &YYYY
  %END;
  %ELSE %DO;
  WHERE RB250 IN (11,12,13,14) 
  AND RB010 = &YYYY
  %END;
  ;
QUIT;

DATA MSG;
MERGE SAMP_ADJ_DEN_by_P16
  SAMP_ADJ_NUM_by_P16
  ;
  LENGTH MSG00 $125 MSG01 $125 MSG1 $125 MSG2 $125 MSG3 $125;
  ACTUAL = 100 - 100 * (NUM/DEN);
  TARGET = 1;
  MSG00 =  "5. Personal non-response rate (%)";
  MSG01 =  "   (1 - ({RB250 in (11,12,13,14)} / {RB245 in (1,2,3)})) * 100 =< 1%";
  if ACTUAL > TARGET then do;
	MSG2 = "Bad : Personal non-response rate out of target"; 
  end;
  else do;
	MSG2 = "Good : Personal non-response rate in target"; 
  end;
  MSG3 = "N(RB245 in (1,2,2)) = " || compress(PUT(DEN,BEST.)) || "/ N(RB250 in (11,12,13,14)) = " || compress(PUT(NUM,BEST.)); 
  put " ";
RUN; 

PROC APPEND BASE=SAMP_SIZE_STAT DATA=MSG FORCE;
RUN;

%mend check_SSIZE;




%macro check_DURATION;

	/**
     * Interview Duration
(disabled)
     
	PROC SQL;
	 CREATE TABLE tmp7 AS 
     SELECT HB030,
		(SUM(PB120)) AS sum_PB120,
	    (HB100 + CALCULATED sum_PB120) AS tot_hh
	 FROM RAW.&ss&cc&YY.H INNER JOIN RAW.&ss&cc&YY.P
	 ON (HB030 = INT(PB030/100)) AND (HB010 = PB010)
	 WHERE HB010 = &YYYY
	 GROUP BY HB030
     ;
 	QUIT; 

	PROC SQL;
	 CREATE TABLE tmp8 AS SELECT DISTINCT   
		 (MEAN(tot_hh)) AS hhmean
	 FROM tmp7
     ;
	QUIT;

	DATA MSG;
	set tmp8;
    LENGTH MSG00 MSG01 MSG2 MSG3 $125;
	format hhmean 3.0;
	MSG00 = "6. Average Interview Duration (min)";
	MSG01 = "   Mean({HB100+PB120}by HH) =< 60";
	TARGET = 60; 
	ACTUAL = FLOOR(hhmean) ;
	if ACTUAL > target then do;
		MSG2 = "Warning : Average Interview Duration out of target"; 
	end;
	else do;
		MSG2 = "Good : Average Interview Duration in target"; 
	end;
	put " ";
	RUN;

	PROC APPEND BASE=SAMP_SIZE_STAT DATA=MSG FORCE;
	RUN;
	*/
		/**
		 * Proxy Interviews
		 */	
	PROC CONTENTS DATA=RAW.&ss&cc&YY.R OUT=R_VARS NOPRINT;
	PROC CONTENTS DATA=RAW.&ss&cc&YY.P OUT=P_VARS NOPRINT;
	RUN;

	%LET RB250_FOUND=0;
	%LET PB260_FOUND=0;
	PROC SQL NOPRINT;
	  SELECT COUNT(*) INTO :RB250_FOUND
	  FROM   R_VARS
	  WHERE  UPCASE(NAME) = 'RB250'
	  ;
	  SELECT COUNT(*) INTO :PB260_FOUND
	  FROM   P_VARS
	  WHERE  UPCASE(NAME) = 'PB260'
	  ;
	QUIT;
	
	%PUT *I* RB250 | PB260 FOUND: &RB250_FOUND | &PB260_FOUND
	;
	%IF &RB250_FOUND * &PB260_FOUND GT 0 %THEN %DO;

		PROC SQL;
		 CREATE TABLE tmp9 AS 
	     SELECT COUNT(PB260_F) AS PB260_den
		 FROM RAW.&ss&cc&YY.P P LEFT JOIN RAW.&ss&cc&YY.R R
		 ON P.PB010=R.RB010 AND P.PB020=R.RB020 AND P.PB030=R.RB030 
		 WHERE PB260_F=1 AND RB110 < 5 AND RB010 = &YYYY
		 ;
		SQL;
	 
		PROC SQL;
		 CREATE TABLE tmp10 AS 
	     SELECT DISTINCT (COUNT(PB260_F)) AS PB260_num
		 FROM RAW.&ss&cc&YY.P P LEFT JOIN RAW.&ss&cc&YY.R R
		 ON P.PB010=R.RB010 AND P.PB020=R.RB020 AND P.PB030=R.RB030 
		 WHERE PB260 = 2 AND PB260_F=1 AND PB010 = &YYYY AND RB110 < 5
		 ;	 
		QUIT;


		DATA MSG;
		merge tmp9 tmp10;
        LENGTH MSG00 MSG01 MSG2 MSG3 $125;
		prox = 100 * PB260_num / PB260_den ;
		TARGET = 20;
		ACTUAL= FLOOR(prox) ;
		MSG00 = "6. Proxy Interviews %";
		MSG01 = "   100 * {PB260 = 2}/ {PB260_F=1}  =< 20";
		if ACTUAL > TARGET then do;
			MSG2 = "Warning : Number of proxy interviews out of target"; 
		end;
		else do;
			MSG2 = "Good : Number of proxy interviews in target";
		end;
		MSG3 = "(N(PB260=2)=" || compress(PUT(PB260_num,BEST.)) || ") / (N(PB260_F=1)=" || compress(PUT(PB260_den,BEST.)) || ") = " || compress(PUT(prox,BEST.)) || "%";
		put " ";
		RUN;

	  PROC APPEND BASE=SAMP_SIZE_STAT DATA=MSG FORCE;
	  RUN;
  %END;

%mend check_DURATION;

%macro check_SAMP_SIZE;

DATA HEADER (KEEP= MSG);
  LENGTH T MSG $125;
  LABEL MSG = 'VALIDATION STATUS';
  T = PUT(DATETIME(),DATETIME20.);
  MSG = "SAMPLE SIZE VALIDATION ACCOMPLISHED ON " || COMPRESS(T);
RUN;

%check_SSIZE;
%check_DURATION;

OPTIONS NODATE NONUMBER;

ODS &OUTPUTFORMAT FILE = "&OUT&_dirsp_%quote(&CC&YY.-Samp_size.&EXTENSION)" &OUTOPTION ;

TITLE1 "&CC - &YYYY / TRANSMISSION=&SS";
TITLE2 'SAMPLE SIZE ASSESSMENT';

PROC REPORT DATA=SAMP_SIZE_STAT;
  COLUMN  MSG00 MSG01 TARGET ACTUAL MSG2;
  define MSG00 / "Topic";
  define MSG01 / "Formula";
  define TARGET / "Target" ;
  define ACTUAL / "Actual" ;
  define MSG2 / "Result";
  compute MSG00;
	if substr(MSG00,1,1) in ("2", "4", "6") then call define  (_row_,'style','style=[background=Lightgrey');
  endcomp;
  compute MSG2;
	if substr(MSG2,1,4)="Good" then call define  (_col_,'style','style=[background=Lightgreen');
	if substr(MSG2,1,4)="Bad " then call define  (_col_,'style','style=[background=Red');
	if substr(MSG2,1,4)="Warn" then call define  (_col_,'style','style=[background=Orange');
  endcomp;
RUN;

PROC REPORT DATA=HEADER;
  COLUMN  MSG;
;
RUN;

TITLE1;
TITLE2;

ODS &OUTPUTFORMAT CLOSE;

%mend check_SAMP_SIZE;


