/**
 * Revision History
 * - MAR 31st 2022
 */



%MACRO mk_AUX_CALIB_DSET;
%global list_rotation list_rotation2 list_rotation3;

DATA YEARS;
%DO k=1 %TO &ROTATION;
DB010=%EVAL(&YYYY-&ROTATION+&k);output;
%END;
run; 


/*to select the number of the new rotation group by year*/
proc means data=RAW.&ss&cc&YY.D (keep=DB010 DB020 DB030 DB075 DB110) noprint;
	var DB030;
	class DB010 DB075 ;
	ways 2;
	where DB110=9;
	output out=rotation_groups n=nb_hh;
run;
proc sort data=rotation_groups;by DB010 descending nb_hh;run;
proc sort data=rotation_groups (keep=DB010 DB075 rename=DB075=DB075_new) nodupkey;by DB010 ;run;

proc means data=RAW.&ss&cc&YY.D (keep=DB010 DB020 DB030 DB075 DB110) noprint;
	var DB030;
	class DB010 DB075 ;
	ways 2;
	*where DB110=9;
	output out=rotation_groups2 n=nb_hh;
run;
proc sort data=rotation_groups2 nodupkey;by DB075 ;run;
proc sort data=rotation_groups2;by DB010 descending nb_hh;run;

proc transpose data=rotation_groups2 out=rotation_groups2 (drop=_name_) ;var db075;run;
data rotation_groups2;set rotation_groups2;length list_rotation list_rotation2 list_rotation3 $150 ;
	list_rotation="";
	list_rotation2="";
	list_rotation3="";
	array x col1-col&rotation;
	do i=1 to &rotation;
		if x(i) ne . then do;
			list_rotation=compbl (list_rotation !! x(i) !!" ");
			list_rotation2=compbl (list_rotation2 !! "_"!! compress(x(i)," ") !!" ");
			list_rotation3=compbl (list_rotation3 !! "l_"!! compress(x(i)," ") !!" ");
		end;
	end;
	call symput ("list_rotation" ,list_rotation);
	call symput ("list_rotation2" ,list_rotation2);
	call symput ("list_rotation3" ,list_rotation3);
run;



PROC SORT DATA= RAW.&ss&cc&YY.D; BY DB010 ;RUN;
DATA D0 D1 D2 D3;
	MERGE RAW.&ss&cc&YY.D (keep=DB010 DB020 DB030 DB080 DB090 DB095 DB075 DB076 DB135 DB110) rotation_groups;BY DB010;
	IF DB010 = &YYYY THEN OUTPUT D0;
	IF DB010 = &YYYY AND DB135=1 THEN OUTPUT D1;
	IF DB010 < &YYYY THEN OUTPUT D2;
	IF DB010 < &YYYY AND DB135=1 THEN OUTPUT D3;
RUN;

DATA R0 R2;
	SET RAW.&ss&cc&YY.R (keep=RB010 RB020 RB030 RB040 RB050 RB060 RB062 RB063 RB064 RB065 RB066 RB110 RB245 RL070) ;
	IF RB010 = &YYYY THEN OUTPUT R0;
	IF RB010 < &YYYY THEN OUTPUT R2;
RUN;

DATA P0 P1 P2;
 SET RAW.&ss&cc&YY.P (keep=PB010 PB020 PB030 PB040 PB050 PB060 PB070 PB080);
 IF PB010 = &YYYY THEN OUTPUT P0;
 IF PB010 = &YYYY AND PB060>0 THEN OUTPUT P1;
 IF PB010 < &YYYY THEN OUTPUT P2;
RUN;

PROC SQL;CREATE TABLE R0  AS SELECT R0.*,  D0.DB075, D0.DB075_NEW, D0.DB076, D0.DB135  
	FROM R0  LEFT JOIN D0  ON R0.RB010=D0.DB010   AND R0.RB020=D0.DB020   AND R0.RB040=D0.DB030  ;QUIT;RUN;
PROC SQL;CREATE TABLE R2 AS SELECT R2.*, D2.DB075,D2.DB075_NEW,D2.DB076, D2.DB135 
	FROM R2 LEFT JOIN D2 ON R2.RB010=D2.DB010 AND R2.RB020=D2.DB020 AND R2.RB040=D2.DB030;QUIT;RUN;
PROC SQL;CREATE TABLE P0  AS SELECT P0.*,  R0.DB075, R0.DB075_NEW, R0.DB076, R0.DB135, R0.RB110, R0.RB245  
	FROM P0  LEFT JOIN R0  ON P0.PB010=R0.RB010   AND P0.PB020=R0.RB020   AND P0.PB030=R0.RB030  ;QUIT;RUN;
PROC SQL;CREATE TABLE P1 AS SELECT P1.*, R0.DB075, R0.DB075_NEW, R0.DB076, R0.DB135, R0.RB110, R0.RB245  
	FROM P1 LEFT JOIN R0  ON P1.PB010=R0.RB010  AND P1.PB020=R0.RB020  AND P1.PB030=R0.RB030 ;QUIT;RUN;
PROC SQL;CREATE TABLE P2 AS SELECT P2.*, R2.DB075,R2.DB075_NEW,R2.DB076, R2.DB135, R2.RB110, R2.RB245 
	FROM P2 LEFT JOIN R2 ON P2.PB010=R2.RB010 AND P2.PB020=R2.RB020 AND P2.PB030=R2.RB030;QUIT;RUN;

DATA RES;
 SET _NULL_;
RUN;

DATA STATS;
  LENGTH VARIABLE $15 FILTER $50 FILE $1;
  LENGTH YEAR N NMISS MIN MAX SUM FILLED 8;
  STOP;
RUN;


DATA  OUTL_HIGH OUTL_LOW;
  LENGTH VARIABLE $15;
  LENGTH ID 8;
  LENGTH WEIGHT 8;
  LENGTH BOUND 8;
  LENGTH MEDIAN 8;
  LENGTH FACTOR 8;
  STOP;
RUN;

DATA HEADER (KEEP=MSG);
  LENGTH MSG T $125;
  LABEL MSG = 'VALIDATION STATUS';
  T = PUT(DATETIME(),DATETIME20.);
  MSG = UPCASE("WEIGHT VALIDATION ACCOMPLISHED ON " || COMPRESS(T));
RUN;

%MEND mk_AUX_CALIB_DSET;

%MACRO ROTATION_GROUPS;
DATA WAVES;SET RAW.&ss&cc&YY.D;KEEP DB010 DB020 DB030 DB075 DB076 DB110 DB135;RUN;
PROC SORT DATA=WAVES;by DB010 DB076;RUN;

proc means data=WAVES noprint ;
	var db030;
	class DB010 DB075 ;
	ways 2;
	output out=rotation_table n=;
run;
proc transpose data=rotation_table out=rotation_table2 (drop=_name_) prefix=_;var _freq_;by db010;id db075;run;
data rotation_table2;merge years rotation_table2;by db010;run;
data rotation_table2;set rotation_table2;
	array x &list_rotation2;
	array y &list_rotation3;
	do i = 1 to dim(x);y{i}=lag(x{i});end;
run;

TITLE3 "ROTATION BY YEAR : TABLE MUST BE A TRIANGLE AND VALUES SHOULD DECREASE EVERY YEAR";
proc report data=rotation_table2;
columns db010 ("DB075-Rotation group" &list_rotation3 &list_rotation2);
define db010 /display "DB010-Year";
%do i = 1 %to &rotation;
	%IF %LENGTH(%scan(&list_rotation,&i)) > 0 %THEN %DO;
		define %scan(&list_rotation3,&i) /analysis noprint;
		define %scan(&list_rotation2,&i) /analysis "%scan(&list_rotation,&i)";
		compute %scan(&list_rotation2,&i);
			if %scan(&list_rotation2,&i).sum=. and db010 >= %eval(&yyyy - &rotation + &i) then call define  (_col_,'style','style=[background=Red');
			if %scan(&list_rotation2,&i).sum>0 then call define  (_col_,'style','style=[background=Lightgreen');
			if %scan(&list_rotation2,&i).sum>%scan(&list_rotation3,&i).sum and %scan(&list_rotation3,&i).sum ne . then call define  (_col_,'style','style=[background=Orange');
		endcomp;
	%END;
%end;
run;

/*PROC TABULATE DATA=WAVES ORDER=DATA;
	CLASS DB010 DB075 / MISSING ;
	CLASSLEV DB075 / style=[background=ROT.];
	TABLE DB010=" ", DB075="DB075-Rotation Group"*N=" " / BOX="DB010-Year";
RUN;

TITLE3 "WAVES BY YEAR : TABLE SHOULD BE A TRIANGLE FROM 2021 ONWARDS AND NO WAVE SHOULD BE GREATER THAN &ROTATION";
PROC TABULATE DATA=WAVES ORDER=DATA;
	CLASS DB010 DB076 / MISSING ;
	CLASSLEV DB076 / style=[background=ROT.];
	TABLE DB010=" ", DB076="DB076-Waves"*N=" " / BOX="DB010-Year";
RUN;

TITLE3 "NEW ROTATION GROUP BY YEAR : TABLE SHOULD BE A DIAGONAL";
PROC TABULATE DATA=WAVES (where=(db076=1)) ORDER=DATA;
	CLASS DB010 DB075 DB076/ MISSING ;
	CLASSLEV DB075 / style=[background=ROT.];
	TABLE DB010*DB076=" ", DB075="DB075-Rotation Group"*N=" " / BOX="DB076-Wave";
RUN;
TITLE3;



PROC MEANS DATA=WAVES NOPRINT;
	VAR DB030;
	WHERE DB075 > &ROTATION;
	CLASS DB010 DB075;
	WAYS 2;
	OUTPUT OUT=MNS (DROP=_TYPE_ _FREQ_ ) N=N ;
RUN;


*/

%MEND;

/*************************************/
/* SUMMARY STATISTICS ON THE WEIGHTS */
/*************************************/

%MACRO SUMMARY_STAT(F=,VARINT=,FILTER=, MSG=);
 
PROC MEANS DATA=&F NOPRINT;
	VAR &VARINT;
	WHERE &FILTER;
	CLASS %SUBSTR(&F,1,1)B010 DB075_NEW;
	WAYS 2;
	OUTPUT OUT=MNS (DROP=_TYPE_ _FREQ_ ) 
	N=N NMISS=NMISS MIN=MIN MAX=MAX SUM=SUM ;
RUN;


DATA MNS;
SET  MNS;
	RENAME %SUBSTR(&F,1,1)B010=YEAR;
	LENGTH VARIABLE $15 ;
	LENGTH FILTER $50;
	FILTER="&FILTER";
	FILTER = TRANWRD (FILTER,"RB110 GT 4","NON-HH MEMBERS");
	FILTER = TRANWRD (FILTER,"RB245 NE 2","NON-SELECTED RESPONDENT");
	FILTER = TRANWRD (FILTER,"DB135 NE 1","NON-RESPONDENT");
	FILTER = TRANWRD (FILTER,"DB075_NEW","NEW");
	FILTER = TRANWRD (FILTER,"DB075","ROTATION");
	FILTER = TRANWRD (FILTER," EQ ","=");
	FILTER = TRANWRD (FILTER," NE ","<>");
	FILTER = TRANWRD (FILTER," LT ","<");
	FILTER = TRANWRD (FILTER," GT ",">");
	FILTER = TRANWRD (FILTER,"DB010","YEAR");
	FILTER = TRANWRD (FILTER,"RB010","YEAR");
	FILTER = TRANWRD (FILTER,"PB010","YEAR");
	VARIABLE = "&VARINT";

	FILE=SUBSTR(VARIABLE,1,1);
	IF SUBSTR(VARIABLE,1,2) = "_H" THEN FILE ="D";
	IF SUBSTR(VARIABLE,1,2) = "_P" THEN FILE ="R";

	FILLED=1;
	IF VARIABLE IN ("RB062","RB063","RB064", "RB065", "RB066") AND TRANWRD(FILTER,"ROTATION=","")+0=DB075_NEW THEN FILLED=0;
	IF FILTER="NON-HH MEMBERS" THEN FILLED=0;
	IF FILTER="NON-SELECTED RESPONDENT" THEN FILLED=0;
	IF FILTER="NON-RESPONDENT" THEN FILLED=0;
	IF FILTER="YEAR<&YYYY" THEN FILLED=0;
	IF FILTER="ROTATION>&ROTATION" THEN FILLED=0;

	IF FILLED=0 AND SUBSTR(FILTER,1,9)="ROTATION=" THEN DO;
		FILTER = TRANSLATE (FILTER,"NNNNNNNNN","123456789");
		FILTER = TRANWRD (FILTER,"ROTATION=N","ROTATION=NEW");
	END;
RUN;



PROC APPEND BASE=STATS DATA=MNS FORCE;
RUN;

  


%MEND SUMMARY_STAT;


/*********************/
/* OUTLIER DETECTION */
/*********************/

%MACRO OUTLIERS(F=,VARINT=,FACTOR=20, LOWVAL=1);

/**** Calculation of the weights <0 or weights> median weight * factor ;*/
PROC UNIVARIATE DATA=&F NOPRINT;
	VAR &VARINT;
	OUTPUT OUT=UNV N=N SUM=SUM MEDIAN=MEDIAN;
RUN;

PROC SQL;
	CREATE TABLE OUTLIERS_&VARINT AS
	SELECT %SUBSTR(&F,1,1)B030,
		&VARINT,
	     MEDIAN,
		%SYSEVALF(&FACTOR) AS FACTOR,
	   	%SYSEVALF(&LOWVAL) AS LB,
	    MEDIAN * %SYSEVALF(&FACTOR)  AS UB,
	    CASE
	      	WHEN (A.&VARINT < CALCULATED LB) THEN 1
	      	WHEN (A.&VARINT = CALCULATED LB) THEN 2
	      	WHEN (A.&VARINT > CALCULATED UB) THEN 3
	      	ELSE 0
	    END AS OUTLIER
	FROM  &F AS A, UNV AS B
	WHERE &VARINT IS NOT NULL
	;
QUIT;


*** Editing the outlying observations;

DATA OUTLIERS_LOW_&VARINT;SET OUTLIERS_&VARINT (WHERE=(OUTLIER=1));
PROC SORT;BY DESCENDING &VARINT;RUN;
PROC MEANS DATA=OUTLIERS_LOW_&VARINT NOPRINT;VAR OUTLIER;OUTPUT OUT=MNS N=N;RUN;


DATA OUTLIERS_HIGH_&VARINT;SET OUTLIERS_&VARINT (WHERE=(OUTLIER=3));;
PROC SORT;BY DESCENDING &VARINT;RUN;
PROC MEANS DATA=OUTLIERS_HIGH_&VARINT NOPRINT;VAR OUTLIER;OUTPUT OUT=MNS N=N;RUN;


DATA OUTLIERS_LOW_&VARINT (DROP=OUTLIER);
	SET  OUTLIERS_LOW_&VARINT;
	LENGTH VARIABLE $15;
	VARIABLE = "&VARINT";
	WEIGHT = &VARINT;
	ID = %SUBSTR(&F,1,1)B030;
	IF _N_ <= 5;
	BOUND=LB;
RUN;
PROC APPEND BASE=OUTL_LOW DATA=_LAST_ FORCE;RUN;

DATA OUTLIERS_HIGH_&VARINT (DROP=OUTLIER);
	SET  OUTLIERS_HIGH_&VARINT;
	LENGTH VARIABLE $15;
	VARIABLE = "&VARINT";
	WEIGHT = &VARINT;
	ID = %SUBSTR(&F,1,1)B030;
	IF _N_ <= 5;
	BOUND=UB;
RUN;
PROC APPEND BASE=OUTL_HIGH DATA=_LAST_ FORCE;RUN;

%MEND OUTLIERS;



/*******************************/
/* STATISTICS ON THE G-WEIGHTS */
/*******************************/





/*****************************************************************************************/
/************************************* MAIN PROGRAM **************************************/
/*****************************************************************************************/

%MACRO check_WEIGHT;
%mk_AUX_CALIB_DSET;

%LOCAL i IS_SELRESP;

/**
 * CHECK WHETHER cc IS A SELECTED-RESPONDENT-COUNTRY
 */
PROC SQL NOPRINT;
  SELECT MIN(COUNT(*),1) INTO :IS_SELRESP
  FROM   Sval_selresp_cntry_lst
  WHERE  CNTRY = "&cc"
  ;
QUIT;

%PUT *I* SELECTED RESPONDENT (1=Y|0=N): &IS_SELRESP;


/*run this part only for eurostat*/
%IF "&EUSILC" = "/ec/acc/0eusilc" OR "&EUSILC" = "/ec/prod/0eusilc" %THEN %DO;  
	/*STATS ON POPULATION FROM DEMOGRAPHY*/
	PROC IMPORT OUT=POPDEMO DATAFILE="&EUSILC/XML_CONFIG/Total population.xlsx" DBMS=XLSX REPLACE;
	     RANGE="Population$A4:AZ45"; 
	     GETNAMES=YES;
	RUN;

	DATA POPDEMO;SET POPDEMO;
	KEEP CODE __:;
	%DO i=1997 %TO &YYYY;
		rename __&i= _&i;
		__&i=_&i + 0;
	%END;
	CODE=UPCASE(CODE);
	IF CODE="&CC";
	run;
	PROC TRANSPOSE OUT=POPDEMO name=_DB010 PREFIX=_POP_DEMO;BY CODE;RUN;
	DATA POPDEMO ;
		SET POPDEMO (where=(_POP_DEMO1 ne .)) END=LAST_OBS;
		KEEP CODE DB010 PB010 DB075_NEW _POP_DEMO1;RENAME _POP_DEMO1=_POP_DEMO_;
		DB010=COMPRESS(_DB010,"_")+0;
		PB010=DB010;
		DB075_NEW=0;
		IF LAST_OBS=1 THEN OUTPUT;
		CALL SYMPUT('YY_DEMO', DB010);
	RUN;

	%SUMMARY_STAT(F=POPDEMO,VARINT=_POP_DEMO_,MSG=NO);

	/*STATS ON POPULATION 16+ FROM DEMOGRAPHY*/
	PROC IMPORT OUT=POP16DEMO DATAFILE="&EUSILC/XML_CONFIG/popul_16.xlsx" DBMS=XLSX REPLACE;
	     RANGE="Population 16+$A3:AZ46"; 
	     GETNAMES=YES;
	RUN;
	DATA POP16DEMO;SET POP16DEMO;
	KEEP CODE __:;
	%DO i=2004 %TO &YYYY;
		rename __&i= _&i;
		__&i=_&i + 0;
	%END;
	CODE=UPCASE(CODE);
	IF CODE="&CC";
	run;
	PROC TRANSPOSE OUT=POP16DEMO name=_DB010 PREFIX=_16POP_DEMO;BY CODE;RUN;
	DATA POP16DEMO ;
		SET POP16DEMO (where=(_16POP_DEMO1 ne .)) END=LAST_OBS;
		KEEP CODE DB010 PB010 DB075_NEW _16POP_DEMO1;RENAME _16POP_DEMO1=_16POP_DEMO_;
		DB010=COMPRESS(_DB010,"_")+0;
		PB010=DB010;
		DB075_NEW=0;
		IF LAST_OBS=1 THEN OUTPUT;
		CALL SYMPUT('YY_DEMO16', DB010);
	RUN;


	%SUMMARY_STAT(F=POP16DEMO,VARINT=_16POP_DEMO_,MSG=NO);


	/*STATS ON HOUSEHOLDS FROM LFS*/
	PROC IMPORT OUT=HHLFS DATAFILE="&EUSILC/XML_CONFIG/Nbr of HH.xlsx" DBMS=XLSX REPLACE;
	     RANGE="Households$A4:AZ43"; 
	     GETNAMES=YES;
	RUN;

	DATA HHLFS;SET HHLFS;
	KEEP CODE __:;
	%DO i=1998 %TO &YYYY;
		rename __&i= _&i;
		__&i=_&i * 1000;
	%END;
	CODE=UPCASE(CODE);
	IF CODE="&CC";
	run;
	PROC TRANSPOSE OUT=HHLFS name=_DB010 PREFIX=_HH_LFS;BY CODE;RUN;
	DATA HHLFS ;
		SET HHLFS (where=(_HH_LFS1 ne .)) END=LAST_OBS;
		KEEP CODE DB010 HB010 DB075_NEW _HH_LFS1;RENAME _HH_LFS1=_HH_LFS_;
		DB010=COMPRESS(_DB010,"_")+0;
		HB010=DB010;
		DB075_NEW=0;
		IF LAST_OBS=1 THEN OUTPUT;
		CALL SYMPUT('YY_LFS', DB010);
	RUN;


	%SUMMARY_STAT(F=HHLFS,VARINT=_HH_LFS_,MSG=NO);
%END;
/*end of specific eurostat part*/
/*
Main weights used on data files
o DB090 is the cross-sectional weight used for weighting household data;
o RB050 (adjusted) is the cross-sectional weight used for weighting R-file data;
o PB040 is the cross-sectional weight for weighting individual P-file data;
o PB060 is the cross-sectional weight for weighting individuals in the P-file for selected respondent countries. 
Other weights used (base weight or design weight)
o DB080 Household design weight
o RB060: Personal base weight (R-file data;
o PB050 Personal base weight (all household members aged 16 and over)
o PB070 Personal design weight for a selected respondent
o PB080 Personal base weight for a selected respondent
Longitudinal weight
o DB095: Household longitudinal weight
o RB062 Longitudinal weight (two-year duration)
o RB063 Longitudinal weight (three-year duration)
o RB064 Longitudinal weight (four-year duration)
o RB065 Longitudinal weight (five-year duration)
o RB066 Longitudinal weight (six-year duration)
*/

/*D-FILE*/
%SUMMARY_STAT(F=D0,VARINT=DB080,FILTER=DB075 EQ DB075_NEW /*AND DB135 EQ 1*/);*DB080 for new rotation group;
%SUMMARY_STAT(F=D0,VARINT=DB090);*DB090 for HH members;
%SUMMARY_STAT(F=D0,VARINT=DB095/*,FILTER=DB075 NE DB075_NEW AND DB135 EQ 1*/);*DB095 not new rotation group;
%SUMMARY_STAT(F=D2,VARINT=DB080,FILTER=DB075 EQ DB075_NEW ,MSG=NO);*DB080 previous years;
%SUMMARY_STAT(F=D2,VARINT=DB095,MSG=NO);*DB095 previous years;

/*R-FILE*/
%SUMMARY_STAT(F=R0,VARINT=RB050);
%DO I = 1 %TO &ROTATION;
	%LET K= %SCAN(&list_rotation,&I);
	%IF %LENGTH(&K) > 0 %THEN %DO;
		%PUT K : &K;
		%SUMMARY_STAT(F=R0,VARINT=RB060,FILTER=DB075 EQ &K);
		%SUMMARY_STAT(F=R2,VARINT=RB060,FILTER=DB075 EQ &K);
	%END;
%END;

%SUMMARY_STAT(F=R0,VARINT=RB062,MSG=NO);
%SUMMARY_STAT(F=R0,VARINT=RB063,MSG=NO);
%SUMMARY_STAT(F=R0,VARINT=RB064,MSG=NO);
%IF &ROTATION > 4 %THEN %DO;
%SUMMARY_STAT(F=R0,VARINT=RB065,MSG=NO);
%SUMMARY_STAT(F=R0,VARINT=RB066,MSG=NO);
%END;

/*P-FILE*/

%SUMMARY_STAT(F=P0,VARINT=PB040);
%DO I = 1 %TO &ROTATION;
	%LET K= %SCAN(&list_rotation,&I);
	%IF %LENGTH(&K) > 0 %THEN %DO;
		%SUMMARY_STAT(F=P0,VARINT=PB050,FILTER=DB075 EQ &K);
		%SUMMARY_STAT(F=P2,VARINT=PB050,FILTER=DB075 EQ &K);
	%END;
%END;

/*PREVIOUS YEARS (MUST BE . OR 0)*/
%SUMMARY_STAT(F=D2,VARINT=DB090,FILTER=DB010<&YYYY);
%SUMMARY_STAT(F=R2,VARINT=RB050,FILTER=RB010<&YYYY);
%SUMMARY_STAT(F=R2,VARINT=RB062,FILTER=RB010<&YYYY);
%SUMMARY_STAT(F=R2,VARINT=RB063,FILTER=RB010<&YYYY);
%SUMMARY_STAT(F=R2,VARINT=RB064,FILTER=RB010<&YYYY);
%SUMMARY_STAT(F=R2,VARINT=RB065,FILTER=RB010<&YYYY);
%SUMMARY_STAT(F=R2,VARINT=RB066,FILTER=RB010<&YYYY);
%SUMMARY_STAT(F=P2,VARINT=PB040,FILTER=PB010<&YYYY);
%SUMMARY_STAT(F=P2,VARINT=PB060,FILTER=PB010<&YYYY);

/*NON-HH MEMBERS (MUST BE . OR 0)*/
%SUMMARY_STAT(F=R0,VARINT=RB050,FILTER=RB110 GT 4);
%SUMMARY_STAT(F=R0,VARINT=RB060,FILTER=RB110 GT 4);
%SUMMARY_STAT(F=R0,VARINT=RB062,FILTER=RB110 GT 4);
%SUMMARY_STAT(F=R0,VARINT=RB063,FILTER=RB110 GT 4);
%SUMMARY_STAT(F=R0,VARINT=RB064,FILTER=RB110 GT 4);
%SUMMARY_STAT(F=R0,VARINT=RB065,FILTER=RB110 GT 4);
%SUMMARY_STAT(F=R0,VARINT=RB066,FILTER=RB110 GT 4);
%SUMMARY_STAT(F=P0,VARINT=PB040,FILTER=RB110 GT 4);
%SUMMARY_STAT(F=P0,VARINT=PB050,FILTER=RB110 GT 4);
%SUMMARY_STAT(F=R2,VARINT=RB050,FILTER=RB110 GT 4);
%SUMMARY_STAT(F=R2,VARINT=RB060,FILTER=RB110 GT 4);
%SUMMARY_STAT(F=R2,VARINT=RB062,FILTER=RB110 GT 4);
%SUMMARY_STAT(F=R2,VARINT=RB063,FILTER=RB110 GT 4);
%SUMMARY_STAT(F=R2,VARINT=RB064,FILTER=RB110 GT 4);
%SUMMARY_STAT(F=R2,VARINT=RB065,FILTER=RB110 GT 4);
%SUMMARY_STAT(F=R2,VARINT=RB066,FILTER=RB110 GT 4);
%SUMMARY_STAT(F=P2,VARINT=PB040,FILTER=RB110 GT 4);
%SUMMARY_STAT(F=P2,VARINT=PB050,FILTER=RB110 GT 4);

/*NON-RESPONDENTS (MUST BE . OR 0)*/
%SUMMARY_STAT(F=D0,VARINT=DB090,FILTER=DB135 NE 1);
%SUMMARY_STAT(F=D0,VARINT=DB095,FILTER=DB135 NE 1);
%SUMMARY_STAT(F=D2,VARINT=DB090,FILTER=DB135 NE 1);
%SUMMARY_STAT(F=D2,VARINT=DB095,FILTER=DB135 NE 1);


%IF &IS_SELRESP EQ 1 %THEN %DO;
    %SUMMARY_STAT(F=P1,VARINT=PB060,MSG=Y);
    %SUMMARY_STAT(F=P0,VARINT=PB070,FILTER=DB075 EQ DB075_NEW,MSG=NO);
	%DO I = 1 %TO &ROTATION;
		%LET K= %SCAN(&list_rotation,&I);
	    %SUMMARY_STAT(F=P0,VARINT=PB080,FILTER=DB075 EQ &K , MSG=NO);
	    %SUMMARY_STAT(F=P2,VARINT=PB080,FILTER=DB075 EQ &K, MSG=NO);
	%END;
	/*NON SELECTED RESPONDENTS (MUST BE . OR 0)*/
    %SUMMARY_STAT(F=P0,VARINT=PB060,FILTER=RB245 NE 2);
    %SUMMARY_STAT(F=P0,VARINT=PB070,FILTER=RB245 NE 2);
    %SUMMARY_STAT(F=P0,VARINT=PB080,FILTER=RB245 NE 2);

%END;

/*OUTLIERS*/
%OUTLIERS(F=D1,VARINT=DB090);
%OUTLIERS(F=D1,VARINT=DB095);
%OUTLIERS(F=R0,VARINT=RB050);
%OUTLIERS(F=R0,VARINT=RL070);/*child weight*/
%OUTLIERS(F=P0,VARINT=PB040);
%IF &IS_SELRESP EQ 1 %THEN %DO;
    %OUTLIERS(F=P1,VARINT=PB060);
%END;



OPTIONS NODATE NONUMBER;
ODS &OUTPUTFORMAT FILE = "&OUT&_dirsp_%quote(&CC&YY.-Weights.&EXTENSION)" &OUTOPTION ;
PROC FORMAT ;
	PICTURE milliers /*(ROUND)*/
	0 - HIGH = "000 000 009"
	;
	PICTURE millierscomma /*(ROUND)*/
	0 - HIGH = "000 000 009.00"
	;
  VALUE ROT
  LOW-0.9 = 'red'
  1-&ROTATION = 'light green'
  &ROTATION-high = 'orange'
  ;
RUN;


%IF "&EUSILC" = "/ec/acc/0eusilc" OR "&EUSILC" = "/ec/prod/0eusilc" %THEN %DO;  
	DATA STATS_REF;
		SET STATS;
		IF VARIABLE IN ("_POP_DEMO_", "_16POP_DEMO_", "_HH_LFS_") AND FILTER="";
		IF VARIABLE ="_POP_DEMO_" THEN VARIABLE ="R";
		IF VARIABLE ="_16POP_DEMO_" THEN VARIABLE ="P"; 
		IF VARIABLE ="_HH_LFS_" THEN VARIABLE ="D";
		KEEP VARIABLE SUM;
		RENAME SUM=SUM_REF;
	RUN;
%END;
%ELSE %DO;
	DATA STATS_REF;
		SET STATS;
		IF VARIABLE IN ("DB090", "RB050", "PB040") AND YEAR=&YYYY AND FILTER="";
		KEEP VARIABLE SUM;
		RENAME SUM=SUM_REF;
	RUN;
%END;


PROC SQL;CREATE TABLE STATS AS SELECT STATS.* , STATS_REF.SUM_REF FROM STATS LEFT JOIN STATS_REF ON SUBSTR(STATS.VARIABLE,1,1)=SUBSTR(STATS_REF.VARIABLE,1,1);QUIT;RUN;
DATA STATS;SET STATS;IF SUM_REF=. THEN SUM_REF=SUM;RUN;
PROC SQL;
	CREATE TABLE STATS AS SELECT a.*, b.DB075_NEW FROM STATS a LEFT JOIN ROTATION_GROUPS b 
	ON a.YEAR=b.DB010;
QUIT;RUN;
PROC SORT;BY YEAR VARIABLE FILTER;RUN;


/*RESULTS*/
TITLE1 "&CC - &YYYY / TRANSMISSION=&SS";
TITLE2 "&YYYY ROTATION GROUPS BY YEAR";
%ROTATION_GROUPS;

TITLE1 "&CC - &YYYY / TRANSMISSION=&SS";
TITLE2 "&YYYY WEIGHTS D-FILE, SUMS MUST BE APPROXIMATELY EQUAL TO TOTAL NUMBER OF HOUSEHOLDS";


PROC REPORT DATA=STATS (WHERE=((FILLED=1 AND FILE="D") OR SUBSTR(VARIABLE,1,3) ="_HH")) spanrows;
	COLUMNS VARIABLE YEAR FILTER DB075_NEW N NMISS SUM_REF SUM TARGET;
	DEFINE YEAR / group format=4.0 ;*style=[background=#EDF2F9 fontweight=bold color=navy];
	DEFINE VARIABLE / group format=$VARLABEL. ;*style=[background=#EDF2F9 fontweight=bold color=navy];
	DEFINE FILTER / display format=$50. ;*style=[background=#EDF2F9];
	DEFINE DB075_NEW / noprint;
	DEFINE N / display format=milliers.;
	DEFINE NMISS / display format=milliers.;
	DEFINE SUM_REF / noprint;
	DEFINE SUM / display format=milliers.;
	DEFINE TARGET / computed format=milliers. ;*style(column)=[background=#EDF2F9];
	COMPUTE VARIABLE;
		IF SUBSTR(VARIABLE,1,1)="_"  then call define  (_row_,'style','style=[background=lightblue');
	ENDCOMP;
	COMPUTE SUM;
		IF ABS(SUM/SUM_REF.sum - 1)>0.1  OR  SUM = .  then call define  (_col_,'style','style=[background=Yellow');
		IF ABS(SUM/SUM_REF.sum - 1)>0.2  OR  SUM = .  then call define  (_col_,'style','style=[background=Orange');
		IF ABS(SUM/SUM_REF.sum - 1)>0.5  OR  SUM = .  then call define  (_col_,'style','style=[background=Orange');
		IF ABS(SUM/SUM_REF.sum - 1)<=0.1 AND SUM NE . then call define  (_col_,'style','style=[background=lightgreen');
	ENDCOMP;
	COMPUTE TARGET;
		TARGET=ROUND(SUM_REF.sum,1000);
	ENDCOMP;
RUN; 

TITLE1 "&CC - &YYYY / TRANSMISSION=&SS";
TITLE2 "&YYYY WEIGHTS R-FILE, SUMS MUST BE APPROXIMATELY EQUAL TO TOTAL POPULATION";

PROC REPORT DATA=STATS (WHERE=((FILLED=1 AND FILE="R") OR SUBSTR(VARIABLE,1,3) ="_PO")) spanrows;
	COLUMNS VARIABLE YEAR FILTER N NMISS SUM_REF SUM TARGET;
	DEFINE YEAR / group format=4.0 ;*style=[background=#EDF2F9 fontweight=bold color=navy];
	DEFINE VARIABLE / group format=$VARLABEL. ;*style=[background=#EDF2F9 fontweight=bold color=navy];
	DEFINE FILTER / display format=$50. ;*style=[background=#EDF2F9];
	DEFINE N / display format=milliers.;
	DEFINE NMISS / display format=milliers.;
	DEFINE SUM_REF / noprint;
	DEFINE SUM / display format=milliers.;
	DEFINE TARGET / computed format=milliers. ;*style(column)=[background=#EDF2F9];
	COMPUTE VARIABLE;
		IF SUBSTR(VARIABLE,1,1)="_"  then call define  (_row_,'style','style=[background=lightblue');
	ENDCOMP;
	COMPUTE SUM;
		IF ABS(SUM/SUM_REF.sum - 1)>0.1  OR  SUM = .  then call define  (_col_,'style','style=[background=Yellow');
		IF ABS(SUM/SUM_REF.sum - 1)>0.2  OR  SUM = .  then call define  (_col_,'style','style=[background=Orange');
		IF ABS(SUM/SUM_REF.sum - 1)>0.5  OR  SUM = .  then call define  (_col_,'style','style=[background=Orange');
		IF ABS(SUM/SUM_REF.sum - 1)<=0.1 AND SUM NE . then call define  (_col_,'style','style=[background=lightgreen');
	ENDCOMP;
	COMPUTE TARGET;
		TARGET=ROUND(SUM_REF.sum,1000);
	ENDCOMP;
RUN; 

TITLE1 "&CC - &YYYY / TRANSMISSION=&SS";
TITLE2 "&YYYY WEIGHTS P-FILE, SUMS MUST BE APPROXIMATELY EQUAL TO POPULATION 16+";

PROC REPORT DATA=STATS (WHERE=((FILLED=1 AND FILE="P") OR SUBSTR(VARIABLE,1,3) ="_16")) spanrows;
	COLUMNS VARIABLE YEAR FILTER N NMISS SUM_REF SUM TARGET;
	DEFINE YEAR / group format=4.0 ;*style=[background=#EDF2F9 fontweight=bold color=navy];
	DEFINE VARIABLE / group format=$VARLABEL. ;*style=[background=#EDF2F9 fontweight=bold color=navy];
	DEFINE FILTER / display format=$50. ;*style=[background=#EDF2F9];
	DEFINE N / display format=milliers.;
	DEFINE NMISS / display format=milliers.;
	DEFINE SUM_REF / noprint;
	DEFINE SUM / display format=milliers.;
	DEFINE TARGET / computed format=milliers. ;*style(column)=[background=#EDF2F9];
	COMPUTE VARIABLE;
		IF SUBSTR(VARIABLE,1,1)="_"  then call define  (_row_,'style','style=[background=lightblue');
	ENDCOMP;
	COMPUTE SUM;
		IF ABS(SUM/SUM_REF.sum - 1)>0.1  OR  SUM = .  then call define  (_col_,'style','style=[background=Yellow');
		IF ABS(SUM/SUM_REF.sum - 1)>0.2  OR  SUM = .  then call define  (_col_,'style','style=[background=Orange');
		IF ABS(SUM/SUM_REF.sum - 1)>0.5  OR  SUM = .  then call define  (_col_,'style','style=[background=Orange');
	IF ABS(SUM/SUM_REF.sum - 1)<=0.1 AND SUM NE . then call define  (_col_,'style','style=[background=lightgreen');
	ENDCOMP;
	COMPUTE TARGET;
		TARGET=ROUND(SUM_REF.sum,1000);
	ENDCOMP;
RUN; 

/*TABLE DISABLED? REPLACED BY LOGICAL CHECKS*/
/*
TITLE1 "&CC - &YYYY / TRANSMISSION=&SS";
TITLE2 "&YYYY WEIGHTS, OUT OF SCOPE, SUMS MUST BE 0 OR MISSING";

PROC REPORT DATA=STATS (WHERE=(FILLED=0)) spanrows;
	COLUMNS VARIABLE YEAR FILTER N NMISS SUM;
	DEFINE YEAR / group format=4.0 ;*style=[background=#EDF2F9 fontweight=bold color=navy];
	DEFINE VARIABLE / group format=$VARLABEL. ;*style=[background=#EDF2F9 fontweight=bold color=navy];
	DEFINE FILTER / display format=$50. ;*style=[background=#EDF2F9];
	DEFINE N / display format=milliers.;
	DEFINE NMISS / display format=milliers.;
	DEFINE SUM / display format=milliers.;
	COMPUTE SUM;
	IF SUM>0 then call define  (_col_,'style','style=[background=Orange');
	IF SUM=. OR SUM=0 then call define  (_col_,'style','style=[background=lightgreen');
	IF FILTER="ROTATION>&ROTATION" then call define  (_col_,'style','style=[background=red');
	ENDCOMP;
RUN; 
*/


TITLE1 "&CC - &YYYY / TRANSMISSION=&SS";
TITLE2 "CROSS-WEIGHT LOW OUTLIERS, LOWER THAN BOUND (5 first obs)";

PROC TABULATE DATA=OUTL_LOW F=millierscomma.;
  CLASS VARIABLE ID;
  FORMAT VARIABLE $VARLABEL. ;
  VAR WEIGHT BOUND;
  TABLE VARIABLE*ID, WEIGHT BOUND;
  KEYLABEL SUM = ' ';
RUN;
TITLE1 "&CC - &YYYY / TRANSMISSION=&SS";
TITLE2 "CROSS-WEIGHT HIGH OUTLIERS, GREATER THAN BOUND = FACTOR * MEDIAN (5 first obs)";
PROC TABULATE DATA=OUTL_HIGH F=milliers.;
  CLASS VARIABLE FACTOR ID;
  FORMAT VARIABLE $VARLABEL. ;
  VAR WEIGHT BOUND MEDIAN  ;
  TABLE VARIABLE*FACTOR*ID, WEIGHT BOUND MEDIAN*f=millierscomma. ;
  KEYLABEL SUM = ' ';
RUN;



/*DISPLAY DATE OF EXECUTION*/
PROC REPORT DATA=HEADER;
  COLUMN MSG;
RUN;

TITLE1;
TITLE2;

ODS &OUTPUTFORMAT CLOSE;

%MEND check_WEIGHT;







