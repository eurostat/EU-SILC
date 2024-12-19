/**
 * Revision History
 * - AUGUST 3rd 2022
 */

/**
 * THIS MODULE PROVIDES THE FUNCTIONALITIES TARGETED
 * TO THE LOGICAL VALIDATION CHECKS
 */
%MACRO getLVAL;

%LOCAL INDIR INFNAME MAPDIR MAPNAME;
%LOCAL N_LOOPS N_STEPS I;

%LET INDIR=%SYSFUNC(PATHNAME(XMLCFG))&_dirsp_%quote(LVAL);
%LET INFNAME=LVAL-R-&YYYY..xml;

LIBNAME IN_XML XMLV2 "&INDIR&_dirsp_%quote(&INFNAME)" XMLMAP="%SYSFUNC(PATHNAME(MAPDIR))&_dirsp_%quote(LVAL.map)";
DATA LVAL;
SET  IN_XML.LVAL (in=A);
  SELECT = UPCASE(SELECT);
  FROM = UPCASE(FROM);
  JOIN = UPCASE(JOIN);
  WHERE = UPCASE(WHERE);
  GROUP = UPCASE(GROUP);
  HAVING = UPCASE(HAVING);
  ID_num = INPUT(ID,BEST.);
RUN;

%PUT *I* LVAL: FILTER PAST MODULE CHECKS;

DATA LVAL;
SET LVAL;
  MODULE = UPCASE(MODULE);
  IF MODULE = 'RECUR3' THEN DO;
      MOD_T = MOD( &YYYY - START, 3 );
      IF MOD_T eq 0 THEN OUTPUT;
  END;
  ELSE IF MODULE = 'RECUR6' THEN DO;
      MOD_T = MOD( &YYYY - START, 6 );
      IF MOD_T eq 0 THEN OUTPUT;
  END;
  ELSE DO;
  	  IF START = . and END = . THEN OUTPUT;
  	  ELSE IF &YYYY GE START AND END = . THEN OUTPUT;
  	  ELSE IF START = . AND &YYYY LE END THEN OUTPUT;
  	  ELSE IF &YYYY GE START AND &YYYY LE END THEN OUTPUT;
  END;
RUN;

PROC SORT;
  BY ID_num;
RUN;

/**
 * LOGICAL VALIDATION LOOP Handling
 *        [ADVANCED FEATURE]
 */
%LET N_LOOPS=0;
DATA LVAL_LOOP;
SET  IN_XML.LVAL_LOOP (in=A);
  X = UPCASE(X);
  CALL SYMPUTX('N_LOOPS',_N_);
  ID_num = INPUT(ID,BEST.);
PROC SORT;
  BY ID_num;
RUN;

%PUT *I* LVAL: N OF LOOPED CHECKS: &N_LOOP;

DATA LVAL;
MERGE LVAL (IN=A) LVAL_LOOP;
  BY ID_num;
  IF A;
RUN;

%IF &N_LOOPS GT 0 %THEN %DO;

        DATA LVAL (DROP=lg_ID_num lg_ID);
        SET  LVAL;
          RETAIN lg_ID_num lg_ID;
          IF X ^= '' THEN DO;
                 PUT 'HANDLING LOOPED CHECK=' ID ' ON X=' X;
             SELECT = TRANWRD(SELECT,'$X',TRIM(X));
             ON     = TRANWRD(ON,'$X',TRIM(X));
             WHERE  = TRANWRD(WHERE,'$X',TRIM(X));
             GROUP  = TRANWRD(GROUP,'$X',TRIM(X));
             HAVING = TRANWRD(HAVING,'$X',TRIM(X));
                 TITLE  = TRANWRD(TITLE,'$X',TRIM(X));
                 TITLE  = TRANWRD(TITLE,'$x',TRIM(X));
                 IF lg_ID = ID THEN DO;
                        ID_num = lg_ID_num + 1;
                 END;
          END;
          lg_ID_num = ID_num;
          lg_ID     = ID;
        RUN;

        DATA LVAL;
        SET  LVAL;
                ID = compress(PUT(ID_num,BEST.));
        PROC SORT;
      BY ID_num;
        RUN;
%END;

/**
 * LOGICAL VALIDATION STEPS Handling
 *        [ADVANCED FEATURE]
 */
DATA LVAL_STEPS;
SET  IN_XML.LVAL_STEPS;
RUN;

%MEND getLVAL;

%MACRO getSQL;

DATA LVAL (drop=txt:);
SET  LVAL;
  LENGTH SQL txt1 txt2 txt22 txt23 txt3 txt4 txt5 $32000;
  txt1 = '';   txt2 = '';   txt22 = '';   txt23 = '';   txt3 = '';   txt4 = '';   txt5 = '';
  txt1 = "SELECT " || trim(SELECT) || " FROM " || trim(FROM) || " AS a";
  IF JOIN ne "" THEN DO;
         txt2 = "LEFT JOIN " || trim(JOIN) ||" AS b ON " || trim(ON);
  END;
  IF JOIN2 ne "" THEN DO;
         txt22 = "LEFT JOIN " || trim(JOIN2) ||" AS c ON " || trim(ON2);
  END;
  IF JOIN3 ne "" THEN DO;
         txt23 = "LEFT JOIN " || trim(JOIN3) ||" AS d ON " || trim(ON3);
  END;
  IF WHERE NE "" THEN DO;
     WHERE = TRANWRD(WHERE,'$YEAR',"&YYYY");
         txt3 = "WHERE " || trim (WHERE);
  END;
  IF GROUP NE "" THEN DO;
     HAVING = TRANWRD(HAVING,'$YEAR',"&YYYY");
         txt4 = "GROUP BY " || trim(GROUP)||" HAVING " || trim(HAVING);
  END;
  txt5 = "ORDER BY " || scan(SELECT,1,",")||" DESC," || scan(SELECT,2,",");
  SQL = trim(txt1)||' '||trim(txt2)||' '||trim(txt22)||' '||trim(txt23)||' '||trim(txt3)||' '||trim(txt4)||' '||trim(txt5)||' ';
  XB010 = SUBSTR(FROM,1,1) || 'B010';
RUN;

%MEND getSQL;

%MACRO purgeSQL;/*delete checks using obsolete variables*/

%LOCAL VARS;

%PUT *I* PURGING LOGICAL VALID CHECKS ...
;

/*select variables from D file and put them in macro var &VARS_D*/
%IF %SYSFUNC(EXIST(RAW.&ss&cc&YY.D)) %THEN %DO;
PROC CONTENTS DATA=RAW.&ss&cc&YY.D OUT=DICTIONARY_D NOPRINT;
RUN;
PROC SQL NOPRINT;
  SELECT DISTINCT "'"||TRIM(UPCASE(NAME))||"'" INTO :VARS_D SEPARATED BY ' ' FROM DICTIONARY_D ;
QUIT;
%END;
%ELSE %LET VARS_D='FOO';

/*select variables from R file and put them in macro var &VARS_R*/
%IF %SYSFUNC(EXIST(RAW.&ss&cc&YY.R)) %THEN %DO;
PROC CONTENTS DATA=RAW.&ss&cc&YY.R OUT=DICTIONARY_R NOPRINT;
RUN;
PROC SQL NOPRINT;
  SELECT DISTINCT "'"||TRIM(UPCASE(NAME))||"'" INTO :VARS_R SEPARATED BY ' ' FROM DICTIONARY_R ;
QUIT;
%END;
%ELSE %LET VARS_R='FOO';

/*select variables from H file and put them in macro var &VARS_H*/
%IF %SYSFUNC(EXIST(RAW.&ss&cc&YY.H)) %THEN %DO;
PROC CONTENTS DATA=RAW.&ss&cc&YY.H OUT=DICTIONARY_H NOPRINT;
RUN;
PROC SQL NOPRINT;
  SELECT DISTINCT "'"||TRIM(UPCASE(NAME))||"'" INTO :VARS_H SEPARATED BY ' ' FROM DICTIONARY_H ;
QUIT;
%END;
%ELSE %LET VARS_H='FOO';

/*select variables from P file and put them in macro var &VARS_P*/
%IF %SYSFUNC(EXIST(RAW.&ss&cc&YY.P)) %THEN %DO;
PROC CONTENTS DATA=RAW.&ss&cc&YY.P OUT=DICTIONARY_P NOPRINT;
RUN;
PROC SQL NOPRINT;
  SELECT DISTINCT "'"||TRIM(UPCASE(NAME))||"'" INTO :VARS_P SEPARATED BY ' ' FROM DICTIONARY_P ;
%END;
%ELSE %LET VARS_P='FOO';

DATA LVAL;
SET  LVAL;
  LENGTH NOT_FOUND_TOK $32;
  SKIP = 0;
  K = 1;
  /*clean SELECT clause to keep variables only*/
	select_clean=SELECT;
	select_clean=tranwrd(select_clean,'as',',') ;
	select_clean=translate(select_clean, ',,,,,,', '()+-/*');
	select_clean=compress(select_clean," ");
	select_clean=tranwrd(select_clean,',,',',') ;
	select_clean=tranwrd(select_clean,'sum,','') ;
	select_clean=tranwrd(select_clean,'count,','') ;
	select_clean=tranwrd(select_clean,'mean,','') ;
	select_clean=tranwrd(select_clean,'min,','') ;
	select_clean=tranwrd(select_clean,'max,','') ;
	select_clean=tranwrd(select_clean,'floor,','') ;
	select_clean=tranwrd(select_clean,'ceil,','') ;
	select_clean=compress(select_clean," "); 

  /*scan the words (separated by ,) in the cleaned select clause of each lval check*/
  TOK = UPCASE(SCAN(select_clean,K,','));
  DO WHILE (TOK NE '');
     TOK = SCAN(TOK,1,' ');
         IF SUBSTR(TOK,1,2) IN ( 'A.' 'B.' 'C.' 'D.' ) THEN DO;
                IF SUBSTR(TOK,1,1) = 'A' THEN DO;
                   F = COMPRESS(FROM);
                END;
                ELSE IF SUBSTR(TOK,1,1) = 'B' THEN DO;
                   F = COMPRESS(JOIN);
                END;
                ELSE IF SUBSTR(TOK,1,1) = 'C' THEN DO;
                   F = COMPRESS(JOIN2);
                END;
                ELSE IF SUBSTR(TOK,1,1) = 'D' THEN DO;
                   F = COMPRESS(JOIN3);
                END;
                TOK = SUBSTR(TOK,3);
         END;
         ELSE DO;
                F = SUBSTR(TOK,1,1);
         END;
         /**
          * TOKEN Handling
          */
     RES = 0;
         IF (LENGTH(TOK)>6) 
			or (SUBSTR(TOK,1,1) eq '_') 
			or (SUBSTR(TOK,1,1) in ('0','1','2','3','4','5','6','7','8','9'))
			or TOK in ("SUM","MEAN","MAX","MIN","COUNT","CEIL","FLOOR","SUBSTR") 
		 THEN RES = 1;
         ELSE DO;
             IF F EQ 'D'      THEN RES = (TOK IN (&VARS_D));
             ELSE IF F EQ 'R' THEN RES = (TOK IN (&VARS_R));
             ELSE IF F EQ 'H' THEN RES = (TOK IN (&VARS_H));
             ELSE IF F EQ 'P' THEN RES = (TOK IN (&VARS_P));
                 ELSE RES = (TOK IN (&VARS_D &VARS_R &VARS_H &VARS_P));
         END;
         IF RES = 0 THEN DO;
            SKIP = 1;
                NOT_FOUND_TOK = TOK;
         END;
         K = K + 1;
         TOK = UPCASE(SCAN(select_clean,K,','));
  END;
RUN;

DATA LVAL;
SET  LVAL;
  IF SKIP = 1 THEN DO;
         PUT '*I* LVAL: PURGING ID=' ID ' ...';
         PUT '*I* LVAL: TOKEN=' NOT_FOUND_TOK ' ' 'NOT Found';
         DELETE;
  END;
  /* IF ID_num lt 999; FILTER */
RUN;


%MEND purgeSQL;


%MACRO check_SQL;

%LOCAL K N_SQL CNT ID XB010 MAXCNT N_STEPS J TITLE ID SQL SELECT;

OPTIONS NODATE NONUMBER;
*ODS PDF FILE = "&OUT&_dirsp_%quote(LVAL-&SS-&CC-&YY..pdf)";
ODS &OUTPUTFORMAT FILE = "&OUT&_dirsp_%quote(&CC&YY.-Logical.&EXTENSION)" &OUTOPTION ;


TITLE1 "LOGICAL CHECKS";
TITLE2 "&CC - &YYYY / TRANSMISSION=&SS";

DATA _NULL_;
SET  LVAL NOBS=NOBS;
  CALL SYMPUTX('N_SQL',NOBS);
  STOP;
RUN;

/**
 * LOGICAL VALIDATION PRE-PROCESSING
 */
%LET N_STEPS=0;
DATA LVAL_STEPS;
SET  LVAL_STEPS;
  CALL SYMPUTX('N_STEPS',_N_);
RUN;

  %IF %SYSFUNC(EXIST(RAW.&ss&cc&YY.D)) %THEN %DO;
DATA D;
SET  RAW.&ss&cc&YY.D;
RUN;
%END;

  %IF %SYSFUNC(EXIST(RAW.&ss&cc&YY.R)) %THEN %DO;
DATA R;
SET  RAW.&ss&cc&YY.R;
RUN;
%END;

  %IF %SYSFUNC(EXIST(RAW.&ss&cc&YY.H)) %THEN %DO;
DATA H;
SET  RAW.&ss&cc&YY.H;
RUN;
%END;

  %IF %SYSFUNC(EXIST(RAW.&ss&cc&YY.P)) %THEN %DO;
DATA P;
SET  RAW.&ss&cc&YY.P;
RUN;
%END;

%IF &N_STEPS GT 0 %THEN %DO;

        %DO J = 1 %TO &N_STEPS;

        DATA _NULL_;
        SET  LVAL_STEPS (FIRSTOBS=&J OBS=&J);
          CALL SYMPUTX('STEP',STEP);
        RUN;

        %PUT PREPROCESSING STEP: &STEP
        ;
        %&STEP;

        %END;

%END;

/**
 * LOGICAL VALIDATION LOOP
 */
DATA MISM_CSV;
  LENGTH ID $5;
  STOP;
RUN;

DATA MISM_SQL;
        LENGTH ID $8;
        LENGTH XB010 8;
        LENGTH _FREQ_ 8;
        STOP;
RUN;

%PUT *I* check_SQL:Cnt &N_SQL;

PROC SORT DATA=LVAL;
  BY ID_num;
RUN;

%LET MAXCNT=0;
%LET K=1;
%DO K = 1 %TO %SYSFUNC(MIN(9999,&N_SQL));

    /**
     * TITLE
     */
    %LET TITLE_2=;
    %LET TITLE_3=;
    %LET TITLE_4=;
    %LET TITLE_5=;
    %LET TITLE_6=;
    %LET TITLE_7=;
    %LET TITLE_8=;
    %LET TITLE_9=;
    %LET TITLE_10=;
    %LET TITLE_11=;

    DATA _NULL_;
      SET  LVAL (FIRSTOBS=&K OBS=&K KEEP=ID TITLE SELECT SQL XB010);
      CALL SYMPUT('ID',TRIM(ID));
      CALL SYMPUT('TITLE',TRIM(TITLE));
      CALL SYMPUT('XB010',TRIM(XB010));
      CALL SYMPUT('SQL',TRIM(SQL));

	  /*perl expression to replace , by . when between parentheses*/
	  SELECT = prxchange('s/\((\w+ *)(,)( *\w+)\)/($1.$3)/', -1, SELECT);
	  SELECT = prxchange('s/\((\w+ *)(,)( *\w+ *)(,)( *\w+ *)\)/($1.$3.$5)/', -1, SELECT);


      K = 1; T = 1;
      TOK = UPCASE(SCAN(SELECT,K,','));

      DO WHILE (TOK NE '');
         TOK2 = SCAN(TOK,1,' ');
         IF SUBSTR(TOK2,1,2) IN ( 'A.' 'B.' 'C.' 'D.') THEN DO;
            TOK2 = SUBSTR(TOK2,3);
         END;
         SFX = UPCASE(SUBSTR(TOK2,LENGTH(TOK2)-1,2));
         IF SFX ne '_F' THEN DO;
            IF LENGTH(PUT(TOK2,$PRELABEL.)) gt LENGTH(TOK2) THEN DO;
               CALL SYMPUT('TITLE_'||COMPRESS(PUT(T+1,2.)),TRIM(TRANWRD(PUT(TOK2,$PRELABEL.),"'","")));
            END;
            ELSE IF LENGTH(PUT(TOK2,$VARLABEL.)) gt LENGTH(TOK2) THEN DO;
               CALL SYMPUT('TITLE_'||COMPRESS(PUT(T+1,2.)),TRIM(TRANWRD(PUT(TOK2,$VARLABEL.),"'","")));
            END;
            ELSE DO;
			   TOK=COMPBL(prxchange('s/^(.*?)\s+(?i)as\s+(.*)$/$2=$1/', -1, TOK));
               CALL SYMPUT('TITLE_'||COMPRESS(PUT(T+1,2.)),TRIM(TRANWRD(PUT(TOK,$80.),"'","")));
            END;
            T + 1;
         END;
         K + 1;
         TOK = UPCASE(SCAN(SELECT,K,','));
      END;
    RUN;
    %PUT TITLE_2=&TITLE_2;
    %PUT TITLE_3=&TITLE_3;
    %PUT TITLE_4=&TITLE_4;
    %PUT TITLE_5=&TITLE_5;
    %PUT TITLE_6=&TITLE_6;
    %PUT TITLE_7=&TITLE_7;
    %PUT TITLE_8=&TITLE_8;
    %PUT TITLE_9=&TITLE_9;
    %PUT TITLE_10=&TITLE_10;
    %PUT TITLE_11=&TITLE_11;

        PROC SQL;
          CREATE TABLE ID_&ID AS
          %QUOTE(&SQL)
          ;
        QUIT;

        %LET CNT=0;
        PROC SQL NOPRINT;
      SELECT COUNT(*) INTO :CNT FROM ID_&ID
          ;
    QUIT;

        /**
         * INCONSISTENCY HANDLING
         * CNT > 0 -> INCONSISTENCY FOUND
         */
        %IF &CNT GT 0 %THEN %DO;

            %IF &CNT GT &MAXCNT %THEN %DO;
                %LET MAXCNT=&CNT;
            %END;

            DATA MISM_CSV;
            SET  MISM_CSV ID_&ID (IN=B);
                 IF B THEN ID = "&ID";
            RUN;

            /**
             * UPDATE WARNING STATISTICS (COUNTS by YEAR)
             */
            DATA ID_&ID ;
            SET  ID_&ID;
                 ID = "&ID";
				 XB010=&XB010;
				 IF ID="108" THEN XB010=__DB010_prev+1;*XB010 filled for check 108 to have counting on next step;
            RUN;

            PROC MEANS DATA=ID_&ID NWAY NOPRINT;
                       CLASS ID XB010;
                       OUTPUT OUT=MNS NMISS(XB010)=N;
            PROC APPEND BASE=MISM_SQL FORCE;
            RUN;

            /**
             * REPORT
             */
            %reduce_LVAL_TBL(TBL=ID_&ID);
            TITLE1 "#&ID - %QUOTE(&TITLE)";
            %IF %LENGTH("&TITLE_2")  ne 2 %THEN  TITLE2  BOX=1 C=PGR  ITALIC H=2 JUSTIFY=LEFT %QUOTE(%sysfunc(translate("&TITLE_2","()","[]")));;
            %IF %LENGTH("&TITLE_3")  ne 2 %THEN  TITLE3  BOX=1 C=MEGR ITALIC H=2 JUSTIFY=LEFT %QUOTE(%sysfunc(translate("&TITLE_3","()","[]")));;
            %IF %LENGTH("&TITLE_4")  ne 2 %THEN  TITLE4  BOX=1 C=PGR  ITALIC H=2 JUSTIFY=LEFT %QUOTE(%sysfunc(translate("&TITLE_4","()","[]")));;
            %IF %LENGTH("&TITLE_5")  ne 2 %THEN  TITLE5  BOX=1 C=MEGR ITALIC H=2 JUSTIFY=LEFT %QUOTE(%sysfunc(translate("&TITLE_5","()","[]")));;
            %IF %LENGTH("&TITLE_6")  ne 2 %THEN  TITLE6  BOX=1 C=PGR  ITALIC H=2 JUSTIFY=LEFT %QUOTE(%sysfunc(translate("&TITLE_6","()","[]")));;
            %IF %LENGTH("&TITLE_7")  ne 2 %THEN  TITLE7  BOX=1 C=MEGR ITALIC H=2 JUSTIFY=LEFT %QUOTE(%sysfunc(translate("&TITLE_7","()","[]")));;
            %IF %LENGTH("&TITLE_8")  ne 2 %THEN  TITLE8  BOX=1 C=PGR  ITALIC H=2 JUSTIFY=LEFT %QUOTE(%sysfunc(translate("&TITLE_8","()","[]")));;
            %IF %LENGTH("&TITLE_9")  ne 2 %THEN  TITLE9  BOX=1 C=MEGR ITALIC H=2 JUSTIFY=LEFT %QUOTE(%sysfunc(translate("&TITLE_9","()","[]")));;
            %IF %LENGTH("&TITLE_10") ne 2 %THEN  TITLE10 BOX=1 C=MEGR ITALIC H=2 JUSTIFY=LEFT %QUOTE(%sysfunc(translate("&TITLE_10","()","[]")));;
            %IF %LENGTH("&TITLE_11") ne 2 %THEN  TITLE11 BOX=1 C=MEGR ITALIC H=2 JUSTIFY=LEFT %QUOTE(%sysfunc(translate("&TITLE_11","()","[]")));;

            PROC PRINT DATA=ID_&ID (DROP=XB010) NOOBS;FORMAT _NUMERIC_ 12.0;
            RUN;
            TITLE1;
            TITLE2;
            TITLE3;
            TITLE4;
            TITLE5;
            TITLE6;
            TITLE7;
            TITLE8;
            TITLE9;
            TITLE10;
            TITLE11;

        %END;
        %ELSE %DO;
                PROC SQL;
                        DROP TABLE ID_&ID
                        ;
                QUIT;
        %END;

%END;


DATA HEADER;
  LENGTH MSG T $125;
  LABEL MSG = 'VALIDATION STATUS';
  T = PUT(DATETIME(),DATETIME20.);
  MSG = "LOGICAL VALIDATION ACCOMPLISHED ON " || COMPRESS(T);
RUN;
OPTIONS LINESIZE=250;
PROC REPORT;
  COLUMN MSG;
RUN;

TITLE1;
TITLE2;

ODS &OUTPUTFORMAT CLOSE;

%IF &CSV=YES %THEN %DO;
	PROC EXPORT DATA=MISM_CSV FILE="&OUT&_dirsp_%quote(&CC&YY.-Logical.csv)" REPLACE;
	RUN;
%END;

%MEND check_SQL;


%MACRO reduce_LVAL_TBL(TBL=,MAXREC=5);

PROC SORT DATA=&TBL;
  BY XB010;
DATA _LAST_ (DROP=C);
SET;
  BY XB010;
  RETAIN C 1;
  IF FIRST.XB010 THEN DO;
         C = 1;
  END;
  ELSE C + 1;
  IF C <= &MAXREC;
RUN;

%MEND reduce_LVAL_TBL;
