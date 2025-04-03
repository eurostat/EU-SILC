/**
 * Revision History
 * - November 25th 2022
 */

/**
 * --------------------- STRUCTURAL VALIDATION ---------------
 * getSVAL
 * check_VARIABLES
 * check_FLAGS
 * check_V_FLAGS
 * check_NA
 * check_MISS
 * FMT_LIST
 * REP_SVAL
 * > REDUCE_TBL
 */
%MACRO getSVAL (YR=,MODE=);*import SVAL xml config files;

%LOCAL INDIR INFNAME MAPDIR MAPNAME;

%LET INDIR=%SYSFUNC(PATHNAME(XMLCFG))&_dirsp_%quote(SVAL);
%LET INFNAME=SILC-SVAL-R-&YR..xml;

LIBNAME IN_XML XMLV2 "&INDIR&_dirsp_%quote(&INFNAME)" XMLMAP="%SYSFUNC(PATHNAME(MAPDIR))&_dirsp_%quote(SVAL.map)";

/** MISSING RATES GLOBAL THRESHOLD */ 
DATA SVAL_MISSTHRES&MODE;
SET IN_XML.MISSTHRES;
RUN;

/** VARIABLES */ 
DATA SVAL&MODE;
LENGTH mod $6;
LENGTH label $200;
SET
  IN_XML.NUCLEUS (in=A)
  IN_XML.ADHOC   (in=B)
  IN_XML.RECUR3  (in=C)
  IN_XML.RECUR6  (in=D)
  ;
  IF B THEN mod = 'ADHOC';
  ELSE IF C THEN mod = 'RECUR3';
  ELSE IF D THEN mod = 'RECUR6';
RUN;

DATA SVAL&MODE;
SET  SVAL&MODE;
  IF mod EQ '' or mod EQ 'NUCLEUS' THEN DO;
         OUTPUT;
  END;
  ELSE IF SWITCH NE 'OFF' THEN DO;
          IF mod = 'ADHOC' THEN DO;
                 IF START_M LE &YR THEN OUTPUT;
          END;
          ELSE IF mod = 'RECUR3' THEN DO;
                 MOD_T = MOD( &YR - START_M, 3 );
                 IF MOD_T EQ 0 THEN OUTPUT;
          END;
          ELSE IF mod = 'RECUR6' THEN DO;
                 MOD_T = MOD( &YR - START_M, 6 );
                 IF MOD_T EQ 0 THEN OUTPUT;
          END;
  END;
PROC SORT;
  BY VARIABLE;
RUN;

DATA SVAL_TRANS_LIST (RENAME=(ID=VARIABLE));
SET  IN_XML.TRANS_LIST;
RUN;

/**
 * MINVAL & MAXVAL HANDLING
 */
DATA SVAL&MODE (DROP=I RVAL MIN_STR MAX_STR);
SET  SVAL&MODE (rename=(MINVAL=MIN_STR MAXVAL=MAX_STR));
     LENGTH MINVAL MAXVAL 8;
     MIN_STR = UPCASE(MIN_STR);
     MAX_STR = UPCASE(MAX_STR);

     I = INDEX(MIN_STR,'$YEAR');
     IF I gt 0 THEN DO;
        IF LENGTH(MIN_STR) gt 6 AND SUBSTR(MIN_STR,I+5,1) = '-' THEN DO;
           RVAL = INPUT( SUBSTR(MIN_STR,I+6), BEST. );
           MINVAL = &YYYY - RVAL;
        END;
        ELSE DO;
           MINVAL = &YYYY;
        END;
     END;
     ELSE DO;
        MINVAL = INPUT(MIN_STR,BEST.);
     END;

     I = INDEX(MAX_STR,'$YEAR');
     IF I gt 0 THEN DO;
        IF LENGTH(MAX_STR) gt 6 AND SUBSTR(MAX_STR,I+5,1) = '-' THEN DO;
           RVAL = INPUT( SUBSTR(MAX_STR,I+6), BEST. );
           MAXVAL = &YYYY - RVAL;
        END;
        ELSE DO;
           MAXVAL = &YYYY;
        END;
     END;
     ELSE DO;
        MAXVAL = INPUT(MAX_STR,BEST.);
     END;
RUN;


/**
 * MULTIPLE CHILD ELEMENTS HANDLING (flag)
 */
DATA SVAL_FLAGS&MODE;
LENGTH mod $6;
SET
  IN_XML.NUCLEUS_FLAGS (in=A)
  IN_XML.ADHOC_FLAGS   (in=B)
  IN_XML.RECUR3_FLAGS  (in=C)
  IN_XML.RECUR6_FLAGS  (in=D)
  ;
  IF B THEN mod = 'ADHOC';
  ELSE IF C THEN mod = 'RECUR3';
  ELSE IF D THEN mod = 'RECUR6';
  IF FLAG_DIGIT = . THEN FLAG_DIGIT = 0;
  IF FLAG ^= '';
  IF mod EQ '' or mod EQ 'NUCLEUS' THEN DO;
         OUTPUT;
  END;
  ELSE IF SWITCH NE 'OFF' THEN DO;
          IF mod = 'ADHOC' THEN DO;
                 IF START_M LE &YR THEN OUTPUT;
          END;
          ELSE IF mod = 'RECUR3' THEN DO;
                 MOD_T = MOD( &YR - START_M, 3 );
                 IF MOD_T EQ 0 THEN OUTPUT;
          END;
          ELSE IF mod = 'RECUR6' THEN DO;
                 MOD_T = MOD( &YR - START_M, 6 );
                 IF MOD_T EQ 0 THEN OUTPUT;
          END;
  END;
PROC SORT;
  BY VARIABLE FLAG_DIGIT;
RUN;

PROC SORT NODUPKEY OUT=FLAGS_PRIMARY;
  BY VARIABLE;
RUN;

DATA SVAL&MODE;
MERGE SVAL&MODE (in=A) FLAGS_PRIMARY (in=B);
  BY VARIABLE;
  IF A;
RUN;

DATA SVAL_FLAGS&MODE;
MERGE SVAL_FLAGS&MODE (in=A) SVAL&MODE (in=B KEEP=VARIABLE);
  BY VARIABLE;
  IF B;
RUN;

PROC SORT DATA=SVAL_FLAGS&MODE OUT=SVAL_FLAG_LEN&MODE NODUPKEY;
  BY VARIABLE;
DATA _LAST_;
SET;
  IF FLAG_LEN ne '' AND INPUT(SUBSTR(FLAG_LEN,1,1),BEST.) gt 0;
RUN;


/**
 * SELRESP HANDLING
 */
DATA SVAL_SELRESP&MODE;
SET  IN_XML.SELRESP;
     CALL SYMPUT('SELRESP_NA',TRIM(NA));
     CALL SYMPUT('SELRESP_DISP',TRIM(DISP));
RUN;

%PUT *I* SELRESP N/A: &SELRESP_NA;

DATA SVAL_SELRESP_CNTRY_LST&MODE;
SET  IN_XML.SELRESP_CNTRY_LST;
RUN;

/**
 * HH MEMBERSHIP STATUS
 */
%LET HH_MEMBERSHIP_STATUS_FILTER=;
%LET HH_MEMBERSHIP_STATUS_LX=XXX;

DATA SVAL_HH_MBR_STAT_LST&MODE;
SET  IN_XML.HH_MEMBERSHIP_STATUS_LIST;
     CALL SYMPUT('HH_MEMBERSHIP_STATUS_FILTER',TRIM(LX)||' IN '||TRIM(RX));
     CALL SYMPUTX('HH_MEMBERSHIP_STATUS_LX',LX);
RUN;

DATA SVAL_HH_MEMBER_MANDATORY&MODE (RENAME=(STATUS=&HH_MEMBERSHIP_STATUS_LX));
SET  IN_XML.HH_MEMBERSHIP_MANDATORY;
RUN;

%PUT *I* HH_MEMBERSHIP_STATUS_FILTER: &HH_MEMBERSHIP_STATUS_FILTER;

/**
 * MULTIPLE CHILD ELEMENTS HANDLING (na)
 */
DATA SVAL_NAS&MODE;
SET
  IN_XML.NUCLEUS_NAS (in=A)
  IN_XML.ADHOC_NAS   (in=B)
  IN_XML.RECUR3_NAS  (in=C)
  IN_XML.RECUR6_NAS  (in=D)
  ;
  LENGTH DISP $125;
  IF NA_FLAG = . THEN NA_FLAG = -2;
  IF NA_FLAG = -3 THEN DO;
         NA = "&SELRESP_NA";
         DISP = "&SELRESP_DISP";
  END;
  IF NA_FLAG IN ( -7, -8 ) THEN DO;
         NA = 'DEFAULT';
         DISP = '';
  END;
  IF NA ^= '';
PROC SORT;
  BY VARIABLE DESCENDING NA_FLAG;
RUN;

DATA SVAL_DISPS&MODE;
SET
  IN_XML.NUCLEUS_DISPS (in=A)
  IN_XML.ADHOC_DISPS   (in=B)
  IN_XML.RECUR3_DISPS  (in=C)
  IN_XML.RECUR6_DISPS  (in=D)
  ;
  IF NA_FLAG = . THEN NA_FLAG = -2;
PROC SORT;
  BY VARIABLE DESCENDING NA_FLAG;
RUN;

DATA SVAL_NAS&MODE;
MERGE SVAL_NAS&MODE (in=A) SVAL_DISPS&MODE (in=B);
  BY VARIABLE DESCENDING NA_FLAG;
  IF A;
RUN;

PROC SORT NODUPKEY OUT=NAS_PRIMARY;
  BY VARIABLE;
RUN;

DATA SVAL&MODE;
MERGE SVAL&MODE (in=A) NAS_PRIMARY (in=B);
  BY VARIABLE MOD_ID;
  IF A;
RUN;

DATA SVAL_NAS&MODE ;
MERGE SVAL_NAS&MODE (in=A) SVAL&MODE (in=B KEEP=VARIABLE FORMAT MOD_ID);
  BY VARIABLE MOD_ID;
  IF B;
RUN;

DATA SVAL_NAS&MODE (DROP=I PART1 PART2 NA_STR);
SET  SVAL_NAS&MODE (rename=(NA=NA_STR));
     LENGTH NA PART1 PART2 $1024;
     NA_STR = UPCASE(NA_STR);
     I = INDEX(NA_STR,'$YEAR');
     IF I gt 0 THEN DO;
        PART2 = '';
        PART1 = SUBSTR(NA_STR,1,I-1);
        PART2 = SUBSTR(NA_STR,I+5);
        NA = TRIM(PART1) || "&YYYY" || TRIM(PART2);
     END;
     ELSE DO;
        NA = NA_STR;
     END;
RUN;


/**
 * PARENT & CHECKSUM Elements
 */
DATA SVAL_PARENT&MODE;
SET  IN_XML.NUCLEUS_PARENT (in=A) IN_XML.ADHOC_PARENT (in=B) IN_XML.RECUR3_PARENT (in=C);
PROC SORT NODUPKEY;
  BY VARIABLE;
RUN;

DATA SVAL_CHKSUM&MODE;
SET  IN_XML.NUCLEUS_CHKSUM (in=A) IN_XML.ADHOC_CHKSUM (in=B) IN_XML.RECUR3_CHKSUM (in=C);
PROC SORT NODUPKEY;
  BY VARIABLE CHILD_ID;
RUN;

/**
 * AD-HOC MODULE HANDLING
 */
%IF &MODE eq _ANTE_ %THEN %DO;


    DATA SVAL&MODE;
    SET  SVAL&MODE;
         IF MOD eq 'ADHOC' AND &YR ne &YYYY THEN DO;
            IF REQ = 'Y' THEN DO;
               REQ = 'N';
            END;
         END;
    RUN;

%END;

/**
 * BREAK-IN-SERIES HANDLING
 */
DATA SVAL_BRK_LOV&MODE;
SET IN_XML.BRK_LOV;
  IF ANTE = . THEN ANTE = &YR;
PROC SORT;
  BY VARIABLE DESCENDING ANTE;
PROC SORT NODUPKEY;
  BY VARIABLE;
RUN;

DATA SVAL_BRK_FLAG&MODE;
SET IN_XML.BRK_FLAG;
  IF ANTE = . THEN ANTE = &YR;
  IF FLAG_DIGIT = . THEN FLAG_DIGIT = 0;
PROC SORT;
  BY VARIABLE DESCENDING ANTE;
PROC SORT NODUPKEY;
  BY VARIABLE;
RUN;

%MEND getSVAL;


/**
 * BREAK (LOV)
 */
%MACRO check_BRK_LOV (F=D);*check when new values appear after 2021;

%LET DS=RAW.&_SPLIT_MODE_&ss&cc&YY&F;

%LOCAL k CNT VAR VARS FMT ANTE LOV;

PROC SQL NOPRINT;
  SELECT VARIABLE INTO :VARS SEPARATED BY ' '
  FROM   SVAL_BRK_LOV_&F
  WHERE SUBSTR(VARIABLE,1,3) NE "RG_"
  ;
QUIT;

%LET k=1;
%LET VAR=%SCAN(&VARS, &k);
%DO %WHILE (&VAR ne);

    PROC SQL NOPRINT;
      SELECT ANTE INTO :ANTE
      FROM   SVAL_BRK_LOV_&F
      WHERE  VARIABLE = "&VAR"
      ;
    QUIT;

    DATA _NULL_;
    SET  SVAL_BRK_LOV_&F (WHERE=(VARIABLE="&VAR"));
         LOV = TRANWRD(TRANWRD(TRANWRD(TRANWRD(TRIM(LIST),'(',''),')',''),',',''),'"','');
         CALL SYMPUTX('LOV',LOV);
    RUN;

    DATA _NULL_;
    SET  SVAL_&F (WHERE=(VARIABLE="&VAR"));
         CALL SYMPUTX('FMT',SUBSTR(FORMAT,1,1));
    RUN;


    DATA BRK_ANTE_&k (KEEP=&F.B010 &F.B020 &F.B030 VARIABLE VALUE);
    SET  &DS (WHERE=(&F.B010 lt &ANTE));
         LENGTH VARIABLE $8;
         LENGTH VALUE $12;
         %IF &FMT NE $ %THEN %DO;
             IF &VAR ^= . THEN DO;
                IF &VAR IN ( &LOV ) THEN RES = 1;
                ELSE RES = 0;
             END;
         %END;
         %ELSE %DO;
             IF &VAR ^= '' THEN DO;
                IF &VAR IN ( &LOV ) THEN RES = 1;
                ELSE RES = 0;
             END;
         %END;
         IF RES = 0 THEN DO;
           VARIABLE = "&VAR";
           %IF &FMT NE $ %THEN %DO; VALUE = COMPRESS(PUT(&VAR,best8.)); %END;
           %ELSE %DO; VALUE = &VAR; %END;
           OUTPUT;
         END;
    RUN;

    /* PURGING FORMER WARNING MESSAGES */
    PROC SQL;
         DELETE
         FROM   SVAL_MISM_V
         WHERE  VARIABLE = "&VAR"
         AND    &F.B010 lt &ANTE
         ;
    QUIT;

    PROC APPEND FORCE BASE=SVAL_MISM_V DATA=BRK_ANTE_&k;
    RUN;

    %LET k=%EVAL(&k+1);
    %LET VAR=%SCAN(&VARS, &k);
%END;

%MEND  check_BRK_LOV;


/**
 * BREAK (FLAG)
 */
%MACRO check_BRK_FLAG (F=D);*check when new flags appear after 2021;

%LET DS=RAW.&_SPLIT_MODE_&ss&cc&YY&F;

%LOCAL k CNT VAR VARS FMT ANTE LOV;

PROC SQL NOPRINT;
  SELECT VARIABLE INTO :VARS SEPARATED BY ' '
  FROM   SVAL_BRK_FLAG_&F
  ;
QUIT;

%LET k=1;
%LET VAR=%SCAN(&VARS, &k);
%DO %WHILE (&VAR ne);

    PROC SQL NOPRINT;
      SELECT ANTE INTO :ANTE
      FROM   SVAL_BRK_FLAG_&F
      WHERE  VARIABLE = "&VAR"
      ;
    QUIT;

    DATA _NULL_;
    SET  SVAL_BRK_FLAG_&F (WHERE=(VARIABLE="&VAR"));
         LOV = TRANWRD(TRANWRD(TRANWRD(TRIM(FLAG),'(',''),')',''),',','');
         CALL SYMPUTX('LOV',LOV);
         CALL SYMPUTX('DIGIT',PUT(FLAG_DIGIT,Z1.));
    RUN;

    DATA
         BRK_FLG_ANTE_&k (KEEP=&F.B010 &F.B020 &F.B030 VARIABLE VALUE DIGIT LOV)
         BRK_FLG_ANTE_FNEG_&k (KEEP=&F.B010 &F.B020 &F.B030 VARIABLE VALUE)
         ;
    SET  &DS (WHERE=(&F.B010 lt &ANTE));
         LENGTH VARIABLE $8;
         LENGTH VALUE 8;
         LENGTH LOV $256;
         LENGTH S $8;
         RES = 1;
         SKIP = 0;
         %IF %QUOTE(&LOV) NE %THEN %DO;
             %IF (&DIGIT GT 0) %THEN %DO;
                 S = COMPRESS(PUT(&VAR._F,best8.));
                 DIGIT=&DIGIT;
                 %IF (&DIGIT EQ 1) %THEN %DO;
                     IF SUBSTR(S,1,1) =  '-' THEN S = SUBSTR(S,1,2);
                     ELSE S = SUBSTR(S,1,1);
                     VALUE = INPUT(S,BEST.);
                 %END;
                 %ELSE %DO;
                     IF SUBSTR(S,1,1) ^=  '-' AND LENGTH(S) GE &DIGIT THEN DO;
                        S = SUBSTR(S,&DIGIT,1);
                        VALUE = INPUT(S,BEST.);
                     END;
                     ELSE DO;
                        IF SUBSTR(S,1,1) = '-' AND LENGTH(S) GT &DIGIT THEN DO;
                           VALUE = INPUT(S,BEST.);
                           SKIP = 1;
                           OUTPUT BRK_FLG_ANTE_FNEG_&k;
                        END;
                        ELSE DO;
                           VALUE = .;
                           SKIP = 1;
                        END;
                     END;
                 %END;
             %END;
             %ELSE %DO;
                 DIGIT=0;
                 VALUE = &VAR._F;
             %END;
             IF SKIP = 0 THEN DO;
                IF VALUE IN ( &LOV ) THEN RES = 1;
                ELSE RES = 0;
             END;
         %END;

         IF RES = 0 THEN DO;
            VARIABLE = "&VAR";
            VALUE = &VAR._F;
            OUTPUT BRK_FLG_ANTE_&k;
         END;
    RUN;

    PROC SQL;
      UPDATE BRK_FLG_ANTE_&k T
      SET    LOV =
      (      SELECT FLAG
             FROM   SVAL_BRK_FLAG_&F
             WHERE  VARIABLE = T.VARIABLE
             AND    FLAG_DIGIT = T.DIGIT    )
      ;
    QUIT;

    /* PURGING FORMER WARNING MESSAGES */
    PROC SQL;
         DELETE
         FROM   SVAL_MISM_F
         WHERE  VARIABLE = "&VAR"
         AND    &F.B010 lt &ANTE
         ;
    QUIT;

    PROC SQL;
         DELETE
         FROM   SVAL_MISM_FNEG
         WHERE  VARIABLE = "&VAR"
         AND    &F.B010 lt &ANTE
         ;
    QUIT;

    PROC APPEND FORCE BASE=SVAL_MISM_F DATA=BRK_FLG_ANTE_&k;
    RUN;

    PROC APPEND FORCE BASE=SVAL_MISM_FNEG DATA=BRK_FLG_ANTE_FNEG_&k;
    RUN;

    %LET k=%EVAL(&k+1);
    %LET VAR=%SCAN(&VARS, &k);
%END;

%MEND  check_BRK_FLAG;


/**
 * LOV
 */
%MACRO check_VARIABLES(F=D,MODE=RANGE);*check if values are in allowed range or list;

%LET DS=RAW.&_SPLIT_MODE_&ss&cc&YY&F;

%IF &MODE EQ RANGE %THEN %DO;
        DATA SVAL_MISM_V;
          LENGTH &F.B010 &F.B030 8;
          LENGTH &F.B020 $2;
          LENGTH VARIABLE $8;
          LENGTH VALUE $32;
          STOP;
        RUN;
%END;

%LOCAL K RC CHECK VARS VAR VARX LB UB FMT LIST LIST_STR
;
%IF &MODE EQ RANGE %THEN %DO;
        DATA _NULL_;
        SET SVAL_&F END=EOF;
          WHERE  1 EQ 1
          AND    MINVAL is not null
          AND    MAXVAL is not null
          AND    ( TYPE is null OR TYPE = 'ID' )
          ;
          LENGTH VARS $32767;
          RETAIN VARS '';
          VARS = TRIM(VARS) || TRIM(VARIABLE) || ':' ||
                SUBSTR(FORMAT,1,1) || ':'  ||
                COMPRESS(PUT(MINVAL,best8.)) || ':' ||
                COMPRESS(PUT(MAXVAL,best8.))
          ;
          IF NOT EOF THEN VARS = TRIM(VARS) || '|';
          ELSE CALL SYMPUT('VARS',TRIM(VARS));
        RUN;

        DATA
         SVAL_MISM_V_1 (KEEP=&F.B010 &F.B020 &F.B030 VARIABLE VALUE);
        SET  &DS;
                LENGTH VARIABLE $8;
                LENGTH VALUE $12;
                %LET K=1;
                %LET VAR=%SCAN(&VARS,&K,|);
                %LET VARX=%SCAN(&VAR,1,:);
                %DO %WHILE(&VARX NE);
                        %LET FMT =%SCAN(&VAR,2,:);
                        %LET LB  =%SCAN(&VAR,3,:);
                        %LET UB  =%SCAN(&VAR,4,:);
                        IF "&FMT" ^= '$' THEN DO;
                                IF &VARX ^= . THEN DO;
                                        IF &VARX >= &LB AND &VARX <= &UB THEN RES=1;
                                        ELSE RES=0;
                                        IF RES = 0 THEN DO;
                                           VARIABLE = "&VARX";
                                           VALUE = COMPRESS(PUT(&VARX,$10.));
                                           OUTPUT;
                                        END;
                                END;
                        END;
                        %LET K=%EVAL(&K+1);
                        %LET VAR=%SCAN(&VARS,&K,|);
                        %LET VARX=%SCAN(&VAR,1,:);
                 %END;
                 IF VARIABLE ^= '';
        RUN;

        PROC APPEND FORCE BASE=SVAL_MISM_V DATA=SVAL_MISM_V_1;
        RUN;
%END;

%IF &MODE EQ LIST %THEN %DO;

        DATA _NULL_;
        SET SVAL_&F END=EOF;
          WHERE  1 EQ 1
          AND    LIST is not null
          AND    TYPE is null
          ;
          LENGTH VARS _LIST_ $32767;
          RETAIN VARS '';
          _LIST_ = TRANWRD(TRANWRD(TRANWRD(TRANWRD(TRIM(LIST),'(',''),')',''),',',''),'"','');
          VARS = TRIM(VARS) || TRIM(VARIABLE) || ':' ||
                SUBSTR(FORMAT,1,1) || ':'  ||
        TRIM(_LIST_)
          ;
          IF NOT EOF THEN VARS = TRIM(VARS) || '|';
          ELSE CALL SYMPUT('VARS',TRIM(VARS));
        RUN;

        DATA
         SVAL_MISM_V_2 (KEEP=&F.B010 &F.B020 &F.B030 VARIABLE VALUE);
        SET  &DS;
                LENGTH VARIABLE $8;
                LENGTH VALUE $12;
                %LET K=1;
                %LET VAR=%SCAN(&VARS,&K,|);
                %LET VARX=%SCAN(&VAR,1,:);
                %DO %WHILE(&VARX NE);
                        %LET FMT =%SCAN(&VAR,2,:);
                        %LET LIST=%SCAN(&VAR,3,:);
                        RES = 1;
                        %IF &FMT NE $ %THEN %DO;
                                IF &VARX ^= . THEN DO;
                                        IF &VARX IN ( &LIST ) THEN RES = 1;
                                        ELSE RES = 0;
                                END;
                        %END;
                        %ELSE %DO;
                                IF &VARX ^= '' THEN DO;
                                        IF &VARX IN ( &LIST ) THEN RES = 1;
                                        ELSE RES = 0;
                                END;
                        %END;
                        IF RES = 0 THEN DO;
                           VARIABLE = "&VARX";
                           %IF &FMT NE $ %THEN %DO; VALUE = COMPRESS(PUT(&VARX,$10.)); %END;
                           %ELSE %DO; VALUE = &VARX; %END;
                           OUTPUT;
                        END;
                        %LET K=%EVAL(&K+1);
                        %LET VAR=%SCAN(&VARS,&K,|);
                        %LET VARX=%SCAN(&VAR,1,:);
                 %END;
                 IF VARIABLE ^= '';
        RUN;

        PROC APPEND FORCE BASE=SVAL_MISM_V DATA=SVAL_MISM_V_2;
        RUN;
%END;

%MEND check_VARIABLES;

%MACRO check_FLAGS(F=);*check if flag is in allowed list;

        %IF %SYSFUNC(EXIST(SVAL_MISM_F)) %THEN %DO;
                PROC SQL;
                  DROP TABLE SVAL_MISM_F
                  ;
                QUIT;
        %END;

        %LET DS=RAW.&_SPLIT_MODE_&ss&cc&YY&F;

        %LOCAL K RC CHECK VARS VAR VARX LIST DIGIT LIST_STR;

        PROC SQL;
          DELETE FROM SVAL_FLAGS_&F
          WHERE  VARIABLE IN
          (      SELECT VARIABLE FROM SVAL_&F
                 WHERE  FLAG is null or TYPE is not null    )
      ;
        QUIT;

        DATA _NULL_;
        SET SVAL_FLAGS_&F END=EOF;
          LENGTH VARS _LIST_ $32767;
          RETAIN VARS '';
          _LIST_ = TRANWRD(TRANWRD(TRANWRD(TRIM(FLAG),'(',''),')',''),',','');
          VARS = TRIM(VARS) || TRIM(VARIABLE) || ':' || PUT(FLAG_DIGIT,Z1.) || ':' ||
        TRIM(_LIST_)
          ;
          IF NOT EOF THEN VARS = TRIM(VARS) || '|';
          ELSE CALL SYMPUT('VARS',TRIM(VARS));
        RUN;

        DATA
         SVAL_MISM_F (KEEP=&F.B010 &F.B020 &F.B030 VARIABLE VALUE DIGIT LOV)
         SVAL_MISM_FNEG (KEEP=&F.B010 &F.B020 &F.B030 VARIABLE VALUE)
        ;
        SET  &DS;
         LENGTH VARIABLE $8;
         LENGTH VALUE 8;
         LENGTH LOV $256;
         LENGTH S $8;
         %LET K=1;
         %LET VAR=%SCAN(&VARS,&K,|);
         %LET VARX=%SCAN(&VAR,1,:);
         %DO %WHILE(&VARX NE);
            %LET DIGIT=%SCAN(&VAR,2,:);
            %LET LIST=%SCAN(&VAR,3,:);
            RES = 1;
            SKIP = 0;
            %IF %QUOTE(&LIST) NE %THEN %DO;
              %IF (&DIGIT GT 0) %THEN %DO;
                  S = COMPRESS(PUT(&VARX._F,best8.));
                  DIGIT=&DIGIT;
                  /*PUT '*S*' S "&VARX._F" &VARX._F;*/
                  %IF (&DIGIT EQ 1) %THEN %DO;
                    IF SUBSTR(S,1,1) =  '-' THEN S = SUBSTR(S,1,2);
                    ELSE S = SUBSTR(S,1,1);
                    VALUE = INPUT(S,BEST.);
                  %END;
                  %ELSE %DO;
                    IF SUBSTR(S,1,1) ^=  '-' AND LENGTH(S) GE &DIGIT THEN DO;
                      S = SUBSTR(S,&DIGIT,1);
                      VALUE = INPUT(S,BEST.);
                    END;
                    ELSE DO;
                      IF SUBSTR(S,1,1) = '-' AND LENGTH(S) GT &DIGIT THEN DO;
                        VALUE = INPUT(S,BEST.);
                        SKIP = 1;
                        OUTPUT SVAL_MISM_FNEG;
                      END;
                      ELSE DO;
                        VALUE = .;
                        SKIP = 1;
                      END;
                    END;
                  %END;
              %END;
              %ELSE %DO;
                DIGIT=0;
                VALUE = &VARX._F;
              %END;

              IF SKIP = 0 THEN DO;
                IF VALUE IN ( &LIST ) THEN RES = 1;
                ELSE RES = 0;
              END;
            %END;

            IF RES = 0 THEN DO;
              VARIABLE = "&VARX";
              VALUE = &VARX._F; /* FIX */
              OUTPUT SVAL_MISM_F;
            END;
            %LET K=%EVAL(&K+1);
            %LET VAR=%SCAN(&VARS,&K,|);
            %LET VARX=%SCAN(&VAR,1,:);
         %END;
         IF VARIABLE ^= '';
        RUN;

        PROC SORT DATA=SVAL_MISM_FNEG NODUPKEY;
          BY &F.B010 &F.B030 VARIABLE;
        RUN;

        PROC SQL;
          UPDATE SVAL_MISM_F T
          SET    LOV =
          (      SELECT FLAG
                 FROM   SVAL_FLAGS_&F
                 WHERE  VARIABLE = T.VARIABLE
                 AND    FLAG_DIGIT = T.DIGIT )
          ;
        QUIT;


%MEND check_FLAGS;



%MACRO check_FLAG_LENGTHS(F=);*check if flag is in allowed list when more than 1 digit (income var);

        %LOCAL K RC VARS VAR VARX FLEN;

        %IF %SYSFUNC(EXIST(SVAL_MISM_F)) %THEN %DO;
            PROC SQL;
                 DROP TABLE SVAL_MISM_FLEN
                 ;
            QUIT;
        %END;

        %LET DS=RAW.&_SPLIT_MODE_&ss&cc&YY&F;

        DATA _NULL_;
        SET SVAL_FLAG_LEN_&F END=EOF;
          LENGTH VARS _LIST_ $32767;
          RETAIN VARS '';
          VARS = TRIM(VARS) || TRIM(VARIABLE) || ':' || strip(FLAG_LEN) || ':' ||
        TRIM(_LIST_)
          ;
          IF NOT EOF THEN VARS = TRIM(VARS) || '|';
          ELSE CALL SYMPUT('VARS',TRIM(VARS));
        RUN;



        DATA
         SVAL_MISM_FLEN (KEEP=&F.B010 &F.B020 &F.B030 VARIABLE VALUE LEN);
        SET  &DS;
                LENGTH VARIABLE $8;
                LENGTH VALUE 8;
                LENGTH LEN 3;
                LENGTH FLEN_MINUS $1;
                %LET K=1;
                %LET VAR=%quote(%SCAN(&VARS,&K,|));
                %DO %WHILE(%quote(&VAR) NE);
                    %LET VARX=%SCAN(&VAR,1,:);
                    %LET FLEN=%SCAN(&VAR,2,:);
                    %LET FLEN_DIGIT=%SUBSTR(&FLEN,1,1);
                    RES = 1;
                    %IF %length(&FLEN) eq 2 %THEN %DO;
                        IF LENGTH(strip(put(&VARX._F,best8.))) gt &FLEN_DIGIT AND &VARX._F gt 0 THEN RES = 0;
                        IF LENGTH(strip(put(&VARX._F,best8.))) lt (&FLEN_DIGIT-1) AND &VARX._F gt 0 THEN RES = 0;
                    %END;
                    %ELSE %DO;
                        IF LENGTH(strip(put(&VARX._F,best8.))) ne &FLEN_DIGIT AND &VARX._F gt 0 THEN RES = 0;
                    %END;
                    IF RES = 0 THEN DO;
                       VARIABLE = "&VARX";
                       VALUE = &VARX._F;
                       LEN = &FLEN_DIGIT;
                       OUTPUT;
                    END;
                    %LET K=%EVAL(&K+1);
                    %LET VAR=%SCAN(&VARS,&K,|);
                %END;
                IF VARIABLE ^= '';
        RUN;

        %PUT *I* XML_SVAL: MERGING FLAG LENGHT (-X) CASE AND (X) CASE
        ;
        PROC APPEND BASE=SVAL_MISM_FLEN DATA=SVAL_MISM_FNEG FORCE;
        RUN;

        %PUT *I* XML_SVAL: PURGING FLAG LENGHT MSGS BASED ON FLAG ERR ...
        ;

        PROC SORT DATA=SVAL_MISM_FLEN;
          BY &F.B010 &F.B030 VARIABLE;
        PROC SORT DATA=SVAL_MISM_F;
          BY &F.B010 &F.B030 VARIABLE;
        DATA  SVAL_MISM_FLEN;
        MERGE SVAL_MISM_FLEN (in=A) SVAL_MISM_F (in=B);
          BY &F.B010 &F.B030 VARIABLE;
          IF NOT B;
        RUN;

%MEND check_FLAG_LENGTHS;


%MACRO check_V_FLAGS(F=);*check if value and flag are consistent;

        %IF %SYSFUNC(EXIST(SVAL_MISM_VF)) %THEN %DO;
                PROC SQL;
                  DROP TABLE SVAL_MISM_VF
                  ;
                QUIT;
        %END;

        %LET DS=RAW.&_SPLIT_MODE_&ss&cc&YY&F;

        %LOCAL K RC CHECK VARS VAR VARX FMT LIST LIST_STR FOUND_0;

        DATA _NULL_;
        SET SVAL_&F END=EOF;
          WHERE  1 EQ 1
          AND    FLAG is not null
          AND    TYPE is null
          ;
          LENGTH VARS _LIST_ $32767;
          RETAIN VARS '';
          /* CHECK FLAG 0 */
          _LIST_ = TRANWRD(TRANWRD(TRANWRD(TRIM(FLAG),'(',''),')',''),',','');
          FOUND_0 = 0;
          j = 1;
          FLAG_OPTION = SCAN( _LIST_, j );
          DO WHILE ( FLAG_OPTION ne '' );
                 IF FLAG_OPTION = 0 THEN FOUND_0 = 1;
                 j = j + 1;
                 FLAG_OPTION = SCAN( _LIST_, j );
          END;
          PUT '>' VARIABLE 'FOUND_0' FOUND_0;
          IF INCOME = '' THEN INCOME = 'N';
          VARS = TRIM(VARS) || TRIM(VARIABLE) || ':' ||
        SUBSTR(FORMAT,1,1) || ':' ||
                TRIM(INCOME) || ':' || COMPRESS(PUT(FOUND_0,best8.))
          ;
          IF NOT EOF THEN VARS = TRIM(VARS) || '|';
          ELSE CALL SYMPUT('VARS',TRIM(VARS));
        RUN;

        DATA
         SVAL_MISM_VF (KEEP=&F.B010 &F.B020 &F.B030 VARIABLE VALUE FLAG);
        SET  &DS;
            LENGTH VARIABLE $8;
                LENGTH VALUE $12;
                %LET K=1;
                %LET VAR=%SCAN(&VARS,&K,|);
                %LET VARX=%SCAN(&VAR,1,:);
                %DO %WHILE(&VARX NE);
                        %LET FMT=%SCAN(&VAR,2,:);
                        %LET INCOME=%SCAN(&VAR,3,:);
                        %LET FOUND_0=%SCAN(&VAR,4,:);
                        RES = 1;
                        %IF &FMT NE $ %THEN %DO;
                                %IF &INCOME EQ N %THEN %DO;
                                        IF &VARX ^= . AND &VARX._F ^= . AND &VARX._F LT 0 THEN DO;
                                           RES = 0;
                                        END;
                                        ELSE IF &VARX = . AND &VARX._F GT 0 THEN DO;
                                           RES = 0;
                                        END;
                                %END;
                                %ELSE %DO;
                                        IF &VARX ^= . AND &VARX._F ^= . AND &VARX._F LT 0 THEN DO;
                                           RES = 0;
                                        END;
                                        %IF &FOUND_0 EQ 1 %THEN %DO;
                                        ELSE IF &VARX ^= 0 AND &VARX._F EQ 0 THEN DO;
                                           RES = 0;
                                        END;
                                        %END;
                                        ELSE IF &VARX = . AND &VARX._F GT 0 THEN DO;
                                           RES = 0;
                                        END;
                                        %IF &FOUND_0 EQ 1 %THEN %DO;
                                        ELSE IF &VARX = 0 AND &VARX._F NE 0 THEN DO;
                                           RES = 0;
                                        END;
                                        %END;
                                %END;
                        %END;
                        %ELSE %DO;
                                IF &VARX ^= '' AND &VARX._F ^= . AND &VARX._F LT 0 THEN DO;
                                   RES = 0;
                                END;
                                ELSE IF &VARX = '' AND &VARX._F GT 0 THEN DO;
                                   RES = 0;
                                END;
                        %END;
                        IF RES = 0 THEN DO;
                           VARIABLE = "&VARX";
                           %IF &FMT NE $ %THEN %DO; VALUE = COMPRESS(PUT(&VARX,$10.)); %END;
                           %ELSE %DO; VALUE = &VARX; %END;
                           FLAG  = &VARX._F;
                           OUTPUT;
                        END;
                        %LET K=%EVAL(&K+1);
                        %LET VAR=%SCAN(&VARS,&K,|);
                        %LET VARX=%SCAN(&VAR,1,:);
                 %END;
                 IF VARIABLE ^= '';
        RUN;

%MEND check_V_FLAGS;

%MACRO check_IMPUT_FACTORS (F=);*check if imputation factor and flag are consistent;

    %IF %SYSFUNC(EXIST(SVAL_MISM_IF_MISSING)) %THEN %DO;
            PROC SQL;
              DROP TABLE SVAL_MISM_IF_MISSING
              ;
            QUIT;
    %END;
    %IF %SYSFUNC(EXIST(SVAL_MISM_IF_WRONG)) %THEN %DO;
            PROC SQL;
              DROP TABLE SVAL_MISM_IF_WRONG
              ;
            QUIT;
    %END;

    %LET DS=RAW.&_SPLIT_MODE_&ss&cc&YY&F;

    %LOCAL K RC CHECK VARS VAR ;

	%IF %SYSFUNC(EXIST(SVAL_IMPUTE_&F)) %THEN %DO;
	    DATA _NULL_;
	    SET SVAL_IMPUTE_&F END=EOF;
	      LENGTH VARS $32767;
	      RETAIN VARS '';
	      VARS = TRIM(VARS) || TRIM(VARIABLE)       ;
	      IF NOT EOF THEN VARS = TRIM(VARS) || '|';
	      ELSE CALL SYMPUT('VARS',TRIM(VARS));
	    RUN;
		%put vars : &vars;
	%END;

    DATA SVAL_MISM_IF_MISSING (KEEP=&F.B010 &F.B020 &F.B030 VARIABLE VALUE FLAG IMPUT_FACTOR);
    SET  &DS;
        LENGTH VARIABLE $8;
            LENGTH VALUE $12;
            %LET K=1;
            %LET VAR=%SCAN(&VARS,&K,|);
            %DO %WHILE(&VAR NE);
                RES = 1;
                IF &VAR._IF = . AND &VAR._F GT 0 AND &VAR NE 0 THEN DO;*Imput Factor wrongly missing;
					RES = 0 ;
					VARIABLE = "&VAR";
					VALUE = COMPRESS(PUT(&VAR,10.)); 
					FLAG  = &VAR._F;
					IMPUT_FACTOR  = &VAR._IF;
					OUTPUT;
                END;
                %LET K=%EVAL(&K+1);
                %LET VAR=%SCAN(&VARS,&K,|);
             %END;
             IF VARIABLE ^= '';
    RUN;

	DATA SVAL_MISM_IF_WRONG (KEEP=&F.B010 &F.B020 &F.B030 VARIABLE VALUE FLAG IMPUT_FACTOR);
    SET  &DS;
        LENGTH VARIABLE $8;
            LENGTH VALUE $12;
            %LET K=1;
            %LET VAR=%SCAN(&VARS,&K,|);
            %DO %WHILE(&VAR NE);
                RES = 1;
                IF &VAR._IF ^= . AND &VAR._F LT 0 THEN DO;RES = 0;END;*Imput Factor wrongly filled;
                ELSE IF &VAR._IF not in (.,100) AND &VAR._F GT 0 AND &VAR = 0 THEN DO;RES = 0;END;*Imput Factor wrongly filled;
                IF RES = 0 THEN DO;
                   VARIABLE = "&VAR";
                   VALUE = COMPRESS(PUT(&VAR,10.)); 
                   FLAG  = &VAR._F;
                   IMPUT_FACTOR  = &VAR._IF;
                   OUTPUT;
                END;
                %LET K=%EVAL(&K+1);
                %LET VAR=%SCAN(&VARS,&K,|);
             %END;
             IF VARIABLE ^= '';
    RUN;

%MEND check_IMPUT_FACTORS;

/**
 * @MOD = ADHOC | RECUR3 | blank
 */
%MACRO check_MINUS7(F=,MOD=);*check if -7 flag is used properly;

        %IF %SYSFUNC(EXIST(SVAL_MISM_MINUS7&MOD)) %THEN %DO;PROC SQL;DROP TABLE SVAL_MISM_MINUS7&MOD;QUIT;%END;
        %IF %SYSFUNC(EXIST(SVAL_MUST_MINUS7&MOD)) %THEN %DO;PROC SQL;DROP TABLE SVAL_MUST_MINUS7&MOD;QUIT;%END;

        %LET DS=RAW.&_SPLIT_MODE_&ss&cc&YY&F;

        %LOCAL K RC CHECK VARS VARS_T VAR VARX FMT START;

        DATA SVAL_&F;
        MERGE SVAL_&F (in=A) SVAL_NAS_&F (in=B WHERE=(NA_FLAG=-7));
          BY VARIABLE;
          IF B THEN NA_FLAG_7 = 1;
          ELSE NA_FLAG_7 = 0;
        RUN;

        %LET VARS=;
        DATA _NULL_;
        SET SVAL_&F END=EOF;
          WHERE  1 EQ 1
          AND    MOD  EQ "&MOD"
          AND    NA_FLAG_7 EQ 1
          AND    TYPE is null
          AND    MAX( START_V, START_M ) is not null
          ;
          LENGTH VARS $32767;
          RETAIN VARS '';
		  IF END_V=. THEN END=&YYYY;ELSE END=END_V;
          VARS = TRIM(VARS) || TRIM(VARIABLE) || ':' || PUT(MAX(START_M,START_V),4.) || ':' || PUT(END,4.);
          IF NOT EOF THEN VARS = TRIM(VARS) || '|';
          ELSE CALL SYMPUT('VARS',TRIM(VARS));
        RUN;
        DATA _NULL_;
        SET SVAL_TRANS_LIST_&F END=EOF;	          ;
          LENGTH VARS $32767;
          RETAIN VARS '';
          %IF &MOD NE %THEN %DO;
		  RUN;
          %END;
          %ELSE %DO;
          VARS = TRIM(VARS) || TRIM(VARIABLE) || ':' || "2000" || ':' || "&_SPLIT_CUT_";
          IF NOT EOF THEN VARS = TRIM(VARS) || '|';
          ELSE CALL SYMPUT('VARS_T',TRIM(VARS));
 		  RUN;
		  %LET VARS = &VARS | &VARS_T;
         %END;
	   %PUT VARS : &VARS;
	   %PUT VARS_T : &VARS_T;
	   
        %IF %QUOTE(&VARS) NE %THEN %DO;
				/*WRONG USE OF -7 FLAGS*/
                DATA
                 SVAL_MISM_MINUS7&MOD (KEEP=&F.B010 &F.B020 &F.B030 VARIABLE FLAG);
                SET  &DS;
                     LENGTH VARIABLE $8;
                        %LET K=1;
                        %LET VAR=%SCAN(&VARS,&K,|);
                        %LET VARX=%SCAN(&VAR,1,:);
                        %LET START=%SCAN(&VAR,2,:);
                        %LET END=%SCAN(&VAR,3,:);
                        %DO %WHILE(&VARX NE);
                                RES = 1;
                                %IF &MOD EQ ADHOC %THEN %DO;
                                        IF &VARX._F EQ -7 AND &F.B010 GE &START AND &F.B010 LE &END THEN DO;
                                           RES = 0;
                                        END;
                                %END;
/*                                %ELSE %IF &MOD EQ RECUR3 %THEN %DO;*/
/*                                        IF &VARX._F EQ -7 AND &F.B010 GE &START AND &F.B010 LE &END AND MOD( ( &F.B010 - &START ), 3 ) EQ 0 THEN DO;*/
/*                                           RES = 0;*/
/*                                        END;*/
/*                                %END;*/
                                %ELSE %IF &MOD EQ RECUR3 %THEN %DO;
                                        IF &VARX._F EQ -7 AND &F.B010 = &YYYY THEN DO;
                                           RES = 0;
                                        END;
                                %END;
                                %ELSE %IF &MOD EQ RECUR6 %THEN %DO;
                                        IF &VARX._F EQ -7 AND &F.B010 = &YYYY THEN DO;
                                           RES = 0;
                                        END;
                                %END;
                                %ELSE %DO;
                                        IF &VARX._F EQ -7 AND &F.B010 GE &START AND &F.B010 LE &END THEN DO;
                                           RES = 0;
                                        END;
                                %END;
                                IF RES = 0 THEN DO;
                                   VARIABLE = "&VARX";
                                   FLAG  = &VARX._F;
                                   OUTPUT;
                                END;
                                %LET K=%EVAL(&K+1);
                                %LET VAR=%SCAN(&VARS,&K,|);
                                %LET VARX=%SCAN(&VAR,1,:);
                                %LET START=%SCAN(&VAR,2,:);
                                %LET END=%SCAN(&VAR,3,:);
                         %END;
                         IF VARIABLE ^= '';
                RUN;
				/*-7 FLAGS SHOULD BE USED AND IS NOT*/
                 DATA
                 SVAL_MUST_MINUS7&MOD (KEEP=&F.B010 &F.B020 &F.B030 VARIABLE FLAG);
                SET  &DS;
                     LENGTH VARIABLE $8;
                        %LET K=1;
                        %LET VAR=%SCAN(&VARS,&K,|);
                        %LET VARX=%SCAN(&VAR,1,:);
                        %LET START=%SCAN(&VAR,2,:);
                        %LET END=%SCAN(&VAR,3,:);
                        %DO %WHILE(&VARX NE);
                                RES = 1;
                                %IF &MOD EQ ADHOC %THEN %DO;
                                        IF &VARX._F NE -7 AND (&F.B010 LT &START OR &F.B010 GT &END) THEN DO;
                                           RES = 0;
                                        END;
                                %END;
                                %ELSE %IF &MOD EQ RECUR3 %THEN %DO;
                                        IF &VARX._F NE -7 AND MOD( ( &F.B010 - &START ), 3 ) NE 0 THEN DO;
                                           RES = 0;
                                        END;
                                %END;
/*								%ELSE %IF &MOD EQ RECUR3 %THEN %DO;*/
/*                                        IF &VARX._F NE -7 AND &F.B010 LT &YYYY THEN DO;*/
/*                                           RES = 0;*/
/*                                        END;*/
/*                                %END;*/
                                %ELSE %IF &MOD EQ RECUR6 %THEN %DO;
                                        IF &VARX._F NE -7 AND &F.B010 LT &YYYY THEN DO;
                                           RES = 0;
                                        END;
                                %END;
                                %ELSE %DO;
                                        IF &VARX._F NE -7 AND (&F.B010 LT &START OR &F.B010 GT &END) THEN DO;
                                           RES = 0;
                                        END;
                                %END;
                                IF RES = 0 THEN DO;
                                   VARIABLE = "&VARX";
                                   FLAG  = &VARX._F;
                                   OUTPUT;
                                END;
                                %LET K=%EVAL(&K+1);
                                %LET VAR=%SCAN(&VARS,&K,|);
                                %LET VARX=%SCAN(&VAR,1,:);
                                %LET START=%SCAN(&VAR,2,:);
		                        %LET END=%SCAN(&VAR,3,:);
                         %END;
                         IF VARIABLE ^= '';
                RUN;
       %END;

%MEND check_MINUS7;


/**
 * CHECKSUM
 * HY050G = *51G + ... + 52G
 */
%MACRO check_CHKSUM(F=);*check if total var is the sum of its components +/- 10%;

        %IF %SYSFUNC(EXIST(SVAL_MISM_CHKSUM)) %THEN %DO;
                PROC SQL;
                  DROP TABLE SVAL_MISM_CHKSUM
                  ;
                QUIT;
        %END;

        %LET DS=RAW.&_SPLIT_MODE_&ss&cc&YY&F;

        %LOCAL K RC CHECK VARS VAR VARX FMT EXPR TOL;

        DATA _NULL_;
        SET SVAL_CHKSUM_&F END=EOF;
          BY VARIABLE;
          LENGTH VARS EXPR CHK_DTL $32767;
          RETAIN VARS '';
          RETAIN EXPR '';
          RETAIN CHK_DTL '';
          IF _N_ = 1 OR FIRST.VARIABLE THEN DO;
                 EXPR = 'SUM(';
                 CHK_DTL = '';
          END;
          EXPR = TRIM(EXPR) || TRIM(CHILD_ID);
          CHK_DTL = TRIM(CHK_DTL) || "'" || TRIM(CHILD_ID) || "='||" || "STRIP(" || TRIM(CHILD_ID) || ")";

          IF NOT(LAST.VARIABLE) THEN CHK_DTL = TRIM(CHK_DTL) || "||' '||";
          IF LAST.VARIABLE THEN DO;
                 EXPR = TRIM(EXPR) || ')';
             TOL = 0.1;
                 VARS = TRIM(VARS) || TRIM(VARIABLE) || ':' ||
                         COMPRESS(EXPR) || ':' || TRIM(CHK_DTL) || ':' || COMPRESS(PUT(TOL,best8.))
                 ;
                 IF NOT EOF THEN VARS = TRIM(VARS) || '/';
                 ELSE CALL SYMPUT('VARS',TRIM(VARS));
          END;
          ELSE DO;
                EXPR = TRIM(EXPR) || ',';
          END;
        RUN;

        DATA SVAL_MISM_CHKSUM (KEEP=&F.B010 &F.B020 &F.B030 VARIABLE DISP VALUE SUM);
        SET  &DS;
          LENGTH VARIABLE $8;
          LENGTH VALUE $8;
          LENGTH DISP $256;
          LENGTH SUM 8;
        %LET K=1;
        %LET VAR=%SCAN(&VARS,&K,/);
        %LET VARX=%SCAN(&VAR,1,:);
        %DO %WHILE(&VARX NE);
                %LET EXPR=%SCAN(&VAR,2,:);
                %LET DISP=%SCAN(&VAR,3,:);
                %LET TOL=%SCAN(&VAR,4,:);
                IF NOT( %QUOTE( &EXPR ) = . OR %QUOTE( &EXPR ) = 0 ) THEN DO;
                        VARIABLE = "&VARX";
                        VALUE = &VARX;
                        DISP = %QUOTE(&DISP);
                        IF &VARX GE 0 THEN DO;
                                IF &VARX LT %QUOTE( &EXPR ) * ( 1-&TOL ) THEN DO;
                                   SUM = %QUOTE( &EXPR );
                                   OUTPUT;
                                END;
                                /* sometimes total can be filled but not components, so we don't put it as an error
								ELSE IF &VARX GT %QUOTE( &EXPR ) * ( 1+&TOL ) THEN DO;
                                   SUM = %QUOTE( &EXPR );
                                  OUTPUT;
                                END;
								*/
                        END;
                        ELSE DO; /* NEGATIVE VALUE CASE */
                                IF &VARX LT %QUOTE( &EXPR ) * ( 1+&TOL ) THEN DO;
                                   SUM = %QUOTE( &EXPR );
                                   OUTPUT;
                                END;
                                /* sometimes total can be filled but not components, so we don't put it as an error
                                ELSE IF &VARX GT %QUOTE( &EXPR ) * ( 1-&TOL ) THEN DO;
                                   SUM = %QUOTE( &EXPR );
                                   OUTPUT;
                                END;
								*/
                        END;
                END;
                %LET K=%EVAL(&K+1);
                %LET VAR=%SCAN(&VARS,&K,/);
                %LET VARX=%SCAN(&VAR,1,:);
        %END;
        IF VARIABLE ^= '';
        RUN;


%MEND check_CHKSUM;

/**
 * AGGREGATE COMPONENT > 0 -> AT LEAST ONE DETAILED COMPONENT > 0 -> SUM( COMPONENTS ) > 0
 */
%MACRO check_CHILDNULL(F=);*check if disagregated is filled >0 then total is filled >0;

        %IF %SYSFUNC(EXIST(SVAL_MISM_CHILDNULL)) %THEN %DO;
                PROC SQL;
                  DROP TABLE SVAL_MISM_CHILDNULL
                  ;
                QUIT;
        %END;

        %LET DS=RAW.&_SPLIT_MODE_&ss&cc&YY&F;

        %LOCAL K RC CHECK VARS VAR VARX FMT EXPR TOL;

        DATA _NULL_;
        SET SVAL_CHKSUM_&F END=EOF;
          BY VARIABLE;
          LENGTH VARS EXPR CHK_DTL $32767;
          RETAIN VARS '';
          RETAIN EXPR '';
          RETAIN CHK_DTL '';
          IF _N_ = 1 OR FIRST.VARIABLE THEN DO;
                 EXPR = 'SUM(';
                 CHK_DTL = '';
          END;
          EXPR = TRIM(EXPR) || TRIM(CHILD_ID);
          CHK_DTL = TRIM(CHK_DTL) || "'" || TRIM(CHILD_ID) || "='||" || "STRIP(" || TRIM(CHILD_ID) || ")";
          IF NOT(LAST.VARIABLE) THEN CHK_DTL = TRIM(CHK_DTL) || "||' '||";
          IF LAST.VARIABLE THEN DO;
                 EXPR = TRIM(EXPR) || ')';
             TOL = 0.01;
                 VARS = TRIM(VARS) || TRIM(VARIABLE) || ':' ||
                         COMPRESS(EXPR) || ':' || TRIM(CHK_DTL) || ':' || COMPRESS(PUT(TOL,best8.))
                 ;
                 IF NOT EOF THEN VARS = TRIM(VARS) || '/';
                 ELSE CALL SYMPUT('VARS',TRIM(VARS));
          END;
          ELSE DO;
                EXPR = TRIM(EXPR) || ',';
          END;
        RUN;

        DATA SVAL_MISM_CHILDNULL (KEEP=&F.B010 &F.B020 &F.B030 VARIABLE DISP VALUE SUM);
        SET  &DS;
          LENGTH VARIABLE $8;
          LENGTH DISP $256;
          LENGTH VALUE $8;
          LENGTH SUM 8;
        %LET K=1;
        %LET VAR=%SCAN(&VARS,&K,/);
        %LET VARX=%SCAN(&VAR,1,:);
        %DO %WHILE(&VARX NE);
                %LET EXPR=%SCAN(&VAR,2,:);
                %LET DISP=%SCAN(&VAR,3,:);
                %LET TOL=%SCAN(&VAR,4,:);
                IF &VARX ^= . AND &VARX ^= 0 THEN DO;
                        VARIABLE = "&VARX";
                        VALUE = &VARX;
                        DISP = %QUOTE(&DISP);
                        IF %QUOTE( &EXPR ) = . OR %QUOTE( &EXPR ) = 0 THEN DO;
                           SUM = %QUOTE( &EXPR );
                           OUTPUT;
                        END;
                END;
                %LET K=%EVAL(&K+1);
                %LET VAR=%SCAN(&VARS,&K,/);
                %LET VARX=%SCAN(&VAR,1,:);
        %END;
        IF VARIABLE ^= '';
        RUN;


%MEND check_CHILDNULL;

/**
 * NET vs GROSS
 */
%MACRO check_PARENT(F=);*check if gross/net are consistent (gross>=net);

        %IF %SYSFUNC(EXIST(SVAL_MISM_PARENT)) %THEN %DO;
                PROC SQL;
                  DROP TABLE SVAL_MISM_PARENT
                  ;
                QUIT;
        %END;

        %LET DS=RAW.&_SPLIT_MODE_&ss&cc&YY&F;

        %LOCAL K RC CHECK VARS VAR VARX FMT EXPR;

        DATA _NULL_;
        SET SVAL_PARENT_&F END=EOF;
          BY VARIABLE;
          LENGTH VARS EXPR $32767;
          RETAIN VARS '';
          RETAIN CHK_DTL '';
          VARS = TRIM(VARS) || TRIM(VARIABLE) || ':' ||
                         COMPRESS(PARENT_ID)
          ;
          IF NOT EOF THEN VARS = TRIM(VARS) || '|';
          ELSE CALL SYMPUT('VARS',TRIM(VARS));
        RUN;

        DATA SVAL_MISM_PARENT (KEEP=&F.B010 &F.B020 &F.B030 VARIABLE DISP VALUE GROSS);
        SET  &DS;
          LENGTH VARIABLE $8;
          LENGTH DISP $256;
          LENGTH VALUE $8;
          LENGTH GROSS 8;
        %LET K=1;
        %LET VAR=%SCAN(&VARS,&K,|);
        %LET VARX=%SCAN(&VAR,1,:);
        %DO %WHILE(&VARX NE);
                %LET EXPR=%SCAN(&VAR,2,:);
                IF %QUOTE( &EXPR ) = . OR %QUOTE( &EXPR ) = 0 THEN DO;
                        VARIABLE = "&VARX";
                        VALUE = &VARX;
                        DISP = "&EXPR=" || STRIP(&EXPR);
                        IF &VARX ^= . AND &VARX ^= 0 THEN DO;
                           GROSS = %QUOTE( &EXPR );
                           OUTPUT;
                        END;
                END;
                %LET K=%EVAL(&K+1);
                %LET VAR=%SCAN(&VARS,&K,|);
                %LET VARX=%SCAN(&VAR,1,:);
        %END;
        IF VARIABLE ^= '';
        RUN;


%MEND check_PARENT;

%MACRO check_UNIQUE(F=);

        %IF %SYSFUNC(EXIST(SVAL_UNIQUE)) %THEN %DO;
                PROC SQL;
                  DROP TABLE SVAL_UNIQUE;
                QUIT;
        %END;

        %LET DS=RAW.&_SPLIT_MODE_&ss&cc&YY&F;

        %LOCAL K RC CHECK VARS VAR VARX KEYS;

        %LET KEYS=;
        DATA _NULL_;
        SET SVAL_&F (WHERE=(KEY='Y' AND VARIABLE NE "RB040")) END=EOF;
          LENGTH KEYS $32767;
          RETAIN KEYS '';
          KEYS = TRIM(KEYS) || TRIM(VARIABLE);
          IF NOT EOF THEN KEYS = TRIM(KEYS) || ',';
          ELSE CALL SYMPUT('KEYS',TRIM(KEYS));
        RUN;

        %IF &KEYS NE %THEN %DO;
                PROC SQL;
                    CREATE TABLE SVAL_UNIQUE AS
                        SELECT &KEYS, "&KEYS" AS VARIABLE, COUNT(*) as NB
                        FROM   &DS %IF &F=R %THEN %DO;(WHERE=(RB110>0 and RB110<5)) %END;
                        GROUP  BY &KEYS
                        HAVING COUNT(*) GT 1
                        ;
                QUIT;
        %END;

%MEND check_UNIQUE;

%MACRO check_OPTIONAL(F=);

        %IF %SYSFUNC(EXIST(SVAL_MISM_NA)) %THEN %DO;
                PROC SQL;
                  DROP TABLE SVAL_MISM_OPTIONAL;
                QUIT;
        %END;

        %LET DS=RAW.&_SPLIT_MODE_&ss&cc&YY&F;

        PROC SQL;
          SELECT TRIM(VARIABLE)||':'||SUBSTR(FORMAT,1,1)
          INTO :VARS SEPARATED BY '|'
          FROM   SVAL_&F
          WHERE  REQ = 'Y'
          ;
        QUIT;

        DATA SVAL_MISM_OPTIONAL
                (KEEP=&F.B010 &F.B020 &F.B030 VARIABLE VALUE FLAG)
        ;
        SET  &DS;
          LENGTH VARIABLE $8;
          LENGTH VALUE $8;
          LENGTH FLAG 8;
        %LET K=1;
        %LET VAR=%SCAN(&VARS,&K,|);
        %LET VARX=%SCAN(&VAR,1,:);
        %LET FMT=%SCAN(&VAR,2,:);
        %DO %WHILE(&VARX NE);
                 VARIABLE = "&VARX";
                 %IF &FMT NE $ %THEN %DO;
                 VALUE = PUT(&VARX, best8.);
                 %END;
                 %ELSE %DO;
                 VALUE = &VARX;
                 %END;
                 FLAG = &VARX._F;
                 IF FLAG = -8 THEN OUTPUT;
                 %LET K=%EVAL(&K+1);
                 %LET VAR=%SCAN(&VARS,&K,|);
                 %LET VARX=%SCAN(&VAR,1,:);
                 %LET FMT=%SCAN(&VAR,2,:);
         %END;
         IF VARIABLE ^= '';
         RUN;

%MEND check_OPTIONAL;

/**
 * PRECONDITION
 * <NA flag="-3"> -> ALREADY PREPROCESSED
 */
%MACRO check_NA(F=);

        %IF %SYSFUNC(EXIST(SVAL_MISM_NA)) %THEN %DO;
                PROC SQL;
                  DROP TABLE SVAL_MISM_NA;
                QUIT;
        %END;

        %LET DS=RAW.&_SPLIT_MODE_&ss&cc&YY&F;

        %LOCAL K RC CHECK VARS VAR;

        DATA _NULL_;
        SET SVAL_NAS_&F (where=(NA_FLAG not in (-7 -8))) END=EOF;
          LENGTH VARS _LIST_ $32767;
          RETAIN VARS '';
          VARS = TRIM(VARS) || TRIM(VARIABLE) || ':' || SUBSTR(FORMAT,1,1) || ':' || COMPRESS(PUT(NA_FLAG,best8.))
          ;
          IF NOT EOF THEN VARS = TRIM(VARS) || '|';
          ELSE CALL SYMPUT('VARS',TRIM(VARS));
        RUN;

        /* PURGING */
        PROC SQL;
          DELETE FROM SVAL_NAS_&F
          WHERE  VARIABLE IN
          (      SELECT VARIABLE FROM SVAL_&F
                 WHERE  TYPE is not null    )
      ;
        QUIT;

        DATA _NULL_;
        SET SVAL_NAS_&F (where=(NA_FLAG not in (-7 -8))) END=EOF;
          LENGTH VARS _LIST_ $32767;
          RETAIN VARS '';
          VARS = TRIM(VARS) || TRIM(VARIABLE) || ':' || SUBSTR(FORMAT,1,1) || ':' || COMPRESS(PUT(NA_FLAG,best8.))
          ;
          IF NOT EOF THEN VARS = TRIM(VARS) || '|';
          ELSE CALL SYMPUT('VARS',TRIM(VARS));
        RUN;

        DATA _NULL_;
        SET SVAL_NAS_&F (where=(NA_FLAG not in (-7 -8))) END=EOF;
      RETAIN K 1;
          LENGTH NA $32767;
          VARNAME = 'NA_' || COMPRESS(PUT(K,best8.));
          CALL SYMPUTX(VARNAME,NA);
          K + 1;
        RUN;

        PROC SQL NOPRINT;
          SELECT DISTINCT "'"||TRIM(CNTRY)||"'" INTO: SELRESP_CNTRY_LST SEPARATED BY ' '
          FROM   Sval_selresp_cntry_lst
          ;
        QUIT;
        %PUT *I* SELECTED RESP COUNTRY LIST: &SELRESP_CNTRY_LST
        ;
        DATA SVAL_MISM_NA ;
        SET  &DS;
          LENGTH VARIABLE $8;
          LENGTH VALUE $8;
          LENGTH FLAG 8;
          LENGTH CHECK $3000;
        %LET K=1;
        %LET VAR=%SCAN(&VARS,&K,|);
        %LET VARX=%SCAN(&VAR,1,:);
        %DO %WHILE(&VARX NE);
                %LET FMT=%SCAN(&VAR,2,:);
                %LET NA_FLAG=%SCAN(&VAR,3,:);
                NA_FLAG=&NA_FLAG;
                %IF &NA_FLAG EQ -2 %THEN %DO;
	                IF &VARX._F NE - 8 AND &VARX._F NE -7 AND &VARX._F NE -5 AND &VARX._F NE -4 AND &VARX._F NE -3 THEN DO;
                %END;
                %ELSE %IF &NA_FLAG EQ -3 %THEN %DO;
	                IF &VARX._F NE - 8 AND &VARX._F NE -7 /*AND &VARX._F NE -5 AND &VARX._F NE -4 AND &VARX._F NE -2*/ THEN DO;
                %END;
                %ELSE %IF &NA_FLAG EQ -4 %THEN %DO;
	                IF &VARX._F NE - 8 AND &VARX._F NE -7 AND &VARX._F NE -5 AND &VARX._F NE -3 AND &VARX._F NE -2 THEN DO;
                %END;
                %ELSE %IF &NA_FLAG EQ -5 %THEN %DO;
	                IF &VARX._F NE - 8 AND &VARX._F NE -7 AND &VARX._F NE -4 AND &VARX._F NE -3 AND &VARX._F NE -2 THEN DO;
                %END;
                %ELSE %DO;
	                IF 1 EQ 2 THEN DO;
                %END;
                   IF &VARX._F ne . THEN DO;
                         VARIABLE = "&VARX";
                         %IF &FMT NE $ %THEN %DO;
	                         VALUE = COMPRESS(PUT(&VARX, best8.));
                         %END;
                         %ELSE %DO;
	                         VALUE = &VARX;
                         %END;
                         FLAG = &VARX._F;
                         /* FLAG = -2, -4, -5 */
                         %IF &NA_FLAG EQ -2 OR &NA_FLAG EQ -4 OR &NA_FLAG EQ -5 %THEN %DO;
                         IF ( %QUOTE(&&&NA_&K) ) AND ( &VARX._F NE &NA_FLAG ) THEN DO;
                                    CHECK = "%QUOTE(&&&NA_&K)";
                                        OUTPUT; /* HERE THE HIERARCHY MATTERS */
                                 END;
                                 ELSE IF NOT( ( %QUOTE(&&&NA_&K) ) OR ( &VARX._F NE &NA_FLAG ) ) THEN DO;
                                    CHECK = "%QUOTE(&&&NA_&K)";
                                        OUTPUT;
                                 END;
                         %END;
                         %ELSE %IF &NA_FLAG EQ -3 %THEN %DO;
							/* SELRESP COUNTRY */
							IF &F.B020 IN ( &SELRESP_CNTRY_LST ) THEN DO;
								IF ( %QUOTE(&&&NA_&K) ) AND ( &VARX._F NE &NA_FLAG ) THEN OUTPUT; /*Non-selected respondent with flag other than -3*/ 
								ELSE IF %QUOTE(&&&NA_&K)  AND &VARX._F NE &NA_FLAG AND NOT (VARIABLE IN ("PB260","PB270") AND PB010<=2023) THEN OUTPUT;
								ELSE IF NOT(%QUOTE(&&&NA_&K)) AND ( &VARX._F = &NA_FLAG ) THEN OUTPUT; /*[LV 20250316]Line added: Selected respondent with flag = -3*/
							END;
							/* REGULAR COUNTRY (-3 NOT ALLOWED) */
							ELSE DO;
								/*we allow -3 flags for PB260 and PB270 until 2024 then a -2 must be used*/
								IF &VARX._F EQ &NA_FLAG AND NOT (VARIABLE IN ("PB260","PB270") AND PB010<=2023) THEN DO;OUTPUT;END;
							END;
                         %END;
                   END;
                END;
                %LET K=%EVAL(&K+1);
                %LET VAR=%SCAN(&VARS,&K,|);
                %LET VARX=%SCAN(&VAR,1,:);
         %END;
         IF VARIABLE ^= '';
         RUN;

         /**
          * DISP cfg column handling
          */
         %LET VARS=;
         PROC SQL NOPRINT;
           SELECT DISTINCT TRIM(VARIABLE)||':'||PUT(NA_FLAG,2.) INTO :VARS SEPARATED BY '|'
           FROM   SVAL_MISM_NA
           ;
         QUIT;
         %PUT &VARS;
         DATA SVAL_MISM_NA;
         SET  SVAL_MISM_NA;
           LENGTH DISP $3000;
           DISP = '';
         RUN;
         %LET K=1;
         %LET VAR=%SCAN(&VARS,&K,|);
         %LET VARX=%SCAN(&VAR,1,:);
         %LET FLAGX=%SCAN(&VAR,2,:);

         %DO %WHILE(&VARX NE);
            DATA _NULL_;
                SET SVAL_NAS_&F (WHERE=(VARIABLE="&VARX" AND NA_FLAG=&FLAGX));
                  LENGTH DISP_STR $32767;
                  LENGTH DISP_TOK $10;
                  J = 1;
                  DISP_TOK = SCAN(DISP,J,',');
                  DO WHILE (DISP_TOK ^= '');
                     IF J EQ 1 THEN DISP_STR = "'" || TRIM(DISP_TOK) || "=" || "' " || '|| ' || DISP_TOK;
                         ELSE DO;
                           DISP_STR = TRIM(DISP_STR) || ' || ' || "' | " || TRIM(DISP_TOK) || '=' || "' " || '|| ' || DISP_TOK;
                         END;
                         J + 1;
                         DISP_TOK = SCAN(DISP,J,',');
                  END;
                  CALL SYMPUTX('DISP_STR',DISP_STR);
        RUN;

                %IF &DISP_STR NE %THEN %DO;
                DATA SVAL_MISM_NA;
                SET  SVAL_MISM_NA;
                     LABEL DISP = 'CHECK';
                         IF VARIABLE = "&VARX" AND NA_FLAG=&FLAGX THEN DO;
                                DISP = &DISP_STR;
                                DISP = COMPRESS( DISP );
                         END;
                RUN;
                %END;
                %LET K=%EVAL(&K+1);
                %LET VAR=%SCAN(&VARS,&K,|);
                %LET VARX=%SCAN(&VAR,1,:);
                %LET FLAGX=%SCAN(&VAR,2,:);
         %END;

         DATA SVAL_MISM_NA (KEEP=&F.B010 &F.B020 &F.B030 NA_FLAG VARIABLE VALUE FLAG DISP CHECK);
         SET  SVAL_MISM_NA;
         RUN;

%MEND check_NA;

%MACRO check_MISS(F=);

        %IF %SYSFUNC(EXIST(SVAL_MISS)) %THEN %DO;
                PROC SQL;
                  DROP TABLE SVAL_MISS;
                QUIT;
        %END;

        %LET DS=RAW.&_SPLIT_MODE_&ss&cc&YY&F;

        %LOCAL K RC CHECK VARS VAR VARX LIST LIST_STR N_REC;

        DATA _NULL_;
        SET SVAL_&F END=EOF;
          WHERE  1 EQ 1
          ;
          LENGTH VARS $32767;
          RETAIN VARS '';
          VARS = TRIM(VARS) || TRIM(VARIABLE) || ':' || SUBSTR(FORMAT,1,1);
          IF NOT EOF THEN VARS = TRIM(VARS) || '|';
          IF EOF THEN CALL SYMPUT('VARS',TRIM(VARS));
        RUN;

        DATA SVAL_MISS (KEEP=&F.B010 &F.B020 &F.B030 VARIABLE FLAG);
        SET  &DS NOBS=NOBS END=EOF;
          LENGTH VARIABLE $8;
                %LET K=1;
                %LET VAR=%SCAN(&VARS,&K,|);
                %LET VARX=%SCAN(&VAR,1,:);
                %DO %WHILE(&VARX NE);
                        %LET FMT=%SCAN(&VAR,2,:);
                                IF &VARX._F = -1 or &VARX._F = -8 THEN DO;
                                   VARIABLE = "&VARX";
								   FLAG=&VARX._F;
                                   OUTPUT SVAL_MISS;
                                END;
                                /*
                                %IF &FMT NE $ %THEN %DO;
                                IF &VARX = . AND (&VARX._F = . OR &VARX._F = -1) THEN DO;
                                   VARIABLE = "&VARX";
                                   OUTPUT SVAL_MISS;
                                END;
                                %END;
                                %ELSE %DO;
                                IF &VARX = '' AND (&VARX._F = . OR &VARX._F = -1) THEN DO;
                                   VARIABLE = "&VARX";
                                   OUTPUT SVAL_MISS;
                                END;
                                %END;
                                */
                        %LET K=%EVAL(&K+1);
                        %LET VAR=%SCAN(&VARS,&K,|);
                        %LET VARX=%SCAN(&VAR,1,:);
                 %END;
        RUN;

%MEND check_MISS;


%MACRO count_MISS (F=,METHOD=OLD);

  %LOCAL VAR VARS;

  %LET DS=RAW.&_SPLIT_MODE_&ss&cc&YY&F;

  %LET VARS=;
  PROC SQL NOPRINT;
          SELECT DISTINCT VARIABLE INTO :VARS SEPARATED BY ' '
          FROM   SVAL_MISS
          ;
  QUIT;

  %IF &VARS ne %THEN %DO;

        %IF &METHOD EQ OLD %THEN %DO;
            %LET K=1;
            %LET VAR=%SCAN(&VARS, &K);
            %DO %WHILE(&VAR ne);
                PROC MEANS NWAY DATA=&DS (WHERE=(&VAR._F ge -1 or &VAR._F=-8)) NOPRINT;
                 CLASS &F.B010;
                 VAR &VAR._F;
                 OUTPUT OUT=MISS_SIZE_&k N=SIZE;
                RUN;
                /*PROC CONTENTS DATA=&DS; RUN;
                 PROC FREQ DATA=&DS; TABLE PB240_F; RUN;*/
                %IF &K eq 1 %THEN %DO;
                    DATA MISS_SIZE;
                    SET  MISS_SIZE_1;
                       LENGTH VARIABLE $8;
                       VARIABLE = "&VAR";
                    RUN;
                %END;
                %ELSE %DO;
                    DATA MISS_SIZE_&k;
                    SET  MISS_SIZE_&k;
                       LENGTH VARIABLE $8;
                       VARIABLE = "&VAR";
                    RUN;
                    PROC APPEND BASE=MISS_SIZE DATA=MISS_SIZE_&k;
                    RUN;
                    PROC SQL;
                             DROP TABLE MISS_SIZE_&k;
                    QUIT;
                %END;
                %LET K=%EVAL(&K+1);
                %LET VAR=%SCAN(&VARS, &K);
            %END;
            PROC SORT DATA=MISS_SIZE;
              BY VARIABLE &F.B010;
            PROC MEANS NWAY DATA=SVAL_MISS NOPRINT;
              CLASS VARIABLE FLAG &F.B010;
              OUTPUT OUT=MISS_STAT N=N;
            RUN;
            PROC SORT DATA=MISS_STAT;
              BY VARIABLE &F.B010;
            DATA MISS_STAT;
            MERGE MISS_STAT MISS_SIZE;
              BY VARIABLE &F.B010;
              FORMAT PC PERCENT7.2;
              PC = N / SIZE;
            PROC SORT;
	          BY VARIABLE FLAG;
            RUN;
        %END;
        %ELSE %DO;
            PROC MEANS NWAY DATA=&DS NOPRINT;
              CLASS &F.B010;
              OUTPUT OUT=MISS_SIZE N=SIZE;
            RUN;
            PROC MEANS NWAY DATA=SVAL_MISS NOPRINT;
              CLASS &F.B010 FLAG VARIABLE;
              OUTPUT OUT=MISS_STAT N=N;
            RUN;
            DATA MISS_STAT;
            MERGE MISS_STAT SIZE;
              BY &F.B010;
              FORMAT PC PERCENT7.2;
              PC = N / SIZE;
            PROC SORT;
	          BY VARIABLE FLAG;
            RUN;
        %END;

        PROC TRANSPOSE OUT=SVAL_MISS_STAT_&F&_SPLIT_MODE_;
          BY VARIABLE FLAG;
          ID &F.B010;
          VAR PC;
        RUN;

		DATA SVAL_&F;
		SET  SVAL_&F;
		IF _N_ = 1 THEN SET SVAL_MISSTHRES (rename=(MISSTHRES=MISSTHRES_DEFAULT));
		IF MISSTHRES lt 0 THEN MISSTHRES = MISSTHRES_DEFAULT;
		RUN;

        DATA SVAL_MISS_STAT_&F&_SPLIT_MODE_;
	        SET  SVAL_MISS_STAT_&F&_SPLIT_MODE_;
			FORMAT MAXPC PERCENT7.2;
			MAXPC = MAX( OF _NUMERIC_ );
        RUN;

		DATA SVAL_MISS_STAT_&F&_SPLIT_MODE_;
			MERGE SVAL_MISS_STAT_&F&_SPLIT_MODE_ (in=A) SVAL_&F (keep=VARIABLE MISSTHRES);
			BY VARIABLE;
			IF A;
			IF MAXPC >= MISSTHRES;
		RUN;

  %END;
  %ELSE %DO;
         DATA SVAL_MISS_STAT_&F&_SPLIT_MODE_;
           LENGTH VARIABLE $8 _NAME_ $8 FLAG 8;
           STOP;
         RUN;
  %END;

%MEND count_MISS;

%MACRO FMT_LIST(SLIST);

%LET LIST_STR=;
%LET K=1;
%LET S=%SCAN(&SLIST,&K);
%DO %WHILE(&S NE);
        %LET LIST_STR=&LIST_STR "&S";
        %LET K=%EVAL(&K+1);
        %LET S=%SCAN(&SLIST,&K);
%END;

%MEND FMT_LIST;

%MACRO purge_FALSE_ANTE_ERRORS_24_25(F=);*purging errors in 2024 and 2025 due to ANTE (i.e.2019 and 2020) for 6 year-panel countries ;
%IF (&YYYY = 2024 or &YYYY = 2025) AND &ROTATION = 6 %THEN %DO;
	/*we delete errors on routing conditions, non HH members, 
	flags, flags length and imputation factors for income variables which were already existing before 2021*/
	DATA SVAL_MISM_NA;SET SVAL_MISM_NA;IF &F.B010 > 2020 ;RUN;
	DATA SVAL_NON_MEMB;SET SVAL_NON_MEMB;IF &F.B010 > 2020 ;RUN;
	DATA SVAL_NON_MEMB_MISS;SET SVAL_NON_MEMB_MISS;IF &F.B010 > 2020 ;RUN;

	/*proxy for income variable : 2nd letter in name is Y*/
	DATA SVAL_MISM_F;SET SVAL_MISM_F;IF &F.B010 <= 2020 AND SUBSTR(VARIABLE,2,1)="Y" THEN DELETE;RUN;

	DATA SVAL_MISM_FLEN;SET SVAL_MISM_FLEN;IF &F.B010 <= 2020 AND SUBSTR(VARIABLE,2,1)="Y" THEN DELETE;RUN; 

	DATA SVAL_MISM_IF_MISSING;SET SVAL_MISM_IF_MISSING;IF &F.B010 <= 2020 AND SUBSTR(VARIABLE,2,1)="Y" THEN DELETE;RUN;


%END;

%MEND;
/**
 * REPORT SVAL
 */
%MACRO rep_SVAL(F=);*create sval pdf/doc reports and export errors in csv files;

%LOCAL N_REC_OK;
/*calculate first and last year before and after split (2 digits)*/
%IF &_SPLIT_MODE_ = _ANTE_ %THEN %DO;%LET y1=%EVAL(&yy-&rotation+1);%LET y2=%SUBSTR(&_SPLIT_CUT_,3,2);%END;
%IF &_SPLIT_MODE_ = _POST_ %THEN %DO;%LET y1=%EVAL(%SUBSTR(&_SPLIT_CUT_,3,2)+1);%LET y2=&yy;%END;
%IF &_SPLIT_MODE_ =        %THEN %DO;%LET y1=;%LET y2=;%END;
%IF &y1=&y2 %THEN %DO;%LET y2=;%END;

OPTIONS ORIENTATION=LANDSCAPE NODATE NONUMBER;
ODS &OUTPUTFORMAT FILE = "&OUT&_dirsp_%quote(&CC&YY.-Struct%LOWCASE(&_SPLIT_MODE_)&y1&y2._&F.file.&EXTENSION)" &OUTOPTION ;


/**
 * ID UNIQUENESS
 * table: SVAL_UNIQUE
 */
%LET NOBS=0;
DATA SVAL_UNIQUE;
SET  SVAL_UNIQUE NOBS=NOBS;
  WARNING = 'ID UNIQUENESS';
  CALL SYMPUTX('NOBS',NOBS);
RUN;
%reduce_TBL(TBL=SVAL_UNIQUE,F=&F);
%IF &NOBS GT 0 %THEN %DO;
        TITLE1 "ID IS NOT UNIQUE";
        TITLE2 "&CC - &YYYY / TRANSMISSION=&SS";
        PROC PRINT NOOBS DATA=SVAL_UNIQUE;
        RUN;
        TITLE1;
        PROC APPEND
          BASE=SVAL_MISS_FULL_CSV_&F&_SPLIT_MODE_
          DATA=_LAST_
          FORCE
          ;
        RUN;
%END;

/**
MISSING AND UNEXPECTED VAR
*/
%LET NOBS=0;
DATA MISSVARS_&F&_SPLIT_MODE_;
SET  MISSVARS NOBS=NOBS;
  CALL SYMPUTX('NOBS',NOBS);
RUN;
%LET NOBS=0;
PROC SQL;
  DELETE FROM DIRTYVARS
  WHERE  VARIABLE IN ( SELECT VARIABLE FROM MISSVARS )
  ;
QUIT;
DATA DIRTYVARS_&F&_SPLIT_MODE_;
SET  DIRTYVARS NOBS=NOBS;
  CALL SYMPUTX('NOBS',NOBS);
RUN;


%IF %SYSFUNC(EXIST(SVAL_TRANS_LIST)) %THEN %DO;
	%LET NOBS=0;
    DATA _NULL_;
		SET  SVAL_TRANS_LIST NOBS=NOBS;
		CALL SYMPUTX('NOBS',NOBS);
    RUN;
    %IF &NOBS GT 0 %THEN %DO;
	    DATA TRN_VARLABELS (KEEP=START END LABEL FMTNAME TYPE);
	    SET  SVAL_TRANS_LIST;
	         START = VARIABLE; END = START; FMTNAME = 'TRNLABEL'; TYPE = 'C';
	         LABEL = TRIM(LABEL);
	         OUTPUT;
	         START = TRIM(VARIABLE) || '_F'; END = START;
	         OUTPUT;
	    RUN;
	    PROC FORMAT CNTLIN=TRN_VARLABELS LIBRARY=WORK;
	    RUN;
	%END;
%END;

%IF %SYSFUNC(EXIST(MISM_TRANS)) %THEN %DO;
	%LET NOBS=0;
	DATA _NULL_;
	    SET  MISM_TRANS NOBS=NOBS;
	    CALL SYMPUTX('NOBS',NOBS);
    RUN;
    %IF &NOBS GT 0 %THEN %DO;
	    DATA MISM_TRANS;
	    SET  MISM_TRANS;
	      LENGTH LABEL $256;
	      LABEL = PUT(VARIABLE,$TRNLABEL.);
	    RUN;

       /**
        * TRANSMISSION GUIDELINES (1/2)
        */
        *TITLE1 "VARIABLES COMPULSORY BEFORE &YYYY NOT FOUND";
        TITLE1 "VARIABLES COMPULSORY BEFORE 20&Y1 NOT FOUND";
        PROC TABULATE;
          TITLE2 "&CC - &YYYY / TRANSMISSION=&SS";
          CLASS VARIABLE LABEL DESC / MISSING;
          TABLE (VARIABLE*LABEL ALL), DESC='MSG';
          KEYLABEL N = ' ';
          KEYLABEL ALL = 'TOTAL';
        RUN;

    %END;

%END;

%IF %SYSFUNC(EXIST(MISM_TRANS_PCT)) %THEN %DO;
	%LET NOBS=0;
    DATA _NULL_;
		SET  MISM_TRANS_PCT NOBS=NOBS;
		CALL SYMPUTX('NOBS',NOBS);
    RUN;
    %IF &NOBS GT 0 %THEN %DO;
	    DATA MISM_TRANS_PCT;
	    SET  MISM_TRANS_PCT ;
	      LENGTH LABEL $256;
	      LABEL = PUT(VARIABLE,$TRNLABEL.);
	      &F.B010 = &YYYY; &F.B030 = _N_;
	      WARNING = 'TRNS-GDLNS-COMPLIANCE';
	    RUN;
       /**
        * TRANSMISSION GUIDELINES (2/2)
        */
        TITLE1 "TRANSMISSION GUIDELINES COMPLIANCE";
        PROC TABULATE;
          TITLE2 "&CC - &YYYY / TRANSMISSION=&SS";
          TITLE3 "VARIABLES NOT COLLECTED FROM 20&Y1 ONWARDS";
          CLASS VARIABLE LABEL FLAG FL_EXPECTED / MISSING;
          VAR PERCENT;
          TABLE VARIABLE*LABEL*FLAG='FLAG (WRONG)'*FL_EXPECTED='FLAG (EXPECTED)', PERCENT*F=PERCENT9.2;
          KEYLABEL N = ' ';
          KEYLABEL SUM = ' ';
        RUN;
        TITLE3;

    %END;

%END;

%LET NOBS=0;
DATA NOFLAGS;
	SET  NOFLAGS NOBS=NOBS;
	CALL SYMPUTX('NOBS',NOBS);
RUN;
%IF &NOBS GT 0 %THEN %DO;
        TITLE1 'MISSING FLAGS';
        PROC FREQ;
          TITLE2 "&CC - &YYYY / TRANSMISSION=&SS";
          TABLE VARIABLE / NOPCT;
          FORMAT VARIABLE $VARLABEL.;
        RUN;
%END;

%LET NOBS=0;
%IF %SYSFUNC(EXIST(NOIMPUTE)) %THEN %DO;
    DATA NOIMPUTE;
    SET  NOIMPUTE NOBS=NOBS;
         CALL SYMPUTX('NOBS',NOBS);
    RUN;
    %IF &NOBS GT 0 %THEN %DO;
        TITLE1 'MISSING IMPUTATION FACTORS (*_IF)';
        PROC FREQ;
          TITLE2 "&CC - &YYYY / TRANSMISSION=&SS";
          TABLE VARIABLE / NOPCT;
          FORMAT VARIABLE $VARLABEL.;
        RUN;
    %END;
%END;

%LET NOBS=0;
DATA MISMVARS;
SET  MISMVARS NOBS=NOBS;
  CALL SYMPUTX('NOBS',NOBS);
RUN;
%IF &NOBS GT 0 %THEN %DO;
        TITLE1 'TYPE-MISMATCHING VARIABLES';
        PROC FREQ;
          TITLE2 "&CC - &YYYY / TRANSMISSION=&SS";
          TABLE VARIABLE / NOPCT;
          FORMAT VARIABLE $VARLABEL.;
        RUN;
%END;

DATA SVAL_MISS_FULL_CSV_&F&_SPLIT_MODE_;
LENGTH WARNING $32;
LENGTH &F.B010 &F.B030 8;
LENGTH RB110 8;
LENGTH VARIABLE $12;
LENGTH DIGIT 3.;
LENGTH VALUE $32;
LENGTH REF $32;
LENGTH DISP $256;
LENGTH FLAG  8;
LENGTH LEN 3;
  STOP;
RUN;

%IF %SYSFUNC(EXIST(ERR)) %THEN %DO;
        %LET NOBS=0;
        DATA ERR;
        SET  ERR NOBS=NOBS;
          WARNING = 'TYPE-MISMATCH';
          CALL SYMPUTX('NOBS',NOBS);
        RUN;
        %reduce_TBL(TBL=ERR,F=&F);
        %IF &NOBS GT 0 %THEN %DO;
                TITLE1 'TYPE-MISMATCHING RECORDS';
                PROC TABULATE;
                  TITLE2 "&CC - &YYYY / TRANSMISSION=&SS";
                  CLASS &F.B010 &F.B030 LABEL SHLABEL VARIABLE VALUE;
                  TABLE VARIABLE*SHLABEL*&F.B030="ID# (&F.B030)"*VALUE, &F.B010;
                RUN;
                PROC APPEND
                  BASE=SVAL_MISS_FULL_CSV_&F&_SPLIT_MODE_
                  DATA=ERR
                  FORCE
                  ;
                RUN;
        %END;
%END;

%LET NOBS=0;
DATA SVAL_MISM_V;
SET  SVAL_MISM_V (WHERE=(VARIABLE IS NOT NULL)) NOBS=NOBS;
  WARNING = 'VALUE';
  CALL SYMPUTX('NOBS',NOBS);
RUN;
%IF &NOBS GT 0 %THEN %DO;
	   PROC SORT NODUPKEY;
		  BY &F.B010 &F.B030 VARIABLE;
	   RUN;
	   %reduce_TBL(TBL=SVAL_MISM_V,F=&F);
       TITLE1 'VALUE ERROR LIST';
        PROC TABULATE;
          TITLE2 "&CC - &YYYY / TRANSMISSION=&SS";
          CLASS &F.B010 &F.B030 LABEL SHLABEL VARIABLE VALUE / MISSING;
          TABLE VARIABLE*SHLABEL*&F.B030="ID# (&F.B030)"*VALUE, &F.B010;
        RUN;
        DATA SVAL_MISM_V;
        SET  SVAL_MISM_V;
       VALUE = VALUE;
    RUN;
        PROC APPEND
          BASE=SVAL_MISS_FULL_CSV_&F&_SPLIT_MODE_
          DATA=_LAST_
          FORCE
          ;
        RUN;
%END;

%LET NOBS=0;
DATA SVAL_MISM_F;
SET  SVAL_MISM_F NOBS=NOBS;
  WARNING = 'FLAG';
  IF DIGIT in (0,.) THEN DIGIT = 1;
  CALL SYMPUTX('NOBS',NOBS);
PROC SORT NODUPKEY;
  BY &F.B010 &F.B030 VARIABLE;
RUN;
proc format;
  value ordinal
    1='1st'
    2='2nd'
    3='3rd'
    ;
run;
%reduce_TBL(TBL=SVAL_MISM_F,F=&F);

%IF &NOBS GT 0 %THEN %DO;
        TITLE1 'FLAG ERROR LIST (1st 3 ERRORS MAX)';
        PROC TABULATE ;
          TITLE2 "&CC - &YYYY / TRANSMISSION=&SS";
          CLASS &F.B010 &F.B030 LABEL SHLABEL VARIABLE VALUE DIGIT LOV / MISSING;
          TABLE VARIABLE*SHLABEL*&F.B030="ID# (&F.B030)"*VALUE='FLAG VALUE'*DIGIT='WRONG DIGIT'*LOV='ALLOWED VALUES', &F.B010;
          LABEL VALUE = 'FLAG';
		  FORMAT DIGIT ordinal.; 
        RUN;
        DATA SVAL_MISM_F;
        LENGTH VALUE $32;
        SET  SVAL_MISM_F (RENAME=(VALUE=VARX));
          VALUE = COMPRESS(PUT(VARX,$10.));
        RUN;
        PROC APPEND
          BASE=SVAL_MISS_FULL_CSV_&F&_SPLIT_MODE_
          DATA=_LAST_
          FORCE
          ;
        RUN;
%END;

%LET NOBS=0;
%IF %SYSFUNC(EXIST(SVAL_MISM_FLEN)) %THEN %DO;
    DATA SVAL_MISM_FLEN;
    SET  SVAL_MISM_FLEN NOBS=NOBS;
      WARNING = 'FLAG_LENGTH';
      CALL SYMPUTX('NOBS',NOBS);
    PROC SORT NODUPKEY;
      BY &F.B010 &F.B030 VARIABLE VALUE LEN;
    RUN;
    %reduce_TBL(TBL=SVAL_MISM_FLEN,F=&F);
%END;

%IF &NOBS GT 0 %THEN %DO;
        TITLE1 'FLAG LENGTH ISSUES';
        PROC TABULATE;
          TITLE2 "&CC - &YYYY / TRANSMISSION=&SS";
          CLASS &F.B010 &F.B030 LABEL SHLABEL VARIABLE VALUE LEN / MISSING;
          TABLE VARIABLE*SHLABEL*&F.B030="ID# (&F.B030)"*VALUE*LEN='#DIGITS (REQUIRED)', &F.B010;
          LABEL VALUE = 'FLAG';
        RUN;
        DATA SVAL_MISM_FLEN;
        LENGTH VALUE $32;
        SET  SVAL_MISM_FLEN (RENAME=(VALUE=VARX));
          VALUE = COMPRESS(PUT(VARX,$10.));
        RUN;
        PROC APPEND
          BASE=SVAL_MISS_FULL_CSV_&F&_SPLIT_MODE_
          DATA=_LAST_
          FORCE
          ;
        RUN;
%END;
%LET NOBS=0;
DATA SVAL_MISM_VF;
SET  SVAL_MISM_VF NOBS=NOBS;
  WARNING = 'VALUE_vs_FLAG';
  CALL SYMPUTX('NOBS',NOBS);
PROC SORT NODUPKEY;
  BY &F.B010 &F.B030 VARIABLE;
RUN;
%reduce_TBL(TBL=SVAL_MISM_VF,F=&F);
%IF &NOBS GT 0 %THEN %DO;
        TITLE1 "VALUE vs FLAG ERROR LIST";
        TITLE2 "&CC - &YYYY / TRANSMISSION=&SS";
        PROC TABULATE;
          CLASS &F.B010 &F.B030 LABEL SHLABEL VARIABLE VALUE FLAG / MISSING;
          TABLE VARIABLE*SHLABEL*&F.B030="ID# (&F.B030)"*VALUE*FLAG, &F.B010;
        RUN;
        DATA SVAL_MISM_VF;
        SET  SVAL_MISM_VF;
          VALUE = VALUE;
        RUN;
        PROC APPEND
          BASE=SVAL_MISS_FULL_CSV_&F&_SPLIT_MODE_
          DATA=_LAST_
          FORCE
          ;
        RUN;
%END;

%IF &_SPLIT_MODE_ NE _ANTE_ %THEN %DO;
	%LET NOBS=0;
	DATA SVAL_MISM_IF_MISSING;
	SET  SVAL_MISM_IF_MISSING NOBS=NOBS;
	  WARNING = 'MISSING_IF';
	  CALL SYMPUTX('NOBS',NOBS);
	PROC SORT NODUPKEY;
	  BY &F.B010 &F.B030 VARIABLE;
	RUN;
	%reduce_TBL(TBL=SVAL_MISM_IF_MISSING,F=&F);
	%IF &NOBS GT 0 %THEN %DO;
	        TITLE1 "IMPUTATION FACTORS SHOULD BE FILLED IN";
	        TITLE2 "&CC - &YYYY / TRANSMISSION=&SS";
	        PROC TABULATE;
	          CLASS &F.B010 &F.B030 LABEL SHLABEL VARIABLE VALUE FLAG IMPUT_FACTOR / MISSING;
	          TABLE VARIABLE*SHLABEL*&F.B030="ID# (&F.B030)"*VALUE*FLAG*IMPUT_FACTOR, &F.B010;
	        RUN;
	        DATA SVAL_MISM_IF_MISSING;
	        SET  SVAL_MISM_IF_MISSING;
	          VALUE = VALUE;
	        RUN;
	        PROC APPEND
	          BASE=SVAL_MISS_FULL_CSV_&F&_SPLIT_MODE_
	          DATA=_LAST_
	          FORCE
	          ;
	        RUN;
	%END;

	%LET NOBS=0;
	DATA SVAL_MISM_IF_WRONG;
	SET  SVAL_MISM_IF_WRONG NOBS=NOBS;
	  WARNING = 'WRONG_IF';
	  CALL SYMPUTX('NOBS',NOBS);
	PROC SORT NODUPKEY;
	  BY &F.B010 &F.B030 VARIABLE;
	RUN;
	%reduce_TBL(TBL=SVAL_MISM_IF_WRONG,F=&F);
	%IF &NOBS GT 0 %THEN %DO;
	        TITLE1 "IMPUTATION FACTORS SHOULD BE EMPTY";
	        TITLE2 "&CC - &YYYY / TRANSMISSION=&SS";
	        PROC TABULATE;
	          CLASS &F.B010 &F.B030 LABEL SHLABEL VARIABLE VALUE FLAG IMPUT_FACTOR / MISSING;
	          TABLE VARIABLE*SHLABEL*&F.B030="ID# (&F.B030)"*VALUE*FLAG*IMPUT_FACTOR, &F.B010;
	        RUN;
	        DATA SVAL_MISM_IF_WRONG;
	        SET  SVAL_MISM_IF_WRONG;
	          VALUE = VALUE;
	        RUN;
	        PROC APPEND
	          BASE=SVAL_MISS_FULL_CSV_&F&_SPLIT_MODE_
	          DATA=_LAST_
	          FORCE
	          ;
	        RUN;
	%END;
%END;


%IF %SYSFUNC(EXIST(SVAL_MISM_MINUS7)) or %SYSFUNC(EXIST(SVAL_MISM_MINUS7ADHOC)) 
	or %SYSFUNC(EXIST(SVAL_MISM_MINUS7RECUR3)) or %SYSFUNC(EXIST(SVAL_MISM_MINUS7RECUR6))%THEN %DO;
		%IF %SYSFUNC(EXIST(SVAL_MISM_MINUS7)) %THEN %DO;%END;%ELSE %DO;data SVAL_MISM_MINUS7;set _NULL_;run;%END;
		%IF %SYSFUNC(EXIST(SVAL_MISM_MINUS7ADHOC)) %THEN %DO;DATA SVAL_MISM_MINUS7;SET SVAL_MISM_MINUS7 SVAL_MISM_MINUS7ADHOC;run;%END;
		%IF %SYSFUNC(EXIST(SVAL_MISM_MINUS7RECUR3)) %THEN %DO;DATA SVAL_MISM_MINUS7;SET SVAL_MISM_MINUS7 SVAL_MISM_MINUS7RECUR3;run;%END;
		%IF %SYSFUNC(EXIST(SVAL_MISM_MINUS7RECUR6)) %THEN %DO;DATA SVAL_MISM_MINUS7;SET SVAL_MISM_MINUS7 SVAL_MISM_MINUS7RECUR6;run;%END;

        %LET NOBS=0;
        DATA SVAL_MISM_MINUS7;
        SET  SVAL_MISM_MINUS7 NOBS=NOBS;
          WARNING = 'WRONG FLAG -7';
          CALL SYMPUTX('NOBS',NOBS);
        PROC SORT NODUPKEY;
          BY &F.B010 &F.B030 VARIABLE;
        RUN;
        %reduce_TBL(TBL=SVAL_MISM_MINUS7,F=&F);
        %IF &NOBS GT 0 %THEN %DO;
                TITLE1 "ERROR : FLAG -7 NOT ALLOWED";
                TITLE2 "&CC - &YYYY / TRANSMISSION=&SS";
                PROC TABULATE;
                  CLASS &F.B010 &F.B030 LABEL SHLABEL VARIABLE FLAG / MISSING;
                  TABLE VARIABLE*SHLABEL*&F.B030="ID# (&F.B030)"*FLAG, &F.B010;
                RUN;
                DATA SVAL_MISM_MINUS7;
                LENGTH VALUE $32;
                SET  SVAL_MISM_MINUS7 (RENAME=(FLAG=VARX));
                  VALUE = COMPRESS(PUT(VARX,$10.));
                RUN;
                PROC APPEND
                  BASE=SVAL_MISS_FULL_CSV_&F&_SPLIT_MODE_
                  DATA=_LAST_
                  FORCE
                  ;
                RUN;
        %END;
%END;

%IF %SYSFUNC(EXIST(SVAL_MUST_MINUS7)) or %SYSFUNC(EXIST(SVAL_MUST_MINUS7ADHOC)) 
	or %SYSFUNC(EXIST(SVAL_MUST_MINUS7RECUR3)) or %SYSFUNC(EXIST(SVAL_MUST_MINUS7RECUR6))%THEN %DO;
		%IF %SYSFUNC(EXIST(SVAL_MUST_MINUS7)) %THEN %DO;%END;%ELSE %DO;data SVAL_MUST_MINUS7;set _NULL_;run;%END;
		%IF %SYSFUNC(EXIST(SVAL_MUST_MINUS7ADHOC)) %THEN %DO;DATA SVAL_MUST_MINUS7;SET SVAL_MUST_MINUS7 SVAL_MUST_MINUS7ADHOC;run;%END;
		%IF %SYSFUNC(EXIST(SVAL_MUST_MINUS7RECUR3)) %THEN %DO;DATA SVAL_MUST_MINUS7;SET SVAL_MUST_MINUS7 SVAL_MUST_MINUS7RECUR3;run;%END;
		%IF %SYSFUNC(EXIST(SVAL_MUST_MINUS7RECUR6)) %THEN %DO;DATA SVAL_MUST_MINUS7;SET SVAL_MUST_MINUS7 SVAL_MUST_MINUS7RECUR6;run;%END;

        %LET NOBS=0;
        DATA SVAL_MUST_MINUS7;
        SET  SVAL_MUST_MINUS7 NOBS=NOBS;
          WARNING = 'MUST FLAG -7';
          CALL SYMPUTX('NOBS',NOBS);
        PROC SORT NODUPKEY;
          BY &F.B010 &F.B030 VARIABLE;
        RUN;
        %reduce_TBL(TBL=SVAL_MUST_MINUS7,F=&F);
        %IF &NOBS GT 0 %THEN %DO;
                TITLE1 "ERROR : FLAG -7 MUST BE USED";
                TITLE2 "&CC - &YYYY / TRANSMISSION=&SS";
                PROC TABULATE;
                  CLASS &F.B010 &F.B030 LABEL SHLABEL VARIABLE FLAG / MISSING;
                  TABLE VARIABLE*SHLABEL*&F.B030="ID# (&F.B030)"*FLAG, &F.B010;
                RUN;
                DATA SVAL_MUST_MINUS7;
                LENGTH VALUE $32;
                SET  SVAL_MUST_MINUS7 (RENAME=(FLAG=VARX));
                  VALUE = COMPRESS(PUT(VARX,$10.));
                RUN;
                PROC APPEND
                  BASE=SVAL_MISS_FULL_CSV_&F&_SPLIT_MODE_
                  DATA=_LAST_
                  FORCE
                  ;
                RUN;
        %END;
%END;


/**
 * ROUTING
 */
%LET NOBS=0;
DATA SVAL_MISM_NA_2 SVAL_MISM_NA_4 SVAL_MISM_NA_5 SVAL_MISM_NA_3;
SET  SVAL_MISM_NA;
         IF NA_FLAG = -3 THEN OUTPUT SVAL_MISM_NA_3;
         ELSE IF NA_FLAG = -4 THEN OUTPUT SVAL_MISM_NA_4;
         ELSE IF NA_FLAG = -5 THEN OUTPUT SVAL_MISM_NA_5;
         ELSE IF NA_FLAG = -2 THEN OUTPUT SVAL_MISM_NA_2;
RUN;

DATA SVAL_MISM_NA_2;
SET  SVAL_MISM_NA_2 NOBS=NOBS;
  WARNING = 'ROUTING';
  CALL SYMPUTX('NOBS',NOBS);
PROC SORT NODUPKEY;
  BY &F.B010 &F.B030 VARIABLE;
RUN;
%reduce_TBL(TBL=SVAL_MISM_NA_2,F=&F);
%IF &NOBS GT 0 %THEN %DO;
        TITLE1 'ROUTING (N/A) (-2) ERROR LIST';
        PROC TABULATE;
          TITLE2 "&CC - &YYYY / TRANSMISSION=&SS";
          CLASS &F.B010 &F.B030 LABEL SHLABEL VARIABLE VALUE FLAG DISP CHECK / MISSING;
          TABLE VARIABLE*SHLABEL*&F.B030="ID# (&F.B030)"*VALUE*FLAG*DISP='CHECK VALUE'*CHECK='CHECK RULE', &F.B010;
        RUN;
        DATA SVAL_MISM_NA_2;
        SET  SVAL_MISM_NA_2;
          VALUE = VALUE;
        RUN;
        PROC APPEND
          BASE=SVAL_MISS_FULL_CSV_&F&_SPLIT_MODE_
          DATA=_LAST_
          FORCE
          ;
        RUN;
%END;

%LET NOBS=0;
DATA SVAL_MISM_NA_4;
SET  SVAL_MISM_NA_4 NOBS=NOBS;
  WARNING = 'ROUTING';
  CALL SYMPUTX('NOBS',NOBS);
PROC SORT NODUPKEY;
  BY &F.B010 &F.B030 VARIABLE;
RUN;
%reduce_TBL(TBL=SVAL_MISM_NA_4,F=&F);
%IF &NOBS GT 0 %THEN %DO;
        TITLE1 'ROUTING (N/A) (-4) ERROR LIST';
        PROC TABULATE;
          TITLE2 "&CC - &YYYY / TRANSMISSION=&SS";
          CLASS &F.B010 &F.B030 LABEL SHLABEL VARIABLE VALUE FLAG DISP CHECK / MISSING;
          TABLE VARIABLE*SHLABEL*&F.B030="ID# (&F.B030)"*VALUE*FLAG*DISP='CHECK VALUE'*CHECK='CHECK RULE', &F.B010;
        RUN;
        DATA SVAL_MISM_NA_4;
        SET  SVAL_MISM_NA_4;
          VALUE = VALUE;
        RUN;
        PROC APPEND
          BASE=SVAL_MISS_FULL_CSV_&F&_SPLIT_MODE_
          DATA=_LAST_
          FORCE
          ;
        RUN;
%END;

%LET NOBS=0;
DATA SVAL_MISM_NA_5;
SET  SVAL_MISM_NA_5 NOBS=NOBS;
  WARNING = 'ROUTING';
  CALL SYMPUTX('NOBS',NOBS);
PROC SORT NODUPKEY;
  BY &F.B010 &F.B030 VARIABLE;
RUN;
%reduce_TBL(TBL=SVAL_MISM_NA_5,F=&F);
%IF &NOBS GT 0 %THEN %DO;
        TITLE1 'ROUTING (N/A) (-5) ERROR LIST';
        PROC TABULATE;
          TITLE2 "&CC - &YYYY / TRANSMISSION=&SS";
          CLASS &F.B010 &F.B030 LABEL SHLABEL VARIABLE VALUE FLAG DISP CHECK / MISSING;
          TABLE VARIABLE*SHLABEL*&F.B030="ID# (&F.B030)"*VALUE*FLAG*DISP='CHECK VALUE'*CHECK='CHECK RULE', &F.B010;
        RUN;
        DATA SVAL_MISM_NA_5;
        SET  SVAL_MISM_NA_5;
          VALUE = VALUE;
        RUN;
        PROC APPEND
          BASE=SVAL_MISS_FULL_CSV_&F&_SPLIT_MODE_
          DATA=_LAST_
          FORCE
          ;
        RUN;
%END;

/**
 * SELECTED RESPONDENT
 * table: SVAL_MISM_3
 * [] VARIABLE $
 * [] VALUE
 * [] FLAG
 * [] DISP $
 */
%LET NOBS=0;
DATA SVAL_MISM_NA_3;
SET  SVAL_MISM_NA_3 NOBS=NOBS;
  WARNING = 'SELECTED RESP';
  CALL SYMPUTX('NOBS',NOBS);
PROC SORT NODUPKEY;
  BY &F.B010 &F.B030 VARIABLE;
RUN;
%reduce_TBL(TBL=SVAL_MISM_NA_3,F=&F);
%IF &NOBS GT 0 %THEN %DO;
        TITLE1 'SELECTED RESPONDENT (N/A) (-3) ERROR LIST';
        PROC TABULATE;
          TITLE2 "&CC - &YYYY / TRANSMISSION=&SS";
          CLASS &F.B010 &F.B030 LABEL SHLABEL VARIABLE VALUE FLAG DISP / MISSING;
          TABLE VARIABLE*SHLABEL*&F.B030="ID# (&F.B030)"*VALUE*FLAG*DISP, &F.B010;
        RUN;
        DATA SVAL_MISM_NA_3;
        SET  SVAL_MISM_NA_3;
          VALUE = VALUE;
        RUN;
        PROC APPEND
          BASE=SVAL_MISS_FULL_CSV_&F&_SPLIT_MODE_
          DATA=_LAST_
          FORCE
          ;
        RUN;
%END;

/**
 * CHECKSUM
 * table: SVAL_MISM_CHKSUM
 * [] VARIABLE $
 * [] VALUE N -> $
 * [] LBOUND
 * [] UBOUND
 */
%LET NOBS=0;
DATA SVAL_MISM_CHKSUM;
SET  SVAL_MISM_CHKSUM NOBS=NOBS;
  WARNING = 'TOTAL_DIFF_SUM_OF_COMP';
  REF = MAX( LBOUND, UBOUND );
  CALL SYMPUTX('NOBS',NOBS);
RUN;
%reduce_TBL(TBL=SVAL_MISM_CHKSUM,F=&F);
%IF &NOBS GT 0 %THEN %DO;
        TITLE1 "TOTAL SHOULD BE EQUAL OR BIGGER THAN SUM OF COMPONENTS";
        PROC TABULATE;
          TITLE2 "&CC - &YYYY / TRANSMISSION=&SS";
          CLASS &F.B010 &F.B030 LABEL SHLABEL VARIABLE VALUE DISP SUM / MISSING;
          TABLE VARIABLE*SHLABEL*&F.B030="ID# (&F.B030)"*VALUE*DISP*SUM, &F.B010;
        RUN;
        DATA SVAL_MISM_CHKSUM;
        SET  SVAL_MISM_CHKSUM (RENAME=(VALUE=VARX));
          VALUE = COMPRESS(PUT(VARX,$10.));
        RUN;
        PROC APPEND
          BASE=SVAL_MISS_FULL_CSV_&F&_SPLIT_MODE_
          DATA=_LAST_
          FORCE
          ;
        RUN;
%END;

/**
 * CHECK DETAILED COMP
 * table: SVAL_MISM_CHILDNULL
 * [] VARIABLE $
 * [] VALUE N -> $
 * [] SUM
*/
/* sometimes total can be filled but not components, so we don't put it as an error
%LET NOBS=0;
DATA SVAL_MISM_CHILDNULL;
SET  SVAL_MISM_CHILDNULL NOBS=NOBS;
  WARNING = 'DETAILED COMPONENTS NULL';
  REF = SUM;
  CALL SYMPUTX('NOBS',NOBS);
RUN;
%reduce_TBL(TBL=SVAL_MISM_CHILDNULL,F=&F);
%IF &NOBS GT 0 %THEN %DO;
        TITLE1 'DETAILED COMPONENTS NULL ALTHOUGH TOTAL NOT NULL';
        PROC TABULATE;
          TITLE2 "&CC - &YYYY / TRANSMISSION=&SS";
          CLASS &F.B010 &F.B030 LABEL SHLABEL VARIABLE VALUE DISP SUM / MISSING;
          TABLE VARIABLE*SHLABEL*&F.B030="ID# (&F.B030)"*VALUE*DISP*SUM, &F.B010;
        RUN;
        DATA SVAL_MISM_CHILDNULL;
        SET  SVAL_MISM_CHILDNULL (RENAME=(VALUE=VARX));
          VALUE = COMPRESS(PUT(VARX,$10.));
        RUN;
        PROC APPEND
          BASE=SVAL_MISS_FULL_CSV_&F&_SPLIT_MODE_
          DATA=_LAST_
          FORCE
          ;
        RUN;
%END;
 */

/**
 * GROSS
 * table: SVAL_MISM_PARENT
 * [] VARIABLE
 * [] VALUE -> $
 * [] GROSS
 */
%LET NOBS=0;
DATA SVAL_MISM_PARENT;
SET  SVAL_MISM_PARENT NOBS=NOBS;
  WARNING = 'NET-GROSS INCONSISTENCY';
  REF = GROSS;
  CALL SYMPUTX('NOBS',NOBS);
RUN;
%reduce_TBL(TBL=SVAL_MISM_PARENT,F=&F);
%IF &NOBS GT 0 %THEN %DO;
        TITLE1 "NET GREATER THAN GROSS";
        PROC TABULATE;
          TITLE2 "&CC - &YYYY / TRANSMISSION=&SS";
          CLASS &F.B010 &F.B030 LABEL SHLABEL VARIABLE VALUE DISP GROSS / MISSING;
          TABLE VARIABLE*SHLABEL*&F.B030="ID# (&F.B030)"*VALUE="NET"*GROSS/**DISP*/, &F.B010;
        RUN;
        DATA SVAL_MISM_PARENT;
        SET  SVAL_MISM_PARENT (RENAME=(VALUE=VARX));
          VALUE = COMPRESS(PUT(VARX,$10.));
        RUN;
        PROC APPEND
          BASE=SVAL_MISS_FULL_CSV_&F&_SPLIT_MODE_
          DATA=_LAST_
          FORCE
          ;
        RUN;
%END;

/**
 * HH GRID
 */
%LET _doGRID_=0;
PROC SQL NOPRINT;
     SELECT MIN(COUNT(*),1) INTO :_doGRID_
     FROM   SVAL_&F
     WHERE  TYPE = 'HGRID'
     %IF "&_SPLIT_MODE_" eq "_ANTE_" %THEN %DO;
         AND START_V le &_SPLIT_CUT_
     %END;
     ;
QUIT;

%PUT *I* doGRID: &_doGRID_
;
%IF &_doGRID_ EQ 1 AND &F EQ R %THEN %DO;

   %IF %SYSFUNC(EXIST(GRID_MISM_SIZ)) %THEN %DO;
        %LET NOBS=0;
        DATA GRID_MISM_SIZ;
        SET  GRID_MISM_SIZ NOBS=NOBS;
          WARNING = 'HHGRID SIZE';
          CALL SYMPUTX('NOBS',NOBS);
        RUN;
        %IF &NOBS GT 0 %THEN %DO;
                TITLE1 'HOUSEHOLD-GRID SIZE ERROR';
                TITLE2 "&CC - &YYYY / TRANSMISSION=&SS";
                PROC TABULATE;
                   VAR DF_SIZE DOC65_SIZE;
                   TABLE DF_SIZE='DATAFILE SIZE'*F=3. DOC65_SIZE='DOC65 SIZE'*F=3.;
                   KEYLABEL SUM = ' ';
                RUN;
        %END;
  %END;


  %IF %SYSFUNC(EXIST(GRID_MISM_SEQ)) %THEN %DO;
        %LET NOBS=0;
        DATA GRID_MISM_SEQ;
        SET  GRID_MISM_SEQ NOBS=NOBS;
          WARNING = 'HHGRID SEQUENCE';
          CALL SYMPUTX('NOBS',NOBS);
        RUN;
        %IF &NOBS GT 0 %THEN %DO;
                TITLE1 'HOUSEHOLD-GRID SEQUENCE ERRORS';
                TITLE2 "&CC - &YYYY / TRANSMISSION=&SS";

				%reduce_TBL(TBL=GRID_MISM_SEQ,COND=(WHERE=(MSG="VALUE NULL")),F=&F);
				DATA GRID_MISM_SEQ_2REP2;SET GRID_MISM_SEQ_2REP;RUN;
				%reduce_TBL(TBL=GRID_MISM_SEQ,COND=(WHERE=(MSG="VALUE OUT OF RANGE")),F=&F);
				DATA GRID_MISM_SEQ_2REP2;SET GRID_MISM_SEQ_2REP2 GRID_MISM_SEQ_2REP;RUN;
				%reduce_TBL(TBL=GRID_MISM_SEQ,COND=(WHERE=(MSG="VALUE <> 1 FOR 1st ROW")),F=&F);
				DATA GRID_MISM_SEQ_2REP2;SET GRID_MISM_SEQ_2REP2 GRID_MISM_SEQ_2REP;RUN;
				%reduce_TBL(TBL=GRID_MISM_SEQ,COND=(WHERE=(MSG="VALUES NOT SEQUENTIAL")),F=&F);
				DATA GRID_MISM_SEQ_2REP2;SET GRID_MISM_SEQ_2REP2 GRID_MISM_SEQ_2REP;IF MSG NE "VALUES NOT SEQUENTIAL" THEN ROW_PREV=.;RUN;

				PROC TABULATE;
		          CLASS RB010 RB040 RB030 VARIABLE ROW ROW_PREV EXP_ROW MSG / MISSING;
		          TABLE VARIABLE*MSG="ERROR"*RB040*RB030="ID# (RB030)"*ROW_PREV="PREVIOUS ROW"*ROW="ROW (VALUE)"*EXP_ROW="ROW (EXPECTED)", RB010;
		        RUN;

                PROC APPEND
                  BASE=SVAL_MISS_FULL_CSV_&F&_SPLIT_MODE_
                  DATA=GRID_MISM_SEQ
                  FORCE
                  ;
                RUN;
        %END;
  %END;

  %IF %SYSFUNC(EXIST(GRID_MISM_V)) %THEN %DO;
        %LET NOBS=0;
        DATA GRID_MISM_V;
        SET  GRID_MISM_V NOBS=NOBS;
          WARNING = 'HHGRID VALUES';
          CALL SYMPUTX('NOBS',NOBS);
        RUN;
        %IF &NOBS GT 0 %THEN %DO;
                TITLE1 'HOUSEHOLD-GRID VALUE ERRORS';
                TITLE2 "&CC - &YYYY / TRANSMISSION=&SS";
				%reduce_TBL(TBL=GRID_MISM_V,F=R);
		        PROC TABULATE;
		          CLASS RB010 RB040 RB030 COL ROW VALUE MSG / MISSING;
		          TABLE COL="RG_"*RB040*RB030="ID# (RB030)"*ROW*VALUE*MSG="ALLOWED VALUES", RB010;
		        RUN;
                PROC APPEND
                  BASE=SVAL_MISS_FULL_CSV_&F&_SPLIT_MODE_
                  DATA=GRID_MISM_V
                  FORCE
                  ;
                RUN;
        %END;
  %END;

  %IF %SYSFUNC(EXIST(GRID_MISM_MATRIX)) %THEN %DO;
        %LET NOBS=0;
        DATA GRID_MISM_MATRIX;
        SET  GRID_MISM_MATRIX NOBS=NOBS;
          WARNING = 'HHGRID FLAGS';
          CALL SYMPUTX('NOBS',NOBS);
        RUN;
        %IF &NOBS GT 0 %THEN %DO;
                TITLE1 'HOUSEHOLD-GRID FLAG ERRORS';
                TITLE2 "&CC - &YYYY / TRANSMISSION=&SS";

				%reduce_TBL(TBL=GRID_MISM_MATRIX,MAXREC=1,COND=(WHERE=(MSG="DIAGONAL")),F=&F);
				DATA GRID_MISM_MATRIX_2REP2;SET GRID_MISM_MATRIX_2REP;RUN;
				%reduce_TBL(TBL=GRID_MISM_MATRIX,MAXREC=1,COND=(WHERE=(MSG="UPPER TRIANGLE")),F=&F);
				DATA GRID_MISM_MATRIX_2REP2;SET GRID_MISM_MATRIX_2REP2 GRID_MISM_MATRIX_2REP;RUN;
				%reduce_TBL(TBL=GRID_MISM_MATRIX,MAXREC=3,COND=(WHERE=(MSG="LOWER TRIANGLE")),F=&F);
				DATA GRID_MISM_MATRIX_2REP2;SET GRID_MISM_MATRIX_2REP2 GRID_MISM_MATRIX_2REP;RUN;
				%reduce_TBL(TBL=GRID_MISM_MATRIX,MAXREC=1,COND=(WHERE=(MSG="OUTSIDE GRID")),F=&F);
				DATA GRID_MISM_MATRIX_2REP2;SET GRID_MISM_MATRIX_2REP2 GRID_MISM_MATRIX_2REP;RUN;

				PROC TABULATE;
		          CLASS RB010 RB040 RB030 COL ROW FLAG EXP_FLAG MSG / MISSING;
		          TABLE MSG="GRID LOCATION"*COL="RG_"*RB040*RB030="ID# (RB030)"*ROW*FLAG="FLAG (VALUE)"*EXP_FLAG="FLAG (EXPECTED)", RB010;
		        RUN;

                PROC APPEND
                  BASE=SVAL_MISS_FULL_CSV_&F&_SPLIT_MODE_
                  DATA=GRID_MISM_MATRIX
                  FORCE
                  ;
                RUN;
        %END;
  %END;

  %IF %SYSFUNC(EXIST(GRID_MISM_AGE)) %THEN %DO;
        %LET NOBS=0;
        DATA GRID_MISM_AGE;
        SET  GRID_MISM_AGE NOBS=NOBS;
          WARNING = 'HHGRID AGE';
          CALL SYMPUTX('NOBS',NOBS);
        RUN;
        %IF &NOBS GT 0 %THEN %DO;
                TITLE1 'HOUSEHOLD-GRID AGE ERRORS';
                TITLE2 "&CC - &YYYY / TRANSMISSION=&SS";

				%reduce_TBL(TBL=GRID_MISM_AGE,MAXREC=1,COND=(WHERE=(MSG="PARENT YOUNGER THAN CHILD")),F=&F);
				DATA GRID_MISM_AGE_2REP2;SET GRID_MISM_AGE_2REP;RUN;
				%reduce_TBL(TBL=GRID_MISM_AGE,MAXREC=1,COND=(WHERE=(MSG="CHILD OLDER THAN PARENT")),F=&F);
				DATA GRID_MISM_AGE_2REP2;SET GRID_MISM_AGE_2REP2 GRID_MISM_AGE_2REP;RUN;
				%reduce_TBL(TBL=GRID_MISM_AGE,MAXREC=1,COND=(WHERE=(MSG="CHILD OLDER THAN GRAND-PARENT")),F=&F);
				DATA GRID_MISM_AGE_2REP2;SET GRID_MISM_AGE_2REP2 GRID_MISM_AGE_2REP;RUN;

				PROC TABULATE DATA= GRID_MISM_AGE_2REP2;
		          CLASS RB010 RB040 RB030 COL ROW AGE_ROW AGE_CHILD AGE_PARENT MSG / MISSING;
		          TABLE MSG="ERROR"*COL="RG_"*RB040*RB030="ID# (RB030)"*ROW*AGE_CHILD="CHILD's AGE"*AGE_PARENT="(GRAND-)PARENT's AGE", RB010;
		        RUN;
                PROC APPEND
                  BASE=SVAL_MISS_FULL_CSV_&F&_SPLIT_MODE_
                  DATA=GRID_MISM_AGE
                  FORCE
                  ;
                RUN;
        %END;
  %END;

  %IF %SYSFUNC(EXIST(GRID_MISM_RED)) %THEN %DO;
        %LET NOBS=0;
        DATA GRID_MISM_RED;
        SET  GRID_MISM_RED NOBS=NOBS;
          WARNING = 'HHGRID REDUN.';
          CALL SYMPUTX('NOBS',NOBS);
        RUN;
        %IF &NOBS GT 0 %THEN %DO;
                TITLE1 'HOUSEHOLD-GRID REDUNDANT VARIABLES ERRORS';
                TITLE2 "&CC - &YYYY / TRANSMISSION=&SS";

				%reduce_TBL(TBL=GRID_MISM_RED,COND=(WHERE=(VARIABLE="RB220")),F=&F);
				DATA GRID_MISM_RED_2REP2;SET GRID_MISM_RED_2REP;RUN;
				%reduce_TBL(TBL=GRID_MISM_RED,COND=(WHERE=(VARIABLE="RB230")),F=&F);
				DATA GRID_MISM_RED_2REP2;SET GRID_MISM_RED_2REP2 GRID_MISM_RED_2REP;RUN;
				%reduce_TBL(TBL=GRID_MISM_RED,COND=(WHERE=(VARIABLE="RB240")),F=&F);
				DATA GRID_MISM_RED_2REP2;SET GRID_MISM_RED_2REP2 GRID_MISM_RED_2REP;RUN;

				PROC TABULATE;
		          CLASS RB010 RB040 RB030 VARIABLE ROW ROLE MSG PARENT_ID_GRID PARENT_ID_FILE / MISSING;
		          TABLE VARIABLE*MSG="ERROR"*RB040*RB030="ID# (RB030)"*ROW*PARENT_ID_FILE="R-FILE (RB2x0)"*PARENT_ID_GRID="HH-GRID", RB010;
		        RUN;

				PROC APPEND
                  BASE=SVAL_MISS_FULL_CSV_&F&_SPLIT_MODE_
                  DATA=GRID_MISM_RED
                  FORCE
                  ;
                RUN;
        %END;
  %END;

%END; /* XML CHECK */


%IF %SYSFUNC(EXIST(SVAL_NON_MEMB)) AND %SYSFUNC(EXIST(SVAL_HH_MBR_STAT_LST)) AND &F EQ R %THEN %DO;
        %LET LX=;
        %LET HH_MEMBERSHIP_STATUS_FILTER=;
        DATA _NULL_;
        SET  SVAL_HH_MBR_STAT_LST;
          CALL SYMPUT('HH_MEMBERSHIP_STATUS_FILTER',TRIM(LX)||' IN '||TRIM(RX));
          CALL SYMPUTX('LX',UPCASE(LX));
        RUN;

        %LET NOBS=0;
        DATA SVAL_NON_MEMB;
        SET  SVAL_NON_MEMB NOBS=NOBS;
          WARNING = 'NON-HH-MEMBER';
          CALL SYMPUTX('NOBS',NOBS);
        RUN;

        PROC APPEND
          BASE=SVAL_MISS_FULL_CSV_&F&_SPLIT_MODE_
          DATA=SVAL_NON_MEMB
          FORCE
          ;
        RUN;

        PROC SORT DATA=SVAL_NON_MEMB;
          BY RB030 RB110;
        RUN;
        DATA SVAL_NON_MEMB;
        SET  SVAL_NON_MEMB;
          BY RB030;
          RETAIN RB110_1ST;
          IF FIRST.RB030 THEN RB110_1ST=RB110;
          IF RB110 eq RB110_1ST THEN OUTPUT;
        RUN;

        %reduce_TBL(TBL=SVAL_NON_MEMB,F=&F,MAXREC=3);

        %IF &NOBS GT 0 %THEN %DO;
                TITLE1 "NON HOUSEHOLD MEMBER CHECK: NOT(&HH_MEMBERSHIP_STATUS_FILTER)";
                TITLE2 "&CC - &YYYY / TRANSMISSION=&SS";
                TITLE3 "LISTED VARIABLES SHOULD REMAIN EMPTY";
                PROC TABULATE;
                  CLASS &F.B010 &F.B030 &LX VARIABLE VALUE FLAG / MISSING;
                  TABLE &F.B030="ID# (&F.B030)"*&LX*VARIABLE*VALUE*FLAG, &F.B010;
                RUN;
                TITLE3;
        %END;

        %LET NOBS=0;
        DATA SVAL_NON_MEMB_MISS;
        SET  SVAL_NON_MEMB_MISS NOBS=NOBS;
          WARNING = 'NON-HH-MEMBER MISS';
          CALL SYMPUTX('NOBS',NOBS);
        RUN;

        PROC APPEND
          BASE=SVAL_MISS_FULL_CSV_&F&_SPLIT_MODE_
          DATA=SVAL_NON_MEMB_MISS
          FORCE
          ;
        RUN;

        PROC SORT DATA=SVAL_NON_MEMB_MISS;
          BY RB030 RB110;
        RUN;
        DATA SVAL_NON_MEMB_MISS;
        SET  SVAL_NON_MEMB_MISS;
          BY RB030;
          RETAIN RB110_1ST;
          IF FIRST.RB030 THEN RB110_1ST=RB110;
          IF RB110 eq RB110_1ST THEN OUTPUT;
        RUN;

        %reduce_TBL(TBL=SVAL_NON_MEMB_MISS,F=&F,MAXREC=3);

        %IF &NOBS GT 0 %THEN %DO;
                TITLE1 "NON HOUSEHOLD MEMBER CHECK: NOT(&HH_MEMBERSHIP_STATUS_FILTER)";
                TITLE2 "&CC - &YYYY / TRANSMISSION=&SS";
                TITLE3 "LISTED VARIABLES SHOULD BE FILLED";
                PROC TABULATE;
                  CLASS &F.B010 &F.B030 &LX VARIABLE FLAG VALUE / MISSING;
                  TABLE &F.B030="ID# (&F.B030)"*&LX*VARIABLE*VALUE*FLAG, &F.B010;
                RUN;
                TITLE3;
        %END;


%END;



DATA HEADER;
  LENGTH MSG T $125;
  LABEL MSG = 'VALIDATION STATUS';
  T = PUT(DATETIME(),DATETIME20.);
  MSG = "&F FILE (VER:&FVER) VALIDATION ACCOMPLISHED ON " || COMPRESS(T);
RUN;

TITLE1 "STRUCTURAL CHECKS &F.-FILE";
PROC PRINT DATA=HEADER LABEL NOOBS
  style(header)={just=c}
  ;
  VAR MSG;
RUN;

ODS &OUTPUTFORMAT CLOSE;

%IF &CSV=YES %THEN %DO;
PROC EXPORT DATA=SVAL_MISS_FULL_CSV_&F&_SPLIT_MODE_
dbms=csv
FILE="&OUT&_dirsp_%quote(&CC&YY.-Struct%LOWCASE(&_SPLIT_MODE_)&y1&y2.-&F.file.csv)" REPLACE;
RUN;
%END;

%MEND rep_SVAL;

%MACRO reduce_TBL(TBL=,F=,MAXREC=3, COND=);

PROC SORT DATA=&TBL &COND OUT=&TBL._2REP;
  BY VARIABLE DESCENDING &F.B010;
DATA _LAST_ (DROP=C);
SET;
  BY VARIABLE;
  RETAIN C 1;
  YR = &F.B010;
  LGYR = LAG(&F.B010);
  IF FIRST.VARIABLE THEN DO;
         C = 1;
         LGYR = 0;
  END;
  ELSE DO;
          IF YR = LGYR AND LGYR ^= . THEN DO;
                 C + 1;
          END;
          ELSE C = 1;
  END;
  IF C <= &MAXREC;
RUN;

DATA _LAST_;
SET  _LAST_;
     LENGTH LABEL $150;
     LENGTH SHLABEL_1 SHLABEL_2 SHLABEL $150;
     LABEL SHLABEL = 'LABEL';
     LABEL = PUT(VARIABLE,$VARLABEL.);
     _I_ = INDEX(LABEL,'-');
     SHLABEL_1 = UPCASE(SUBSTR(LABEL,_I_+2,1));
     SHLABEL_2 = LOWCASE(SUBSTR(LABEL,_I_+3));
     SHLABEL = STRIP(SHLABEL_1)||SHLABEL_2;
     LABEL = STRIP(VARIABLE) || ' - ' || SHLABEL;
     DROP SHLABEL_: _I_;
RUN;

%MEND reduce_TBL;


/**
 * RK MODULE SPECIFICITIES (NON-HOUSEHOLD MEMBERS)
 */
%MACRO purge_NON_HH_MEMBER (F=);

%LOCAL LX LX_PFX;

%LET DS=RAW.&_SPLIT_MODE_&ss&cc&YY&F;

%IF %SYSFUNC(EXIST(SVAL_NON_MEMB)) %THEN %DO;

    PROC SQL;
      DROP TABLE SVAL_NON_MEMB;
    QUIT;

%END;

%LET HH_MEMBERSHIP_STATUS_FILTER=;
%LET LX=;
%LET LX_PFX=;
DATA _NULL_;
SET  SVAL_HH_MBR_STAT_LST;
     CALL SYMPUT('HH_MEMBERSHIP_STATUS_FILTER',TRIM(LX)||' IN '||TRIM(RX));
     CALL SYMPUTX('LX',UPCASE(LX));
     CALL SYMPUTX('LX_PFX',UPCASE(SUBSTR(LX,1,1)));
RUN;

%PUT &LX &HH_MEMBERSHIP_STATUS_FILTER;


%IF &LX_PFX ne %THEN %DO;

        %PUT *I* LX: &LX | LX(1,1): &LX_PFX;

        %IF &F EQ &LX_PFX %THEN %DO;

          DATA NON_MEMB_LST (KEEP=&LX_PFX.B010 &LX_PFX.B030 &LX);
          SET  &DS;
            IF NOT( &HH_MEMBERSHIP_STATUS_FILTER );
          PROC SORT NODUPKEY;
            BY &LX_PFX.B010 &LX_PFX.B030;
          RUN;

        %END;

        %sub_purge_NON_HH_MEMBER (Sval_mism_v);
        %sub_purge_NON_HH_MEMBER (Sval_mism_f);
        %sub_purge_NON_HH_MEMBER (Sval_mism_vf);

        %sub_purge_NON_HH_MEMBER (Sval_mism_optional);
        %sub_purge_NON_HH_MEMBER (Sval_mism_na);
        %sub_purge_NON_HH_MEMBER (Sval_mism_parent);
        %sub_purge_NON_HH_MEMBER (Sval_mism_childnull);
        %sub_purge_NON_HH_MEMBER (Sval_mism_chksum);
        %sub_purge_NON_HH_MEMBER (Sval_miss);
        %sub_purge_NON_HH_MEMBER (Grid_mism_seq);
        %sub_purge_NON_HH_MEMBER (Grid_mism_matrix);
        %sub_purge_NON_HH_MEMBER (Grid_mism_age);
        %sub_purge_NON_HH_MEMBER (Grid_mism_red);

        %PUT *I* SVAL_MISM_MINUS7 IS NOT SUBJECT TO THE NON-HH PURGING
        ;

        %sub_compl_NON_HH_MEMBER;

%END;
%ELSE %DO;
  %PUT *I* UNDEFINED NON_HH_MEMBER POLICY
  ;
%END;

%MEND purge_NON_HH_MEMBER;

%MACRO sub_purge_NON_HH_MEMBER (TBL);

%LOCAL K LX_ITEM ID_LST;

%IF %SYSFUNC(EXIST(&TBL)) %THEN %DO;
    PROC SORT DATA=&TBL;
      BY &LX_PFX.B010 &LX_PFX.B030;
    RUN;
        DATA
          &TBL (DROP=&LX)
          &TBL._NON_MEMB
        ;
        MERGE &TBL (in=A) NON_MEMB_LST (in=B);
          BY &LX_PFX.B010 &LX_PFX.B030;
          IF A AND B THEN OUTPUT &TBL._NON_MEMB;
          ELSE IF A THEN OUTPUT &TBL;
        RUN;

        PROC SQL NOPRINT;
          SELECT DISTINCT &LX INTO :LX_LST SEPARATED BY ' '
          FROM   SVAL_HH_MEMBER_MANDATORY
          ;
        QUIT;
        %LET K=1;
        %LET LX_ITEM=%SCAN(&LX_LST,&K);
        %DO %WHILE(&LX_ITEM ne);
                PROC SQL NOPRINT;
                  SELECT DISTINCT "'"||TRIM(ID)||"'" INTO :ID_LST SEPARATED BY ' '
                  FROM   SVAL_HH_MEMBER_MANDATORY
                  WHERE  &LX = &LX_ITEM
                  ;
                QUIT;

                %PUT *I* XML_SVAL:HH_NON_MEMBER STATUS[&LX]:&LX_ITEM;
                %PUT *I* &ID_LST;

                DATA ADD_&LX_ITEM;
                SET  &TBL._NON_MEMB;
                        IF &LX eq &LX_ITEM AND VARIABLE IN ( &ID_LST );
                RUN;

                PROC APPEND FORCE BASE=&TBL DATA=ADD_&LX_ITEM;
                RUN;
                %LET K=%EVAL(&K+1);
                %LET LX_ITEM=%SCAN(&LX_LST,&K);
        %END;
%END;

%MEND sub_purge_NON_HH_MEMBER;

%MACRO sub_compl_NON_HH_MEMBER;

/*%LOCAL;*/%GLOBAL K LX_ITEM ID ID_LST ID_LST2 VAR VARS;

        DATA NON_MEMB_LST (KEEP=&LX_PFX.B010 &LX_PFX.B030 &LX);
        SET  &DS;
          IF NOT( &HH_MEMBERSHIP_STATUS_FILTER );
        PROC SORT NODUPKEY;
            BY &LX_PFX.B010 &LX_PFX.B030 &LX;
        RUN;

        PROC SORT DATA=&DS OUT=R;
          BY &LX_PFX.B010 &LX_PFX.B030 &LX;
        RUN;

        DATA
            R_NON_MEMB
            DUMP
        ;
        MERGE R (in=A) NON_MEMB_LST (in=B);
          BY &LX_PFX.B010 &LX_PFX.B030 &LX;
          IF A AND B THEN OUTPUT R_NON_MEMB;
          ELSE IF B THEN OUTPUT DUMP;
        RUN;

        PROC SQL NOPRINT;
          SELECT DISTINCT &LX INTO :LX_LST SEPARATED BY ' '
          FROM   SVAL_HH_MEMBER_MANDATORY
          WHERE  CHECKNULL = 'Y'
          ;
        QUIT;
		RUN;
		%PUT *I* LX_LST:  &LX_LST;

        %LET K=1;
        %LET LX_ITEM=%SCAN(&LX_LST,&K," ");
        %DO %WHILE(&LX_ITEM ne);
                PROC SQL NOPRINT;
                  SELECT DISTINCT "'"||TRIM(ID)||"'" INTO :ID_LST SEPARATED BY ' '
                  FROM   SVAL_HH_MEMBER_MANDATORY
                  WHERE  &LX = &LX_ITEM
                  ;
                QUIT;
                PROC SQL NOPRINT;
                  SELECT DISTINCT TRIM(ID) INTO :ID_LST2 SEPARATED BY ' '
                  FROM   SVAL_HH_MEMBER_MANDATORY
                  WHERE  &LX = &LX_ITEM and ID NOT IN ("RB010","RB020","RB030")
                  ;
                QUIT;

                %LET ID_LST=&ID_LST "&LX._F";


                PROC CONTENTS DATA=R_NON_MEMB OUT=MEMBERS NOPRINT;
                RUN;

                PROC SQL;
                  DELETE FROM MEMBERS T
                  WHERE  NAME NOT IN
                  ( SELECT VARIABLE FROM SVAL_&F WHERE FLAG_NAME IS NOT NULL )
                  ;
                QUIT;

                PROC SQL;
                  DELETE FROM MEMBERS T
                  WHERE  NAME IN ( &ID_LST )
                  ;
                QUIT;

                PROC SQL NOPRINT;
                     SELECT strip(NAME) || ':' || put(TYPE,Z1.) INTO :VARS SEPARATED BY ' '
                     FROM   MEMBERS
                     ;
                QUIT;

                DATA SVAL_NON_MEMB_&K (KEEP=&F.B010 &F.B030 &LX VARIABLE VALUE FLAG WHERE=(FLAG ne -7));
                SET  R_NON_MEMB (WHERE=(&LX=&LX_ITEM));
                     LENGTH VARIABLE $12;
                     LENGTH VALUE $32;
                     LENGTH FLAG 8;
                     %LET J=1;
                     %LET VAR=%SCAN(&VARS,&J);
                     %DO  %WHILE(&VAR ne);
                          %LET VARX=%SCAN(&VAR,1,:);
                          %LET TYP=%SCAN(&VAR,2,:);
                          IF NOT ( ( &VARX = . ) AND ( &VARX._F = . ) ) THEN DO;
                             VARIABLE = "&VARX";
                             %IF &TYP EQ 1 %THEN %DO;
                             VALUE = STRIP(put(&VARX,$10.));
                             %END;
                             %ELSE %IF &TYP EQ 2 %THEN %DO;
                             VALUE = &VARX;
                             %END;
                             FLAG  = &VARX._F;
                             OUTPUT;
                          END;
                          %LET J=%EVAL(&J+1);
                          %LET VAR=%SCAN(&VARS,&J);
                     %END;
                RUN;

                DATA SVAL_NON_MEMB_MISS_&K (KEEP=&F.B010 &F.B030 &LX VARIABLE VALUE FLAG WHERE=(FLAG ne -7));
                SET  R_NON_MEMB (WHERE=(&LX=&LX_ITEM));
                     LENGTH VARIABLE $12;
                     LENGTH VALUE $32;
                     LENGTH FLAG 8;
                     %LET J=1;
                     %LET ID=%SCAN(&ID_LST2,&J);
                     %DO  %WHILE(&ID ne);
                          IF ( &ID = . and &ID._F=. ) THEN DO;
                             VARIABLE = "&ID";
                             VALUE = &ID;
                             FLAG  = &ID._F;
                             OUTPUT;
                          END;
                          %LET J=%EVAL(&J+1);
                          %LET ID=%SCAN(&ID_LST2,&J);
                     %END;
                RUN;


                %IF &K eq 1 %THEN %DO;
                    DATA SVAL_NON_MEMB;
                    SET  SVAL_NON_MEMB_&K;
                    RUN;
                    DATA SVAL_NON_MEMB_MISS;
                    SET  SVAL_NON_MEMB_MISS_&K;
                    RUN;
                %END;
                %ELSE %DO;
                    PROC APPEND BASE=SVAL_NON_MEMB
                         DATA=SVAL_NON_MEMB_&K;
                    RUN;
                    PROC APPEND BASE=SVAL_NON_MEMB_MISS
                         DATA=SVAL_NON_MEMB_MISS_&K;
                    RUN;
                %END;

                %LET K=%EVAL(&K+1);
                %LET LX_ITEM=%SCAN(&LX_LST,&K," ");

       %END;

%MEND sub_compl_NON_HH_MEMBER;

