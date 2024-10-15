/**
 * Revision History
 * - October 21st 2022
 */
%MACRO check_GRID (F=);

/**
 * HH GRID XML PARAMETERS
 * -------------------------------------------------------------
 */

%LET PERS_ID=RB030;
%LET FATHER_ID=RB220;
%LET MOTHER_ID=RB230;
%LET SPOUSE_ID=RB240;
%LET GENDER_ID=RB090;
%LET AGE_ID=RB082;
%LET HH_ID=RB040;
%LET GRID_KEY=RB032;

/* ------------------------------------------------------------- */

%check_str_GRID(F=&F);
%check_vars_GRID(F=&F);
%check_age_GRID(F=&F);
%check_red_GRID(F=&F);
%check_aggr_GRID(F=&F);

%MEND check_GRID;

%MACRO detect_max_k(DS=);

%LOCAL DF_SIZ XML_SIZ;

PROC SQL NOPRINT;
  SELECT MIN( VARIABLE ) INTO :HGRID_PFX
  FROM   SVAL
  WHERE  TYPE = 'HGRID'
  ;
QUIT;

DATA _NULL_;
  C = INDEX("&HGRID_PFX",'1');
  PFX = SUBSTR("&HGRID_PFX",1,C-1);
  CALL SYMPUTX('RG',PFX);
  CALL SYMPUTX('RG_LEN',LENGTH(PFX));
RUN;

%PUT &RG (&RG_LEN);

PROC CONTENTS DATA=&DS OUT=MEMBERS NOPRINT;
RUN;

DATA MAX_K (KEEP=COL);
SET;
  COL = 0;
  NAME = UPCASE(NAME);
  IF SUBSTR(NAME,1,&RG_LEN) = "&RG" AND SCAN(NAME,3,'_') ^= 'F' THEN DO;
     IF SUBSTR(NAME,&RG_LEN+1,1) IN ( '0' '1' '2' '3' '4' '5' '6' '7' '8' '9' ) THEN DO;
        COL = INPUT( SUBSTR(NAME,&RG_LEN+1), BEST. );
     END;
  END;
  IF COL > 0;
PROC SORT;
  BY DESCENDING COL;
DATA MAX_K;
SET  MAX_K (OBS=1);
     CALL SYMPUTX('DF_SIZ',COL);
RUN;

%PUT *I* HHGRID DATAFILE MAX K: &DF_SIZ;

DATA HHGRID_MEMBERS (KEEP=NAME PFX SFX_STR SFX);
SET  SVAL;
     WHERE TYPE = 'HGRID';
     PFX = "&RG";
     IF SUBSTR(VARIABLE,1,&RG_LEN) = "&RG" THEN DO;
        SFX_STR = SUBSTR(VARIABLE,&RG_LEN+1,2);
        SFX = INPUT(SFX_STR,BEST.);
        OUTPUT;
     END;
PROC SORT;
  BY DESCENDING SFX;
DATA _NULL_;
SET;
  IF _N_ = 1 THEN CALL SYMPUTX('XML_SIZ',SFX);
RUN;

%PUT *I* HHGRID XML MAX K: &XML_SIZ;

DATA GRID_MISM_SIZ;
  DF_SIZE = &DF_SIZ;
  DOC65_SIZE = &XML_SIZ;
  IF DF_SIZE lt DOC65_SIZE THEN OUTPUT;
RUN;

%MEND  detect_max_k;

/**
 * STRUCTURAL CHECKS
 */
%MACRO check_str_GRID(DS=,F=);

%LOCAL I N_YEARS YEAR_LST YEAR;
%LOCAL GRID GRID_FLAG;

%IF &DS EQ %THEN %DO;
    %LET DS=RAW.&_SPLIT_MODE_&ss&cc&YY&F;
%END;

PROC SQL NOPRINT;
  SELECT DISTINCT RB010 INTO :YEAR_LST SEPARATED BY ' '
  FROM   &DS
  ;
QUIT;

%LET RG=;
%detect_max_k (DS=&DS);
DATA _NULL_;
SET  MAX_K;
   CALL SYMPUT('GRID_SIZE',TRIM(COL));
RUN;

%PUT *I* GRID SIZE: &GRID_SIZE (&RG)
;
%LET K=1;
%LET YEAR=%SCAN( &YEAR_LST, &K );
%DO %WHILE( &YEAR NE );
    PROC SORT DATA=&DS (WHERE=(RB010=&YEAR))
      OUT=DS (KEEP=RB010 RB110 &PERS_ID &FATHER_ID &MOTHER_ID &SPOUSE_ID &GENDER_ID &AGE_ID &HH_ID &GRID_KEY &RG: );
      BY &HH_ID &GRID_KEY;
    RUN;

    ;
    %LET HH_MBR_BOOL=0;
    PROC SQL NOPRINT;
      SELECT COUNT(*) INTO :HH_MBR_BOOL
      FROM   SVAL_HH_MBR_STAT_LST
  ;
    QUIT;

    %IF &HH_MBR_BOOL gt 0 %THEN %DO;
    DATA _NULL_;
    SET  SVAL_HH_MBR_STAT_LST;
         CALL SYMPUT('HH_MEMBERSHIP_STATUS_FILTER',TRIM(LX)||' IN '||TRIM(RX));
    RUN;

    DATA DS;
    SET  DS;
     IF &HH_MEMBERSHIP_STATUS_FILTER;
    RUN;
    %END;
    %ELSE %DO;
    DATA DS;
    SET  DS;
             IF RB110 lt 5;
    RUN;
    %END;

    PROC FREQ DATA=DS NOPRINT;
      TABLE &HH_ID / OUT=A1_HH_LST;
    DATA _LAST_ (WHERE=(COUNT=1));
      SET;
    RUN;

    DATA DS;
    MERGE DS A1_HH_LST (in=B);
      BY &HH_ID;
      IF NOT B;
    RUN;

	DATA GRID_MISM_SEQ_&YEAR (KEEP=WARN_SEQ VARIABLE RB010 RB030 &HH_ID ROW EXP_ROW MSG ROW_PREV);
	SET  DS;
		LENGTH VARIABLE $8;
		LENGTH MSG $60;
		BY &HH_ID &GRID_KEY;
		ROW = &GRID_KEY;
		MSG = '';
		WARN = 0;
		WARN_SEQ = 0;
		ROW_PREV=LAG(&GRID_KEY);

		/* NULL AND OUT OF RANGE */
		IF ROW = . THEN DO;
		     WARN=1;
		     WARN_SEQ = 1;
		     VARIABLE = "&GRID_KEY";
		      MSG = 'VALUE NULL';
		END;
		ELSE IF ROW < 1 OR ROW > &GRID_SIZE THEN DO;
		     WARN=1;
		     WARN_SEQ = 1;
		     VARIABLE = "&GRID_KEY";
			 IF ROW<1 THEN EXP_ROW=1;ELSE EXP_ROW=&GRID_SIZE;
		      MSG = 'VALUE OUT OF RANGE';
		END;
		/* SMALLEST SEQUENCE = 1 */
		IF FIRST.&HH_ID THEN DO;
 		     IF ROW ^= 1 AND WARN=0 THEN DO;
				WARN=1;
				VARIABLE = "&GRID_KEY";
				EXP_ROW=1;
				WARN_SEQ = 1;
				MSG = "VALUE <> 1 FOR 1st ROW";
		     END;
		END;
		/* SEQUENTIAL ROWS */
		IF NOT FIRST.&HH_ID THEN DO;
			IF ROW ^= ROW_PREV+1 AND ROW_PREV NE . AND WARN=0  THEN DO;
			WARN=1;
			VARIABLE = "&GRID_KEY";
			EXP_ROW=ROW_PREV+1;
			WARN_SEQ = 1;
			MSG = "VALUES NOT SEQUENTIAL";
			END;
		END;
		IF WARN = 1;
	RUN;

    PROC MEANS NWAY NOPRINT;
      CLASS &HH_ID;
      OUTPUT OUT=WARN_MNS_&YEAR MAX(WARN_SEQ)=WARN_SEQ;
    RUN;
    DATA DS;
    MERGE DS WARN_MNS_&YEAR;
      BY &HH_ID;
      IF WARN_SEQ <= 0;
    RUN;


    DATA _NULL_;
    SET  MAX_K;
       LENGTH GRID GRID_FLAG $1024;
       GRID = ''; GRID_FLAG = '';
   DO I = 1 TO COL;
              GRID      = TRIM(GRID) || ' ' || COMPRESS("&RG"||PUT(I,BEST.));
              GRID_FLAG = TRIM(GRID_FLAG) || ' ' || COMPRESS("&RG"||PUT(I,BEST.)||'_F');
       END;
       CALL SYMPUT('GRID_SIZE',TRIM(COL));
       CALL SYMPUT('GRID'     ,TRIM(GRID));
       CALL SYMPUT('GRID_FLAG',TRIM(GRID_FLAG));
    RUN;

    %PUT *I* GRID SIZE: &GRID_SIZE;
    %PUT *I* GRID: &GRID;
    %PUT *I* GRID_FLAG: &GRID_FLAG;

	PROC SORT DATA=DS;BY &HH_ID DESCENDING &GRID_KEY;RUN;

    DATA GRID_MISM_MATRIX_&YEAR (KEEP=TYPE_ERR RB010 RB030 VARIABLE &HH_ID J COL FLAG EXP_FLAG MSG RENAME=(J=ROW));
    SET  DS;
      LENGTH TYPE_ERR $40;
      LENGTH VARIABLE $8;
      LENGTH MAX_Z 8;
      LENGTH MSG $60;
      RETAIN MAX_Z;
      BY &HH_ID DESCENDING  &GRID_KEY;
      ARRAY GRID &GRID;
      ARRAY GRID_FLAG &GRID_FLAG;
      J = &GRID_KEY;
      MSG = '';
      WARN = 0;
      IF FIRST.&HH_ID THEN DO;
             MAX_Z = &GRID_KEY;
      END;
      IF WARN_SEQ < 1 THEN DO;
              /**** FLAG = -2 ****/
              IF GRID_FLAG[J] ^= -2 THEN DO;
                     WARN = 1;
                     COL = J;
                     FLAG = GRID_FLAG[J];
					 EXP_FLAG=-2;
                     VARIABLE = COMPRESS("&RG"||PUT(J,BEST.));
					 MSG="DIAGONAL";
              END;
              /**** FLAG = -5 LOWER ****/
              DO K = 1 TO J-1;
                    IF GRID_FLAG[K] < -1  AND WARN=0 THEN DO;
                       WARN = 1;
                       COL = K;
                       FLAG = GRID_FLAG[K];
					   EXP_FLAG=-1;
                       VARIABLE = COMPRESS("&RG"||PUT(K,BEST.));
					   MSG="LOWER TRIANGLE";
                    END;
              		IF WARN > 0 AND MSG = '' THEN
                      MSG = 'MISSING VAL' ||
                            ' ROW=' || COMPRESS(PUT(J,BEST.)) ||
                            ' COL=' || COMPRESS(PUT(K,BEST.)) ||
                            ' FLAG=' || COMPRESS(PUT(GRID_FLAG[K],BEST.))
                      ;
              END;
              /**** FLAG = -5 UPPER ****/
              DO K = J+1 TO &GRID_SIZE;
                    IF K LE MAX_Z AND GRID_FLAG[K] ^= -5 AND WARN=0 THEN DO;
                       WARN = 1;
                       COL = K;
                       FLAG = GRID_FLAG[K];
					   EXP_FLAG=-5;
                       VARIABLE = COMPRESS("&RG"||PUT(K,BEST.));
					   MSG="UPPER TRIANGLE";
                    END;
                    ELSE IF K GT MAX_Z AND GRID_FLAG[K] ^= -4 AND WARN=0 THEN DO;
                       WARN = 1;
                       COL = K;
                       FLAG = GRID_FLAG[K];
					   EXP_FLAG=-4;
                       VARIABLE = COMPRESS("&RG"||PUT(K,BEST.));
					   MSG="OUTSIDE GRID";
                    END;
              END;
      END;
      ELSE WARN = 0;
      /**** WARNING HANDLING ****/
      IF WARN = 1;
    RUN;

    %LET K=%EVAL(&K+1);
    %LET YEAR=%SCAN( &YEAR_LST, &K );
%END;

%MEND check_str_GRID;

/**
 * SEMANTIC CHECKS / AGE
 */
%MACRO check_age_GRID(DS=,F=);

%LOCAL I N_YEARS YEAR_LST YEAR;
%LOCAL GRID RGY_AGE RGX_AGE RG_AGE;

%IF &DS EQ %THEN %DO;
    %LET DS=RAW.&_SPLIT_MODE_&ss&cc&YY&F;
%END;

PROC SQL NOPRINT;
  SELECT DISTINCT RB010 INTO :YEAR_LST SEPARATED BY ' '
  FROM   &DS
  ;
QUIT;

%LET RG=;
%detect_max_k (DS=&DS);
DATA _NULL_;
SET  MAX_K;
   LENGTH GRID RGY_AGE RGX_AGE RG_AGE $1024;
   GRID = ''; RGY_AGE = ''; RGX_AGE = ''; RG_AGE = '';
   DO I = 1 TO COL;
      GRID      = TRIM(GRID)    || ' ' || COMPRESS("&RG"||PUT(I,BEST.));
          RGY_AGE   = TRIM(RGY_AGE) || ' ' || COMPRESS('RGY_AGE_'||PUT(I,BEST.));
          RGX_AGE   = TRIM(RGX_AGE) || ' ' || COMPRESS('RGX_AGE_'||PUT(I,BEST.));
          RG_AGE    = TRIM(RG_AGE)  || ' ' || COMPRESS('RG_AGE_' ||PUT(I,BEST.));
   END;
   CALL SYMPUT('GRID_SIZE',TRIM(COL));
   CALL SYMPUT('GRID',TRIM(GRID));
   CALL SYMPUT('RGY_AGE' ,TRIM(RGY_AGE));
   CALL SYMPUT('RGX_AGE' ,TRIM(RGX_AGE));
   CALL SYMPUT('RG_AGE'  ,TRIM(RG_AGE));
RUN;

%PUT *I* GRID SIZE: &GRID_SIZE;
%PUT *I* RGY_AGE: &RGY_AGE;
%PUT *I* RGX_AGE: &RGX_AGE;
%PUT *I* RG_AGE: &RG_AGE;

%LET K=1;
%LET YEAR=%SCAN( &YEAR_LST, &K );
%DO %WHILE( &YEAR NE );
        PROC SORT DATA=&DS (WHERE=(RB010=&YEAR))
          OUT=DS (KEEP=RB010 RB110 &PERS_ID &FATHER_ID &MOTHER_ID &SPOUSE_ID
                                &GENDER_ID &AGE_ID &HH_ID &GRID_KEY &RG: AGE RB080 RB080_F )
      ;
          BY &HH_ID &GRID_KEY;
        RUN;

        %LET HH_MBR_BOOL=0;
        PROC SQL NOPRINT;
          SELECT COUNT(*) INTO :HH_MBR_BOOL
          FROM   SVAL_HH_MBR_STAT_LST
      ;
        QUIT;

        %IF &HH_MBR_BOOL gt 0 %THEN %DO;
    	DATA _NULL_;
        SET  SVAL_HH_MBR_STAT_LST;
             CALL SYMPUT('HH_MEMBERSHIP_STATUS_FILTER',TRIM(LX)||' IN '||TRIM(RX));
        RUN;

        DATA DS;
        SET  DS;
         IF &HH_MEMBERSHIP_STATUS_FILTER;
        RUN;
        %END;
        %ELSE %DO;
        DATA DS;
        SET  DS;
                 IF RB110 lt 5;
        RUN;
        %END;

        DATA DS;
        MERGE DS A1_HH_LST (in=B);
          BY &HH_ID;
          IF NOT B;
        RUN;

        DATA DS;
        MERGE DS WARN_MNS_&YEAR;
          BY &HH_ID;
          IF WARN_SEQ <= 0;
        RUN;

        DATA DS;
        SET  DS (WHERE=(&GRID_KEY gt 0));
          RETAIN &RGY_AGE;
          BY &HH_ID &GRID_KEY;
          ARRAY AGE &RGY_AGE;
          IF FIRST.&HH_ID THEN DO;
             K = &GRID_KEY;
                 DO J = K+1 TO DIM(AGE);
                        AGE[J] = .;
                 END;
          END;
          K = &GRID_KEY;
          AGE[K] = &AGE_ID;
        RUN;

        PROC SORT DATA=DS;
          BY &HH_ID DESCENDING &GRID_KEY;
        RUN;

        DATA DS;
        SET  DS;
          RETAIN &RGX_AGE;
          BY &HH_ID DESCENDING &GRID_KEY;
          ARRAY AGE &RGX_AGE;
          IF FIRST.&HH_ID THEN DO;
             K = &GRID_KEY;
                 DO J = 1 TO K-1;
                        AGE[J] = .;
                 END;
          END;
          K = &GRID_KEY;
          AGE[K] = &AGE_ID;
        RUN;

        DATA DS (DROP=RGX_AGE: RGY_AGE:);
        SET  DS;
          ARRAY AGEY &RGY_AGE;
          ARRAY AGEX &RGX_AGE;
          ARRAY AGE  &RG_AGE;
          DO J = 1 TO DIM(AGE);
                 AGE[J] = MAX( AGEX[J], AGEY[J] );
          END;
        RUN;

        PROC SORT DATA=DS;
          BY &HH_ID &GRID_KEY;
        RUN;

        DATA GRID_MISM_AGE_&YEAR (KEEP=RB010 RB030 VARIABLE &HH_ID J COL ROLE AGE_ROW AGE_CHILD AGE_PARENT MSG RENAME=(J=ROW));
        SET  DS;
          LENGTH VARIABLE $8;
          LENGTH MSG $60;
          ARRAY GRID &GRID;
          ARRAY AGE  &RG_AGE;
          WARN = 0;
          J = &GRID_KEY;
          DO K = 1 TO DIM(GRID);
                 IF GRID[K] IN ( 20 21 22 ) THEN DO;
                        IF MAX( &AGE_ID, AGE[K] ) ^= . AND &AGE_ID >= AGE[K] THEN DO;
                           COL = K;
                           ROLE = GRID[K];
                           AGE_ROW = &AGE_ID;
                           VARIABLE = COMPRESS("&RG"||PUT(K,BEST.));
						   AGE_CHILD=&AGE_ID;
						   AGE_PARENT=AGE[K];
                           MSG = "PARENT YOUNGER THAN CHILD"
                           ;
                           WARN = 1;
                        END;
                 END;
                 IF GRID[K] IN ( 50 60 ) THEN DO;
                        IF MAX( &AGE_ID, AGE[K] ) ^= . AND &AGE_ID <= AGE[K] THEN DO;
                           COL = K;
                           ROLE = GRID[K];
                           AGE_ROW = &AGE_ID;
                           VARIABLE = COMPRESS("&RG"||PUT(K,BEST.));
						   AGE_CHILD=AGE[K];
						   AGE_PARENT=&AGE_ID;
                           MSG = "CHILD OLDER THAN PARENT"
                           ;
                           WARN = 1;
                        END;
                 END;
                 IF GRID[K] = 70 THEN DO;
                        IF MAX( &AGE_ID, AGE[K] ) ^= . AND &AGE_ID <= AGE[K] THEN DO;
                           COL = K;
                           ROLE = GRID[K];
                           AGE_ROW = &AGE_ID;
                           VARIABLE = COMPRESS("&RG"||PUT(K,BEST.));
						   AGE_CHILD=AGE[K];
						   AGE_PARENT=&AGE_ID;
                           MSG = "CHILD OLDER THAN GRAND-PARENT"
                           ;
                           WARN = 1;
                        END;
                 END;
          END;
          IF WARN = 1;
        RUN;

        %LET K=%EVAL(&K+1);
        %LET YEAR=%SCAN( &YEAR_LST, &K );

%END;

%MEND check_age_GRID;

/**
 * SEMANTIC CHECKS / REDUNDANT FIELDS
 */
%MACRO check_red_GRID(DS=,F=);

%LOCAL I N_YEARS YEAR_LST YEAR;

%IF &DS EQ %THEN %DO;
    %LET DS=RAW.&_SPLIT_MODE_&ss&cc&YY&F;
%END;

PROC SQL NOPRINT;
  SELECT DISTINCT RB010 INTO :YEAR_LST SEPARATED BY ' '
  FROM   &DS
  ;
QUIT;

%LET RG=;
%detect_max_k (DS=&DS);
DATA _NULL_;
SET  MAX_K;
   LENGTH RGY_PID RGX_PID RG_PID $1024;
   RGY_PID = ''; RGX_PID = ''; RG_PID = '';
   DO I = 1 TO COL;
      GRID      = TRIM(GRID)    || ' ' || COMPRESS("&RG"||PUT(I,BEST.));
          RGY_PID   = TRIM(RGY_PID) || ' ' || COMPRESS('RGY_PID_'||PUT(I,BEST.));
          RGX_PID   = TRIM(RGX_PID) || ' ' || COMPRESS('RGX_PID_'||PUT(I,BEST.));
          RG_PID    = TRIM(RG_PID)  || ' ' || COMPRESS('RG_PID_' ||PUT(I,BEST.));
   END;
   CALL SYMPUT('GRID_SIZE',TRIM(COL));
   CALL SYMPUT('RGY_PID' ,TRIM(RGY_PID));
   CALL SYMPUT('RGX_PID' ,TRIM(RGX_PID));
   CALL SYMPUT('RG_PID'  ,TRIM(RG_PID));
RUN;

DATA _NULL_;
SET  MAX_K;
   LENGTH GRID RGY_SEX RGX_SEX RG_SEX $1024;
   GRID = ''; RGY_SEX = ''; RGX_SEX = ''; RG_SEX = '';
   DO I = 1 TO COL;
      GRID      = TRIM(GRID)    || ' ' || COMPRESS("&RG"||PUT(I,BEST.));
          RGY_SEX   = TRIM(RGY_SEX) || ' ' || COMPRESS('RGY_SEX_'||PUT(I,BEST.));
          RGX_SEX   = TRIM(RGX_SEX) || ' ' || COMPRESS('RGX_SEX_'||PUT(I,BEST.));
          RG_SEX    = TRIM(RG_SEX)  || ' ' || COMPRESS('RG_SEX_' ||PUT(I,BEST.));
   END;
   CALL SYMPUT('GRID',TRIM(GRID));
   CALL SYMPUT('RGY_SEX' ,TRIM(RGY_SEX));
   CALL SYMPUT('RGX_SEX' ,TRIM(RGX_SEX));
   CALL SYMPUT('RG_SEX'  ,TRIM(RG_SEX));
RUN;

%PUT *I* GRID SIZE: &GRID_SIZE;
%PUT *I* GRID: &GRID;
%PUT *I* RGY_SEX: &RGY_SEX;
%PUT *I* RGX_SEX: &RGX_SEX;
%PUT *I* RG_SEX: &RG_SEX;
%PUT *I* RGY_PID: &RGY_PID;
%PUT *I* RGX_PID: &RGX_PID;
%PUT *I* RG_PID: &RG_PID;

%LET K=1;
%LET YEAR=%SCAN( &YEAR_LST, &K );
%PUT &YEAR;
%DO %WHILE( &YEAR NE );
        PROC SORT DATA=&DS (WHERE=(RB010=&YEAR))
          OUT=DS (KEEP=RB010 RB110 &PERS_ID &FATHER_ID &MOTHER_ID &SPOUSE_ID
                                &GENDER_ID &AGE_ID &HH_ID &GRID_KEY &RG: )
      ;
          BY &HH_ID &GRID_KEY;
        RUN;

        %LET HH_MBR_BOOL=0;
        PROC SQL NOPRINT;
          SELECT COUNT(*) INTO :HH_MBR_BOOL
          FROM   SVAL_HH_MBR_STAT_LST
      ;
        QUIT;

        %IF &HH_MBR_BOOL gt 0 %THEN %DO;
        DATA _NULL_;
        SET  SVAL_HH_MBR_STAT_LST;
             CALL SYMPUT('HH_MEMBERSHIP_STATUS_FILTER',TRIM(LX)||' IN '||TRIM(RX));
        RUN;

        DATA DS;
        SET  DS;
         IF &HH_MEMBERSHIP_STATUS_FILTER;
        RUN;
        %END;
        %ELSE %DO;
        DATA DS;
        SET  DS;
                 IF RB110 gt 5;
        RUN;
        %END;

        DATA DS;
        MERGE DS A1_HH_LST (in=B);
          BY &HH_ID;
          IF NOT B;
        RUN;

        DATA DS;
        MERGE DS WARN_MNS_&YEAR;
          BY &HH_ID;
          IF WARN_SEQ <= 0;
        RUN;

        DATA DS;
        SET  DS (WHERE=(&GRID_KEY gt 0));
          RETAIN &RGY_PID;
          RETAIN &RGY_SEX;
          BY &HH_ID &GRID_KEY;
          ARRAY PID &RGY_PID;
          ARRAY SEX &RGY_SEX;
          IF FIRST.&HH_ID THEN DO;
             K = &GRID_KEY;
                 DO J = K+1 TO DIM(PID);
                        PID[J] = .;
                        SEX[J] = .;
                 END;
          END;
          K = &GRID_KEY;
          PID[K] = &PERS_ID;
          SEX[K] = &GENDER_ID;
        RUN;

        PROC SORT DATA=DS;
          BY &HH_ID DESCENDING &GRID_KEY;
        RUN;

        DATA DS;
        SET  DS;
          RETAIN &RGX_PID;
          RETAIN &RGX_SEX;
          BY &HH_ID DESCENDING &GRID_KEY;
          ARRAY PID &RGX_PID;
          ARRAY SEX &RGX_SEX;
          IF FIRST.&HH_ID THEN DO;
             K = &GRID_KEY;
                 DO J = 1 TO K-1;
                        PID[J] = .;
                        SEX[J] = .;
                 END;
          END;
          K = &GRID_KEY;
          PID[K] = &PERS_ID;
          SEX[K] = &GENDER_ID;
        RUN;

        DATA DS (DROP=RGX_PID: RGY_PID: RGX_SEX: RGY_SEX:);
        SET  DS;
          ARRAY PIDY &RGY_PID;
          ARRAY PIDX &RGX_PID;
          ARRAY PID &RG_PID;
          ARRAY SEXY &RGY_SEX;
          ARRAY SEXX &RGX_SEX;
          ARRAY SEX &RG_SEX;
          DO J = 1 TO DIM(PID);
                 PID[J] = MAX( PIDX[J], PIDY[J] );
                 SEX[J] = MAX( SEXX[J], SEXY[J] );
          END;
        RUN;

        PROC SORT DATA=DS;
          BY &HH_ID &GRID_KEY;
        RUN;

        DATA GRID_MISM_RED_&YEAR (KEEP=RB010 VARIABLE RB030 &HH_ID J COL ROLE MSG PARENT_ID_GRID PARENT_ID_FILE RENAME=(J=ROW));
        SET  DS;
          LENGTH VARIABLE $8;
          LENGTH MSG $60;
          ARRAY GRID &GRID;
          ARRAY PID &RG_PID;
          ARRAY SEX &RG_SEX;
          WARN = 0;
          J = &GRID_KEY;
          DO K = 1 TO DIM(GRID);
                IF GRID[K] IN ( 20 21 ) THEN DO;
                        IF SEX[K] = 1 THEN DO;
                                IF MAX( &FATHER_ID, PID[K] ) ^= . AND ( &FATHER_ID ^= PID[K] ) THEN DO;
                                   ROLE = GRID[K];
                                   VARIABLE = "&FATHER_ID";
								   PARENT_ID_GRID=PID[K];
								   PARENT_ID_FILE=&FATHER_ID;
                                   MSG = "FATHER ID (RB220) DOES NOT MATCH MALE PARENT (HHGRID)"/*'FATHER_ID' ||
                                          ' J=' || COMPRESS(PUT(J,BEST.)) ||
                                          ' K=' || COMPRESS(PUT(K,BEST.)) ||
                                          " &FATHER_ID[J]=" || COMPRESS(PUT(&FATHER_ID,BEST.)) ||
                                          ' PID[K]=' || COMPRESS(PUT(PID[K],BEST.))*/
                                   ;
                                   WARN = 1;
                                END;
                        END;
                        ELSE IF SEX[K] = 2 THEN DO;
                                IF MAX( &MOTHER_ID, PID[K] ) ^= . AND ( &MOTHER_ID ^= PID[K] ) THEN DO;
                                   ROLE = GRID[K];
                                   VARIABLE = "&MOTHER_ID";
								   PARENT_ID_GRID=PID[K];
								   PARENT_ID_FILE=&MOTHER_ID;
                                   MSG = "MOTHER ID (RB230) DOES NOT MATCH FEMALE PARENT (HHGRID)"/*'MOTHER_ID' ||
                                          ' J=' || COMPRESS(PUT(J,BEST.)) ||
                                          ' K=' || COMPRESS(PUT(K,BEST.)) ||
                                          " &MOTHER_ID[J]=" || COMPRESS(PUT(&MOTHER_ID,BEST.)) ||
                                          ' PID[K]=' || COMPRESS(PUT(PID[K],BEST.))*/
                                   ;
                                   WARN = 1;
                                END;
                        END;
                END;
                IF GRID[K] IN ( 10 11 ) THEN DO;
                        IF MAX( &SPOUSE_ID, PID[K] ) ^= . AND ( &SPOUSE_ID ^= PID[K] ) THEN DO;
                           ROLE = GRID[K];
						   PARENT_ID_GRID=PID[K];
						   PARENT_ID_FILE=&SPOUSE_ID;
                           VARIABLE = "&SPOUSE_ID";
                           MSG = "PARTNER ID (RB240) DOES NOT MATCH PARTNER ID (HHGRID)"/*'SPOUSE_ID' ||
                                  ' J=' || COMPRESS(PUT(J,BEST.)) ||
                                  ' K=' || COMPRESS(PUT(K,BEST.)) ||
                                  " &SPOUSE_ID[J]=" || COMPRESS(PUT(&SPOUSE_ID,BEST.)) ||
                                  ' PID[K]=' || COMPRESS(PUT(PID[K],BEST.))*/
                           ;
                           WARN = 1;
                        END;
                END;
          END;
          IF WARN = 1;
        RUN;

        %LET K=%EVAL(&K+1);
        %LET YEAR=%SCAN( &YEAR_LST, &K );

%END;

%MEND check_red_GRID;


/**
 * LOV FOR HH-GRID
 */
%MACRO check_vars_GRID(DS=,F=);
%LOCAL K L RC CHECK VARS VAR VARX LB UB FMT LIST LIST_STR;
%LOCAL I N_YEARS YEAR_LST YEAR YEAR_ANTE;
%LOCAL GRID GRID_FLAG;

%IF &DS EQ %THEN %DO;
    %LET DS=RAW.&_SPLIT_MODE_&ss&cc&YY&F;
%END;

PROC SQL NOPRINT;
  SELECT DISTINCT RB010 INTO :YEAR_LST SEPARATED BY ' '
  FROM   &DS
  ;
QUIT;

%LET RG=;
%detect_max_k (DS=&DS);
DATA _NULL_;
SET  MAX_K;
   CALL SYMPUT('GRID_SIZE',TRIM(COL));
RUN;

DATA _NULL_;
SET  SVAL_BRK_LOV;
	IF SUBSTR(VARIABLE,1,3)="RG_";
	CALL SYMPUT('YEAR_ANTE',TRIM(ANTE));
RUN;


%PUT *I* GRID SIZE: &GRID_SIZE (&RG)
;
%LET K=1;
%LET YEAR=%SCAN( &YEAR_LST, &K );
%DO %WHILE( &YEAR NE );
    PROC SORT DATA=&DS (WHERE=(RB010=&YEAR))
      OUT=DS (KEEP=RB010 RB110 &PERS_ID &FATHER_ID &MOTHER_ID &SPOUSE_ID &GENDER_ID &AGE_ID &HH_ID &GRID_KEY &RG: );
      BY &HH_ID &GRID_KEY;
    RUN;

    %LET HH_MBR_BOOL=0;
    PROC SQL NOPRINT;
      SELECT COUNT(*) INTO :HH_MBR_BOOL
      FROM   SVAL_HH_MBR_STAT_LST
  ;
    QUIT;

    %IF &HH_MBR_BOOL gt 0 %THEN %DO;
    DATA _NULL_;
    SET  SVAL_HH_MBR_STAT_LST;
         CALL SYMPUT('HH_MEMBERSHIP_STATUS_FILTER',TRIM(LX)||' IN '||TRIM(RX));
    RUN;

    DATA DS;
    SET  DS;
     IF &HH_MEMBERSHIP_STATUS_FILTER;
    RUN;
    %END;
    %ELSE %DO;
    DATA DS;
    SET  DS;
             IF RB110 lt 5;
    RUN;
    %END;

    PROC FREQ DATA=DS NOPRINT;
      TABLE &HH_ID / OUT=A1_HH_LST;
    DATA _LAST_ (WHERE=(COUNT=1));
      SET;
    RUN;

    DATA DS;
    MERGE DS A1_HH_LST (in=B);
      BY &HH_ID;
      IF NOT B;
    RUN;



	DATA _NULL_;
	%IF &YEAR>=&YEAR_ANTE %THEN %DO;SET SVAL END=EOF;WHERE  LIST is not null AND    TYPE = "HGRID";%END;
	%IF &YEAR<&YEAR_ANTE %THEN %DO;SET SVAL_BRK_LOV END=EOF;WHERE  LIST is not null ;%END;
	  ;
	  LENGTH VARS _LIST_ $32767;
	  RETAIN VARS '';
	  _LIST_ = TRANWRD(TRANWRD(TRANWRD(TRANWRD(TRIM(LIST),'(',''),')',''),',',''),'"','');
	  VARS = TRIM(VARS) || TRIM(VARIABLE) || ':' || SUBSTR(FORMAT,1,1) || ':'  ||TRIM(_LIST_) || ':' ||TRIM(LIST)	  ;
	  IF NOT EOF THEN VARS = TRIM(VARS) || '|';
	  ELSE CALL SYMPUT('VARS',TRIM(VARS));
	RUN;

	DATA
	 GRID_MISM_V_&YEAR (KEEP=RB010 VARIABLE RB030 &HH_ID ROW COL MSG VALUE);
	SET  &DS (WHERE=(RB010=&YEAR));
        LENGTH VARIABLE $8;
        LENGTH VALUE $12;
        LENGTH MSG $100;
        %LET L=1;
        %LET VAR=%SCAN(&VARS,&L,|);
        %LET VARX=%SCAN(&VAR,1,:);
		ROW=&GRID_KEY;
        %DO %WHILE(&VARX NE);
                %LET FMT =%SCAN(&VAR,2,:);
                %LET LIST=%SCAN(&VAR,3,:);
				%LET LIST2=%SCAN(&VAR,4,:);
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
				   COL = INPUT(TRANWRD(VARIABLE,'RG_',''),BEST8.);
				   MSG="&LIST2";
                   %IF &FMT NE $ %THEN %DO; VALUE = COMPRESS(PUT(&VARX,BEST.)); %END;
                   %ELSE %DO; VALUE = &VARX; %END;
                   OUTPUT;
                END;
                %LET L=%EVAL(&L+1);
                %LET VAR=%SCAN(&VARS,&L,|);
                %LET VARX=%SCAN(&VAR,1,:);
         %END;
         IF VARIABLE ^= '';
	RUN;
    %LET K=%EVAL(&K+1);
    %LET YEAR=%SCAN( &YEAR_LST, &K );
%END;



%MEND check_vars_GRID;


%MACRO check_aggr_GRID(DS=,F=);

%LOCAL YEAR_LST YEAR START_V;

%IF &DS EQ %THEN %DO;
    %LET DS=RAW.&_SPLIT_MODE_&ss&cc&YY&F;
%END;

PROC SQL NOPRINT;
  SELECT DISTINCT RB010 INTO :YEAR_LST SEPARATED BY ' '
  FROM   &DS
  ;
QUIT;

%LET K=1;
%LET YEAR=%SCAN( &YEAR_LST, &K );
%DO %WHILE( &YEAR NE );
    %IF &K EQ 1 %THEN %DO;
                DATA GRID_MISM_SEQ;
                SET  GRID_MISM_SEQ_&YEAR;
                RUN;

                DATA GRID_MISM_MATRIX;
                SET  GRID_MISM_MATRIX_&YEAR;
                RUN;

                DATA GRID_MISM_AGE;
                SET  GRID_MISM_AGE_&YEAR;
                RUN;

                DATA GRID_MISM_RED;
                SET  GRID_MISM_RED_&YEAR;
                RUN;

				DATA GRID_MISM_V;
                SET  GRID_MISM_V_&YEAR;
                RUN;

        %END;
        %ELSE %DO;
                PROC APPEND BASE=GRID_MISM_SEQ
                         DATA=GRID_MISM_SEQ_&YEAR
                         ;
                RUN;

                PROC APPEND BASE=GRID_MISM_MATRIX
                         DATA=GRID_MISM_MATRIX_&YEAR
                         ;
                RUN;

                PROC APPEND BASE=GRID_MISM_AGE
                         DATA=GRID_MISM_AGE_&YEAR
                         ;
                RUN;

                PROC APPEND BASE=GRID_MISM_RED
                         DATA=GRID_MISM_RED_&YEAR
                         ;
                RUN;

                PROC APPEND BASE=GRID_MISM_V
                         DATA=GRID_MISM_V_&YEAR
                         ;
                RUN;
        %END;
        %LET K=%EVAL(&K+1);
        %LET YEAR=%SCAN( &YEAR_LST, &K );

%END;

/**
 * PURGING BASED ON HGRID TIMESTAMP (START_V)
 */
%LET START_V=.;
PROC SQL NOPRINT;
  SELECT MAX(START_V) INTO :START_V
  FROM   SVAL
  WHERE  TYPE = 'HGRID'
  ;
QUIT;

PROC SQL;
  DELETE FROM GRID_MISM_SEQ
  WHERE  RB010 lt &START_V
  ;
QUIT;

PROC SQL;
  DELETE FROM GRID_MISM_MATRIX
  WHERE  RB010 lt &START_V
  ;
QUIT;

PROC SQL;
  DELETE FROM GRID_MISM_AGE
  WHERE  RB010 lt &START_V
  ;
QUIT;

PROC SQL;
  DELETE FROM GRID_MISM_RED
  WHERE  RB010 lt &START_V
  ;
QUIT;

PROC SQL;
  DELETE FROM GRID_MISM_V
  WHERE  RB010 lt &START_V
  ;
QUIT;

%MEND check_aggr_GRID;
