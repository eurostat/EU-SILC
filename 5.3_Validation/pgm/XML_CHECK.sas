/**
 * --------------------- WELL FORMEDNESS CHECKS ---------------
 * getSVAL_F
 * chkEXIST
 * purgeVAR
 * purgeNA
 * chkCOMPL
 * chkFLAG
 * chkFMT
 * chkUNRECOGNIZED_FLAG_IF
 */

/**
 * @F  = SILC FILE { D, R, H, P }
 * @DS = RAW DATASET TO CHECK
 * @CC = COUNTRY TO CHECK
 */
%MACRO getSVAL_F(F=D);
    DATA SVAL_&F;
    SET  SVAL (WHERE=(SUBSTR(VARIABLE,1,1) = "&F"));
      IF KEY NE 'Y' THEN DO;
             FLAG_NAME = TRIM(VARIABLE) || '_F';
      END;
      ELSE FLAG_NAME = '';
    RUN;
    DATA SVAL_FLAGS_&F;
        SET  SVAL_FLAGS (WHERE=(SUBSTR(VARIABLE,1,1) = "&F"));
    RUN;
    DATA SVAL_FLAG_LEN_&F;
        SET  SVAL_FLAG_LEN (WHERE=(SUBSTR(VARIABLE,1,1) = "&F"));
    RUN;
    DATA SVAL_NAS_&F;
        SET  SVAL_NAS (WHERE=(SUBSTR(VARIABLE,1,1) = "&F"));
    RUN;
    DATA SVAL_PARENT_&F;
        SET  SVAL_PARENT (WHERE=(SUBSTR(VARIABLE,1,1) = "&F"));
    RUN;
    DATA SVAL_CHKSUM_&F;
        SET  SVAL_CHKSUM (WHERE=(SUBSTR(VARIABLE,1,1) = "&F"));
    RUN;
    DATA SVAL_TRANS_LIST_&F;
        SET  SVAL_TRANS_LIST (WHERE=(SUBSTR(VARIABLE,1,1) = "&F"));
    RUN;
    DATA SVAL_BRK_LOV_&F;
        SET  SVAL_BRK_LOV (WHERE=(SUBSTR(VARIABLE,1,1) = "&F"));
    RUN;
    DATA SVAL_BRK_FLAG_&F;
        SET  SVAL_BRK_FLAG (WHERE=(SUBSTR(VARIABLE,1,1) = "&F"));
    RUN;
%MEND getSVAL_F;

%MACRO chkEXIST(F=D);

    %LOCAL K RC CHECK VARS VAR;
    %IF &_SPLIT_MODE_ EQ _ANTE_ %THEN %DO;
    PROC SQL NOPRINT;
		SELECT VARIABLE INTO :VARS SEPARATED BY ' '
		FROM   SVAL_&F
		WHERE  REQ = 'Y'
		AND ( START_V IS NULL OR START_V le &_SPLIT_CUT_ )
		AND ( START_M IS NULL OR START_M le &_SPLIT_CUT_ )
		;
    QUIT;
    %END;
    %ELSE %DO;
	PROC SQL NOPRINT;
		SELECT VARIABLE INTO :VARS SEPARATED BY ' '
		FROM   SVAL_&F
		WHERE  REQ = 'Y'
		;
	QUIT;
    %END;

    DATA MISSVARS;
		LENGTH VARIABLE $8;
		STOP;
    RUN;

    %LET DS=RAW.&_SPLIT_MODE_&ss&cc&YY&F;
    %LET dsid = %SYSFUNC(OPEN(&DS));

    %LET K=1;
    %LET VAR=%SCAN(&VARS,&K);
    %DO %WHILE(&VAR NE);
	    %LET CHECK = %SYSFUNC(VARNUM(&dsid,&VAR));
	    %IF &CHECK EQ 0 %THEN %DO;
			PROC SQL;
				INSERT INTO MISSVARS
				VALUES ("&VAR");
			QUIT;
	    %END;
	    %LET K=%EVAL(&K+1);
	    %LET VAR=%SCAN(&VARS,&K);
    %END;

    %LET rc=%SYSFUNC(CLOSE(&dsid));

%MEND chkEXIST;




%MACRO purgeVAR(F=D,STEP=1);
%LOCAL K RC CHECK VARS VAR VARS2PURG;
%IF &STEP EQ 1 %THEN %DO;
    PROC SQL NOPRINT;
		SELECT VARIABLE INTO :VARS SEPARATED BY ' '
		FROM   SVAL_&F;
    QUIT;

    %LET DS=RAW.&_SPLIT_MODE_&ss&cc&YY&F;
    %LET dsid = %SYSFUNC(OPEN(&DS));

    %LET K=1;
    %LET VAR=%SCAN(&VARS,&K);
    %DO %WHILE(&VAR NE);
        %LET CHECK = %SYSFUNC(VARNUM(&dsid,&VAR));
        %IF &CHECK EQ 0 %THEN %DO;
            PROC SQL;
				DELETE FROM SVAL_&F WHERE VARIABLE = "&VAR";
            QUIT;
            PROC SQL;
				DELETE FROM SVAL_FLAGS_&F WHERE VARIABLE = "&VAR";
            QUIT;
            PROC SQL;
				DELETE FROM SVAL_NAS_&F WHERE VARIABLE = "&VAR";
            QUIT;
        %END;
        %LET K=%EVAL(&K+1);
        %LET VAR=%SCAN(&VARS,&K);
    %END;

    %LET rc=%SYSFUNC(CLOSE(&dsid));
%END;
%ELSE %DO;
    PROC SQL;
		DELETE FROM SVAL_&F WHERE VARIABLE IN ( SELECT VARIABLE FROM MISMVARS );
		DELETE FROM SVAL_&F WHERE VARIABLE IN ( SELECT VARIABLE FROM NOFLAGS );
    QUIT;
    PROC SQL;
		DELETE FROM SVAL_FLAGS_&F WHERE VARIABLE IN ( SELECT VARIABLE FROM MISMVARS );
		DELETE FROM SVAL_FLAGS_&F WHERE VARIABLE IN ( SELECT VARIABLE FROM NOFLAGS );
    QUIT;
    PROC SQL;
      DELETE FROM SVAL_NAS_&F WHERE VARIABLE IN ( SELECT VARIABLE FROM MISMVARS );
      DELETE FROM SVAL_NAS_&F WHERE VARIABLE IN ( SELECT VARIABLE FROM NOFLAGS );
    QUIT;

    /**
     * RAW DATASET UPDATE
     */
    PROC CONTENTS DATA=RAW.&_SPLIT_MODE_&ss&cc&YY&F OUT=VARS_FOUND_&F NOPRINT;
    RUN;


%END;

%MEND purgeVAR;

/**
 * ROUTING CONDITIONS (NA) TO BE DISABLED
 * @F  = SILC FILE { D, R, H, P }
 */
%MACRO purge_NA;

	DATA SVAL_NAS_&F._2PURGE (keep=VARIABLE NA_FLAG TOK STATUS);
	SET  SVAL_NAS_&F;
		LENGTH STATUS $3;
		K=1;
		TOK = SCAN(DISP,K,',');
		DO WHILE (TOK ne '');
		    OUTPUT;
		    K=K+1;
		TOK = SCAN(DISP,K,',');
		END;
	RUN;

	PROC SQL;
	  UPDATE SVAL_NAS_&F._2PURGE
	  SET    STATUS = 'OFF'
	  WHERE  TOK IN (SELECT VARIABLE FROM MISSVARS UNION SELECT TRIM(VARIABLE)||'_F' FROM MISSVARS )
	  ;
	QUIT;

	PROC SQL;
	  UPDATE SVAL_NAS_&F._2PURGE
	  SET    STATUS = 'OFF'
	  WHERE  TOK IN (SELECT VARIABLE FROM NOFLAGS UNION SELECT TRIM(VARIABLE)||'_F' FROM NOFLAGS )
	  ;
	QUIT;

	DATA _NULL_;
	SET  SVAL_NAS_&F._2PURGE;
	  PUT '*I* PURGED ROUTING ON ' VARIABLE '(' NA_FLAG ')' ' OWING TO ' TOK ' UNAVAILABILITY';
	RUN;

	PROC SQL;
	  DELETE FROM SVAL_NAS_&F T
	  WHERE  EXISTS (
	        SELECT * FROM SVAL_NAS_&F._2PURGE
	        WHERE  VARIABLE = T.VARIABLE AND NA_FLAG = T.NA_FLAG )
	        ;
	QUIT;

%MEND purge_NA;

/**
 * UNRECOGNIZED VARIABLES
 * @F  = SILC FILE { D, R, H, P }
 * @DS = RAW DATASET TO CHECK
 * @CC = COUNTRY TO CHECK
 */
%MACRO chkCOMPL(F=D);
    %LET DS=RAW.&_SPLIT_MODE_&ss&cc&YY&F;
    OPTIONS VALIDVARNAME=UPCASE;
    PROC CONTENTS DATA=&DS OUT=RAWCOLS NOPRINT;
    RUN;

    DATA RAWCOLS;
    SET  RAWCOLS;
		SFX = UPCASE( SCAN(NAME, 3, '_' ) );
		IF SFX IN ( 'F' 'IF' ) THEN DO;
		     NAME_1 = SCAN(NAME, 1, '_');
		     NAME_2 = SCAN(NAME, 2, '_');
		     NAME   = TRIM(NAME_1) || '_' || TRIM(NAME_2);
		END;
		ELSE DO;
		    SFX = UPCASE( SCAN(NAME, 2, '_' ) );
		    IF SFX IN ( 'F' 'IF' ) THEN DO;
		       NAME = SCAN(NAME, 1, '_');
		    END;
		END;
    PROC SORT NODUPKEY;
      BY NAME;
    RUN;

    %LOCAL K RC CHECK VARS VAR;
    PROC SQL NOPRINT;
      SELECT UPCASE(NAME) INTO :VARS SEPARATED BY ' '
      FROM   RAWCOLS
      ;
    QUIT;

    DATA DIRTYVARS;
      LENGTH VARIABLE $20;
      STOP;
    RUN;

    %LET K=1;
    %LET VAR=%SCAN(&VARS,&K);
    %DO %WHILE(&VAR NE);
	    %LET CHECK=0;
	    PROC SQL NOPRINT;
	      SELECT COUNT(*) INTO :CHECK
	      FROM   SVAL_&F
	      WHERE  VARIABLE = "&VAR"
	      ;
	    QUIT;
	    %IF &CHECK EQ 0 %THEN %DO;
	        PROC SQL;
	          INSERT INTO DIRTYVARS
	          VALUES ("&VAR")
	          ;
	        QUIT;
	    %END;
	    %LET K=%EVAL(&K+1);
	    %LET VAR=%SCAN(&VARS,&K);
    %END;

    %IF &F EQ P %THEN %DO;
        PROC SQL;
          DELETE FROM DIRTYVARS WHERE  VARIABLE IN ( 'RB245' 'SELRES' );
        QUIT;
    %END;

    %PUT *I* PURGE TRANSMISSION GUIDELINES VARIABLES;
    PROC SQL;
      DELETE FROM DIRTYVARS WHERE  VARIABLE IN (SELECT VARIABLE FROM SVAL_TRANS_LIST_&F);
    QUIT;

%MEND chkCOMPL;


/**
 * CHECK TRANSMISSION GUIDELINES VARIABLES CONTENT
 */
%MACRO chkTRANS_LIST (F=D);

	%LOCAL VARS VARX VAR REQ CHECK CHECK_F PERCENT PCT FLAG FLAG_XML;

	DATA MISM_TRANS;
		LENGTH VARIABLE $12;
		LENGTH DESC $32;
		STOP;
	RUN;

	DATA MISM_TRANS_PCT;
		LENGTH VARIABLE $12;
		LENGTH FLAG 8;
		LENGTH FL_EXPECTED 8;
		LENGTH PERCENT 8;
		FORMAT PERCENT PERCENT9.2;
		STOP;
	RUN;

	PROC SQL NOPRINT;
		SELECT trim(VARIABLE) || ':' || trim(REQ) INTO :VARS SEPARATED BY ' '
		FROM   SVAL_TRANS_LIST_&F
		;
	QUIT;

	%LET DS=RAW.&_SPLIT_MODE_&ss&cc&YY&F;

	OPTIONS VALIDVARNAME=UPCASE;
	PROC CONTENTS DATA=&DS OUT=RAWCOLS_F NOPRINT;
	RUN;

	PROC SQL NOPRINT;
		SELECT MAX(FLAG) INTO :FLAG_XML
		FROM   SVAL_TRANS_LIST_&F
		;
	QUIT;

	%LET K=1;
	%LET VARX=%SCAN(&VARS,&K);
	%LET VAR=%SCAN(&VARX,1,:);
	%LET REQ=%SCAN(&VARX,2,:);

	%DO %WHILE(&VARX NE);
		%LET CHECK=0;
		PROC SQL NOPRINT;
			SELECT COUNT(*) INTO :CHECK
			FROM   RAWCOLS_F
			WHERE  NAME = "&VAR"
			;
		QUIT;

		%LET CHECK_F=0;
		PROC SQL NOPRINT;
			SELECT COUNT(*) INTO :CHECK_F
			FROM   RAWCOLS_F
			WHERE  NAME = "&VAR._F"
			;
		QUIT;

		%IF &REQ eq Y %THEN %DO;
			%IF &CHECK eq 0 %THEN %DO;
			    PROC SQL;
			      INSERT INTO MISM_TRANS VALUES("&VAR","NOT FOUND");
			    QUIT;
			%END;
			%IF &CHECK_F eq 0 %THEN %DO;
			    PROC SQL;
			      INSERT INTO MISM_TRANS VALUES("&VAR._F","NOT FOUND");
			    QUIT;
			%END;
		%END;

		%PUT *I* TRANSMISSION GUIDELINES EXISTENCE CHECK ON &VAR: &CHECK &CHECK_F (req:&REQ);
		%IF &CHECK_F gt 0 %THEN %DO;
			PROC FREQ DATA=&DS NOPRINT;
				TABLE &VAR._F / MISSING OUT=&VAR._TRANS;
			RUN;

			PROC SORT DATA=_LAST_ (where=(&VAR._F ne &FLAG_XML));
				BY DESCENDING PERCENT;
			RUN;

			%LET PERCENT=1;
			%LET PCT=100%;
			DATA _LAST_ (KEEP=VARIABLE FLAG FL_EXPECTED PERCENT);
				SET  _LAST_ (RENAME=(&VAR._F=FLAG));
				VARIABLE = "&VAR";
				FL_EXPECTED = &FLAG_XML;
				PERCENT = PERCENT / 100;
				IF _N_ = 1 THEN DO;
					CALL SYMPUTX('FLAG',FLAG);
					CALL SYMPUTX('PERCENT',PERCENT);
					CALL SYMPUTX('PCT',PUT(PERCENT,PERCENT9.2));
				END;
			RUN;
			%IF ( &FLAG ne &FLAG_XML ) AND ( &PERCENT gt 0.00 ) %THEN %DO;
				PROC APPEND BASE=MISM_TRANS_PCT DATA=_LAST_ FORCE;
				RUN;
			%END;
		%END;
		%LET K=%EVAL(&K+1);
		%LET VARX=%SCAN(&VARS,&K);
		%LET VAR=%SCAN(&VARX,1,:);
		%LET REQ=%SCAN(&VARX,2,:);

	%END; /* LOOP */

%MEND chkTRANS_LIST;


%MACRO chkFLAG(F=D);

    PROC SQL NOPRINT;
		SELECT VARIABLE INTO :VARS SEPARATED BY ' '
		FROM   SVAL_&F
		WHERE  FLAG_NAME IS NOT NULL
		;
    QUIT;

    DATA NOFLAGS;
		LENGTH VARIABLE $8;
		STOP;
    RUN;

    %LET DS=RAW.&_SPLIT_MODE_&ss&cc&YY&F;
    %LET dsid = %SYSFUNC(OPEN(&DS));

    %LET K=1;
    %LET VAR=%SCAN(&VARS,&K);
    %DO %WHILE(&VAR NE);
	    %LET CHECK = %SYSFUNC(VARNUM(&dsid,&VAR));
	    %IF  &CHECK GT 0 %THEN %DO;
             %LET FLAG = %SYSFUNC(VARNUM(&dsid,&VAR._F));
             %IF &FLAG EQ 0 %THEN %DO;
             PROC SQL;
				INSERT INTO NOFLAGS
				VALUES ("&VAR");
             QUIT;
             %END;
	    %END;
	    %LET K=%EVAL(&K+1);
	    %LET VAR=%SCAN(&VARS,&K);
    %END;
    %LET rc=%SYSFUNC(CLOSE(&dsid));
%MEND chkFLAG;

/**
 * @F  = SILC FILE { D, R, H, P } XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
 */
%MACRO chkFMT(F=D);
	%LOCAL K RC CHECK FLAGS VARS FLAG VAR TYP;
	PROC SQL NOPRINT;
		SELECT VARIABLE INTO :VARS SEPARATED BY ' '
		FROM   SVAL_&F
		WHERE  SUBSTR(FORMAT,1,1) ^= '$'
		;
		SELECT FLAG_NAME INTO :FLAGS SEPARATED BY ' '
		FROM   SVAL_&F
		;
	QUIT;
	%LET VARS=&VARS &FLAGS;

	DATA MISMVARS;
		LENGTH VARIABLE $12;
		STOP;
	RUN;

	%LET DS=RAW.&_SPLIT_MODE_&ss&cc&YY&F;
    %LET dsid = %SYSFUNC(OPEN(&DS));

    %LET K=1;
    %LET VAR=%SCAN(&VARS,&K);
    %DO %WHILE(&VAR NE);
        %LET CHECK = %SYSFUNC(VARNUM(&dsid,&VAR));
        %IF  &CHECK GT 0 %THEN %DO;
            %LET TYP = %SYSFUNC(VARTYPE(&dsid,&CHECK));
            %IF &TYP EQ N %THEN %LET MISM=0;
            %ELSE %LET MISM=1;
            %IF &MISM EQ 1 %THEN %DO;
                %PUT *W* MISMATCHING FORMAT ON VAR &VAR;
                PROC SQL;
					INSERT INTO MISMVARS
					VALUES ("&VAR")
					;
                QUIT;
            %END;
        %END;
        %LET K=%EVAL(&K+1);
        %LET VAR=%SCAN(&VARS,&K);
    %END;
    %LET rc=%SYSFUNC(CLOSE(&dsid));
	/**
	 * FIX
	 */
	%LET NOBS=0;
	DATA MISMVARS;
	SET  MISMVARS NOBS=NOBS;
        CALL SYMPUTX('NOBS',NOBS);
	RUN;
    %IF &NOBS GT 0 %THEN %DO;
        DATA _NULL_;
        SET  MISMVARS END=EOF;
			LENGTH LINE1 LINE2 VARS $32767;
			RETAIN LINE1 LINE2 VARS '';
			VARS  = TRIM(VARS)  || ' ' || TRIM(VARIABLE);
			LINE1 = TRIM(LINE1) || ' ' || TRIM(VARIABLE) || '=' || TRIM(VARIABLE) || '_input' ;
			LINE2 = TRIM(LINE2) || ' ' || TRIM(VARIABLE) || '_input';
			IF EOF THEN DO;
				CALL SYMPUT('RENAME',TRIM(LINE1));
				CALL SYMPUT('DROP',TRIM(LINE2));
				CALL SYMPUT('VARS',TRIM(VARS));
			END;
        RUN;
        DATA _NULL_;
			CALL SYMPUTX('_DS_',SCAN("&DS",2));
        RUN;
        PROC DATASETS LIB=RAW NOLIST;
			MODIFY &_DS_;
			RENAME &RENAME;
        RUN;
        QUIT;

        DATA &DS (DROP=VARIABLE VALUE) ERR (KEEP=&F.B010 &F.B020 &F.B030 VARIABLE VALUE);
        SET  &DS;
            LENGTH VARIABLE $8;
	        LENGTH VALUE $125;
            %LET K=1;
            %LET VAR=%SCAN(&VARS,&K);
            %DO %WHILE(&VAR NE);
	            LENGTH &VAR 8;
                &VAR = INPUT(&VAR._input, BEST.);
                IF _ERROR_ THEN DO;
                   VARIABLE = "&VAR";
                   VALUE    = &VAR._input;
                   OUTPUT ERR;
                   _ERROR_ = 0;
                   &VAR = .;
                END;
                %LET K=%EVAL(&K+1);
                %LET VAR=%SCAN(&VARS,&K);
            %END;
            OUTPUT &DS;
        RUN;

        PROC SQL;
			DELETE FROM MISMVARS
			WHERE  VARIABLE NOT IN
			(SELECT VARIABLE FROM ERR);
        QUIT;
    %END;
    %ELSE %DO;
	    %IF %SYSFUNC(EXIST(ERR)) %THEN %DO;
	        PROC SQL;
                DROP TABLE ERR;
	        QUIT;
	    %END;
    %END;
%MEND chkFMT;



/**
 * UNRECOGNIZED FLAGS AND IMPUTATION FACTORS
 * @F  = SILC FILE { D, R, H, P }
 * @DS = RAW DATASET TO CHECK
 * @CC = COUNTRY TO CHECK
 */
%MACRO chkUNRECOGNIZED_FLAG_IF(F=D);

    %LET DS=RAW.&_SPLIT_MODE_&ss&cc&YY&F;
    OPTIONS VALIDVARNAME=UPCASE;
    PROC CONTENTS DATA=&DS OUT=RAWVARS NOPRINT;
    RUN;

    DATA RAWVARS_FLAGS RAWVARS_IFS;
	KEEP NAME;
    SET  RAWVARS;
		SFX = UPCASE( SCAN(NAME, -1, '_' ) );
		IF SFX IN ( 'F' ) THEN DO;
			 OUTPUT RAWVARS_FLAGS;
		END;
		ELSE IF SFX IN ( 'IF' ) THEN DO;
			 OUTPUT RAWVARS_IFS;
		END;
    RUN;

    PROC SORT DATA=RAWVARS_FLAGS NODUPKEY;
      BY NAME;
    RUN;
    PROC SORT DATA=RAWVARS_IFS NODUPKEY;
      BY NAME;
    RUN;

	DATA SVAL_FLAG_NAMES_&F;
	SET SVAL_&F (WHERE=(FLAG_NAME IS NOT NULL));
	VARIABLE = FLAG_NAME;
	RUN;

	DATA SVAL_IFS_&F;
	SET SVAL_&F (WHERE=(INCOME = 'Y' AND IMPUTE = 'Y'));
	VARIABLE = TRIM(VARIABLE) || '_IF';
	RUN;

	/*Check Flags*/
    DATA DIRTYFLAGS_&F;
      LENGTH VARIABLE $30;
      STOP;
    RUN;

	%LET NOBS=0;
	DATA _null_;
	SET  RAWVARS_FLAGS NOBS=NOBS;
        CALL SYMPUTX('NOBS',NOBS);
	RUN;
    %IF &NOBS GT 0 %THEN %DO;

	    PROC SQL NOPRINT;
	      SELECT UPCASE(NAME) INTO :VARS SEPARATED BY ' '
	      FROM   RAWVARS_FLAGS;
	    QUIT;

	    %LET K=1;
	    %LET VAR=%SCAN(&VARS,&K);
	    %DO %WHILE(&VAR NE);
		    %LET CHECK=0;
		    PROC SQL NOPRINT;
		      SELECT COUNT(*) INTO :CHECK
		      FROM   SVAL_FLAG_NAMES_&F
		      WHERE  VARIABLE = "&VAR"
		      ;
		    QUIT;
		    %IF &CHECK EQ 0 %THEN %DO;
		        PROC SQL;
		          INSERT INTO DIRTYFLAGS_&F
		          VALUES ("&VAR")
		          ;
		        QUIT;
		    %END;
		    %LET K=%EVAL(&K+1);
		    %LET VAR=%SCAN(&VARS,&K);
	    %END;
	%END;

    %PUT *I* PURGE TRANSMISSION GUIDELINE VARIABLES FLAGS;
    PROC SQL;
      DELETE FROM DIRTYFLAGS_&F WHERE  VARIABLE IN (SELECT compress(VARIABLE)||'_F' FROM SVAL_TRANS_LIST_&F);
    QUIT;
	/*Check Imputation Factors*/
    DATA DIRTYIFS_&F;
      LENGTH VARIABLE $30;
      STOP;
    RUN;

	%LET NOBS=0;
	DATA _null_;
	SET  RAWVARS_IFS NOBS=NOBS;
        CALL SYMPUTX('NOBS',NOBS);
	RUN;

    %IF &NOBS GT 0 %THEN %DO;

	    PROC SQL NOPRINT;
	      SELECT UPCASE(NAME) INTO :VARS SEPARATED BY ' '
	      FROM   RAWVARS_IFS;
	    QUIT;

	    %LET K=1;
	    %LET VAR=%SCAN(&VARS,&K);
	    %DO %WHILE(&VAR NE);
		    %LET CHECK=0;
		    PROC SQL NOPRINT;
		      SELECT COUNT(*) INTO :CHECK
		      FROM   SVAL_IFS_&F
		      WHERE  VARIABLE = "&VAR"
		      ;
		    QUIT;
		    %IF &CHECK EQ 0 %THEN %DO;
		        PROC SQL;
		          INSERT INTO DIRTYIFS_&F
		          VALUES ("&VAR")
		          ;
		        QUIT;
		    %END;
		    %LET K=%EVAL(&K+1);
		    %LET VAR=%SCAN(&VARS,&K);
	    %END;
	%END;
    %PUT *I* PURGE TRANSMISSION GUIDELINE VARIABLES IMPUTATION FACTORS;
    PROC SQL;
      DELETE FROM DIRTYIFS_&F WHERE  VARIABLE IN (SELECT compress(VARIABLE)||'_IF' FROM SVAL_TRANS_LIST_&F);
    QUIT;
%MEND chkUNRECOGNIZED_FLAG_IF;