/**
 * Revision History
 * - November 17th 2022
 */
/**
 * --------------------- INCOME VARIABLE VALIDATION ------------
 * SPLIT_INCOME
 */

/*
------------
EX-ANTE 2021
------------
Specific values:
0 No income
-1 Missing (not allowed for most income component)
-5 Not filled - variable of net series is filled

Otherwise income flag is the concatenation of various digits:
[] 1st digit: collected net or gross
[] 2nd digit: imputation method
[] 3rd and following digits: imputation factor (collected/recorded)

1st digit – Collected net or gross digit
> 1 Net
> 2 Gross
> 3 Net and gross (part of the component collected net, part gross)
> 4 Unknown

2nd digit – Imputation method digit (refer to the main source of imputation used for the components)
> 0 no imputation
> 1 deductive imputation
> 2 statistical imputation
> 3 gross/net conversion

3rd and following digit – Imputation factor
> Imputation factor = collected value / recorded value (in percent with no decimal)
  Example: if the collected value is 912 Euros and the recorded value after imputation is 1000 Euros then the three digits for the imputation factor will be 091.
*/
/**
 * @IMPUTE = Y -> handle the 2021-COMPLIANT incomve variable
 * @IMPUTE = N -> handle the EX-ANTE income variable
 */
%MACRO SPLIT_INCOME_XML(F=D,IMPUTE=);

%LET DS=RAW.&_SPLIT_MODE_&ss&cc&YY&F;

%LOCAL K RC CHECK VARS VAR CNT
;


%IF &IMPUTE EQ N %THEN %DO;
        PROC SQL NOPRINT;
          SELECT TRIM(VARIABLE) || ':' || PUT(FLAG_SUBSTR,Z1.) || ':' || PUT(FLAG_SPLIT,Z.1) INTO :VARS SEPARATED BY ' '
          FROM   SVAL_&F
          WHERE  INCOME = 'Y'
          AND    IMPUTE ^= 'Y'
          ;
        QUIT;

        DATA &DS;
        SET  &DS;
                %LET K=1;
                %LET VAR=%SCAN(&VARS,&K);
                %DO %WHILE(%QUOTE(&VAR) NE);
                        %LET VARX=%SCAN(&VAR,1,:);
                        %LET SUB=%SCAN(&VAR,2,:);
                        %LET SPL=%SCAN(&VAR,3,:);
                        &VARX._F_ante = COMPRESS(PUT(&VARX._F,BEST.));
                        IF SUBSTR( &VARX._F_ante, 1, 1 ) IN ( '-' '0' ) THEN DO;
                           &VARX._F = &VARX._F;
                           &VARX._IF = .;
                           DROP &VARX._F_ante;
                        END;
                        ELSE DO;
                        &VARX._F_S = SUBSTR(&VARX._F_ante,1,&SUB);
                        &VARX._I_S = SUBSTR(&VARX._F_ante,&SPL);
                                &VARX._F = INPUT(&VARX._F_S,BEST.);
                                &VARX._IF = INPUT(&VARX._I_S,BEST.);
                            DROP &VARX._F_ante &VARX._F_S &VARX._I_S;
                        END;
                        %LET K=%EVAL(&K+1);
                        %LET VAR=%SCAN(&VARS,&K);
                %END;
        RUN;

%END;
%ELSE %DO; /* IMPUTE = Y */

	PROC SQL NOPRINT;
	 SELECT COUNT(*) INTO :CNT
	 FROM   SVAL_&F
	 WHERE  INCOME = 'Y'
	 AND    IMPUTE = 'Y'
	 ;
	QUIT;

	DATA NOIMPUTE OKIMPUTE ;
	  LENGTH VARIABLE $12;
	  STOP;
	RUN;

   %IF &CNT gt 0 %THEN %DO;
        PROC SQL NOPRINT;
          SELECT VARIABLE INTO :VARS SEPARATED BY ' '
          FROM   SVAL_&F
          WHERE  INCOME = 'Y'
          AND    IMPUTE = 'Y'
          ;
        QUIT;

        DATA MISMIMPUTE;
          LENGTH VARIABLE $12;
          LENGTH FACTOR $12;
          STOP;
        RUN;

        %LET dsid = %SYSFUNC(OPEN(&DS));

        %LET K=1;
        %LET VAR=%SCAN(&VARS,&K);
        %DO %WHILE(&VAR NE);
                %LET CHECK = %SYSFUNC(VARNUM(&dsid,&VAR._IF));
                %IF &CHECK EQ 0 %THEN %DO;
                        PROC SQL;
                                INSERT INTO NOIMPUTE
                                VALUES ("&VAR")
                            ;
                        QUIT;
                %END;
                %ELSE %DO;
                        %LET TYP = %SYSFUNC(VARTYPE(&dsid,&CHECK));
                        %IF &TYP EQ N %THEN %DO;
                        PROC SQL;
                                INSERT INTO OKIMPUTE
                                VALUES ("&VAR")
                            ;
                        QUIT;
                        %END;
                        %ELSE %DO;
                        PROC SQL;
                                INSERT INTO MISMIMPUTE
                                VALUES ("&VAR", "&VAR._IF")
                            ;
                        QUIT;
                        %END;
                %END;
                %LET K=%EVAL(&K+1);
                %LET VAR=%SCAN(&VARS,&K);
        %END;

        %LET rc=%SYSFUNC(CLOSE(&dsid));

        %fix_MISMIMPUTE;

        PROC SQL NOPRINT;
          SELECT VARIABLE INTO :VARS SEPARATED BY ' '
          FROM   OKIMPUTE
          ;
        QUIT;


        DATA &DS (DROP=VARIABLE VALUE)
                 ERRIMPUTE (KEEP=&F.B010 &F.B020 &F.B030 VARIABLE VALUE)
        ;
        SET  &DS;
                %LET K=1;
                %LET VAR=%SCAN(&VARS,&K);
                %DO %WHILE(&VAR NE);
                        IF &VAR._IF ^= . THEN DO;
                           IF &VAR._IF < -999999.99 OR &VAR._IF > 999999.99 THEN DO;
                              VARIABLE = "&VAR";
                              VALUE = &VAR._IF;
                              OUTPUT ERRIMPUTE;
                           END;
                        END;
                        %LET K=%EVAL(&K+1);
                        %LET VAR=%SCAN(&VARS,&K);
                %END;
                OUTPUT &DS;
        RUN;

		DATA SVAL_IMPUTE_&F;
		    SET  OKIMPUTE (WHERE=(SUBSTR(VARIABLE,1,1) = "&F"));
		RUN;

        %IF &_SPLIT_MODE_ EQ _ANTE_ AND &_SPLIT_CUT_ eq 2020 %THEN %DO;

          %IF %SYSFUNC(EXIST(NOIMPUTE)) %THEN %DO;
            PROC SQL;
              DROP TABLE NOIMPUTE;
            QUIT;
          %END;

          %IF %SYSFUNC(EXIST(OKIMPUTE)) %THEN %DO;
            PROC SQL;
              DROP TABLE OKIMPUTE;
            QUIT;
          %END;

        %END;

   %END;
   %ELSE %DO;

      %IF %SYSFUNC(EXIST(NOIMPUTE)) %THEN %DO;
          PROC SQL;
            DROP TABLE NOIMPUTE;
          QUIT;
      %END;

      %IF %SYSFUNC(EXIST(OKIMPUTE)) %THEN %DO;
          PROC SQL;
            DROP TABLE OKIMPUTE;
          QUIT;
      %END;

   %END;

%END;

%MEND SPLIT_INCOME_XML;


%MACRO fix_MISMIMPUTE;

%LOCAL MISMVARS MISMFACTORS N_MISM;

%LET N_MISM=0;
DATA _NULL_;
SET  MISMIMPUTE;
  CALL SYMPUTX('N_MISM',_N_);
RUN;

%IF &N_MISM gt 0 %THEN %DO;

        PROC SQL NOPRINT;
          SELECT DISTINCT FACTOR INTO :MISMFACTORS SEPARATED BY ' '
          FROM   MISMIMPUTE
          ;
        QUIT;

        %PUT *I* FIXING MISMTACH ON THE FOLLOWING IMP FACTORS ...
        ;
        %PUT *I* &MISMFACTORS
        ;
        DATA &DS;
        SET  &DS (DROP=&MISMFACTORS);
          LENGTH &MISMFACTORS 8;
        RUN;

%END;

%MEND fix_MISMIMPUTE;

%MACRO TEST;

%SPLIT_INCOME_XML(F=H);

%MEND TEST;
