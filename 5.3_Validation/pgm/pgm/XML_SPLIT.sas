/**
 * Revision History
 * - November 17th 2022
 */
%MACRO mrg_SPLITS (F=);

%LOCAL EXIST_ANTE EXIST_POST;

%IF %SYSFUNC(EXIST(RAW._POST_&ss&cc&YY&F)) %THEN %LET EXIST_POST=1;
%ELSE %LET EXIST_POST=0;

%IF %SYSFUNC(EXIST(RAW._ANTE_&ss&cc&YY&F)) %THEN %LET EXIST_ANTE=1;
%ELSE %LET EXIST_ANTE=0;

%IF &EXIST_ANTE * &EXIST_POST EQ 1 %THEN %DO;

    DATA RAW.&ss&cc&YY&F;
    SET  RAW._POST_&ss&cc&YY&F;
        PROC APPEND FORCE
          BASE=RAW.&ss&cc&YY&F
          DATA=RAW._ANTE_&ss&cc&YY&F
          ;
        RUN;

%END;
%ELSE %IF &EXIST_ANTE EQ 1 AND &EXIST_POST EQ 0 %THEN %DO;
    PROC APPEND FORCE
      BASE=RAW.&ss&cc&YY&F
      DATA=RAW._ANTE_&ss&cc&YY&F
      ;
    RUN;
%END;
%ELSE %IF &EXIST_POST EQ 1 %THEN %DO;
    DATA RAW.&ss&cc&YY&F;
    SET  RAW._POST_&ss&cc&YY&F;
        RUN;
%END;
%ELSE %DO;
   %PUT *I* NO SPLIT DATASET FOUND (ANTE:&EXIST_ANTE, POST:&EXIST_POST)
   ;
%END;

%MEND mrg_SPLITS;

%MACRO purge_SPLITS (F=);
/*if there is an _ante_ and _post_*/
%IF &_SPLIT_MODE_ NE %THEN %DO;
    %PUT *I* PURGING SPLIT &F (&_SPLIT_MODE_) WITH CUTOFF=&_SPLIT_CUT_ ;
    DATA RAW.&_SPLIT_MODE_&ss&cc&YY&F;
    SET  RAW.&_SPLIT_MODE_&ss&cc&YY&F;
          %IF &_SPLIT_MODE_ EQ _ANTE_ %THEN %DO;
          IF &F.B010 LE &_SPLIT_CUT_;
          %END;
          %ELSE %IF &_SPLIT_MODE_ EQ _POST_ %THEN %DO;
          IF &F.B010 GT &_SPLIT_CUT_;
          %END;
        RUN;

%END;

%MEND purge_SPLITS;

%MACRO svalxml_SPLITS;

%IF &_SPLIT_MODE_ EQ _ANTE_ %THEN %DO;

        DATA SVAL_POST_ORG_;
        SET  SVAL_POST_;
        RUN;

        PROC SQL;
          DELETE FROM SVAL_POST_
          WHERE  VARIABLE IN
          ( SELECT VARIABLE FROM SVAL_ANTE_ )
          ;
        QUIT;


        PROC SQL;
          DELETE FROM SVAL_FLAGS_POST_
          WHERE  VARIABLE IN
          ( SELECT VARIABLE FROM SVAL_FLAGS_ANTE_ )
          ;
        QUIT;

        PROC SQL;
          DELETE FROM SVAL_NAS_POST_
          WHERE  VARIABLE IN
          ( SELECT VARIABLE FROM SVAL_NAS_ANTE_ )
          ;
        QUIT;

        PROC SQL;
          DELETE FROM SVAL_DISPS_POST_
          WHERE  VARIABLE IN
          ( SELECT VARIABLE FROM SVAL_DISPS_ANTE_ )
          ;
        QUIT;

        /**
         * APPEND
         */
		PROC APPEND
			BASE=SVAL_MISSTHRES_ANTE_
			DATA=SVAL_MISSTHRES_POST_;
		RUN;

		DATA SVAL_MISSTHRES;
			SET  SVAL_MISSTHRES_ANTE_ (OBS=1);
		RUN;

		PROC APPEND
          BASE=SVAL_ANTE_
          DATA=SVAL_POST_
          ;
        DATA SVAL;
        SET  SVAL_ANTE_;
        PROC SORT NODUPKEY;
          BY VARIABLE;
        RUN;

        PROC APPEND
          BASE=SVAL_FLAGS_ANTE_
          DATA=SVAL_FLAGS_POST_
          ;
        DATA SVAL_FLAGS;
        SET  SVAL_FLAGS_ANTE_;
        PROC SORT NODUPKEY;
          BY VARIABLE;
        RUN;

        DATA SVAL_FLAG_LEN;
        SET  SVAL_FLAG_LEN_ANTE_;
        PROC SORT NODUPKEY;
          BY VARIABLE;
        RUN;

        PROC APPEND
          BASE=SVAL_NAS_ANTE_
          DATA=SVAL_NAS_POST_ (WHERE=(NA_FLAG IN (-7,-8)))
          ;
        DATA SVAL_NAS;
        SET  SVAL_NAS_ANTE_;
        PROC SORT;
          BY VARIABLE;
        RUN;

        PROC APPEND
          BASE=SVAL_DISPS_ANTE_
          DATA=SVAL_DISPS_POST_
          ;
        DATA SVAL_DISPS;
        SET  SVAL_DISPS_ANTE_;
        PROC SORT;
          BY VARIABLE;
        RUN;

        /* --- NOT TO BE MERGED DATASETS --- */

        DATA SVAL_HH_MBR_STAT_LST;
        SET  SVAL_HH_MBR_STAT_LST_ANTE_;
        RUN;

        DATA SVAL_HH_MEMBER_MANDATORY;
        SET  SVAL_HH_MEMBER_MANDATORY_ANTE_;
        RUN;

        DATA Sval_chksum;
        SET  Sval_chksum_ANTE_;
        RUN;

        DATA Sval_parent;
        SET  Sval_parent_ANTE_;
        RUN;

        DATA Sval_selresp;
        SET  Sval_selresp_ANTE_;
        RUN;

        DATA Sval_selresp_cntry_lst;
        SET  Sval_selresp_cntry_lst_ANTE_;
        RUN;

        DATA Sval_brk_lov;
        SET  Sval_brk_lov_ANTE_;
        RUN;

        DATA Sval_brk_flag;
        SET  Sval_brk_flag_ANTE_;
        RUN;

%END;

%MEND svalxml_SPLITS;
