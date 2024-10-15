/**
 * Revision History:
 * October 04th 2022
 */

/**
 * SPECIFIC POST-PROCESSING AIMED TO
 * CREATE THE FOLLOWING VARIABLES
 * [] __HH_AGE_0_12_
 * [] __HH_AGE_0_15_
 * [] __HH_AGE_0_17_
 */
%MACRO HH_AGE_0_15;

DATA R;
SET  R;
  IF RB082 = . AND RB010 lt 2021 THEN DO;
     IF RB080_F = 1 THEN RB082 = RB010 - RB080 - 1;
  END;
PROC SORT;
  BY RB040 RB010;
RUN;

PROC MEANS NWAY DATA=R (WHERE=(RB110 IN (1, 2, 3, 4))) NOPRINT;
  CLASS RB040 RB010;
  OUTPUT OUT=R_HH_MNS
    MIN(RB081 RB082) = RB081 RB082
  ;
DATA _LAST_ (DROP=RB081 RB082);
SET  _LAST_ (DROP=_:);
  IF RB082 >= 0 AND RB082 <= 12 THEN __HH_AGE_0_12_ = 1;
  ELSE __HH_AGE_0_12_ = 0;
  IF RB081 >= 0 AND RB081 <= 15 THEN __HH_AGE_0_15_ = 1;
  ELSE __HH_AGE_0_15_ = 0;
  IF RB082 >= 0 AND RB082 <= 17 THEN __HH_AGE_0_17_ = 1;
  ELSE __HH_AGE_0_17_ = 0;
RUN;

DATA  R;
MERGE R R_HH_MNS;
  BY RB040 RB010;
RUN;

PROC SORT DATA=P;
  BY PHID PB010;
RUN;

DATA P;
MERGE P (IN=A) R_HH_MNS (rename=(RB040=PHID RB010=PB010));
  BY PHID PB010;
  IF A;
RUN;

PROC SORT DATA=H;
  BY HB030 HB010;
RUN;

DATA H;
MERGE H (IN=A) R_HH_MNS (rename=(RB040=HB030 RB010=HB010));
  BY HB030 HB010;
  IF A;
RUN;

%MEND HH_AGE_0_15;


/**
 * SPECIFIC POST-PROCESSING AIMED TO
 * CREATE THE FOLLOWING VARIABLES
 * [] __HH_PK010_F_m2_
 */
%MACRO HH_PK010_F;

PROC MEANS NWAY DATA=P NOPRINT;
  CLASS PHID PB010;
  OUTPUT OUT=P_HH_MNS
    MIN(PK010_F) = MIN_PK010_F
    MAX(PK010_F) = MAX_PK010_F
  ;
DATA _LAST_;
SET;
  IF MIN_PK010_F = -2 AND MAX_PK010_F = -2 THEN __HH_PK010_F_ALLM2_ = 1;
  ELSE __HH_PK010_F_ALLM2_ = 0;
RUN;

PROC SORT DATA=H;
  BY HB030 HB010;
RUN;

DATA H;
MERGE H (IN=A)  P_HH_MNS (rename=(PHID=HB030 PB010=HB010));
  BY HB030 HB010;
  IF A;
RUN;

%MEND  HH_PK010_F;



/**
 * SPECIFIC POST-PROCESSING AIMED TO
 * CREATE THE FOLLOWING VARIABLES
 * [] __HH_RL020_
 */
%MACRO HH_RL020;

PROC MEANS NWAY DATA=R NOPRINT;
  CLASS RB040 RB010;
  OUTPUT OUT=R_HH_RL020
    SUM(RL020) = __HH_RL020_
  ;
RUN;

PROC SORT DATA=H;
  BY HB030 HB010;
RUN;

DATA H;
MERGE H (IN=A) R_HH_RL020 (rename=(RB040=HB030 RB010=HB010));
  BY HB030 HB010;
  IF A;
RUN;

%MEND  HH_RL020;

%MACRO RL010_060_SUM;
DATA R;
SET R;
__RL0x0_SUM= sum(RL010,RL020,RL030,RL040,RL050,RL060);
RUN;
%MEND RL010_060_SUM;

%MACRO PAR_CHI_AGE_DIFF;
PROC SQL;
	CREATE TABLE AGE_MOTHER AS SELECT 
	    a.RB010,a.RB030,b.AGE as __AGE_M
	    FROM R a LEFT JOIN R b 
		ON a.RB230 = b.RB030 and a.RB010 = b.RB010 
	    WHERE a.RB230_F = 1 ;
	CREATE TABLE AGE_FATHER AS SELECT 
	    a.RB010,a.RB030,b.AGE as __AGE_F
	    FROM R a LEFT JOIN R b 
		ON a.RB220 = b.RB030 and a.RB010 = b.RB010 
	    WHERE a.RB220_F = 1 ;
	QUIT;
RUN;
PROC SORT DATA=AGE_MOTHER NODUPKEY;BY RB010 RB030;RUN;
PROC SORT DATA=AGE_FATHER NODUPKEY;BY RB010 RB030;RUN;
PROC SORT DATA=R ;BY RB010 RB030;RUN;
DATA R;
	MERGE R (in=A) AGE_MOTHER AGE_FATHER;
	BY RB010 RB030;
	IF A;
	__AGE_DIFF_M= __AGE_M - AGE;
	__AGE_DIFF_F= __AGE_F - AGE;
RUN;
%MEND PAR_CHI_AGE_DIFF;


PROC FORMAT;
     VALUE $PRELABEL
     __HH_AGE_0_15_   = '__HH_AGE_0_15_ - any children aged 0-15'
     __HH_AGE_0_17_   = '__HH_AGE_0_17_ - any children aged 0-17'
	 __RL0x0_SUM      = "__RL0x0_SUM - sum of Childcare variables RL010 to RL060"
     __HH_PK010_F_ALLM2_ = '__HH_PK010_F_ALLM2_ - all HH members having PK010_F -2'
     __HH_RL020_      = '__HH_RL020_ - sum RL020 at HH level'
	 __AGE_DIFF_M= "__AGE_DIFF_M - Age difference with mother"
	 __AGE_DIFF_F= "__AGE_DIFF_F - Age difference with father"
     __AGE_M = "AGE - Mother's age at the end of income reference period"
     __AGE_F = "AGE - Father's age at the end of income reference period"
     SELRES = 'SELRES - Selected respondent (=RB245-Respondent status)'
     RHID = 'RHID - Household ID (R-File)'
     PHID = 'PHID - Household ID (P-File)'
     AGE  = "AGE - Age at the end of income reference period"
	 GROSS  = "GROSS = HY040G + HY050G + HY060G + HY070G + HY080G + HY090G + HY110G + SUM (PY010G,PY021G,PY050G,PY080G,PY090G,PY100G,PY110G,PY120G,PY130G,PY140G)" 
	 HGROINC  = "HGROINC = HY040G + HY050G + HY060G + HY070G + HY080G + HY090G + HY110G" 
	 PGROINC  = "PGROINC = SUM (PY010G,PY021G,PY050G,PY080G,PY090G,PY100G,PY110G,PY120G,PY130G,PY140G)" 
/*	 
	 NET20  = "NET20 = HY040G + HY050G + HY060G + HY070G + HY080G + HY090G + HY110G + SUM (PY010G,PY021G,PY050G,PY080G,PY090G,PY100G,PY110G,PY120G,PY130G,PY140G) - HY120G - HY130G - HY140G" 
	 NET22  = "NET22 = HY040G + HY050G + HY060G + HY070G + HY080G + HY090G + HY110G + SUM (PY010G,PY021G,PY050G,PY080G,PY090G,PY100G,PY110G,PY120G,PY130G,PY140G) - HY120G - HY130G - HY140G - HY050N - HY060N - HY070N - SUM (PY090N,PY120N,PY130N,PY140N)" 
	 NET23  = "NET23 = HY040G + HY050G + HY060G + HY070G + HY080G + HY090G + HY110G + SUM (PY010G,PY021G,PY050G,PY080G,PY090G,PY100G,PY110G,PY120G,PY130G,PY140G) - HY120G - HY130G - HY140G - HY050N - HY060N - HY070N - SUM (PY090N,PY100N,PY110N,PY120N,PY130N,PY140N)" 
*/
	 NET20  = "NET20 = GROSS - HY120G - HY130G - HY140G" 
	 NET22  = "NET22 = NET20 - HY050N - HY060N - HY070N - SUM (PY090N,PY120N,PY130N,PY140N)" 
	 NET23  = "NET23 = NET20 - HY050N - HY060N - HY070N - SUM (PY090N,PY100N,PY110N,PY120N,PY130N,PY140N)" 

	PL211_1  = "PL211_1  - Main activity (Nb of months) : Employee working full-time"
	PL211_2  = "PL211_2  - Main activity (Nb of months) : Employee working part-time"
	PL211_3  = "PL211_3  - Main activity (Nb of months) : Self-employed working full-time (including family worker)"
	PL211_4  = "PL211_4  - Main activity (Nb of months) : Self-employed working part-time (including family worker)"
	PL211_5  = "PL211_5  - Main activity (Nb of months) : Unemployed"
	PL211_6  = "PL211_6  - Main activity (Nb of months) : Student, pupil"
	PL211_7  = "PL211_7  - Main activity (Nb of months) : Retired"
	PL211_8  = "PL211_8  - Main activity (Nb of months) : Unable to work due to long-standing health problems"
	PL211_9  = "PL211_9  - Main activity (Nb of months) : Compulsory military or civilian service"
	PL211_10 = "PL211_10 - Main activity (Nb of months) : Fulfilling domestic tasks"
	PL211_11 = "PL211_11 - Main activity (Nb of months) : Other"
     ;
RUN;

