/**
 * Revision History
 * - November 14th 2022
 */
%MACRO comp_DERIVED;

%PUT *I* XML_DERIVED: COMPUTING AGE & RHID ...
;
DATA RAW.&ss&cc&YY.R;
SET RAW.&ss&cc&YY.R;
        IF RB080_F = 1 AND RB081 = . THEN AGE = RB010 - RB080 - 1;
        ELSE AGE = RB081;
        %IF &SS EQ C %THEN %DO;
        RHID = INT(RB030/100);
        %END;
        %ELSE %DO;
        RHID = RB040;
        %END;
RUN;

%PUT *I* XML_DERIVED: COMPUTING CALENDAR ....
;
/*
        PL211A: Main activity in January
        PL211B: Main activity in February
        PL211C: Main activity in March
        ....
        PL211L: Main activity in December
        1 Employee working full-time
        2 Employee working part-time
        3 Self-employed working full-time (including family worker)
        4 Self-employed working part-time (including family worker)
        5 Unemployed
        6 Pupil, student, further training, unpaid work experience
        7 In retirement or in early retirement or has given up business
        8 Permanently disabled or/and unfit to work
        9 In compulsory military or community service
        10 Fulfilling domestic tasks and care responsibilities
        11 Other inactive person
*/
%IF &SS EQ R %THEN %DO;

    DATA RAW.&ss&cc&YY.P;
    SET RAW.&ss&cc&YY.P;
                if 1 = 1 then do;
                        array actcal_11 (*) PL211A PL211B PL211C PL211D PL211E PL211F
                                     PL211G PL211H PL211I PL211J PL211K PL211L;
                        array actcalf_11 (*) PL211A_F PL211B_F PL211C_F PL211D_F PL211E_F PL211F_F
                                      PL211G_F PL211H_F PL211I_F PL211J_F PL211K_F PL211L_F;
                        array pl211 (*) PL211_1 PL211_2 PL211_3 PL211_4 PL211_5
                                                PL211_6 PL211_7 PL211_8 PL211_9 PL211_10 PL211_11;

                        do i = 1 to dim(pl211);
                                pl211(i) = 0;
                        end;

                        do i = 1 to dim(actcal_11);
                           if actcalf_11(i) >= 1 and actcal_11(i) ne 0 then do;
                                   pl211(actcal_11(i)) = pl211(actcal_11(i)) + 1;
                           end;
                        end;
                end;
    RUN;

%END;

%PUT *I* XML_DERIVED: RECONCILIATION (P vs R) ...
;
        DATA SELRES (KEEP=RB010 RB030 RB245 AGE RHID RENAME=(RB010=PB010 RB030=PB030 RB245=SELRES RHID=PHID));
    SET RAW.&ss&cc&YY.R (WHERE=(RB110 IN (1 2 3 4)));
    PROC SORT NODUPKEY OUT=SELRES (INDEX=(PK=(PB010 PB030)/UNIQUE));
          BY PB010 PB030;
        RUN;

        DATA RAW.&ss&cc&YY.P;
        SET  RAW.&ss&cc&YY.P;
        SET  SELRES KEY=PK/UNIQUE;
                _ERROR_ = 0;
        RUN;

%PUT *I* XML_DERIVED: COMPUTING HGROINC ...
;
DATA RAW.&ss&cc&YY.H;
SET  RAW.&ss&cc&YY.H;
    HGROINC = SUM(HY040G,HY050G,HY060G,HY070G,HY080G,HY090G,HY110G,0);
    %IF &YYYY lt 2022 %THEN %DO;
        HNINCS = ROUND(SUM(HY050G,HY060G,HY070G,0),1);
    %END;
    %ELSE %DO;
        HNINCS = ROUND(SUM(HY050N,HY060N,HY070N,0),1);
    %END;
    %IF &YYYY lt 2022 %THEN %DO;
        HNETINC = HGROINC - sum(HY120G,HY130G,HY140G,0);
    %END;
    %ELSE %DO;
        HNETINC = sum(HY040N,HNINCS,HY080N,HY090N,HY110N,0);
    %END;
RUN;


%PUT *I* XML_DERIVED: COMPUTING PGROINC ...
;
DATA RAW.&ss&cc&YY.P;
SET  RAW.&ss&cc&YY.P;
    PGROINC = ROUND(SUM(PY010G,PY021G,PY050G,PY080G,PY090G,PY100G,PY110G,PY120G,PY130G,PY140G,0),1);
    %IF &YYYY lt 2022 %THEN %DO;
        PNINCS = ROUND(SUM(PY090G,PY120G,PY130G,PY140G,0),1); *social benefits without old-age;
    %END;
    %ELSE %DO;
        PNINCS = ROUND(SUM(PY090N,PY120N,PY130N,PY140N,0),1); *social benefits without old-age;
    %END;
    %IF &YYYY lt 2022 %THEN %DO;
        PNINCP = ROUND(SUM(PY100G,PY110G,0),1); *old-age;
    %END;
    %ELSE %DO;
        PNINCP = ROUND(SUM(PY100N,PY110N,0),1); *old-age;
    %END;
    %IF &YYYY lt 2022 %THEN %DO;
        PNETINC = PGROINC; *total to calculate net;
    %END;
    %ELSE %DO;
        PNETINC = SUM(PY010N,PY021N,PY050N,PY080N,PNINCS,PNINCP,0);
    %END;
RUN;


DATA FILE_H_temp;
SET  RAW.&ss&cc&YY.H;
RUN;

PROC SQL;
   CREATE TABLE RAW.&ss&cc&YY.H AS
   SELECT distinct A.*,
     ROUND(A.HGROINC + sum(B.PGROINC),1) AS GROSS,
     ROUND(CALCULATED GROSS - SUM(A.HY120G, A.HY130G, A.HY140G,0),1)  AS NET20,
     ROUND(CALCULATED NET20 - A.HNINCS - SUM(B.PNINCS),1) AS NET22,
     ROUND(CALCULATED NET22 - SUM(B.PNINCP),1) AS NET23
   FROM  FILE_H_temp as A LEFT JOIN RAW.&ss&cc&YY.P as B
   ON    PHID = HB030 AND PB010 = HB010
   GROUP BY HB030, HB010
   ;
QUIT;


        PROC SORT data=RAW.&ss&cc&YY.D;
        by DB030 DB010;
        run;

        PROC SORT data=RAW.&ss&cc&YY.R;
        by RHID RB030 RB010;
        run;

        PROC SORT data=RAW.&ss&cc&YY.H;
        by HB030 HB010;
        run;

        PROC SORT data=RAW.&ss&cc&YY.P;
        by PHID PB030 PB010;
        run;

%MEND comp_DERIVED;

