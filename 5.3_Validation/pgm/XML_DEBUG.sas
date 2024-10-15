
/**
 * DATA ALTERATION FOR DEBUG PURPOSES
 * ----------------------------------
 * OUTPUT:
 * [] FILE_&F
 */
%MACRO ALTER_DF;

%PUT *I* ALTER DF (OFF)
;
DATA FILE_&F._2021;
SET  FILE_&F;
  IF &F.B010 = 2021;
RUN;

DATA FILE_&F._2022;
SET  FILE_&F._2021;
     &F.B010 = 2022;
     %IF &F eq H %THEN %DO;
         HB060 = HB060 + 1;
     %END;
     %IF &F eq R %THEN %DO;
         RB081 = RB081 + 1;
         RB082 = RB082 + 1;
     %END;
RUN;

DATA FILE_&F;
SET  FILE_&F;
  IF &F.B010 ge 2019;
RUN;


PROC FREQ DATA=FILE_&F;
     TABLE &F.B010;
RUN;

PROC APPEND DATA=FILE_&F._2022 BASE=FILE_&F FORCE;
RUN;


%IF &F eq R %THEN %DO;

    DATA FILE_&F;
    SET  FILE_&F;
         IF RB110 ne 8;
    RUN;

    DATA FILE_&F (DROP=I);
    SET  FILE_&F;
         ARRAY K  RL030   RL040   RL050   RL060   RL070  ;
         ARRAY KF RL030_F RL040_F RL050_F RL060_F RL070_F;
         IF RB010 eq 2022 AND RB082 eq 13 THEN DO;
            DO I = 1 TO DIM(K);
               K[I] = .; KF[I] = -2;
            END;
         END;
    RUN;

%END;

%IF &F eq D %THEN %DO;

    DATA FILE_&F;
    SET  FILE_&F;
         IF DB010 = 2021 and DB095_F = -2 THEN DO;
            DB095   = DB090;
            DB095_F = DB090_F;
         END;
    RUN;

%END;


%IF &F eq H %THEN %DO;

    DATA FILE_&F (DROP=I);
    SET  FILE_&F;
      ARRAY K   HS200   HS210   HS220;
      ARRAY KF  HS200_F HS210_F HS220_F;
      IF HB010 = 2022 THEN DO;
         DO I = 1 TO DIM(K);
            K[I]   = ROUND( RAND('UNIFORM') * 2, 1) + 1;
            KF[I] = 1;
            IF ROUND( RAND('UNIFORM') * 1200, 1) = 1 THEN DO;
                K[I]   = 0;
                KF[I]  = 1;
            END;
            IF ROUND( RAND('UNIFORM') * 250, 1) = 0 THEN DO;
                K[I]   = .;
                KF[I] = -1;
            END;
            IF ROUND( RAND('UNIFORM') * 450, 1) = 0 THEN DO;
                K[I]   = .;
                KF[I]  = -3;
            END;
            IF ROUND( RAND('UNIFORM') * 500, 1) = 0 THEN DO;
                K[I]   = .;
                KF[I]  = 1;
            END;
            IF ROUND( RAND('UNIFORM') * 600, 1) = 0 THEN DO;
                K[I]   = .;
                KF[I]  = -7;
            END;
            IF ROUND( RAND('UNIFORM') * 1000, 1) = 1 THEN DO;
                K[I]   = 1;
                KF[I]  = -1;
            END;

         END;
      END;
      ELSE DO;
         DO I = 1 TO DIM(K);
                K[I]   = .;
                KF[I] = -7;
         END;
      END;
    RUN;


%END;


%IF &F eq P %THEN %DO;
    %let F=P;
    DATA FILE_&F (DROP=I _N_ERR:);
    SET  FILE_&F;
	  RETAIN _N_ERR01_ _N_ERR02_ _N_ERR03_ _N_ERR04_ 0;
      ARRAY V   PS010   PS020   PS030	PS040;
      ARRAY VF  PS010_F PS020_F PS030_F PS040_F;
	  ARRAY UB(4) _TEMPORARY_ ( 7 6 6 7 );
      ARRAY K   PW030   PW120   PW160;
      ARRAY KF  PW030_F PW120_F PW160_F;
      ARRAY J   PS011   PS021   PS031    PS040B;
      ARRAY JF  PS011_F PS021_F PS031_F  PS040B_F;
      IF PB010 = 2022 THEN DO;
         DO I = 1 TO DIM(V);
            V[I]   = ROUND( RAND('UNIFORM') * UB[I], 1) + 1;
            VF[I] = 1;
		 END;
         DO I = 1 TO DIM(K);
            K[I]   = ROUND( RAND('UNIFORM') * 10, 1) + 0;
            KF[I] = 1;
            IF ROUND( RAND('UNIFORM') * 1200, 1) = 1 THEN DO;
                K[I]   = .;
                KF[I]  = -1;
            END;
         END;
         DO I = 1 TO DIM(J);
		    J[I] = ROUND( RAND('UNIFORM') * 1, 1) + 1;
		    JF[I] = 1;
            IF ROUND( RAND('UNIFORM') * 1100, 1) = 1 THEN DO;
                J[I]   = .;
                JF[I]  = -1;
            END;
		 END;
		 /* ROUTING CONDITIONS */
         IF PS010 = 8 AND _N_ERR01_ lt 3 THEN DO;
            PS011 =.;
			PS011_F = -2;
			_N_ERR01_ + 1;
		 END;
         IF PS020 = 7 AND _N_ERR02_ lt 3 THEN DO;
            PS021 =.;
			PS021_F = -2;
			_N_ERR02_ + 1;
		 END;
         IF PS030 = 7 AND _N_ERR03_ lt 3 THEN DO;
            PS031 =.;
			PS031_F = -2;
			_N_ERR03_ + 1;
		 END;
         IF PS040 = 8 AND _N_ERR04_ lt 3 THEN DO;
            PS040B =.;
			PS040B_F = -2;
			_N_ERR04_ + 1;
		 END;
      END;
      ELSE DO;
         DO I = 1 TO DIM(V);
            	V[I]   = .;
            	VF[I]  = -7;
		 END;
         DO I = 1 TO DIM(K);
                K[I]   = .;
                KF[I]  = -7;
         END;
         DO I = 1 TO DIM(J);
                J[I]   = .;
                JF[I]  = -7;
         END;
      END;
    RUN;

%END;

%IF &F eq R %THEN %DO;

    DATA FILE_&F (DROP=I);
    SET  FILE_&F;
      IF RB010 = 2022 THEN DO;
		 RL080 = ROUND( RAND('UNIFORM') * 99, 1) + 0;
		 RL080_F = 1;
         IF ROUND( RAND('UNIFORM') * 1, 1) = 1 THEN DO;
            RL080   = .;
            RL080_F = -2;
         END;
      END;
      ELSE DO;
         RL080 = .;
         RL080_F = -7;
      END;
    RUN;

%END;

%MEND ALTER_DF;
