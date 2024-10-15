%MACRO rep_SUMMARY;
/**
 * CONCATENATING TABLES DERIVED FROM D,H,R,P FILES and _ANTE_/_POST_ MODE 
 */

%IF %sysfunc(exist(MISSVARS_D_ANTE_)) %THEN %DO;
	DATA MISSVARS_ANTE;
	set 
		MISSVARS_D_ANTE_ (in=a)
		MISSVARS_H_ANTE_ (in=b)
		MISSVARS_R_ANTE_ (in=c)
		MISSVARS_P_ANTE_ (in=d)
	;
	order=a+b*2+c*3+d*4;
	if a then file="D-file";
	if b then file="H-file";
	if c then file="R-file";
	if d then file="P-file";
	RUN;
	DATA MISSVARS;
	set 
		MISSVARS_D_POST_ (in=a)
		MISSVARS_H_POST_ (in=b)
		MISSVARS_R_POST_ (in=c)
		MISSVARS_P_POST_ (in=d)
	;
	order=a+b*2+c*3+d*4;
	if a then file="D-file";
	if b then file="H-file";
	if c then file="R-file";
	if d then file="P-file";
	RUN;
	PROC SORT NODUPKEY;BY VARIABLE;RUN;
	PROC SORT;BY order VARIABLE;RUN;
%END;
%ELSE %DO;
	DATA MISSVARS;
	set 
		MISSVARS_D (in=a)
		MISSVARS_H (in=b)
		MISSVARS_R (in=c)
		MISSVARS_P (in=d)
	;
	order=a+b*2+c*3+d*4;
	if a then file="D-file";
	if b then file="H-file";
	if c then file="R-file";
	if d then file="P-file";
	RUN;
	PROC SORT NODUPKEY;BY VARIABLE;RUN;
	PROC SORT;BY order VARIABLE;RUN;
%END;

%IF %sysfunc(exist(DIRTYVARS_D_ANTE_)) %THEN %DO;
	DATA DIRTYVARS;
	set 
		DIRTYVARS_D_ANTE_ (in=a)
		DIRTYVARS_D_POST_ (in=b)
		DIRTYVARS_H_ANTE_ (in=c)
		DIRTYVARS_H_POST_ (in=d)
		DIRTYVARS_R_ANTE_ (in=e)
		DIRTYVARS_R_POST_ (in=f)
		DIRTYVARS_P_ANTE_ (in=g)
		DIRTYVARS_P_POST_ (in=h)
	;
	order=a+b+c*2+d*2+e*3+f*3+g*4+h*4;
	if a or b then file="D-file";
	if c or d then file="H-file";
	if e or f then file="R-file";
	if g or h then file="P-file";
	RUN;
	PROC SORT NODUPKEY;BY VARIABLE;RUN;
	PROC SORT;BY order VARIABLE;RUN;
%END;
%ELSE %DO;
	DATA DIRTYVARS;
	set 
		DIRTYVARS_D (in=a)
		DIRTYVARS_H (in=b)
		DIRTYVARS_R (in=c)
		DIRTYVARS_P (in=d)
	;
	order=a+b*2+c*3+d*4;
	if a then file="D-file";
	if b then file="H-file";
	if c then file="R-file";
	if d then file="P-file";
	RUN;
	PROC SORT NODUPKEY;BY VARIABLE;RUN;
	PROC SORT;BY order VARIABLE;RUN;
%END;
PROC SQL;
  DELETE FROM DIRTYVARS
  WHERE  VARIABLE IN ( SELECT VARIABLE FROM MISSVARS )
  ;
QUIT;



%IF %sysfunc(exist(SVAL_MISS_FULL_CSV_D_ANTE_)) %THEN %DO;
	DATA SVAL_MISS_FULL_CSV;
	set 
		SVAL_MISS_FULL_CSV_D_ANTE_ (rename=(DB010=year))
		SVAL_MISS_FULL_CSV_D_POST_ (rename=(DB010=year))
		SVAL_MISS_FULL_CSV_H_ANTE_ (rename=(HB010=year))
		SVAL_MISS_FULL_CSV_H_POST_ (rename=(HB010=year))
		SVAL_MISS_FULL_CSV_R_ANTE_ (rename=(RB010=year))
		SVAL_MISS_FULL_CSV_R_POST_ (rename=(RB010=year))
		SVAL_MISS_FULL_CSV_P_ANTE_ (rename=(PB010=year))
		SVAL_MISS_FULL_CSV_P_POST_ (rename=(PB010=year))
	;
	if substr(VARIABLE,1,1)="D" then file="D-file";
	if substr(VARIABLE,1,1)="H" then file="H-file";
	if substr(VARIABLE,1,1)="R" then file="R-file";
	if substr(VARIABLE,1,1)="P" then file="P-file";
	RUN;
%END;
%ELSE %DO;
	DATA SVAL_MISS_FULL_CSV;
	set 
		SVAL_MISS_FULL_CSV_D (rename=(DB010=year))
		SVAL_MISS_FULL_CSV_H (rename=(HB010=year))
		SVAL_MISS_FULL_CSV_R (rename=(RB010=year))
		SVAL_MISS_FULL_CSV_P (rename=(PB010=year))
	;
	if substr(VARIABLE,1,1)="D" then file="D-file";
	if substr(VARIABLE,1,1)="H" then file="H-file";
	if substr(VARIABLE,1,1)="R" then file="R-file";
	if substr(VARIABLE,1,1)="P" then file="P-file";
	RUN;
%END;
%IF %sysfunc(exist(SVAL_MISS_STAT_D_ANTE_)) %THEN %DO;
	DATA SVAL_MISS_STAT_D;MERGE SVAL_MISS_STAT_D_ANTE_ SVAL_MISS_STAT_D_POST_;BY VARIABLE FLAG;RUN;
	DATA SVAL_MISS_STAT_H;MERGE SVAL_MISS_STAT_H_ANTE_ SVAL_MISS_STAT_H_POST_;BY VARIABLE FLAG;RUN;
	DATA SVAL_MISS_STAT_R;MERGE SVAL_MISS_STAT_R_ANTE_ SVAL_MISS_STAT_R_POST_;BY VARIABLE FLAG;RUN;
	DATA SVAL_MISS_STAT_P;MERGE SVAL_MISS_STAT_P_ANTE_ SVAL_MISS_STAT_P_POST_;BY VARIABLE FLAG;RUN;
	DATA SVAL_MISS_STAT;DROP MAXPC;
	set 
		SVAL_MISS_STAT_D (in=a)
		SVAL_MISS_STAT_H (in=b)
		SVAL_MISS_STAT_R (in=c)
		SVAL_MISS_STAT_P (in=d)
	;
	order=a+b*2+c*3+d*4;
	if a then file="D-file";
	if b then file="H-file";
	if c then file="R-file";
	if d then file="P-file";
	RUN;
%END;
%ELSE %DO;
	DATA SVAL_MISS_STAT;DROP MAXPC;
	set 
		SVAL_MISS_STAT_D (in=a)
		SVAL_MISS_STAT_H (in=b)
		SVAL_MISS_STAT_R (in=c)
		SVAL_MISS_STAT_P (in=d)
	;
	order=a+b*2+c*3+d*4;
	if a then file="D-file";
	if b then file="H-file";
	if c then file="R-file";
	if d then file="P-file";
	RUN;
%END;


OPTIONS ORIENTATION=LANDSCAPE NODATE NONUMBER;
ODS &OUTPUTFORMAT FILE = "&OUT&_dirsp_%quote(&CC&YY.-Summary.&EXTENSION)" &OUTOPTION ;

/**
 * MISSING AND UNRECOGNIZED VARIABLES
 */

%LET NOBS=0;
DATA MISSVARS;
SET  MISSVARS NOBS=NOBS;
  CALL SYMPUTX('NOBS',NOBS);
  ARRAY X _CHARACTER_;DO OVER X;IF X="" THEN X="X";END;
RUN;
%IF &NOBS GT 0 %THEN %DO;
		TITLE1 "&CC - &YYYY / TRANSMISSION=&SS VERSION=&FVER";
        %IF &YYYY>=2024 %THEN %DO;TITLE2 "MISSING VARIABLES";%END;%ELSE %DO;TITLE2 "MISSING VARIABLES FROM 2021 ONWARDS";%END;
		TITLE3 COLOR=red "REQUIRED ACTION : listed variables should be included in transmission files";
		proc report data=MISSVARS ;
			columns file VARIABLE;
			define file /display "File" format=$10. left;
			define VARIABLE /display "Variable" format=$VARLABEL. left;
			compute file;
				if file="D-file" then call define (_row_,'style','style=[background=Lightblue');
				if file="H-file" then call define (_row_,'style','style=[background=Lightgreen');
				if file="R-file" then call define (_row_,'style','style=[background=Salmon');
				if file="P-file" then call define (_row_,'style','style=[background=Lightyellow');
			endcomp;
		run;

		RUN;
%END;
%IF %sysfunc(exist(MISSVARS_ANTE)) %THEN %DO;
	%LET NOBS=0;
	DATA MISSVARS_ANTE;
	SET  MISSVARS_ANTE NOBS=NOBS;
	  CALL SYMPUTX('NOBS',NOBS);
	  ARRAY X _CHARACTER_;DO OVER X;IF X="" THEN X="X";END;
	RUN;
	%IF &NOBS GT 0 %THEN %DO;
			TITLE1 "&CC - &YYYY / TRANSMISSION=&SS VERSION=&FVER";
	        TITLE2 "MISSING VARIABLES COLLECTED BEFORE 2021";
			TITLE3 COLOR=red "REQUIRED ACTION : listed variables should be included in transmission files";
			proc report data=MISSVARS_ANTE ;
				columns file VARIABLE;
				define file /display "File" format=$10. left;
				define VARIABLE /display "Variable" format=$VARLABEL. left;
				compute file;
					if file="D-file" then call define (_row_,'style','style=[background=Lightblue');
					if file="H-file" then call define (_row_,'style','style=[background=Lightgreen');
					if file="R-file" then call define (_row_,'style','style=[background=Salmon');
					if file="P-file" then call define (_row_,'style','style=[background=Lightyellow');
				endcomp;
			run;

			RUN;
	%END;
%END;

%LET NOBS=0;
PROC SQL;
  DELETE FROM DIRTYVARS
  WHERE  VARIABLE IN ( SELECT VARIABLE FROM MISSVARS )
  ;
QUIT;
DATA DIRTYVARS;
SET  DIRTYVARS NOBS=NOBS;
  CALL SYMPUTX('NOBS',NOBS);
  ARRAY X _CHARACTER_;DO OVER X;IF X="" THEN X="X";END;
RUN;


TITLE1;
TITLE2;
TITLE3;
%IF &NOBS GT 0 %THEN %DO;
		TITLE1 "&CC - &YYYY / TRANSMISSION=&SS VERSION=&FVER";
        TITLE2 "UNRECOGNIZED VARIABLES";
		TITLE3 COLOR=red "REQUIRED ACTION : listed variables should be removed from transmission files";
		proc report data=DIRTYVARS ;
			columns file VARIABLE;
			define file /display "File" format=$10. left;
			define VARIABLE /display "Variable" format=$VARLABEL. left;
			compute file;
				if file="D-file" then call define (_row_,'style','style=[background=Lightblue');
				if file="H-file" then call define (_row_,'style','style=[background=Lightgreen');
				if file="R-file" then call define (_row_,'style','style=[background=Salmon');
				if file="P-file" then call define (_row_,'style','style=[background=Lightyellow');
			endcomp;
		run;
%END;

TITLE1;
TITLE2;
TITLE3;

/**
 * INCOME VARIABLES WHIT ALL VALUES EQUAL TO NULL OR ZERO
 */

data allzero;set sval ;keep variable;if income="Y";run;
proc sort;by variable;run;
%ALLZERO(D);
%ALLZERO(H);
%ALLZERO(R);
%ALLZERO(P);
/*enable this check only from 2021 onwards*/
data allzero;set allzero ;if year >= 2021;run;

proc sort data=allzero;by year file variable;run;

DATA allzero;
SET  allzero NOBS=NOBS;
  CALL SYMPUTX('NOBS',NOBS);
RUN;


TITLE1;
TITLE2;
TITLE3;
%IF &NOBS GT 0 %THEN %DO;
		TITLE1 "&CC - &YYYY / TRANSMISSION=&SS VERSION=&FVER";
        TITLE2 "INCOME VARIABLES WITH ALL VALUES EQUAL TO ZERO";
		TITLE3 COLOR=orange "RECOMMENDED ACTION : explanation should be given";
		proc report data=allzero ;
			columns file year VARIABLE;
			define file /display "File" format=$10. left;
			define year /display "Year" format=4.0 left;
			define VARIABLE /display "Variable" format=$VARLABEL. left;
			compute file;
				if file="D-file" then call define (_row_,'style','style=[background=Lightblue');
				if file="H-file" then call define (_row_,'style','style=[background=Lightgreen');
				if file="R-file" then call define (_row_,'style','style=[background=Salmon');
				if file="P-file" then call define (_row_,'style','style=[background=Lightyellow');
			endcomp;
		run;
%END;

TITLE1;
TITLE2;
TITLE3;


/**
 * MISSING RATES
 */
PROC FORMAT;
  VALUE PCT
  LOW-0.05 = 'white'
  0.05-0.20 = 'yellow'
  0.20-0.50 = 'orange'
  0.50-0.9999 = 'red'
  0.9999-high = 'crimson'
  ;
RUN;
PROC FORMAT;
  VALUE $FILE
  "D-file" = 'Lightblue'
  "H-file" = 'Lightgreen'
  "R-file" = 'Salmon'
  "P-file" = 'Lightyellow'
  ;

PROC FORMAT;
	picture blanck
	low-high=" ";
RUN;




PROC TRANSPOSE DATA=SVAL_MISS_STAT (DROP=_NAME_) 
	OUT=STAT (RENAME=(_NAME_=PB010 COL1=PCT));
	BY order file VARIABLE FLAG MISSTHRES;
RUN;
%LET NOBS=0;
DATA STAT1;
SET  STAT (WHERE=(VARIABLE IS NOT NULL AND FLAG=-1 and PB010 NE "COL1")) NOBS=NOBS;
  CALL SYMPUTX('NOBS',NOBS);
  ARRAY X _CHARACTER_;DO OVER X;IF X="" THEN X="X";END;
  comment="";
RUN;
%PUT &NOBS;
%IF &NOBS GT 0 %THEN %DO;
		TITLE1 "&CC - &YYYY / TRANSMISSION=&SS VERSION=&FVER";
        TITLE2 "UNWEIGHTED MISSING PCT (FLAG=-1)";
        PROC SQL NOPRINT;
           SELECT MISSTHRES FORMAT PERCENT7.2 INTO :MISSTHRES FROM SVAL_MISSTHRES
           ;
        QUIT;
		PROC TABULATE DATA=STAT1;
		  TITLE3 COLOR=orange "RECOMMENDED ACTION : explanation should be given for high percentage of missing values";
          CLASS FILE VARIABLE PB010 MISSTHRES / MISSING ;
		  CLASSLEV FILE / style=[background=$FILE.];
		  CLASSLEV VARIABLE / style=<parent>;
          FORMAT VARIABLE $VARLABEL. MISSTHRES PERCENT7.2;
          VAR   PCT ;
          TABLE FILE="File" * VARIABLE="Variable"*MISSTHRES='Threshold', PB010="YEAR"*PCT=" "*F=PERCENT7.2*{style={background=PCT.}}*sum=" " all="Add_your_comment_"*N=" "*F=blanck. 
			/ BOX="DEFAULT THRESHOLD = &MISSTHRES";
        RUN;
        DATA SVAL_MISS;
        SET  SVAL_MISS;
                WARNING = 'MISSING';
        RUN;

%END;

%LET NOBS=0;
DATA STAT8;
SET  STAT (WHERE=(VARIABLE IS NOT NULL AND FLAG=-8 and PB010 NE "COL1")) NOBS=NOBS;
  CALL SYMPUTX('NOBS',NOBS);
  ARRAY X _CHARACTER_;DO OVER X;IF X="" THEN X="X";END;
  comment="";
RUN;
%PUT &NOBS;
%IF &NOBS GT 0 %THEN %DO;
		TITLE1 "&CC - &YYYY / TRANSMISSION=&SS VERSION=&FVER";
        TITLE2 "UNFILLED VARIABLES (FLAG=-8)";
        PROC TABULATE DATA=STAT8;
		  TITLE3 COLOR=orange "RECOMMENDED ACTION : explanation should be given for not filling these variables if not optional";
          CLASS FILE VARIABLE PB010/ MISSING ;
		  CLASSLEV FILE / style=[background=$FILE.];
		  CLASSLEV VARIABLE / style=<parent>;
          FORMAT VARIABLE $VARLABEL.;
          VAR   PCT ;
          TABLE FILE="File" * VARIABLE="Variable", PB010="YEAR"*PCT=" "*F=PERCENT7.2*{style={background=PCT.}}*sum=" " all="Add_your_comment_"*N=" "*F=blanck. 
			/ ;
        RUN;
        DATA SVAL_MISS;
        SET  SVAL_MISS;
                WARNING = 'MISSING';
        RUN;

%END;


TITLE1;
TITLE2;
TITLE3;

%LET NOBS=0;
DATA SVAL_MISS_FULL_CSV;
SET  SVAL_MISS_FULL_CSV  NOBS=NOBS;
  CALL SYMPUTX('NOBS',NOBS);
  ARRAY X _CHARACTER_;DO OVER X;IF X="" THEN X="X";END;
RUN;

%PUT &NOBS;
%IF &NOBS GT 0 %THEN %DO;
	TITLE1 "&CC - &YYYY / TRANSMISSION=&SS VERSION=&FVER";
	TITLE2 "STRUCTURAL WARNINGS COUNTS";
	TITLE3 COLOR=orange "RECOMMENDED ACTION : correct or explain listed errors";
	PROC TABULATE DATA=SVAL_MISS_FULL_CSV MISSING;
	  CLASS FILE WARNING VARIABLE year;
	  CLASSLEV FILE / style=[background=$FILE.];
	  CLASSLEV WARNING / style=<parent>;
	  CLASSLEV VARIABLE / style=<parent>;
	  FORMAT VARIABLE $VARLABEL.;
	  TABLE FILE*WARNING*VARIABLE, (year)*N*F=COMMA12. all="Add_your_comment_"*N=" "*F=blanck. / MISSTEXT=' ' ;
	  KEYLABEL N = ' ';
      LABEL FILE = 'File';
	RUN;
	TITLE1;
	TITLE2;
	TITLE3;
%END;

%LET NOBS=0;
DATA MISM_SQL;
SET  MISM_SQL NOBS=NOBS;
  CALL SYMPUTX('NOBS',NOBS);
  ID1=ID+0;/*to have ID in numeric format*/
  ARRAY X _CHARACTER_;DO OVER X;IF X="" THEN X="X";END;
RUN;
%IF &NOBS GT 0 %THEN %DO;
	PROC SQL;
	CREATE TABLE MISM_SQL AS 
	SELECT MISM_SQL.*, LVAL.TITLE
	FROM MISM_SQL LEFT JOIN LVAL
	ON MISM_SQL.ID=LVAL.ID;
	QUIT;

	TITLE1 "&CC - &YYYY / TRANSMISSION=&SS VERSION=&FVER";
    TITLE2 'LOGICAL CHECKS WARNINGS';
	TITLE3 COLOR=orange "RECOMMENDED ACTION : correct or explain listed errors";
    PROC TABULATE DATA=MISM_SQL;
      CLASS XB010 ID1 TITLE/ MISSING;
      VAR _FREQ_;
      TABLE ID1*TITLE, (XB010)*_FREQ_=' '*SUM*F=7. all="Add_your_comment_"*N=" "*F=blanck. ;
      LABEL XB010 = 'YEAR';
      KEYLABEL SUM=' ';
    RUN;
    TITLE1;
    TITLE2;
    TITLE3;
%END;

DATA HEADER;
  LENGTH MSG T $125;
  LABEL MSG = 'VALIDATION STATUS';
  T = PUT(DATETIME(),DATETIME20.);
  MSG = "&CC&YY SILC FILES (VERSION=&FVER) VALIDATION ACCOMPLISHED ON " || COMPRESS(T);
RUN;


PROC PRINT DATA=HEADER LABEL NOOBS
  style(header)={just=c}
  ;
  VAR MSG;
RUN;

ODS &OUTPUTFORMAT CLOSE;


%MEND;


%MACRO ALLZERO (file);
proc means data=&file noprint;
class &file.b010;
var _numeric_;
ways 1;
output out=minfile (drop=_type_ _freq_) min=;
output out=maxfile (drop=_type_ _freq_) max=;
run;
proc transpose data=minfile prefix=min out=minfile; by &file.b010;run;proc sort;by &file.b010 _name_;run;
proc transpose data=maxfile prefix=max out=maxfile; by &file.b010;run;proc sort;by &file.b010 _name_;run;
data allzero_&file;merge minfile maxfile;by &file.b010 _name_;
rename _name_=variable &file.b010=year;
file="&file.-file";

if min1 ne 0 then delete;
if max1 ne 0 then delete;
if substr(_name_,length(_name_)-1,2)="_F" then delete;
if substr(_name_,length(_name_)-2,3)="_IF" then delete;
run;
proc sort;by variable;run;

data allzero;merge allzero (in=a) allzero_&file;by variable;if a;run;
%MEND;

