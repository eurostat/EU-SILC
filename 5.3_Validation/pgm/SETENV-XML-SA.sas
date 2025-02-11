/*specify the the path to the folder where the software has been dezipped : */
%LET eusilc=___PATH___ ;

/* / for UNIX environments, \ for windows environments */
%LET _dirsp_=/; 
OPTIONS NOSOURCE MCOMPILENOTE=ALL;

/*** Parameters to specify */
%LET CC= ;  /*Country bigram in uppercase letters*/
%LET YYYY=2023;  /*Year (4 digits)*/
%LET EXTENSION=pdf;  /*pdf by default, could be changed to docx (only if sas>=9.4, keep pdf otherwise) */
%LET CSV=NO;  /*NO by default, could be changed to YES to obtain detailed list of errors in csv files */


/*NOT TO BE SPECIFIED ANYMORE*/
/*
%LET FVER=1;
%LET MODE=S;
%LET _SPLIT_MODE_=_POST_;
%LET _SPLIT_CUT_=2020;
%LET eusilc=/ec/prod/0eusilc/5.3_Validation/NSI releases/eusilc-V2.4;
*/

/** ENV eusilc PREDEFINED FOR EITHER acc OR prod ENV and other calculated macro var */
%LET SRC=&eusilc&_dirsp_.5.3_Validation&_dirsp_.pgm;
%LET yy=%SUBSTR(&yyyy,3,2);
%LET SS=R;
%IF &EXTENSION=docx %THEN %DO;
	%LET OUTPUTFORMAT=WORD;
	%LET OUTOPTION=OPTIONS (BODY_TITLE="ON");
%END;
%ELSE %DO; 
	%LET OUTPUTFORMAT=PDF;
	%LET OUTOPTION=;
%END;

/** NB OF ROTATION YEARS FOR PANEL */
%LET ROTATION=4;
%IF (&CC=BE or &CC=BG or &CC=IT) %THEN %DO;%LET ROTATION=6;%END;
%IF (&CC=IE  or &CC=NL  or &CC=SE) AND &YY>21 %THEN %DO;%LET ROTATION=6;%END; 
%IF (&CC=IE  or &CC=NL  or &CC=SE) AND &YY=21 %THEN %DO;%LET ROTATION=5;%END; 


/** LIBRARY LIST (1/2) */
LIBNAME RAW  "&eusilc&_dirsp_.main&_dirsp_%LOWCASE(&cc)&_dirsp_.stage"; 
LIBNAME OUT  "&eusilc&_dirsp_.main&_dirsp_%LOWCASE(&cc)&_dirsp_.out";  
LIBNAME BACK "&eusilc&_dirsp_.main&_dirsp_%LOWCASE(&cc)&_dirsp_.tmp";

/** LIBRARY LIST (2/2) */
LIBNAME CSV "&eusilc&_dirsp_.main&_dirsp_%LOWCASE(&cc)&_dirsp_.csv";
LIBNAME RAW_DB "&eusilc&_dirsp_%quote(main)&_dirsp_%LOWCASE(&CC)&_dirsp_%LOWCASE(&SS&YY)"  compress=yes;
LIBNAME XMLCFG "&eusilc&_dirsp_.XML_CONFIG&_dirsp_.XML";
LIBNAME MAPDIR "&eusilc&_dirsp_.XML_CONFIG&_dirsp_.MAP";

/** EXECUTION MODE */
%LET _IDX_DEBUG_=1;
%PUT _USER_;
