/*specify the path to the folder where the software has been dezipped : */
%LET eusilc=___PATH___ ;



%LET _dirsp_=/;
OPTIONS NOSOURCE MCOMPILENOTE=ALL;

/**
 * PROMPTS
 */
%LET CC=&RCC;
%LET YYYY=&RYYYY;
%LET EXTENSION=&REXTENSION;
%IF &REXTENSION=docx %THEN %DO;
	%LET OUTPUTFORMAT=WORD;
	%LET OUTOPTION=OPTIONS (BODY_TITLE="ON");
%END;
%ELSE %DO; 
	%LET OUTPUTFORMAT=PDF;
	%LET OUTOPTION=;
%END;
%LET CSV=&RCSV;
/*NO MORE IN PROMPT*/
%LET SS=R;
/*
%LET SS=&RCL
%LET MODE=&MODE;
%LET FVER=&RFVER;
*/

%LET yy=%SUBSTR(&yyyy,3,2);

/** NB OF ROTATION YEARS FOR PANEL */
%LET ROTATION=4;
%IF (&CC=BE or &CC=BG or &CC=IT or &CC=YY) %THEN %DO;%LET ROTATION=6;%END;
%IF (&CC=IE  or &CC=NL  or &CC=SE) AND &YY>21 %THEN %DO;%LET ROTATION=6;%END; 
%IF (&CC=IE  or &CC=NL  or &CC=SE) AND &YY=21 %THEN %DO;%LET ROTATION=5;%END; 


/** LIBRARY LIST (1/2)*/
LIBNAME RAW  "&eusilc&_dirsp_.main&_dirsp_%LOWCASE(&cc)&_dirsp_.stage"; /* mkDIR */
LIBNAME OUT  "&eusilc&_dirsp_.main&_dirsp_%LOWCASE(&cc)&_dirsp_.out";   /* mkDIR */
LIBNAME BACK "&eusilc&_dirsp_.main&_dirsp_%LOWCASE(&cc)&_dirsp_.tmp"; 

/** LIBRARY LIST (2/2)*/
LIBNAME CSV "&eusilc&_dirsp_.main&_dirsp_%LOWCASE(&cc)&_dirsp_.csv";
LIBNAME XMLCFG "&eusilc&_dirsp_.XML_CONFIG&_dirsp_.XML";
LIBNAME MAPDIR "&eusilc&_dirsp_.XML_CONFIG&_dirsp_.MAP";


/** EXECUTION MODE */
%LET _IDX_DEBUG_=1;
%PUT _USER_;




