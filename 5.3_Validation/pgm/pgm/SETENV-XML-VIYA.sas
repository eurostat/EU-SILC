/* Configuration requirements for S3 access
THE SCRIPT WORKS PROPERLY ONLY IF SAS STUDIO IS STARTED USING SILC Context
The parameters are defined at the level of SILC Context.*/
%let parm_s3_region=lux;
%let parm_s3_authd=SILC_S3_Auth;
%let parm_s3_credentials=P01;
%let gRootPathS3=/silc-%sysfunc(substr(&_BASEURL,19,3))-01;


/*specify the path to the folder where the software has been dezipped : */
*%LET eusilc=___PATH___ ;

%LET _dirsp_=/;
OPTIONS NOSOURCE MCOMPILENOTE=ALL;

%LET yy=%SUBSTR(&yyyy,3,2);


/** NB OF ROTATION YEARS FOR PANEL */
%LET ROTATION=4;
%IF (&CC=BE or &CC=BG or &CC=IT or &CC=YY) %THEN %DO;%LET ROTATION=6;%END;
%IF (&CC=IE  or &CC=NL  or &CC=SE) AND &YY>21 %THEN %DO;%LET ROTATION=6;%END; 
%IF (&CC=IE  or &CC=NL  or &CC=SE) AND &YY=21 %THEN %DO;%LET ROTATION=5;%END; 


/*option to enable directory creation automaticaly*/
options dlcreatedir;
/*create parent folders before assigning child libraries below, librairies are not kept*/
LIBNAME silc    "&eusilc";LIBNAME silc clear; 
LIBNAME main    "&eusilc&_dirsp_.main";LIBNAME main clear; 
LIBNAME cc    	"&eusilc&_dirsp_.main&_dirsp_%lowcase(&cc)";LIBNAME cc clear; 
LIBNAME XML_C   "&eusilc&_dirsp_.XML_CONFIG";LIBNAME XML_C clear;


/** LIBRARY LIST (1/2)*/
LIBNAME RAW    "&eusilc&_dirsp_.main&_dirsp_%lowcase(&cc)&_dirsp_.stage"; 
LIBNAME OUT    "&eusilc&_dirsp_.main&_dirsp_%lowcase(&cc)&_dirsp_.out";   
LIBNAME BACK   "&eusilc&_dirsp_.main&_dirsp_%lowcase(&cc)&_dirsp_.tmp"; 
LIBNAME CSV    "&eusilc&_dirsp_.main&_dirsp_%lowcase(&cc)&_dirsp_.csv";
LIBNAME RAW_DB "&eusilc&_dirsp_%quote(main)&_dirsp_%LOWCASE(&CC)&_dirsp_%LOWCASE(&SS&YY)"  compress=yes;

/** LIBRARY LIST (2/2)*/
LIBNAME XMLCFG "&eusilc&_dirsp_.XML_CONFIG&_dirsp_.XML";
LIBNAME MAPDIR "&eusilc&_dirsp_.XML_CONFIG&_dirsp_.MAP";


/*S3 DOWNLOAD*/
/*create folders to download XML files*/
/*create parent folders before assigning child libraries below, librairies are not kept*/
LIBNAME XMLSVAL "&eusilc&_dirsp_.XML_CONFIG&_dirsp_.XML&_dirsp_.SVAL";LIBNAME XMLSVAL clear; 
LIBNAME XMLLVAL "&eusilc&_dirsp_.XML_CONFIG&_dirsp_.XML&_dirsp_.SVAL";LIBNAME XMLLVAL clear; 
LIBNAME XMLSCL  "&eusilc&_dirsp_.XML_CONFIG&_dirsp_.XML&_dirsp_.SVAL";LIBNAME XMLSCL  clear; 
LIBNAME XMLCOMP "&eusilc&_dirsp_.XML_CONFIG&_dirsp_.XML&_dirsp_.SVAL";LIBNAME XMLCOMP clear; 
LIBNAME XMLSIZE "&eusilc&_dirsp_.XML_CONFIG&_dirsp_.XML&_dirsp_.SVAL";LIBNAME XMLSIZE clear; 
LIBNAME XMLOUTL "&eusilc&_dirsp_.XML_CONFIG&_dirsp_.XML&_dirsp_.SVAL";LIBNAME XMLOUTL clear; 

/*download file by folder*/
proc s3 REGION="lux" AUTHDOMAIN="SILC_S3_Auth" CREDENTIALSPROFILE="P01";
/*csv files*/
	getdir "&gRootPathS3/main&_dirsp_%lowcase(&cc)&_dirsp_.csv" "&eusilc/main&_dirsp_%lowcase(&cc)";

/*xml files and map files*/
	getdir "&gRootPathS3&_dirsp_.XML_CONFIG&_dirsp_.MAP" 			  "&eusilc&_dirsp_.XML_CONFIG";
	getdir "&gRootPathS3&_dirsp_.XML_CONFIG&_dirsp_.XML&_dirsp_.SVAL" "&eusilc&_dirsp_.XML_CONFIG&_dirsp_.XML";
	getdir "&gRootPathS3&_dirsp_.XML_CONFIG&_dirsp_.XML&_dirsp_.LVAL" "&eusilc&_dirsp_.XML_CONFIG&_dirsp_.XML";
	getdir "&gRootPathS3&_dirsp_.XML_CONFIG&_dirsp_.XML&_dirsp_.SCL"  "&eusilc&_dirsp_.XML_CONFIG&_dirsp_.XML" ;
	getdir "&gRootPathS3&_dirsp_.XML_CONFIG&_dirsp_.XML&_dirsp_.COMP" "&eusilc&_dirsp_.XML_CONFIG&_dirsp_.XML";
	getdir "&gRootPathS3&_dirsp_.XML_CONFIG&_dirsp_.XML&_dirsp_.SIZE" "&eusilc&_dirsp_.XML_CONFIG&_dirsp_.XML";
	getdir "&gRootPathS3&_dirsp_.XML_CONFIG&_dirsp_.XML&_dirsp_.OUTL" "&eusilc&_dirsp_.XML_CONFIG&_dirsp_.XML";
run;


/** EXECUTION MODE */
%LET _IDX_DEBUG_=1;
%PUT _USER_;




