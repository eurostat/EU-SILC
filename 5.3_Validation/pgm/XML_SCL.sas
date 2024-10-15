/**
 * Revision History
 * - Sep 28th 2022
 */

/**
 * Ancillary Code List
 * [] SCL_NUTS
 */
%MACRO getSCL_NUTS;

%LOCAL SCL_NUTS i j ;

%LET INDIR=%SYSFUNC(PATHNAME(XMLCFG))&_dirsp_%quote(SCL);

LIBNAME IN_XML XMLV2 "&INDIR&_dirsp_%quote(SCL_NUTS.xml)" XMLMAP="%SYSFUNC(PATHNAME(MAPDIR))&_dirsp_%quote(SCL_NUTS.map)";
DATA SCL_NUTS;
SET  IN_XML.SCL_NUTS;
  	  IF START = . and END = . THEN OUTPUT;
  	  ELSE IF &YYYY GE START AND END = . THEN OUTPUT;
  	  ELSE IF START = . AND &YYYY LE END THEN OUTPUT;
  	  ELSE IF &YYYY GE START AND &YYYY LE END THEN OUTPUT;
RUN;

/*for test purpose fake nuts are created*/
%IF &CC EQ XX or &CC EQ YY %THEN %DO;
    data SCL_NUTS;
	length ID $4 CNTRY $2;
		%do i = 1 %to 3;
			%do j = 1 %to 3;
			ID = "&CC&i&j";
			CNTRY = "&CC";
			output;
			%end;
		%end;
    run;
run;
%END;

DATA _LAST_;
SET;
  IF CNTRY = "&cc";
RUN;



%LET SCL_NUTS=;
PROC SQL NOPRINT;
  SELECT "'"||TRIM(ID)||"'" INTO :SCL_NUTS SEPARATED BY ','
  FROM   SCL_NUTS
  WHERE CNTRY = "&cc"
  ;
QUIT;

%PUT *I* SCL_NUTS;
%PUT &SCL_NUTS;

DATA SVAL;
SET  SVAL;
  LENGTH LIST_A LIST_B $32000;
  LENGTH TOK $32;
  IF UPCASE(LIST_REF) = 'SCL_NUTS' THEN DO;
     LIST_A = "&SCL_NUTS";
	 IF ( trim(LIST_A) = '' ) AND ( UPCASE("&cc") eq 'ZZ' ) THEN LIST = "('ZZ01','ZZ02')";
	 ELSE DO;
         LIST = TRANWRD(LIST,'(','');
         LIST = TRANWRD(LIST,')','');
         LIST = TRANWRD(LIST,'"',"'");
         LIST_B = TRIM(LIST);
         IF LIST_B = '' THEN LIST = '(' || TRIM(LIST_A) || ')';
         ELSE LIST = '(' || TRIM(LIST_A) || ',' || TRIM(LIST_B) || ')';
     END;
  END;
RUN;

%MEND getSCL_NUTS;

/**
 * Ancillary Code List
 * [] SCL_GEO
 */
%MACRO getSCL_GEO;

%LOCAL SCL_GEO;

%LET INDIR=%SYSFUNC(PATHNAME(XMLCFG))&_dirsp_%quote(SCL);

LIBNAME IN_XML XMLV2 "&INDIR&_dirsp_%quote(SCL_GEO.xml)" XMLMAP="%SYSFUNC(PATHNAME(MAPDIR))&_dirsp_%quote(SCL_GEO.map)";
DATA SCL_GEO;
	SET  IN_XML.SCL_GEO;
    IF START = . and END = . THEN OUTPUT;
    ELSE IF &YYYY GE START AND END = . THEN OUTPUT;
    ELSE IF START = . AND &YYYY LE END THEN OUTPUT;
    ELSE IF &YYYY GE START AND &YYYY LE END THEN OUTPUT;
RUN;


PROC SQL NOPRINT;
  SELECT "'"||TRIM(ID)||"'" INTO :SCL_GEO SEPARATED BY ','
  FROM   SCL_GEO
  ;
QUIT;

PROC SQL NOPRINT;
  SELECT "'"||TRIM(ID)||"'" INTO :SCL_GEO_B SEPARATED BY ','
  FROM   SCL_GEO
  WHERE  FILT = '' OR FILT = 'B'
  ;
QUIT;

PROC SQL NOPRINT;
  SELECT "'"||TRIM(ID)||"'" INTO :SCL_GEO_C SEPARATED BY ','
  FROM   SCL_GEO
  WHERE  FILT = '' OR FILT = 'C'
  ;
QUIT;

%PUT *I* SCL_GEO;
%PUT &SCL_GEO;

DATA SVAL;
SET  SVAL;
  LENGTH LIST_A LIST_B $32000;
  LENGTH TOK $32;
  IF UPCASE(LIST_REF) = 'SCL_GEO' THEN DO;
     IF LIST_FILT = '' THEN LIST_A = "&SCL_GEO";
     ELSE IF LIST_FILT = 'B' THEN LIST_A = "&SCL_GEO_B";
     ELSE IF LIST_FILT = 'C' THEN LIST_A = "&SCL_GEO_C";
         LIST = TRANWRD(LIST,'(','');
         LIST = TRANWRD(LIST,')','');
         LIST = TRANWRD(LIST,'"',"'");
         LIST_B = TRIM(LIST);
         IF LIST_B = '' THEN LIST = '(' || TRIM(LIST_A) || ')';
         ELSE LIST = '(' || TRIM(LIST_A) || ',' || TRIM(LIST_B) || ')';
  END;
RUN;

%MEND getSCL_GEO;


/**
 * Ancillary Code List
 * [] SCL_NACE
 */
%MACRO getSCL_NACE;

%LOCAL SCL_NACE;

%LET INDIR=%SYSFUNC(PATHNAME(XMLCFG))&_dirsp_%quote(SCL);

LIBNAME IN_XML XMLV2 "&INDIR&_dirsp_%quote(SCL_NACE.xml)" XMLMAP="%SYSFUNC(PATHNAME(MAPDIR))&_dirsp_%quote(SCL_NACE.map)";
DATA SCL_NACE;
	SET  IN_XML.SCL_NACE;
    IF START = . and END = . THEN OUTPUT;
    ELSE IF &YYYY GE START AND END = . THEN OUTPUT;
    ELSE IF START = . AND &YYYY LE END THEN OUTPUT;
    ELSE IF &YYYY GE START AND &YYYY LE END THEN OUTPUT;
RUN;

PROC SQL NOPRINT;
  SELECT TRIM(ID) INTO :SCL_NACE SEPARATED BY ','
  FROM   SCL_NACE
  ;
QUIT;

%PUT *I* SCL_NACE;
%PUT &SCL_NACE;

DATA SVAL;
SET  SVAL;
  LENGTH LIST_A LIST_B $32000;
  LENGTH TOK $32;
  IF UPCASE(LIST_REF) = 'SCL_NACE' THEN DO;
     LIST_A = "&SCL_NACE";
         LIST = TRANWRD(LIST,'(','');
         LIST = TRANWRD(LIST,')','');
         LIST_B = TRIM(LIST);
         IF LIST_B = '' THEN LIST = '(' || TRIM(LIST_A) || ')';
         ELSE LIST = '(' || TRIM(LIST_A) || ',' || TRIM(LIST_B) || ')';
  END;
RUN;

%MEND getSCL_NACE;


/**
 * Ancillary Code List
 * [] SCL_ISCO
 */
%MACRO getSCL_ISCO;

%LOCAL SCL_ISCO;

%LET INDIR=%SYSFUNC(PATHNAME(XMLCFG))&_dirsp_%quote(SCL);

LIBNAME IN_XML XMLV2 "&INDIR&_dirsp_%quote(SCL_ISCO.xml)" XMLMAP="%SYSFUNC(PATHNAME(MAPDIR))&_dirsp_%quote(SCL_ISCO.map)";
DATA SCL_ISCO;
	SET  IN_XML.SCL_ISCO;
    IF START = . and END = . THEN OUTPUT;
    ELSE IF &YYYY GE START AND END = . THEN OUTPUT;
    ELSE IF START = . AND &YYYY LE END THEN OUTPUT;
    ELSE IF &YYYY GE START AND &YYYY LE END THEN OUTPUT;
RUN;

PROC SQL NOPRINT;
  SELECT TRIM(ID) INTO :SCL_ISCO SEPARATED BY ','
  FROM   SCL_ISCO
  ;
QUIT;

%PUT *I* SCL_ISCO;
%PUT &SCL_ISCO;

DATA SVAL;
SET  SVAL;
  LENGTH LIST_A LIST_B $32000;
  LENGTH TOK $32;
  IF UPCASE(LIST_REF) = 'SCL_ISCO' THEN DO;
     LIST_A = "&SCL_ISCO";
         LIST = TRANWRD(LIST,'(','');
         LIST = TRANWRD(LIST,')','');
         LIST_B = TRIM(LIST);
         IF LIST_B = '' THEN LIST = '(' || TRIM(LIST_A) || ')';
         ELSE LIST = '(' || TRIM(LIST_A) || ',' || TRIM(LIST_B) || ')';
  END;
RUN;

%MEND getSCL_ISCO;

