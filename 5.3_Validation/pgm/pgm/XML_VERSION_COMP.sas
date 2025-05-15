%MACRO get_VERSION;
/*search for latest version of csv files*/
data csvfiles0;
	length fref $8 fname $200;
	did = filename(fref,"&eusilc&_dirsp_.main&_dirsp_%LOWCASE(&cc)&_dirsp_.csv");
	did = dopen(fref);
	do i = 1 to dnum(did);
	  fname = dread(did,i);
	  output;
	end;
	did = dclose(did);
	did = filename(fref);
	keep fname;
run;
data csvfiles;set csvfiles0;
	if substr(fname,length(fname)-2,3) not in ("CSV", "csv") then delete;
	if substr(fname,1,6) ne "SILC_R" then delete;
	toto=substr(fname,8,15);
	if substr(fname,8,15) ne "_A_%UPCASE(&CC)_&YYYY._0000" then delete;
	if length(fname) ne 32 then delete;
	version=substr(fname,25,4)+0;
	csvcase=substr(fname,30,3);
run;
proc means data=csvfiles noprint;
var version;
output out=versions min=minver max=maxver;
run;
data versions;
set versions;
call symput("fver",compress(maxver,' '));
call symput("pver",compress(minver,' '));
run;
%mend;
/*
%get_VERSION;
%FETCH_SILC_CSV(MODE=PROD,F=D,VER=&FVER);
%FETCH_SILC_CSV(MODE=PROD,F=H,VER=&FVER);
%FETCH_SILC_CSV(MODE=PROD,F=R,VER=&FVER);
%FETCH_SILC_CSV(MODE=PROD,F=P,VER=&FVER);
*/

%MACRO build_VERSION (f);
%LOCAL DIR FILE csvcase;
%LET DIR=%SYSFUNC(PATHNAME(CSV));

%FETCH_SILC_CSV (F=&F,VER=&PVER, PREV=Y);


proc sort data=file_&f ;by &f.b010 &f.b020 &f.b030;run;
proc sort data=file_&f&PVER;by &f.b010 &f.b020 &f.b030;run;

proc contents noprint data=file_&f  out=contents_&f  (keep=Name Type nobs);proc sort;by Name;run;
proc contents noprint data=file_&f&PVER out=contents_&f&PVER (keep=Name Type nobs);proc sort;by Name;run;

data comp_&f;merge contents_&f&PVER (in=in1 rename=(Type=V&pver)) contents_&f (in=in2 rename=(Type=V&fver));by Name;file="&f-file";run;

proc summary data=contents_&f;output out=contents_&f n=nvar_&FVER max=nobs_&FVER;var nobs;run;
proc summary data=contents_&f&PVER;output out=contents_&f&PVER n=nvar_&PVER max=nobs_&PVER;var nobs;run;
data contents_&f;merge contents_&f contents_&f&PVER;file="&f-file";run; 

proc compare noprint base=file_&f compare=file_&f&PVER outstat=diff_&f;id &f.b010 &f.b020 &f.b030;run;
data diff_&f;set diff_&f;file="&f-file";run;

%MEND;

%MACRO rep_VERSION;
%IF &FVER > &PVER %THEN %DO;

	%build_VERSION(D);
	%build_VERSION(H);
	%build_VERSION(R);
	%build_VERSION(P);

	data contents_tot;set contents_D contents_H contents_R contents_P;run;


	data comp_tot&pver;format name $8.;length name $8;
		set comp_d comp_h comp_r comp_p;
		if V&fver ne . then delete;
	run;
	data comp_tot&fver;format name $8.;length name $8;
		set comp_d comp_h comp_r comp_p;
		if V&pver ne . then delete;
	run;


	data diff_tot;
		format _var_ $VARLABEL.;length _var_ $8;
		keep _var_ _base_ file;
		rename _var_=variable _base_=Diff_nb;
		set diff_d diff_h diff_r diff_p;
		if _type_="NDIF" and _base_ ne 0;
	run;


	%LET OUT=%SYSFUNC(PATHNAME(OUT));

	OPTIONS ORIENTATION=LANDSCAPE NODATE NONUMBER;
	ODS &OUTPUTFORMAT FILE = "&OUT&_dirsp_%quote(&CC&YY.-Versions_comp.&EXTENSION)" &OUTOPTION ;

	%LET NOBS=0;
	DATA contents_tot;
	SET  contents_tot NOBS=NOBS;
	  CALL SYMPUTX('NOBS',NOBS);
	RUN;
	%IF &NOBS GT 0 %THEN %DO;
		TITLE1 "&CC - &YYYY / TRANSMISSION=&SS VERSION=&FVER";
		TITLE2 "COMPARISON VERSION &PVER VS VERSION &FVER";
		TITLE3 "NUMBER OF OBSERVATIONS AND VARIABLES";
		proc report data=contents_tot;
			columns file ("Observations" nobs_&pver nobs_&fver nobs2&pver nobs2&fver) ("Variables" nvar_&pver nvar_&fver nvar2&pver nvar2&fver);
			define file /display "File" format=$10. left;
			define nobs_&pver /analysis sum noprint;
			define nobs_&fver /analysis sum noprint;
			define nvar_&pver /analysis sum noprint;
			define nvar_&fver /analysis sum noprint;
			define nobs2&pver /computed "V&pver" format=COMMAX16.0 ;
			define nobs2&fver /computed "V&fver" format=COMMAX16.0 ;
			define nvar2&pver /computed "V&pver" format=COMMAX16.0 ;
			define nvar2&fver /computed "V&fver" format=COMMAX16.0 ;

compute file;
				if file="D-file" then call define (_col_,'style','style=[background=Lightblue');
				if file="H-file" then call define (_col_,'style','style=[background=Lightgreen');
				if file="R-file" then call define (_col_,'style','style=[background=Salmon');
				if file="P-file" then call define (_col_,'style','style=[background=Lightyellow');
			endcomp;

			compute nobs2&fver;
			nobs2&pver=nobs_&pver..sum;
			nobs2&fver=nobs_&fver..sum;
			if nobs2&pver ne nobs2&fver then call define (_col_,'style','style=[background=Orange');
			if nobs2&pver = nobs2&fver then call define (_col_,'style','style=[background=Greenlight');
			endcomp;
			compute nvar2&fver;
			nvar2&pver=nvar_&pver..sum;
			nvar2&fver=nvar_&fver..sum;
			if nvar2&pver ne nvar2&fver then call define (_col_,'style','style=[background=Orange');
			if nvar2&pver = nvar2&fver then call define (_col_,'style','style=[background=Greenlight');
			endcomp;

		run;
	%END;
	%LET NOBS=0;
	DATA comp_tot&fver;
	SET  comp_tot&fver  NOBS=NOBS;
	  CALL SYMPUTX('NOBS',NOBS);
	RUN;
	%IF &NOBS GT 0 %THEN %DO;
		TITLE1 "&CC - &YYYY / TRANSMISSION=&SS VERSION=&FVER";
		TITLE2 "COMPARISON VERSION &PVER VS VERSION &FVER";
		TITLE3 "NEW VARIABLES APPEARING IN VERSION &FVER";
		proc report data=comp_tot&fver ;
			columns file name V&pver V&fver;
			define file /display "File" format=$10. left;
			define name /display "Variable" format=$VARLABEL. left;
			define V&pver /display "V&pver" format=best8.0 ;
			define V&fver /display "V&fver" format=best8.0 ;
			compute file;
				if file="D-file" then call define (_row_,'style','style=[background=Lightblue');
				if file="H-file" then call define (_row_,'style','style=[background=Lightgreen');
				if file="R-file" then call define (_row_,'style','style=[background=Salmon');
				if file="P-file" then call define (_row_,'style','style=[background=Lightyellow');
			endcomp;
		run;
		TITLE1;
		TITLE2;
		TITLE3;
	%END;
	%LET NOBS=0;
	DATA comp_tot&pver;
	SET  comp_tot&pver NOBS=NOBS;
	  CALL SYMPUTX('NOBS',NOBS);
	RUN;
	%IF &NOBS GT 0 %THEN %DO;
		TITLE1 "&CC - &YYYY / TRANSMISSION=&SS VERSION=&FVER";
		TITLE2 "COMPARISON VERSION &PVER VS VERSION &FVER";
		TITLE3 "OLD VARIABLES DELETED IN VERSION &FVER";
		proc report data=comp_tot&pver ;
			columns file name V&pver V&fver;
			define file /display "File" format=$10. left;
			define name /display "Variable" format=$VARLABEL. left;
			define V&pver /display "V&pver" format=best8.0 ;
			define V&fver /display "V&fver" format=best8.0 ;
			compute file;
				if file="D-file" then call define (_row_,'style','style=[background=Lightblue');
				if file="H-file" then call define (_row_,'style','style=[background=Lightgreen');
				if file="R-file" then call define (_row_,'style','style=[background=Salmon');
				if file="P-file" then call define (_row_,'style','style=[background=Lightyellow');
			endcomp;
		run;
		TITLE1;
		TITLE2;
		TITLE3;
	%END;
	%LET NOBS=0;
	DATA diff_tot;
	SET  diff_tot NOBS=NOBS;
	  CALL SYMPUTX('NOBS',NOBS);
	RUN;
	%IF &NOBS GT 0 %THEN %DO;
		TITLE1 "&CC - &YYYY / TRANSMISSION=&SS VERSION=&FVER";
		TITLE2 "COMPARISON VERSION &PVER VS VERSION &FVER";
		TITLE3 "NB OF VALUES DYSCREPANCIES BY VARIABLE";
		proc report data=diff_tot;
			columns file variable diff_nb;
			define file /display "File" format=$10. left;
			define variable /display "Variable" format=$VARLABEL. left;
			define diff_nb /display "# of diff." format=best8.0 ;
			compute file;
				if file="D-file" then call define (_row_,'style','style=[background=Lightblue');
				if file="H-file" then call define (_row_,'style','style=[background=Lightgreen');
				if file="R-file" then call define (_row_,'style','style=[background=Salmon');
				if file="P-file" then call define (_row_,'style','style=[background=Lightyellow');
			endcomp;
		run;
		TITLE1;
		TITLE2;
		TITLE3;
	%END;
	ODS &OUTPUTFORMAT CLOSE;ODS &OUTPUTFORMAT CLOSE;
%END;

%MEND;


