/* Student Number: 14205449 */
/* Data Programming with SAS */
/* Question 1 */

%let path=C:\SAS\exam;

/* a, b, c */
data work.forage_quality;
	length
		COUNTRY $ 20;
   	infile "&path\quality.csv"
           dlm=',' firstobs = 2;
  	input SITE COUNTRY $ DATE
         P_CLOVER YIELD PROTEIN FIBRE;
	informat DATE DDMMYY.; /* a */
	format DATE DDMMYY6.; /* b */
	PROTEIN_YIELD = (PROTEIN / 100) * YIELD; /* c */
	label  /* b */
		SITE = 'Site identification number'
		YIELD = 'Total yield of plant biomass (kg/ha)'
		PROTEIN = 'Percentage of protein in biomass (% biomass)'
		FIBRE = 'Percentage of fibre in biomass (% biomass)';
run;

/* d */
data work.local;
	set work.forage_quality;
	where COUNTRY='Ireland' or COUNTRY='Wales';
	keep SITE P_CLOVER PROTEIN_YIELD;
run;

/* e */
proc sort data=work.local
		  out=work.local_sorted;
	by P_CLOVER;
run;

proc print data=work.local_sorted;
	by P_CLOVER;
	sum PROTEIN_YIELD;
	var PROTEIN_YIELD;
run;

/* f */

data work.nitrogen;
	input SITE NITROGEN;
	datalines;
1	150
10	150
11	150
13	40
14	80
15	150
18	120
20	120
22	0
23	60
24	135
26	120
27	90
36	93
;
run;

/* g */
data work.combined;
	merge work.forage_quality work.nitrogen;
	by SITE;
run;
