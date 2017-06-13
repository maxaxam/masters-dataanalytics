%let path=C:\SAS\pastexams;

data work.grass_yield;
	length
		Country $ 20;
   	infile "&path\grass.csv"
           dlm=',';
  	input Site Country $ Harvest_Date DDMMYY10.
         Plot Diversity Grass1_Yield Grass2_Yield
		 Clover1_Yield Clover2_Yield Weed_Yield;
	format Harvest_Date DDMMYY6.;
	Harv_Yield = (Grass1_Yield + Grass2_Yield 
		+ Clover1_Yield + Clover2_Yield + Weed_Yield);
	label 
		Site = 'Site identification number'
		Grass1_Yield = 'Harvested yield of grass1 (tonnes ha-1)'
		Grass2_Yield = 'Harvested yield of grass2 (tonnes ha-1)'
		Clover1_Yield = 'Harvested yield of clover1 (tonnes ha-1)'
		Clover2_Yield = 'Harvested yield of clover2 (tonnes ha-1)'
		Weed_Yield = 'Harvested yield of weed (tonnes ha-1)';
run;

data work.Ireland;
	set work.grass_yield;
	where Country='Ireland';
	keep Site Diversity Harv_Yield;
run;

proc sort data=work.Ireland
		  out=work.Ireland_sorted;
	by Diversity;
run;

proc print data=work.Ireland_sorted;
	by Diversity;
	sum Harv_Yield;
	var Harv_Yield;
run;

/*
proc contents data=work.grass_yield;
run;
proc print data=work.grass_yield;
run;
proc print data=work.Ireland;
run;
proc print data=work.Ireland_sorted;
run;
*/

data work.climate;
	input Site Precip Temp;
	datalines;
	1 709.2666667 10.49944702
2 650.5 4.282713027
3 466.5 12.77744771
4 572.775 10.14486322
5 887.5 8.160566957
6 991.2 5.847586174
7 963.925 5.701407478
8 802.32 10.68555973
9 660.0333333 11.99980946
10 474.975 7.280392432
11 452.65 8.279890424
12 452.65 8.279890424
13 650.3 9.991633537
14 1234.95 8.048057115
15 874.28 4.732266174
16 687.15 6.435535601
17 553.9666667 3.499460912
18 484.5 8.631344497
19 557.6333333 10.02695489
20 301.75 13.32018699
21 660.8666667 10.51963747
22 573.5666667 8.52970286
23 592.95 9.126892438
24 630.6666667 2.881979939
25 787.825 8.698532444
26 843.475 10.83741198
27 1146.6 10.66739726
28 885.1 6.452229895
29 649.3666667 10.27370231
30 1085.275 6.245765494
;
run;

proc sort data=work.climate;
	by Site;
run;

data work.combined;
	merge work.grass_yield work.climate;
	by Site;
run;

ods graphics on;
proc corr data=work.combined nomiss plots=matrix(histogram);
	var Harv_Yield Diversity Precip Temp;
run;

proc glm data=work.combined plots=(diagnostics);
	model Harv_Yield = Diversity Precip Temp /;
	output out=glm_out r=r p=p; 
run;

proc gplot data=glm_out;
	plot r * p;
run;

proc univariate data=glm_out noprint;
	histogram r / normal;
	inset n normal(ksdpval) / pos = ne ;
run;

proc univariate data=glm_out noprint;
	qqplot r / normal (mu=est sigma=est);
run;

proc mixed data=work.combined plots=all;
	class Country;
	model Harv_Yield = Diversity Precip Temp / solution;
	repeated / subject=Country type=ar(1) r;
run;


proc mixed data=work.combined plots=all;
	class Country;
	model Harv_Yield = Diversity Precip Temp / solution;
	repeated / subject=Country type=arh(1) r;
run;

proc print data=work.climate;
run;
proc contents data=work.combined;
run;
proc print data=work.combined label;
run;
