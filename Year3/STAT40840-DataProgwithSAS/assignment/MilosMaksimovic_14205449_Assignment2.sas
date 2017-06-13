%let path=C:\SAS\assignment;

/*
Name: Milos Maksimovic
Student ID: 14205449
Course: Data Programming with SAS
*/

/* Assignment 2 */

proc import out=work.assign2xlsx datafile="&path\Assignment 2.xlsx"
            dbms=xlsx REPLACE;
     sheet="Sheet1"; 
     getnames=YES;
run;

/* Converting to correct data types */
data work.assign2;
	set work.assign2xlsx;
	ApoA1_new = input(ApoA1, 8.);
	ApoB_new = input(ApoB, 8.);
	ApoC2_new = input(ApoC2, 8.);
	ApoC3_new = input(ApoC3, 8.);
	ApoE_new = input(ApoE, 8.);
	Glucose_new = input(Glucose, 8.);
	NEFA_new = input(NEFA, 8.);
	Insulin_new = input(Insulin, 8.);
	Chol_new = input(Chol, 8.);
	TAG_new = input(TAG, 8.);
	drop ApoA1 ApoB ApoC2 ApoC3 ApoE Glucose NEFA Insulin Chol TAG;
	rename 	ApoA1_new=ApoA1 ApoB_new=ApoB ApoC2_new=ApoC2 ApoC3_new=ApoC3 ApoE_new=ApoE Glucose_new=Glucose 
			NEFA_new=NEFA Insulin_new=Insulin Chol_new=Chol TAG_new=TAG;
run;

proc contents data=work.assign2;
run;

/* Question 1 */
proc univariate data=work.assign2;
	var Age BMI ApoA1 ApoB ApoC2 ApoC3 ApoE Glucose NEFA Insulin Chol TAG;
run;

ods select Frequencies;
proc univariate data=work.assign2 freq;
	var Sex;
run;

proc sort data=work.assign2;
	by Sex;
run;

proc univariate data=work.assign2;
	by Sex;
	var Age BMI ApoA1 ApoB ApoC2 ApoC3 ApoE Glucose NEFA Insulin Chol TAG;
run;

ods graphics on;
proc univariate data=work.assign2 plot;
	by Sex;
	var ApoA1 ApoB ApoC2 ApoC3 ApoE Glucose NEFA Insulin Chol TAG;
run;

ods graphics on;
proc univariate data=work.assign2 noprint;
	histogram Age BMI ApoA1 ApoB ApoC2 ApoC3 ApoE Glucose NEFA Insulin Chol TAG / normal;
	inset n normal(ksdpval) / pos = ne ;
run;

ods graphics on;
proc univariate data=work.assign2 normal noprint;
	qqplot Age BMI ApoA1 ApoB ApoC2 ApoC3 ApoE Glucose NEFA Insulin Chol TAG / normal (mu=est sigma=est);
run;

proc sort data=work.assign2;
	by Time;
run;

ods graphics on;
proc univariate data=work.assign2 plot;
	by Time;
	var ApoA1 ApoB ApoC2 ApoC3 ApoE Glucose NEFA Insulin Chol TAG;
run;

/* Question 2 */
proc corr data=work.assign2;
	var ApoA1 ApoB ApoC2 ApoC3 ApoE Glucose NEFA Insulin Chol TAG;
run;

ods graphics on;
proc corr data=work.assign2 nomiss plots=matrix(histogram);
	var ApoA1 ApoB ApoC2 ApoC3 ApoE Glucose NEFA Insulin Chol TAG;
run;
ods graphics off;

/* Question 3 */
proc corr data=work.assign2;
	var Glucose Insulin;
	Partial BMI;
run;

/* Question 4 */

data work.assign2base;
	set work.assign2;
	where Time = 0;
run;

ods html;
ods graphics on;
proc glm data=work.assign2base plots=(diagnostics); 
	class Sex; 
	model TAG = Age Sex BMI / ;
	lsmeans Sex / pdiff;
	output out=glm_out r=r p=p; 
run;

/* Question 5 */
ods graphics on;
proc genmod data=work.assign2base plots=all; 
	class Sex; 
	model TAG = Age Sex BMI / link=log dist=normal;
	lsmeans Sex / pdiff;
	output out=genmod_out pred=Pred resraw=Resraw; 
run;

proc univariate data=work.genmod_out;
	var resraw;
	qqplot resraw / normal(mu=est sigma=est);
run;

/* Question 6 */
ods graphics on;
proc glimmix data=work.assign2 plots=all;
	class Sex Time ID;
	model insulin = Age Sex BMI / solution dist=normal;
	random _residual_ /subject=ID type=ar(1) g;
	covtest 'Equal Covariance Matrices' homogeneity;
run;

ods graphics on;
proc glimmix data=work.assign2 plots=all;
	class Sex Time ID;
	model insulin = Age Sex BMI / solution link=log dist=normal;
	random _residual_ /subject=ID type=arh(1) g;
	covtest 'Equal Covariance Matrices' homogeneity;
run;

/* Grouped by sex*/
ods graphics on;
proc glimmix data=work.assign2 plots=all;
	class Sex Time ID;
	model insulin = Age Sex BMI / solution dist=normal;
	random _residual_ /subject=ID type=ar(1) group=Sex g; 
	covtest 'Equal Covariance Matrices' homogeneity;
run;

ods graphics on;
proc glimmix data=work.assign2 plots=all;
	class Sex Time ID;
	model insulin = Age Sex BMI / solution dist=normal;
	random _residual_ /subject=ID type=arh(1) group=Sex g;
	covtest 'Equal Covariance Matrices' homogeneity;
run;

