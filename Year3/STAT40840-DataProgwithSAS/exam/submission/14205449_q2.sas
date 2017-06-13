/* Student Number: 14205449 */
/* Data Programming with SAS */
/* Question 2 */

%let path=C:\SAS\exam;

/* a */
data work.lung;
   	infile "&path\lung.csv"
           dlm=',' firstobs = 2;
  	input ID DATE AGE BMI FEV;
	informat DATE DDMMYY.; /* a */
run;

/* b */
ods trace on;
proc univariate data=work.lung;
	var FEV BMI;
run;

/* 
From the output we can see a lot of different statistics some of them being:
			FEV				BMI
mean 		68.7238296		21.208503
st.dev.		26.2083653		23.9779226
We can also see the quantiles and tests for location. 
Details in the output file.

We can also see that there are 3 outliers. Two outliers are for BMI and one for FEV.
Outliers for BMI: Observation 1042 (1214.34) and Observation 3937 (1654.38)
Outliers for FEV: Observation 6191 (581)


/* remove outliers */
data work.lung;
	set work.lung;
	where BMI<>1214.34 and BMI<>1654.38 /* remove BMI outliers */
		  and FEV<>581; /* remove FEV outliers */
run;

/* repeat analysis to validate */
ods trace on;
proc univariate data=work.lung;
	var FEV BMI;
run;

/* producing histogram */
proc univariate data=work.lung noprint;
	histogram FEV BMI / normal;
	inset n normal(ksdpval) / pos = ne ;
run;

/* producing qq plot */
proc univariate data=work.lung normal noprint;
	qqplot FEV BMI / normal (mu=est sigma=est);
run;

/* Note: After removing outliers we now have 2 potential outliers for BMI however in the context of the ones that were already removed and comparing it to them,
I don't believe they are actually outliers so they are left in.
From the histogram and QQ plot we can see that the new dataset for both variables is following normal distribution.
*/

/* c */
ods graphics on;
proc corr data=work.lung nomiss plots=matrix(histogram);
	var AGE BMI FEV;
run;
/* We can see that there is a positive correlation between AGE and BMI (0.43860) and
weak negative correlation between AGE and FEV (-0.33714).
There is almost no correlation between BMI and FEV (0.05135).
*/

/* d */
proc glm data=work.lung plots=(diagnostics) PLOTS(MAXPOINTS=10000);
	model FEV = AGE BMI /;
	output out=GLM_OUTPUT r=r p=p; 
run;

/* We can see that there is relationship relating FEV to AGE and BMI. We can see that our slope parameters would be for AGE -1.20789665 and for BMI 1.71839059.
p-values for both of them are <.0001 so there is no evidence that they are equal to 0 and we can reject null hypothesis that they are individually equal to zero.

/* e */
proc gplot data=work.GLM_OUTPUT;
	plot r * p;
run;

/* Histogram to test for normality */
proc univariate data=GLM_OUTPUT noprint;
	histogram r / normal;
	inset n normal(ksdpval) / pos = ne ;
run;

proc univariate data=GLM_OUTPUT noprint;
	qqplot r / normal (mu=est sigma=est);
run;

/* From the gplot output we can see that the residuals are randomly distributed vs predicted values.
From the histogram of residuals we can see that they are bell shaped and we can conclude that they are normally distributed.
Similarly, from the QQ plot we can see that they are following theoretical straight line so we can conclude that they are normally distributed.
We can see additional plots that confirm that residuals are normals distributed from the plots produced proc glm under d) part of the question.
*/

/* f */
proc mixed data=work.lung plots=all;
	class ID;
	model FEV = AGE BMI / solution;
	repeated / subject=ID type=ar(1) r;
run;

/* We have used autoregressive covariance sructure for this analysis.
The Subject Effect was ID as we have repeated measurments for a patient.
Our data set has 589 class levels (i.e. 589 different patients).
Our max number of observations per subject are 48 and we have used all of the 7285 observations.
SAS has produced results in 3 iterations.

We have gotten following random effects estimate:
AR(1)	0.8888
with Residual of 560.72

Both AIC (56339.1) and BIC (56347.8). 
This are quite high values but we would need another model to compare to be able to conclude if this model is good.

We got following solutions for the fixed effects. All of the fixed effects are significant as p-value is <.0001 for all of them
			Estimate
Intercept	76.3447
AGE			-0.9495
BMI			0.6810

If we compare to the GLM output we can see that the esitmates for both AGE and BMI have reduced
but kept the same sign. Reduction in BMI is bigger telling us that in MIXED model this parameter has less influence on the final model.

*/
