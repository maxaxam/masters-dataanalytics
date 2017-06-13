
*Computing Confidence Limits for the Mean, Standard Deviation, and Variance;
proc univariate data=work.bodyweight1 cibasic;
	var bodyweight0;
run;
