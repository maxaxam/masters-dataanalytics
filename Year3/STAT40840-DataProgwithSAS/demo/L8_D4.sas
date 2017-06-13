*Grouping descriptive statistics;
proc sort data=work.bodyweight1;
	by gender;
run;

proc univariate data=work.bodyweight1;
	by gender;
	var bodyweight0;
run;
