
*Computing partial correlations;
proc corr data=work.bodyweight1;
	var  Bodyweight0 Energy_Intake0;
	Partial age;
run;
