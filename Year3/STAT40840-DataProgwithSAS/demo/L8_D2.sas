*List the output tables that ods (output delivery system) produces;
ods trace on;
proc univariate data=work.bodyweight;
	var Bodyweight0 Energy_Intake0 Bodyweight6 Energy_Intake6 ;
run;
ods trace off;

*Output Added:
-------------
Name:       Moments
Name:       BasicMeasures
Name:       TestsForLocation
Name:       Quantiles
Name:       ExtremeObs;
quit;
ods select Moments ExtremeObs;
proc univariate data=work.bodyweight ;
	var Bodyweight0 Energy_Intake0 Bodyweight6 Energy_Intake6;
run;


Data work.bodyweight1;
	set work.bodyweight;
	if energy_intake0 gt 16000 then energy_intake0=".";
	else energy_intake0=energy_intake0;
run;
