ods select Moments ExtremeObs;
proc univariate data=work.bodyweight noprint;
	var Bodyweight0 Energy_Intake0 Bodyweight6 Energy_Intake6;
run;
