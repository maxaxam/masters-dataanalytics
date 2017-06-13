*GLM - Comparing means;
 ods graphics on;
 proc glm data=work.bodyweight1 plots=(diagnostics);
 class gender;
    model bodyweight0 = age energy_intake0 gender / ;
	lsmeans gender / pdiff;
 run;
 quit;
