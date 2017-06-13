*Generalised Linear Model;

 proc genmod data=work.bodyweight1 plots=all;
    model bodyweight0 = age energy_intake0 / link=log dist=normal;
	output out=genmod_out pred= Pred resraw = Resraw;
 run;

 proc univariate normal data=work.genmod_out;
 	var resraw;
 	qqplot resraw /normal(mu=est sigma=est);
 run;

 proc genmod data=work.bodyweight1 plots=all;
    model bodyweight0 = age energy_intake0 / link=power(0.5) dist=normal;
	output out=genmod_out2 pred= Pred resraw = Resraw;
 run;
