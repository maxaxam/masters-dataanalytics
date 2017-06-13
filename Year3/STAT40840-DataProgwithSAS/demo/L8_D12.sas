
*Regression analysis;
ods graphics on;
 proc reg data=work.bodyweight1;
    model bodyweight0 = age energy_intake0;
 run;

 *Saving residuals and predicted values;
ods graphics on;
 proc reg data=work.bodyweight1;
    model bodyweight0 = age energy_intake0;
	output out=pred r=r p=p;
 run;
quit;
