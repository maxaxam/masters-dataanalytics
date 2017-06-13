*Producing a scatterplot matrix;
ods graphics on;
proc corr data=work.bodyweight1 nomiss plots=matrix(histogram);
   var Age  Bodyweight0 Energy_Intake0 Bodyweight6 Energy_Intake6;
run;
ods graphics off;
