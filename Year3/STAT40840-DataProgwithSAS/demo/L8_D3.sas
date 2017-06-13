
*Creating a frequency table;
ods select Frequencies;
proc univariate data=work.bodyweight1 freq;
   var gender;
run;
