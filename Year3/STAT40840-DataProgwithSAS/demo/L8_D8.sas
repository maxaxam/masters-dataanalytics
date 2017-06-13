*Creating a normal quantile plot;
proc univariate data=work.bodyweight1 noprint normal;
   qqplot bodyweight0 / normal;
run;

*Add a distribution reference line to quantile plot;
proc univariate data=work.bodyweight1 normal;
   qqplot bodyweight0 / normal (mu=est sigma=est);
run;
