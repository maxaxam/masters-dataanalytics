
*Graphical descriptive statistics;
*Creating a histogram;
proc univariate data=work.bodyweight1 noprint /*suppresses automatic printing of tables*/;
	histogram bodyweight0 ;
run;

*Creating a comparitive histogram;
proc univariate data=work.bodyweight1 noprint;
	class gender;
	histogram bodyweight0 ;
run;


*Adding a normal curve to a histogram;
*An INSET statement places a box or table of summary statistics, called an inset, directly in a graph;
proc univariate data=work.bodyweight1 noprint;
   histogram bodyweight0/ normal;
   inset n normal(ksdpval) / pos = ne ;
run;
