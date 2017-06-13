*Saving Summary Statistics in an OUT= Output Data Set;
proc univariate data=work.bodyweight1;
	var bodyweight0;
	output out=work.means mean=m_weight0 ;
run;
proc print data=work.means;
run;


*Saving Output Table;
ods output Moments=work.Moments;
proc univariate data=work.bodyweight1;
	var bodyweight0;
run;
proc print data=work.moments;
run;
