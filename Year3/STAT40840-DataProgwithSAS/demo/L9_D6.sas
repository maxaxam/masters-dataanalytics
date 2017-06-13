
*Generalised linear mixed model;
proc glimmix data=work.stacked plots=all;
	class gender time Participant_ID;
	model y1 = age  gender y2 / solution link=log dist=normal;
	random   _residual_ /subject=Participant_ID type=arh(1)  g ;
run;

*Blocking of the variance-covariance matrix and parameter heterogeniety;

proc glimmix data=work.stacked plots=all;
	class gender time Participant_ID;
	model y1 = age  gender y2 / solution link=power(0.5) dist=normal;
	random   _residual_ /subject=Participant_ID type=arh(1) group=gender g ;
*hypothesis of equality of the covariance matrices ;
	covtest 'Equal Covariance Matrices'  homogeneity;
run;

*The COVTEST statement requests a test of homogeneity—that is, it tests whether varying the covariance parameters
by the group effect provides a significantly better fit compared to a model in which different groups share the same parameter.; 
