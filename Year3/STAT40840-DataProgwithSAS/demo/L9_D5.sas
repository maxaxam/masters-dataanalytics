*Alternative structures for R matrix;
proc mixed data=work.stacked;
	class gender time Participant_ID;
	model y1 = age  gender y2 / solution;
	repeated   /subject=Participant_ID type=arh(1) r;
run;
