*Repeated Measures;

data work.stacked;
	set work.bodyweight1;
		y1=bodyweight0 ; y2=energy_intake0; time=1; output;
		y1=bodyweight6 ; y2=energy_intake6 ; time=2; output;
	drop bodyweight0 bodyweight6 energy_intake0 energy_intake6;
run;

*Linear Mixed Model;
ods graphics on;
proc mixed data=work.stacked plots=all;
	class gender time Participant_ID;
	model y1 = age  gender y2 / solution;
	repeated   /subject=Participant_ID type=ar(1) r;
run;

proc mixed data=work.stacked plots=all;where participant_id ne "H196";
	class gender time Participant_ID;
	model y1 = age  gender y2 / solution;
	repeated   /subject=Participant_ID type=ar(1) r;
run;

*Note proc mixed does not have an output statement;
proc mixed data=work.stacked ;
	class gender time Participant_ID;
	model y1 = age  gender y2 / solution outp=mixed_out;
	repeated   /subject=Participant_ID type=ar(1) r;
run;
