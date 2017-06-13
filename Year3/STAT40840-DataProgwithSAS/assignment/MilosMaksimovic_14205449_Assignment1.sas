/*
Name: Milos Maksimovic
Student ID: 14205449
Course: Data Programming with SAS
*/

/* Assignment 1 - Question 1 */
proc sort data=assign.assignment1
		  out=work.assignment1_q1;
	by Gender;
run;

proc print data=work.assignment1_q1 noobs split=',';
	var Bodyweight0 Bodyweight6;
	where Age between 40 and 50;
	by Gender;
	id Participant_ID;
	label Bodyweight0 = 'Weight at the start, in kg'
		  Bodyweight6 = 'Weight after 6 months, in kg';
run;

/* Assignment 1 - Question 2 */
data assign.assignment1_q2;
	set assign.assignment1;
	where Age > 35;
	Change_Bodyweight = Bodyweight0 - Bodyweight6;
	Change_EnergyIntake = (Energy_Intake0 - Energy_Intake6) / Energy_Intake0;
	label Change_Bodyweight = 'Change in weight, in kg'
		  Change_EnergyIntake = 'Proportional change in energy intake, in Kcal/Day';
	format Change_EnergyIntake percent10.3;
	if Change_EnergyIntake > 0.1 and 
		Change_Bodyweight ~= . and 
		Change_EnergyIntake ~=.;
run;

proc print data=assign.assignment1_q2 label split=',';
	var Change_Bodyweight Change_EnergyIntake;
run;
