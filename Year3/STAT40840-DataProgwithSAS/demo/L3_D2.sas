
/* Select observations for the report that meet a particular criterion */
proc print data=orion.sales;
	var Last_Name First_Name Salary;
	where Salary<25500;
run;

/* Suppress the printing of the observation number */
proc print data=orion.sales noobs;
	var Last_Name First_Name Salary;
	where Salary<25500;
run;


