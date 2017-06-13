/* At the start of the session make sure that your library is defined*/
%let path=C:\SAS\data;/*Insert your own path here*/
libname orion "&path";

/* Run default Print statement*/
proc print data=orion.sales;
run;

/* Select particular variables for report */
proc print data=orion.sales;
	var Last_Name First_Name Salary;
run;

/* Include the sum of a variable in the report */
proc print data=orion.sales;
	var Last_Name First_Name Salary;
	sum salary;
run;
