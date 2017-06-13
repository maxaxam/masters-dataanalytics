
/* Step 1: Sort the data based on the values of the variable 
or variables listed in the BY statement */

proc sort data=orion.sales
	       out=work.sales;
   by Salary;
run;

/* Step 2: View the output*/
proc print data=work.sales noobs;
   var Employee_ID Last_Name Salary;
run;
