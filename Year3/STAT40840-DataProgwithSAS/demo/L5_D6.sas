/*Use a LABEL statement in a DATA step to permanently assign labels to variables. 
The labels are stored in the descriptor portion of the data set.*/

data work.subset1;
	set orion.sales;
	where Country='AU' and
         Job_Title contains 'Rep';
	Bonus=Salary*.10;
	label Job_Title='Sales Title'
		   Hire_Date='Date Hired';
	drop Employee_ID Gender Country Birth_Date;
run;

proc contents data=work.subset1;
run;

/*To use labels in the PRINT procedure, use the LABEL option in the PROC PRINT statement*/
proc print data=work.subset1 label;
run;
/*Use the PROC PRINT SPLIT= option to split labels across lines based on a split character*/
proc print data=work.subset1 split=' ';
run;
