data work.subset1;
	set orion.sales;
	where Country='AU' and
         Job_Title contains 'Rep';
	Bonus=Salary*.10;
	keep First_Name Last_Name Salary 
		  Job_Title Hire_Date Bonus;
/*The KEEP statement specifies all variables to include in the output data set*/
run;

proc print data=work.subset1;
run;


