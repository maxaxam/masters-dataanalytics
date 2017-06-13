data work.subset1;
	set orion.sales;
	where Country='AU' and
         Job_Title contains 'Rep' and
         Hire_Date<'01jan2000'd;
	Bonus=Salary*.10;
/*The assignment statement evaluates an expression and assigns the result to a new or existing variable*/
run;

proc print data=work.subset1 noobs;
	var First_Name Last_Name Salary Job_Title Bonus Hire_Date;
	format Hire_Date date9.;
run;

