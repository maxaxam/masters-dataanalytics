data work.subset1;
	set orion.sales;
	where Country='AU' and
         Job_Title contains 'Rep';
	Bonus=Salary*.10;
	drop Employee_ID Gender Country Birth_Date;
/*The DROP statement specifies the variables to exclude from the output data set*/
run;

proc print data=work.subset1;
run;


