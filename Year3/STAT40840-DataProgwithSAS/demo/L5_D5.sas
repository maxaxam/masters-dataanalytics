data work.auemps;
   set orion.sales;
   where Country='AU';
	Bonus=Salary*.10;
   if Bonus>=3000;
/*The subsetting IF statement tests a condition to determine whether 
   the DATA step should continue processing the current observation*/
run;

proc print data=work.auemps;
	var First_Name Last_Name Salary Bonus;
run;
