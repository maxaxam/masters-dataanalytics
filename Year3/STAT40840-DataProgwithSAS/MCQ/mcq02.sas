title1 'The First Line';
title2 'The Second Line';
proc print data=orion.sales;
run;
title2 'The Next Line';
proc print data=orion.sales;
run;
title 'The Top Line';
proc print data=orion.sales;
run;
title;

proc print data=orion.sales;
   var Employee_ID Salary;
   where Country='AU';
   by Gender;
   label Salary='Annual Salary';
run;
