/* Creating grouped report */

/*Step 1: Sort the data set to group the observations */ 

proc sort data=orion.sales
	       out=work.sales;
   by Country descending Salary;
run;

proc print data=work.sales noobs;
run;

/* Step 2: Specify report grouping */
proc print data=work.sales noobs;
	by Country;
run;
