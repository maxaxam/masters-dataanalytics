/* We want a report that lists only the Australian sales representatives */

proc print data=orion.sales noobs;
	var Last_Name First_Name Country Job_Title;
run;

/* Include a WHERE statement to subset by Country and Job_Title */
proc print data=orion.sales noobs;
	var Last_Name First_Name Country Job_Title;
	where Country='AU' and 
         Job_Title contains 'Rep';
run;
