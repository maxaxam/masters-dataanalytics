/* Week 3 activities */
/* Activity 9*/

proc print data=orion.sales noobs;
	where Country='AU' and Job_Title contains 'Rep. IV';
run;
