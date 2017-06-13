/*Using a SAS dataset as an input*/

/*The DATA statement begins a DATA step and provides the name of the SAS data set to create*/
data work.subset1;
/*The SET statement reads observations from an existing SAS data set for further 
processing in the DATA step*/
	set orion.sales;
/*The WHERE statement selects observations from a SAS data set that meet a particular condition*/
	where Country='AU' and
         Job_Title contains 'Rep';
run;

proc print data=work.subset1 noobs;
run;


