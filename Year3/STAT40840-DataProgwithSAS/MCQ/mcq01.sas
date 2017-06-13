%let path=C:\Users\milosma\OneDrive\Documents\MSc in Data Analytics\Year 3 - Semester 2\STAT40840 - Data Prog with SAS\Data Files;

data work.staff;
   length First_Name $ 12
          Last_Name $ 18
          Job_Title $ 25;
   infile "&path\newemps.csv"
           dlm=',';
   input First_Name $ Last_Name$
         Job_Title $ Salary;
run;
proc print data=work.staff;
run;
proc means data=work.staff;
   var Salary;
run;

  /*
Report created for budget
presentation; revised October 15.
  */
proc print data=work.staff;
run;

proc print data=work.newsalesemps
run;


proc contents data=work.newsalesemps;
run;

libname reports 'C:\SAS';
libname 3456a 'C:\SAS';
libname orion C:\SAS;

libname reports 'C:\SAS';
libname reports clear;

libname or_01 'C:\SAS';
libname or_01 clear;

libname orion/01 'C:\SAS';
libname 1_or_a 'C:\SAS';
libname orionstar 'C:\SAS';
