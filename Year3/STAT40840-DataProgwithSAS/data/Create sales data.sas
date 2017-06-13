 /****************************************************************/
 /* The INFILE statement uses a Microsoft Windows path.          */
 /*                                                              */
 /* For UNIX, Linux, SAS University Edition, and SAS on Demand:  */
 /*      Change the INFILE statement to:                         */
 /*           infile "&path/newemps.csv" dlm=',';                */
 /*                                                              */
 /* For  z/OS:                                                   */
 /*      Change the INFILE statement to:                         */
 /*            infile "&path..rawdata(newemps)" dlm=',';         */
 /****************************************************************/

%let path=C:\Users\lkirwan\Desktop\Lectures\SAS\Datafiles;
data work.SALES;
   attrib Employee_ID length=8 format=12.;
   attrib First_Name length=$12;
   attrib Last_Name length=$18;
   attrib Gender length=$1;
   attrib Salary length=8;
   attrib Job_Title length=$25;
   attrib Country length=$2;
   attrib Birth_Date length=8;
   attrib Hire_Date length=8;

	infile "&path\sales.csv" dlm=','; 
   input
      Employee_ID
      First_Name
      Last_Name
      Gender
      Salary
      Job_Title
      Country
      Birth_Date
      Hire_Date
   ;
   run;
