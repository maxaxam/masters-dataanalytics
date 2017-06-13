data orion.sales;
   attrib Employee_ID length=8 format=12.;
   attrib First_Name length=$12;
   attrib Last_Name length=$18;
   attrib Gender length=$1;
   attrib Salary length=8;
   attrib Job_Title length=$25;
   attrib Country length=$2;
   attrib Birth_Date length=8;
   attrib Hire_Date length=8;

	infile "&path\sales_updated.csv" dlm=','; 
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

proc contents data=orion.sales;
run;

data orion.customer_dim;
   attrib Customer_ID length=8 format=8.;
   attrib Customer_Country length=$2;
   attrib Customer_Gender length=$1;
   attrib Customer_Name length=$50;
   attrib Customer_FirstName length=$25;
   attrib Customer_LastName length=$25;
   attrib Customer_BirthDate length=8;
   attrib Customer_Age_Group length=$15;
   attrib Customer_Type length=$50;
   attrib Customer_Group length=$50;
   attrib Customer_Age length=8;

	infile "&path\customer_dim.csv" dlm=',' FIRSTOBS=2; 
   input
      Customer_ID
      Customer_Country
      Customer_Gender
      Customer_Name
      Customer_FirstName
      Customer_LastName
      Customer_BirthDate
      Customer_Age_Group
      Customer_Type
	  Customer_Group
	  Customer_Age
   ;
   run;

proc contents data=orion.customer_dim;
run;

proc print data=orion.customer_dim;
run;
