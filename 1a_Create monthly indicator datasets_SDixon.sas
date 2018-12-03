/* Program creating a series of variables for almost all youth in the 1990-1999 birth cohorts */  
/* Will draw on these datasets in the next program */

**Part A. Select a broadly-defined birth cohort population for the purpose of compiling data; 
**Part B. Get data on NCEA credits and all qualification attainment, by calendar year;
**Part C. Create calendar month vectors of place of residence data, days spent overseas;
**Part D. Create calendar month vectors capturing other activities and income flows, including
      school and tertiary enrolment, industry training enrolment,
      employment,  earnings, benefit receipt,  custodial and community sentences served, student allowance receipt;
**Note that the numbering sequence used for all monthly vectors is based on the EMS (or LEED) data structure, 
    where the first calendar month is April 1999;


%let VERSION=20160224;   
libname cen ODBC dsn=idi_clean_&VERSION._srvprd schema=cen_clean;
libname dol ODBC dsn=idi_clean_&VERSION._srvprd schema=dol_clean;
libname hlfs ODBC dsn=idi_clean_&VERSION._srvprd schema=hlfs_clean;
libname leed ODBC dsn=idi_clean_&VERSION._srvprd schema=from_leed_clean;
libname moe ODBC dsn=idi_clean_&VERSION._srvprd schema=moe_clean;
libname msd_leed ODBC dsn=idi_clean_&VERSION._srvprd schema=from_leed_clean;
libname msd ODBC dsn=idi_clean_&VERSION._srvprd schema=msd_clean;
libname sla ODBC dsn=idi_clean_&VERSION._srvprd schema=sla_clean;
libname moe ODBC dsn=idi_clean_&VERSION._srvprd schema=moe_clean;
libname cor ODBC dsn=idi_clean_&VERSION._srvprd schema=cor_clean;
libname moj ODBC dsn=idi_clean_&VERSION._srvprd schema=moj_clean;
libname acc ODBC dsn=idi_clean_&VERSION._srvprd schema=acc_clean;
libname cus ODBC dsn=idi_clean_&VERSION._srvprd schema=cus_clean;
libname lisnz ODBC dsn=idi_clean_&VERSION._srvprd schema=lisnz_clean;
libname ms ODBC dsn=idi_clean_&VERSION._srvprd schema=ms_clean;
libname sofie ODBC dsn=idi_clean_&VERSION._srvprd schema=sofie_clean;
libname dbh ODBC dsn=idi_clean_&VERSION._srvprd schema=dbh_clean;
libname br ODBC dsn=idi_clean_&VERSION._srvprd schema=br_clean;
libname cyf ODBC dsn=idi_clean_&VERSION._srvprd schema=cyf_clean;
libname dia ODBC dsn=idi_clean_&VERSION._srvprd schema=dia_clean;
libname pol ODBC dsn=idi_clean_&VERSION._srvprd schema=pol_clean;
libname moh ODBC dsn=idi_clean_&VERSION._srvprd schema=moh_clean;
libname data ODBC dsn=idi_clean_&VERSION._srvprd schema=data;
libname wff ODBC dsn=idi_clean_&VERSION._srvprd schema=wff_clean;
libname ird ODBC dsn=idi_clean_&VERSION._srvprd schema=ir_clean;
libname security ODBC dsn=idi_clean_&VERSION._srvprd schema=security;
libname sandmoe ODBC dsn=idi_sandpit_srvprd schema="clean_read_moe";
libname sandcen ODBC dsn=idi_sandpit_srvprd schema="clean_read_cen";
libname sandmoh ODBC dsn=idi_sandpit_srvprd schema="clean_read_moh_PRIMHD";

libname Project "\\wprdfs08\TreasuryData\MAA2013-16 Citizen pathways through human services\Social Investment_2016\1_Indicator_at_age_datasets\dataset_rerun_24022016";
libname basedata "\\wprdSAS10\TreasuryData\MAA2013-16 Citizen pathways through human services\Sylvia\Youth Service\Data 2017 revisions\Base datasets";
libname suppdata "\\wprdSAS10\TreasuryData\MAA2013-16 Citizen pathways through human services\Sylvia\Youth Service\Data 2017 revisions\Supp datasets";;

%let path=\\wprdsas10\TreasuryData\MAA2013-16 Citizen pathways through human services\sylvia\Youth Service\Programs to be shared;
* CALL AnI generic macros that not related to specific collections;
* Call macro that includes An I formats;

%include "&path.\Stand_macro_new.sas";
%include "&path.\FORMATS_new.sas";


*%include "\\wprdfs08\TreasuryData\MAA2013-16 Citizen pathways through human services\Common Code\Std_macros_and_libs\Std_macros.txt";
%let folder = basedata ;   **specify the libname you have allocated above to make the code below work;


*****************************************************;
**Part A;
**Select a broadly-defined population for the initial sample;
**Restrict to people who are on the IDI spine, and have an IRD number and an MoE nbr, meaning they have been linked
   to identities in these two domains;
**Birth years from 1990 to 1999;
******************************************************;

proc SQL;
	Connect to sqlservr (server=WPRDSQL36\iLeed database=IDI_clean_&version);
	create table CONC as select * from connection to  sqlservr
		(select 
			snz_uid, 
			snz_ird_uid, 
			snz_dol_uid, 
			snz_moe_uid, 
			snz_msd_uid,
			snz_dia_uid,
			snz_moh_uid, 
			snz_jus_uid from security.concordance);
quit;

proc SQL;
	Connect to sqlservr (server=WPRDSQL36\iLeed database=IDI_clean_&version);
	create table PERS as select * from connection to  sqlservr
		(select 
			snz_uid, 
			snz_sex_code,
			snz_birth_year_nbr,
			snz_birth_month_nbr,			
			snz_deceased_year_nbr,
			snz_deceased_month_nbr,
			snz_person_ind,
			snz_spine_ind
		from data.personal_detail);
quit;

proc sort data=conc;
	by snz_uid;
run;

Proc sort data=pers;
	by snz_uid;
run;

data Birth_1990_1999(keep=snz_uid snz_moh_uid snz_msd_uid snz_moe_uid dob dod birth_year snz_sex_code);
	merge conc (in=a) pers(in=b);
	by snz_uid;
	if a and b;
    if snz_birth_year_nbr>=1990 and snz_birth_year_nbr<=1999;
    if snz_person_ind=1;
	if snz_spine_ind=1;
	if snz_ird_uid~=. and snz_moe_uid~=.;
	format DOB DOD date9.;
	DOB=MDY(snz_birth_month_nbr,15,snz_birth_year_nbr);
	DOD=MDY(snz_deceased_month_nbr,15,snz_deceased_year_nbr);
    birth_year=year(dob);
run;

data &folder..popn;
set Birth_1990_1999;
run;

%let population=&folder..popn;

proc means data=&folder..popn;
run;
**

*****************************************************;
**Part B;
**NCEA Credits and all qualification attainment by year;
*****************************************************;

**School credit and qualification attainment;

%let first_anal_yr=2003;
%let last_anal_yr=2015;

proc sql;
	create table stand as select 
    snz_uid	
	,moe_sst_attained_year_nbr as AttainedYear
    ,moe_sst_study_provider_code as school
	,moe_sst_standard_code as StandardTable
	,moe_sst_exam_result_code as ExamResultCode
	from moe.student_standard
    where snz_uid in (select snz_uid from &population)
    and (AttainedYear>=&first_anal_yr and AttainedYear<=&last_anal_yr) 
    order by snz_uid, AttainedYear; 
quit;

data standard;
set stand;
schoolnbr=school*1;
standardtableid=StandardTable*1;
run;

/*
proc freq data=standard;
tables attainedyear;
run;
*/

* count credits gained at school only;
* where schools are secondary , composite, restricted composite, correspondence or special schools;

proc sql;
	create table STAND1 as select
		a.*,
		b.schooltypeid as open_sch_typeid
	from standard a 
    left join sandmoe.moe_school_profile b on a.schoolnbr=b.schoolnumber
		order by snz_uid, attainedyear;
quit;


data stand_lookup;
	set sandmoe.moe_standard_lookup;
	keep StandardTableID StandardLevel Credit standardtypecode InternalorExternal;
run;

proc freq data=stand_lookup;
tables StandardLevel Credit standardtypecode InternalorExternal /list missing;
run;


proc sort data=stand_lookup;
	by standardTableid;
run;

proc sort data=stand1;
	by standardTableid;
run;




data stand2;
	merge stand1(in=a) stand_lookup;
	by standardtableid;
	if a;
	* limiting to NCEA standards only;
	if standardtypecode=1 or standardtypecode=2;
	* limiting to Unit, acheivement and excluding scholarship standards;
	CR_gained=credit*1;
	if ExamResultCode in ('D','F','N','NC','V','Y') then
		CR_gained=0;
	* mainly failed and not attended exam;
	if AttainedYear>=2003;
	*Level=cats("L",StandardLevel);
    level=StandardLevel;
	if open_sch_typeid in (10026,10039,10060,10028,10031,10029,10030,10033, 10032, 10023, 10034, 10048) then
		inst='sch';
	if open_sch_typeid in (., 10010,10011,10012,10013,10014,10015,10016,10017, 10018, 10021, 10036, 10051) then
		inst='other';
	* inlcudes industry training;
    if InternalorExternal ='  ' then InternalorExternal='UN';
run;


data stand3;
set stand2;
if inst="sch" and CR_gained>0;
run;


**Summing all credits regardless of type;
 

  proc summary data=stand3 nway;
		class  snz_uid attainedyear Level;
		var CR_gained;
		output out=EDU_NQF_ach_sch (drop=_type_ _freq_) sum=;
	run;

**I am ignoring NCEA level 4 credits here as there are so few of them;

data EDU_NQF_ach_sch_2;
	set EDU_NQF_ach_sch;
	array credits (13,3) 
         ncea_cr_2003_L1-ncea_cr_2003_L3
         ncea_cr_2004_L1-ncea_cr_2004_L3  ncea_cr_2005_L1-ncea_cr_2005_L3 
         ncea_cr_2006_L1-ncea_cr_2006_L3  ncea_cr_2007_L1-ncea_cr_2007_L3 
         ncea_cr_2008_L1-ncea_cr_2008_L3  ncea_cr_2009_L1-ncea_cr_2009_L3
         ncea_cr_2010_L1-ncea_cr_2010_L3  ncea_cr_2011_L1-ncea_cr_2011_L3
         ncea_cr_2012_L1-ncea_cr_2012_L3  ncea_cr_2013_L1-ncea_cr_2013_L3
         ncea_cr_2014_L1-ncea_cr_2014_L3   ncea_cr_2015_L1-ncea_cr_2015_L3;
    do j=1 to 3;
    do ind=1 to 13; i=ind+2002;
	   if attainedyear=i and level=j then credits(ind,j)=CR_gained;
	   end;
	   end;
   run;

proc summary data=EDU_NQF_ach_sch_2 nway;
class snz_uid ;
var ncea_cr: ;
output out=credit_sum (drop=_type_ _freq_) sum=;
run;


%macro creditsum;
data &folder..ncea_credit_sum(keep=snz_uid  /*prop_cr_ext:*/  tot_ncea_cr:);
*merge credit_sum(in=a) credits_external2;  
set credit_sum;
*by snz_uid;
*if a;
**Sum up externally assessed and total credits;
/*
%do i=2003 %to 2014;
tot_ext_cr_by_&i=sum(of cr_ext_2003-cr_ext_&i);
tot_cr_by_&i=sum(of cr_tot_2003-cr_tot_&i);
if tot_ext_cr_by_&i~=. and tot_cr_by_&i~=. then do;
  prop_cr_ext_by_&i=tot_ext_cr_by_&i/tot_cr_by_&i;
  prop_cr_ext_by_&i._cat=put(prop_cr_ext_by_&i,credits. );
  end;
%end;
*/
**Rename variables so the following operation will work;
array L1(*) ncea_cr_2003_L1 ncea_cr_2004_L1  ncea_cr_2005_L1 ncea_cr_2006_L1 ncea_cr_2007_L1  ncea_cr_2008_L1
          ncea_cr_2009_L1 ncea_cr_2010_L1 ncea_cr_2011_L1 ncea_cr_2012_L1 ncea_cr_2013_L1 ncea_cr_2014_L1;
array L1a(*) a2003-a2014;
array L2(*) ncea_cr_2003_L2 ncea_cr_2004_L2 ncea_cr_2005_L2 
          ncea_cr_2006_L2 ncea_cr_2007_L2 ncea_cr_2008_L2 ncea_cr_2009_L2 ncea_cr_2010_L2 ncea_cr_2011_L2 
          ncea_cr_2012_L2 ncea_cr_2013_L2 ncea_cr_2014_L2;
array L2b(*) b2003-b2014;
array L3(*) ncea_cr_2003_L3 ncea_cr_2004_L3 ncea_cr_2005_L3
   ncea_cr_2006_L3 ncea_cr_2007_L3 ncea_cr_2008_L3
   ncea_cr_2009_L3 ncea_cr_2010_L3 ncea_cr_2011_L3 ncea_cr_2012_L3 ncea_cr_2013_L3 ncea_cr_2014_L3;
array L3c(*) c2003-c2014;
do i=1 to dim(L1);
   L1a(i)=L1(i);
   L2b(i)=L2(i);
   L3c(i)=L3(i);
end;
**Sum NCEA credits obtained up to the reference date;
%do i=2003 %to 2014;
  tot_ncea_cr_L1_by_&i=sum(of a2003-a&i);
  tot_ncea_cr_L2_by_&i=sum(of b2003-b&i);
  tot_ncea_cr_L3_by_&i=sum(of c2003-c&i);
%end;
run;

%mend creditsum;

%creditsum;


proc means data=&folder..ncea_credit_sum;
run;


**NCEA QUALIFICATIONS OBTAINED;
**The qualification formats are reproduced here for reference purposes;

Proc format;
value HA 
42='National Diploma at level 4 or above'
41='National Certificate at level 4 or above'
40='New Zealand Scholarship award'
39='NCEA level 3 (with Excellence)'
38='NCEA level 3 (with Merit)'
37='NCEA level 3 (with Achieve)'
36='NCEA level 3 (No Endorsement)'
35='Other NQF Qualification at level 3'
29='NCEA level 2 (with Excellence)'
28='NCEA level 2 (with Merit)'
27='NCEA level 2 (with Achieve)'
26='NCEA level 2 (No Endorsement)'
25='Other NQF Qualification at level 2'
19='NCEA level 1 (with Excellence)'
18='NCEA level 1 (with Merit)'
17='NCEA level 1 (with Achievement)'
16='NCEA level 1 (No Endorsement)'
15='Other NQF Qualification at level 1';

value HA_grouped
42,41,40='Level 4 Qualification or above'
39,38,37,36='NCEA level 3 Qualification'
35='Other NQF Qualification at level 3'
29,28,27,26='NCEA level 2 Qualification'
25='Other NQF Qualification at level 2'
19,18,17,16='NCEA level 1 Qualification'
15='Other NQF Qualification at level 1';

Value ha_grp
42,41,40,39,38,37,36,35='NCEA level 3 or above'
29,28,27,26,25='Level 2 Qualification'
19,18,17,16,15='NCEA level 1 Qualification'
0,.='No Formal NCEA Attainment';
run;


data student_qual; 
set moe.student_qualification;
	format nzqaloadeddate1 date9.;
	qual=moe_sql_qual_code;
	result=moe_sql_exam_result_code;
	awardingschool=moe_sql_award_provider_code;
	level=moe_sql_nqf_level_code;
	year=moe_sql_attained_year_nbr;
	end_year=moe_sql_endorsed_year_nbr;
	nzqaloadeddate1=input(compress(moe_sql_nzqa_load_date,"-"),yymmdd10.);
load_year=year(nzqaloadeddate1);
run;

proc freq data=student_qual;
tables year ;
run;

proc sql;
create table sec_qual as
select distinct
      a.*
from student_qual a 
   inner join &population b
on a.snz_uid=b.snz_uid
order by snz_uid, year;
quit;

data qual_lookup;
	set sandmoe.moe_qualification_lookup;
	rename qualificationtableid=qual;
run;

proc sort data=qual_lookup;
	by qual;
run;

proc sort data=sec_qual;
	by qual;
run;


* DEFINE NCEA ATTAINMENT: 
* limiting to national certificates which is 99& percent of records;
* excluding qualifications gained prior to 2006, before NCEA time;

DATA QualsHA; 
merge sec_qual(in=a) qual_lookup(in=b); 
by qual; 
if a; 
HA=0;
* Allows 2 years for loading qualifications;
if year=load_year or load_year-year<=2 or load_year=.; 
if NQFlevel in (0,.) then delete;
if year < 2003 then delete; 
if year>=2003 and year<=2014;
if nqflevel >= 4 and QualificationType=21 then ha=41;
else if nqflevel >= 4 and QualificationType=10 then ha=40;
else if nqflevel >= 4 then ha=42;
else if qualificationcode='1039' and result='E' then HA=39;
else if qualificationcode='1039' and result='M' then HA=38;
else if qualificationcode='1039' and result='ZZ' then HA=37;
else if qualificationcode='1039' and result='N' then HA=36;
else if nqflevel=3 then HA=35;
else if (qualificationcode='0973' or qualificationcode='973') and result='E' then HA=29;
else if (qualificationcode='0973' or qualificationcode='973') and result='M' then HA=28;
else if (qualificationcode='0973' or qualificationcode='973') and result='ZZ' then HA=27;
else if (qualificationcode='0973' or qualificationcode='973') and result='N' then HA=26;
else if nqflevel=2 then HA=25;
else if (qualificationcode='0928' or qualificationcode='928') and result='E' then HA=19;
else if (qualificationcode='0928' or qualificationcode='928') and result='M' then HA=18;
else if (qualificationcode='0928' or qualificationcode='928') and result='ZZ' then HA=17;
else if (qualificationcode='0928' or qualificationcode='928') and result='N' then HA=16;
else if nqflevel=1 then HA=15;
NCEA_L1=0; NCEA_L2=0; NCEA_L3=0; non_NCEA_L1=0; non_NCEA_L2=0; non_NCEA_L3=0; non_NCEA_L4=0;
if HA in (19,18,17,16) then NCEA_L1=1;
else if HA=15 then non_NCEA_L1=1;

else if HA in (29,28,27,26) then NCEA_L2=1;
else if HA=25 then non_NCEA_L2=1;

else if HA in (39,38,37,36) then NCEA_L3=1;
else if HA=35 then non_NCEA_L3=1;
else if HA in (42,41,40) then non_NCEA_L4=1;
*keep snz_uid year HA NCEA_L1 NCEA_L2 NCEA_L3 non_NCEA_L1 non_NCEA_L2 non_NCEA_L3 non_NCEA_L4 ;
run;

**The codes for ncea with no endorsement (16, 26, 36) dont appear in the resulting data set in practive;

/*
proc freq data=QualsHA; 
tables qualificationname /list missing;
where non_NCEA_L2=1;
run;

proc freq data=QualsHA; 
tables HA /list missing;
run;
*/

* Creating final dataset;
proc sql;
create table ind_NCEA_qual as select
snz_uid,
year,
max(NCEA_L1) as NCEA_L1,
max(NCEA_L2) as NCEA_L2,
max(NCEA_L3) as NCEA_L3,
max(non_NCEA_L1) as nonNCEA_L1,
max(non_NCEA_L2) as nonNCEA_L2,
max(non_NCEA_L3) as nonNCEA_L3,
max(non_NCEA_L4) as nonNCEA_L4
from QualsHA
group by snz_uid, year
order by snz_uid, year;
quit;


data &folder..NCEA_qual;
set ind_NCEA_qual;
run;





**TERTIARY QUALS;


proc format;
	value $lv8id
		"40","41","46", "60", "96", "98"      ="1"
		"36"-"37","43"                        ="2"
		"30"-"35"                       ="3"
		"20","21","25"                       ="4"
		"12"-"14"                       ="6"
		"11"                            ="7"
		"01","10"                       ="8"
		"90", "97", "99"                ="9"
		Other                           ="E";
run;

proc sql;
	create table TER_compl as
		select  
			a.snz_uid,
			a.moe_com_year_nbr as year,
			put(a.moe_com_qacc_code,$lv8id.) as att_TER_qual_type,
			a.moe_com_qual_level_code as level_num,
			a.moe_com_qual_nzsced_code			
		from moe.completion a inner join &population b
on a.snz_uid=b.snz_uid
where MDY(12,31,moe_com_year_nbr)<='31Dec2015'd
order by b.snz_uid, a.moe_com_year_nbr, a.moe_com_qual_level_code;
quit;


data TER_COMPL_; 
set TER_COMPL;
level=1*level_num;
if level=. then level=1*att_TER_qual_type;
field=substr(moe_com_qual_nzsced_code,1,2);
run;

**Get dataset containing highest qualification completed per person per year;

proc sort data=TER_COMPL_; 
by snz_uid year descending level; 
run;

data &folder..tertiary_quals(keep=snz_uid year level ); 
set TER_COMPL_; 
by snz_uid year descending level; 
if first.year;
run;

proc freq data=&folder..tertiary_quals;
tables year /list missing;
run;



**Industry training qualifications;

data it deletes;
	set moe.tec_it_learner;
if moe_itl_tot_credits_awarded_nbr>0 and moe_itl_sum_units_consumed_nbr>0;
if moe_itl_programme_type_code in ("NC","TC"); 
   **National certificate or Trade certificate - but nearly all are NC;
   *Limited credit programmes, Supplementary credit programmes, and records with missing
   prog type are not selected;
format startdate enddate  date9.;
	startdate=input(compress(moe_itl_start_date,"-"),yymmdd10.);
    startyr=year(startdate);
	if moe_itl_end_date ne '' then
		enddate=input(compress(moe_itl_end_date,"-"),yymmdd10.);
	if moe_itl_end_date=' ' then do;
        it_enddate_imputed=1;
        enddate=input((strip(31)||strip(12)||strip(startyr)),ddmmyy8.);
		end;
	if startdate>enddate then
		output deletes;
	else output it;
run;

proc freq data=it;
tables startyr /list missing;
run;


proc sql;
	create table itl_event as 
		SELECT distinct
snz_uid,
moe_itl_fund_code,
startdate,
enddate,
it_enddate_imputed
,moe_itl_level1_qual_awarded_nbr as L1
			,moe_itl_level2_qual_awarded_nbr as L2
			,moe_itl_level3_qual_awarded_nbr as L3
			,moe_itl_level4_qual_awarded_nbr as L4
			,moe_itl_level5_qual_awarded_nbr as L5
			,moe_itl_level6_qual_awarded_nbr as L6
			,moe_itl_level7_qual_awarded_nbr as L7
			,moe_itl_level8_qual_awarded_nbr as L8
   FROM IT 
   WHERE snz_uid IN (select distinct snz_uid from &population)
   order by snz_uid, startdate;
quit;


data IT_quals(keep=snz_uid  year level); 
set itl_event;
level=0;
	if L1=1 then level=1;
	if L2=1 then level=2;
	if L3=1 then level=3;
	if L4=1 then level=4;
	if L5=1 then level=5;
	if L6=1 then level=6;
	if L7=1 then level=7;
	if L8=1 then level=8;
qual_type='ITL';
if level>0;
year=year(enddate);
run;

proc summary data=it_quals nway;
class snz_uid year;
var level;
output out=&folder..IT_quals(drop= _:) max=;
run;



************************************************;
***Identify highest qualification level at each calendar year end;
*************Combine school (NCEA and nonNCEA) and tertiary and industry training qualification attainment;
***********************************************;


%let y1=2006;  *Year 1;
%let yn=2014;    *Year n;
%let z=2005;  *y1-1;

data quals;
merge &folder..NCEA_qual(keep=snz_uid year NCEA_L1 NCEA_L2 NCEA_L3 nonNCEA_L1 nonNCEA_L2  nonNCEA_L3 nonNCEA_L4) 
     &folder..tertiary_quals(keep=snz_uid year level rename=(level=tert_level))
     &folder..it_quals(keep=snz_uid year level rename=(level=it_level));
by snz_uid year;
**Highest per year;
array NCEA (*) ncea_&y1-ncea_&yn;
array nonNCEA (*) nonncea_&y1-nonncea_&yn;
array tert (*) terqual_&y1-terqual_&yn;
array it(*) itqual_&y1-itqual_&yn;
do i=1 to dim(NCEA);
   NCEA(i)=0;
   nonNCEA(i)=0;
   tert(i)=0;
   it(i)=0;
   end;
do i=1 to dim(NCEA);
   if year=(i+&z) and NCEA_L1 =1 then NCEA(i)=1;
   if year=(i+&z) and NCEA_L2 =1 then NCEA(i)=2;
   if year=(i+&z) and NCEA_L3 =1 then NCEA(i)=3;
   if year=(i+&z) and nonNCEA_L1 =1 then nonNCEA(i)=1;
   if year=(i+&z) and nonNCEA_L2 =1 then nonNCEA(i)=2;
   if year=(i+&z) and nonNCEA_L3 =1 then nonNCEA(i)=3;
   if year=(i+&z) and nonNCEA_L4 =1 then nonNCEA(i)=4;

   if year=(i+&z) and tert_level =1 then tert(i)=1;
   if year=(i+&z) and tert_level =2 then tert(i)=2;
   if year=(i+&z) and tert_level =3 then tert(i)=3;
   if year=(i+&z) and tert_level =4 then tert(i)=4;
   if year=(i+&z) and tert_level >=5 then tert(i)=5;
   if year=(i+&z) and it_level =1 then it(i)=1;
   if year=(i+&z) and it_level =2 then it(i)=2;
   if year=(i+&z) and it_level =3 then it(i)=3;
   if year=(i+&z) and it_level =4 then it(i)=4;
end;
run;


**Get max qualification level for each of the 3 types;

proc summary data=quals nway;
class snz_uid;
var ncea_&y1-ncea_&yn nonncea_&y1-nonncea_&yn  terqual_&y1-terqual_&yn  itqual_&y1-itqual_&yn;
output out=stats max=;
run;

/*
proc means data=stats;
var ncea_&y1-ncea_&yn nonncea_&y1-nonncea_&yn  terqual_&y1-terqual_&yn  itqual_&y1-itqual_&yn;
run;
*/

**Variables starting with an h show the highest level that had been achieved, before or during each reference year;

data &folder..highest_quals_achieved(keep=snz_uid hqual_&y1-hqual_&yn 
         hncea_&y1-hncea_&yn  hnonncea_&y1-hnonncea_&yn
         hterqual_&y1-hterqual_&yn  hitqual_&y1-hitqual_&yn);
set stats;
array NCEA (*) ncea_&y1-ncea_&yn;
array hncea(*) hncea_&y1-hncea_&yn;
array nonNCEA (*) nonncea_&y1-nonncea_&yn;
array hnonNCEA (*) hnonncea_&y1-hnonncea_&yn;
array tert (*) terqual_&y1-terqual_&yn; 
array htert (*) hterqual_&y1-hterqual_&yn;
array it(*) itqual_&y1-itqual_&yn;
array hit(*) hitqual_&y1-hitqual_&yn;
array hqual(*) hqual_&y1-hqual_&yn;
do i=1 to dim(NCEA);   
    hqual(i)=max(of NCEA(i), nonNCEA(i), tert(i), it(i));
    hncea(i)=ncea(i);
	hnonncea(i)=nonncea(i);
	htert(i)=tert(i);
	hit(i)=it(i);
	end;
do i=2 to dim(NCEA);
  if hncea(i)<hncea(i-1) then hncea(i) = hncea(i-1);
  if hnonncea(i)<hnonncea(i-1) then hnonncea(i) = hnonncea(i-1);
  if htert(i)<htert(i-1) then htert(i) = htert(i-1);
  if hit(i)<hit(i-1) then hit(i) = hit(i-1); 
  if hqual(i)<hqual(i-1) then hqual(i) = hqual(i-1);
end;
run;

proc means data=&folder..highest_quals_achieved;
var hqual_&y1-hqual_&yn hncea_&y1-hncea_&yn ;
run;



*******************************************************************;
**Part C;
**MONTHLY VECTORS OF RESIDENCE DATA, OVERSEAS DAYS;
**In the monthly vector numbering system, 1=April 1999;
*******************************************************************;
**The address event file originally used here in the 2016 evaluation was created by the Analytics and Insights team in Treasury;

**I had to change the code in 2017 and use the SNZ-generated table data.address_notification, which is similar;
**The current code converts the  meshblock from 2015 to 2013 coding in order to merge on the deprivation index etc 
  but in future versions this might need to be changed as SNZ will be using a more recent coding schema; 

**First get address and mb which is coded using 2015 geo codes;
**Add on local board area for Auckland;
**merge on the 2013 mb code using the area concordance file - this was obtained from SNZ;
**then merge on the NZDEP index in 2013 using the nzdep file - this was downloaded from the internet from the Otago School of Medicine pages;
**Then save variables for end of each month in the study period;

/*
proc sql;
create table resfile
as select 
   a.snz_uid
  ,input(b.region, 9.) as region
  ,input(b.ta, 9.) as ta
  ,input(b.meshblock, 18.) as mb
  ,b.startdate
  ,b.enddate
from &population a
inner join project.addressevent_20160224 b
on a.snz_uid=b.snz_uid
order by a.snz_uid, b.startdate;
quit;
*/

proc sql;
create table resfile
as select 
   a.snz_uid
  ,b.ant_meshblock_code 
  ,b.ant_ta_code 
  ,b.ant_region_code 
  ,input(compress(b.ant_notification_date,"-"),yymmdd10.) as startdate
  ,input(compress(b.ant_replacement_date,"-"),yymmdd10.) as enddate
from &population a
inner join data.address_notification b
on a.snz_uid=b.snz_uid
order by a.snz_uid, input(compress(b.ant_notification_date,"-"),yymmdd10.) ;
quit;

proc contents data=resfile;
run;

proc freq data=resfile;
tables /*ant_meshblock_code*/ ant_ta_code ant_region_code;
run;

data resfileb(drop=ant_meshblock_code ant_ta_code ant_region_code);
set resfile;
format startdate enddate date9.;
mb=ant_meshblock_code*1;
ta=ant_ta_code*1 ;
region=ant_region_code*1;
run;


**Add Auckland local boards and NZ dep index at this point;

proc sort data=resfileb ;
	by mb startdate; 
    run;

proc contents data=suppdata.area_concord_2015;
run;


* adding mb2013 to other region breakdowns;
* and creating a new TA variable, with Auckland broken down by Local Board;
**The area concord file was obtained from SNZ;

data local;
set suppdata.area_concord_2015(keep=mb2015_code mb2013_code  TA2015_code TA2015_label CB2015_code CB2015_label
    rename=(mb2015_code=mb mb2013_code=mb13  ));
if ta2015_label = 'Auckland' then TA_Auck_LB = "AKL - "||strip(cb2015_label);
	else TA_Auck_LB = ta2015_label;
ta_ext=TA2015_code;
if TA2015_code=76 then ta_ext=CB2015_code;
**Recode GB island to Devonport;
if ta_ext=7608 then do;
   ta_ext=7605;
   TA_Auck_LB = "AKL - Devonport-Takapuna Local Board Area";
end;
format TA_Auck_LB $45.;
run;


proc freq data=local;
tables ta_ext*TA_Auck_LB /list missing;
where TA2015_label='Auckland';
run;

proc sort data=local;
by mb;
run;

data pop_location;
	merge resfileb(in=a)
		  local( keep=mb mb13 ta_ext TA_Auck_LB);
	by mb;
	if a;	
run;	

proc sort data=pop_location;
by mb13;
run;

data nzdep(keep=mb13 nzdep2013);
set suppdata.nzdep2013(keep=mb_2013 nzdep2013); 
mb13=mb_2013*1;
run;

proc sort data=nzdep;
by mb13;
run;


data pop_locationb;
merge pop_location(in=a) nzdep;
by mb13;
if a;
run;

data check;
set pop_locationb;
if ta_ext~=ta;
run;
proc freq data=check;
tables ta_ext*TA_Auck_LB /list missing;
run;


**Generate monthly vectors containing NZDEP, TA and region, for period Dec 2007-Jun 2015;
**As at the last day of each month;
**Using LEED reference dates - month 105 to month 195;

proc sort data=pop_locationb;
by snz_uid startdate enddate;
run;

proc print data=pop_locationb(obs=100);
run;

%let m=105;
%let n=195;

data resfile2(drop=i start date&m-date&n)  ;
set pop_locationb;
array dates (*) date&m-date&n;
format start date&m-date&n date9.;
start='31Dec2007'd;
do i=1 to dim(dates);
  dates(i)=intnx("MONTH",start,(i-1),'S');
   end;
array nzdep(*) nzdep&m-nzdep&n;
array tla(*) ta&m-ta&n;
array reg(*) reg&m-reg&n;
do i=1 to dim(dates);
  if startdate<=dates(i)<=enddate or (startdate<=dates(i) and enddate=.) then do;
     nzdep(i)=nzdep2013;
     tla(i)=ta_ext;
     reg(i)=region;
    end;
	end;
run;

proc summary data=resfile2 nway;
class snz_uid;
var nzdep&m-nzdep&n ta&m-ta&n reg&m-reg&n;
output out=&folder..residence(drop=_:) max=;
run;


**Some people aren't in the address file for the relevant period;

proc means data=&folder..residence;
run;




***OVERSEAS DAYS PER MONTH;

proc sql;
create table os_spells
as select snz_uid, 
   datepart(pos_applied_date) format date9.  as startdate, 
   datepart(pos_ceased_date) format date9. as enddate
from data.person_overseas_spell
where snz_uid IN 
             (SELECT DISTINCT snz_uid FROM &population ) 
order by snz_uid, startdate;
quit;


data os_spells2;
set os_spells;
	if year(enddate)=9999 then enddate='31Dec2015'd;
	if year(startdate)=1900 then startdate='30Jun1997'd;
run;

proc sort data=os_spells2;
by snz_uid startdate enddate;
run;

**Count all days spent overseas in each calendar month from Jan 2004 to Jun 2015;
**Use LEED dates to index each month;

%let m=58;
%let n=195;

data os_spells3(drop=i start_window end_window days);
set os_spells2;
start='01Jan2004'd; 
array osdays [*] os_da_&m-os_da_&n ; * days os;
do i=1 to dim(osdays);
   start_window=intnx('month',start,i-1,'S');
   end_window=(intnx('month',start,i,'S'))-1;
   format start_window end_window date9.;  
   if not((startdate > end_window) or (enddate < start_window)) then do;	              
		            if (startdate <= start_window) and  (enddate > end_window) then days=(end_window-start_window)+1;
		            else if (startdate <= start_window) and  (enddate <= end_window) then days=(enddate-start_window)+1;
		            else if (startdate > start_window) and  (enddate <= end_window) then days=(enddate-startdate)+1;
		            else if (startdate > start_window) and  (enddate > end_window) then days=(end_window-startdate)+1;     	     
		            osdays[i]=days;				   
		         end;
	end;	          
run;


proc summary data=os_spells3 nway;
class snz_uid;
var os_da_&m-os_da_&n;
output out=&folder..mth_os(drop=_:)  sum=;
run;

proc means data=&folder..mth_os;
var os_da_&m-os_da_&n;
run;


*********************************;
**PART D. SCHOOL ENROLMENT, TERTIARY ENROLMENT, INDUSTRY TRAINING ENROLMENT, 
     EMPLOYMENT AND BENEFIT RECEIPT HISTORY;
*****CORRECTIONS SENTENCES SERVED;
*****STUDENT ALLOWANCE RECEIVED; 
**all these are constructed as vectors of calendar month indicators, 
    with each month referenced using a EMS-based number sequence, where month 1 = April 1999;
*********************************;

**********************************;
**School enrolment monthly vectors;
***********************************;

proc sql;
create table enrol
as select 
snz_uid
,input(compress(moe_esi_start_date,"-"),yymmdd10.) format date9. as startdate
,case when moe_esi_end_date is not null then input(compress(moe_esi_end_date,"-"),yymmdd10.) 
   else input(compress(moe_esi_extrtn_date,"-"),yymmdd10.) end format date9. as enddate
,input(moe_esi_provider_code, 10.) as schoolnbr
,moe_esi_domestic_status_code as domestic_status
,case when moe_esi_end_date='  ' then 1 else 0 end as sch_enddate_imputed
from moe.student_enrol
where snz_uid in (select distinct snz_uid from &population) and moe_esi_start_date is not null 
order by snz_uid, startdate, enddate;
quit;

**Removing any overlaps in enrolment;
%OVERLAP (enrol); 

**CODE FOR SCHOOL ENROLMENT MONTHS;

%let start='01Jan2004'd;
%let first=58;  *Jan2006;
%let last=195;  *Jun2015;

data enrol_month_temp  ;
set enrol_OR;
format start_window end_window date9.;
array sch_enr_id_(*) sch_enr_id_&first-sch_enr_id_&last; * end of jun2015;
array sch_enr_da_(*) sch_enr_da_&first-sch_enr_da_&last; * end of jun2015;

do ind=&first to &last; i=ind-&first+1;
	sch_enr_id_(i)=0;
	sch_enr_da_(i)=0;
* overwriting start and end window as interval equal to one month;

start_window=intnx("month",&start.,i-1,"beginning"); * start is beg of the month;
end_window=intnx("month",&start.,i-1,"end");* end is end of the month;

if not((startdate > end_window) or (enddate < start_window)) then do;
	sch_enr_id_(i)=1; * creating inidcator of school enrolment;
	* measuring the days enrolled;
				if (startdate <= start_window) and  (enddate > end_window) then
					days=(end_window-start_window)+1;
				else if (startdate <= start_window) and  (enddate <= end_window) then
					days=(enddate-start_window)+1;
				else if (startdate > start_window) and  (enddate <= end_window) then
					days=(enddate-startdate)+1;
				else if (startdate > start_window) and  (enddate > end_window) then
					days=(end_window-startdate)+1;
				sch_enr_da_[i]=days*sch_enr_id_(i);

end;
end;
run;


proc means data=enrol_month_temp  ;
run;

proc summary data=enrol_month_temp nway;
class snz_uid ;
var sch_enr_id_&first-sch_enr_id_&last  sch_enr_da_&first-sch_enr_da_&last  sch_enddate_imputed;
output out=TEMP(drop=_:) sum=;
run;


data &folder..mth_sch_enrol(drop=ind i);
set temp;
if sum(of sch_enr_id_&first-sch_enr_id_&last )>0;
array sch_enr_id_(*) sch_enr_id_&first-sch_enr_id_&last ; 
array sch_enr_da_(*) sch_enr_da_&first-sch_enr_da_&last ; 
first_sch_enr_refmth=.;
last_sch_enr_refmth=.;
do ind=&first to &last; 
   i=ind-&first+1;
   if sch_enr_id_[i]>1 then sch_enr_id_[i]=1;
   if sch_enr_id_[i]>=1 and first_sch_enr_refmth=. then first_sch_enr_refmth=ind;
   if sch_enr_id_[i]>=1 then last_sch_enr_refmth=ind;
   end;
run;


**********************************;
**Tertiary enrolment monthly vectors;
**Now using formal programmes only for this vector ;
**********************************;

%let first_anal_yr=2006;
%let last_anal_yr=2015;

proc sql;
	create table enrol as
	SELECT distinct 
		snz_uid
		,moe_enr_year_nbr as year
		,input(moe_enr_prog_start_date,yymmdd10.) format date9.  as startdate
		,input(moe_enr_prog_end_date,yymmdd10.) format date9.  as enddate  
		,sum(moe_enr_efts_consumed_nbr) as EFTS_consumed
		,moe_enr_efts_prog_years_nbr as EFTS_prog_yrs
		,moe_enr_qacc_code as qacc
		,moe_enr_qual_code as Qual
		,moe_enr_prog_nzsced_code as NZSCED
		,moe_enr_funding_srce_code as fund_source
		,moe_enr_subsector_code as subsector format $subsector.
		,moe_enr_qual_level_code as level
		,moe_enr_qual_type_code as qual_type
	FROM moe.enrolment 
		WHERE snz_uid IN 
		(SELECT DISTINCT snz_uid FROM &population) and moe_enr_year_nbr>=&first_anal_yr and moe_enr_year_nbr<=&last_anal_yr 
		group by snz_uid, moe_enr_prog_start_date , moe_enr_prog_end_date, qual, NZSCED
			order by snz_uid;
quit;

proc sql;
create table enrol_1 as
select a.*,
b.DOB
from enrol a left join &population b
on a.snz_uid=b.snz_uid;
quit;


* Formating dates and creating clean enrolment file;
* Defining formal and informal enrolments;

data enrol_clean_formal;
	set enrol_1;
	if EFTS_consumed>0;
	dur=enddate-startdate;
	if dur>0;
	start_year=year(startdate);
	if start_year>=&first_anal_yr and start_year<=&last_anal_yr;
if qual_type="D" then Formal=1; 
if formal=1 then output;
run;

proc means data=enrol_clean_formal;
run;

**2% have programme durations of more than one year.  How do we know they remained enrolled;
**Might be best cut off enrolment at end of one year;

%overlap(enrol_clean_formal);

%let start='01Jan2006'd;
%let first=82;  *Jan2006;
%let last=189;  *Dec2014;  


data TER_ENROL_MON_temp; 
set enrol_clean_formal_OR ;
format start_window end_window date9.;
array ter_enr_id_(*) ter_enr_id_&first-ter_enr_id_&last; 
array ter_enr_da_(*) ter_enr_da_&first-ter_enr_da_&last; 
do ind=&first to &last; i=ind-&first+1;
	ter_enr_id_(i)=0;
	ter_enr_da_(i)=0;

start_window=intnx("month",&start.,i-1,"beginning"); * start is beg of the month;
end_window=intnx("month",&start.,i-1,"end");* end is end of the month;

if not((startdate > end_window) or (enddate < start_window)) then do;
	ter_enr_id_(i)=1; * creating inidcator of school enrolment;
	* measuring the days enrolled;
				if (startdate <= start_window) and  (enddate > end_window) then
					days=(end_window-start_window)+1;
				else if (startdate <= start_window) and  (enddate <= end_window) then
					days=(enddate-start_window)+1;
				else if (startdate > start_window) and  (enddate <= end_window) then
					days=(enddate-startdate)+1;
				else if (startdate > start_window) and  (enddate > end_window) then
					days=(end_window-startdate)+1;
				ter_enr_da_[i]=days*ter_enr_id_(i);
end;
end;
run;

proc summary data=TER_ENROL_MON_temp nway;
class snz_uid ;
var ter_enr_id_&first-ter_enr_id_&last  ter_enr_da_&first-ter_enr_da_&last; 
output out=mth_ter_enrol(drop=_:) sum=;
run;

data &folder..mth_ter_enrol;
set mth_ter_enrol;
array ter_enr_id_(*) ter_enr_id_&first-ter_enr_id_&last; 
do ind=&first to &last; i=ind-&first+1;
   if ter_enr_id_[i]>1 then ter_enr_id_[i]=1;
   end;
run;

proc means data=&folder..MTH_TER_enrol;
run;


*************************;
****Employment;
**I choose to select data from 2006 onwards but the first available month is April 1999;
****************************;

%macro select(year);
proc sql;
create table earners&year as 
   select snz_uid, 
    inc_cal_yr_year_nbr as year,
      sum(inc_cal_yr_mth_01_amt) as earn1,
	  sum(inc_cal_yr_mth_02_amt) as earn2,
      sum(inc_cal_yr_mth_03_amt) as earn3,
      sum(inc_cal_yr_mth_04_amt) as earn4,
      sum(inc_cal_yr_mth_05_amt) as earn5,
	  sum(inc_cal_yr_mth_06_amt) as earn6,
      sum(inc_cal_yr_mth_07_amt) as earn7,
      sum(inc_cal_yr_mth_08_amt) as earn8,
      sum(inc_cal_yr_mth_09_amt) as earn9,
      sum(inc_cal_yr_mth_10_amt) as earn10,
      sum(inc_cal_yr_mth_11_amt) as earn11,
      sum(inc_cal_yr_mth_12_amt) as earn12   
    from data.income_cal_yr
    where inc_cal_yr_income_source_code = 'W&S' and inc_cal_yr_year_nbr=&year and snz_uid in (SELECT DISTINCT snz_uid FROM &population) 
    group by snz_uid, year
    order by snz_uid;
quit;

%mend select;

%select(2006);
%select(2007);
%select(2008);
%select(2009);
%select(2010);
%select(2011);
%select(2012);
%select(2013);
%select(2014);
%select(2015);


**Convert earnings data to Dec 2014 $ values;
**Disregard months where monthly earnings are less than $10;

%let m=82;
%let n=195;
data &folder..mth_emp(keep=snz_uid emp&m-emp&n rearn&m-rearn&n);
merge 
  earners2006(keep=snz_uid earn1-earn12 rename=(earn1-earn12=earn82-earn93))
  earners2007(keep=snz_uid earn1-earn12 rename=(earn1-earn12=earn94-earn105))
  earners2008(keep=snz_uid earn1-earn12 rename=(earn1-earn12=earn106-earn117)) 
  earners2009(keep=snz_uid earn1-earn12 rename=(earn1-earn12=earn118-earn129))
  earners2010(keep=snz_uid earn1-earn12 rename=(earn1-earn12=earn130-earn141))
  earners2011(keep=snz_uid earn1-earn12 rename=(earn1-earn12=earn142-earn153))
  earners2012(keep=snz_uid earn1-earn12 rename=(earn1-earn12=earn154-earn165)) 
  earners2013(keep=snz_uid earn1-earn12 rename=(earn1-earn12=earn166-earn177))
  earners2014(keep=snz_uid earn1-earn12 rename=(earn1-earn12=earn178-earn189))
  earners2015(keep=snz_uid earn1-earn6 rename=(earn1-earn6=earn190-earn195));
by snz_uid;
array earn(*) earn&m-earn&n;
array rearn(*) rearn&m-rearn&n;
array emp(*) emp&m-emp&n;
do i=&m to &n;
    if i<=48 then rearn(i-&m+1) = earn(i-&m+1)*1191/913;
    else if i<=51 then rearn(i-&m+1) = earn(i-&m+1)*1191/913;
    else if i<=54 then rearn(i-&m+1) = earn(i-&m+1)*1191/918;
    else if i<=57 then rearn(i-&m+1) = earn(i-&m+1)*1191/924;
    else if i<=60 then rearn(i-&m+1) = earn(i-&m+1)*1191/928;
    else if i<=63 then rearn(i-&m+1) = earn(i-&m+1)*1191/935;
    else if i<=66 then rearn(i-&m+1) = earn(i-&m+1)*1191/941;
    else if i<=69 then rearn(i-&m+1) = earn(i-&m+1)*1191/949;
    else if i<=72 then rearn(i-&m+1) = earn(i-&m+1)*1191/953;
    else if i<=75 then rearn(i-&m+1) = earn(i-&m+1)*1191/962;
    else if i<=78 then rearn(i-&m+1) = earn(i-&m+1)*1191/973;
    else if i<=81 then rearn(i-&m+1) = earn(i-&m+1)*1191/979;
    else if i<=84 then rearn(i-&m+1) = earn(i-&m+1)*1191/985;
    else if i<=87 then rearn(i-&m+1) = earn(i-&m+1)*1191/1000;
    else if i<=90 then rearn(i-&m+1) = earn(i-&m+1)*1191/1007;
    else if i<=93 then rearn(i-&m+1) = earn(i-&m+1)*1191/1005;
    else if i<=96 then rearn(i-&m+1) = earn(i-&m+1)*1191/1010;
    else if i<=99  then rearn(i-&m+1) = earn(i-&m+1)*1191/1020;
    else if i<=102 then rearn(i-&m+1) = earn(i-&m+1)*1191/1025;
    else if i<=105 then rearn(i-&m+1) = earn(i-&m+1)*1191/1037;
    else if i<=108 then rearn(i-&m+1) = earn(i-&m+1)*1191/1044;
    else if i<=111 then rearn(i-&m+1) = earn(i-&m+1)*1191/1061;
    else if i<=114 then rearn(i-&m+1) = earn(i-&m+1)*1191/1077;
    else if i<=117 then rearn(i-&m+1) = earn(i-&m+1)*1191/1072;
    else if i<=120 then rearn(i-&m+1) = earn(i-&m+1)*1191/1075;    
    else if i<=123 then rearn(i-&m+1) = earn(i-&m+1)*1191/1081;
    else if i<=126 then rearn(i-&m+1) = earn(i-&m+1)*1191/1095;
    else if i<=129 then rearn(i-&m+1) = earn(i-&m+1)*1191/1093;  
    else if i<=132 then rearn(i-&m+1) = earn(i-&m+1)*1191/1097;   
    else if i<=135 then rearn(i-&m+1) = earn(i-&m+1)*1191/1099;
    else if i<=138 then rearn(i-&m+1) = earn(i-&m+1)*1191/1111;
    else if i<=141 then rearn(i-&m+1) = earn(i-&m+1)*1191/1137;
    else if i<=144 then rearn(i-&m+1) = earn(i-&m+1)*1191/1146;   
    else if i<=147 then rearn(i-&m+1) = earn(i-&m+1)*1191/1157;   
    else if i<=150 then rearn(i-&m+1) = earn(i-&m+1)*1191/1162;
    else if i<=153 then rearn(i-&m+1) = earn(i-&m+1)*1191/1158;  * 1176 Dec 2011 ;  
    else if i<=156 then rearn(i-&m+1) = earn(i-&m+1)*1191/1164;
    else if i<=159 then rearn(i-&m+1) = earn(i-&m+1)*1191/1168;   
    else if i<=162 then rearn(i-&m+1) = earn(i-&m+1)*1191/1171;
    else if i<=165 then rearn(i-&m+1) = earn(i-&m+1)*1191/1169;  * Dec 2012;  
    else if i<=168 then rearn(i-&m+1) = earn(i-&m+1)*1191/1174;
    else if i<=171 then rearn(i-&m+1) = earn(i-&m+1)*1191/1176;  * June 2013; 
    else if i<=174 then rearn(i-&m+1) = earn(i-&m+1)*1191/1187;   
    else if i<=177 then rearn(i-&m+1) = earn(i-&m+1)*1191/1188;  *Dec 2013;
	else if i<=180 then rearn(i-&m+1) = earn(i-&m+1)*1191/1192; *Mar 2014;
    else if i<=183 then rearn(i-&m+1) = earn(i-&m+1)*1191/1195; *Jun 2014;
	else if i<=186 then rearn(i-&m+1) = earn(i-&m+1)*1191/1199; *Sep 2014;
	else if i<=189 then rearn(i-&m+1) = earn(i-&m+1)*1191/1197; *Dec 2014;
    else if i<=192 then rearn(i-&m+1) = earn(i-&m+1)*1191/1195; *Mar 2015;
    else if i<=195 then rearn(i-&m+1) = earn(i-&m+1)*1191/1200; *Jun 2015;
end;
do i=1 to dim(rearn);
if rearn(i)<10 then do; 
     emp(i)=0; 
	 rearn(i)=.; end;
else do; 
   emp(i)=1; 
   rearn(i)=round(rearn(i), 1); 
   if rearn(i)>500000 then rearn(i)=500000;
   end;
end;
run;


proc means data=&folder..mth_emp;
run;


******************************;
**Industry training monthly activity;
**Pick up temp dataset from the code that created IT quals, above;
*****************************;

* removing overlaps to count the days in IT programme;

%overlap(itl_event);

%let start='01Jan2006'd;
%let first=82;  *Jan2006;
%let last=195;  *Jun2015;

* Creating monthly arrays;

data itl_mth_temp; 
set itl_event_OR;
format start_window end_window date9.;
array itl_id_(*) itl_id_&first-itl_id_&last; 
array itl_da_(*)  itl_da_&first-itl_da_&last; 
do ind=&first to &last; i=ind-&first+1;
itl_id_(i)=0;
itl_da_(i)=0;	
start_window=intnx("month",&start.,i-1,"beginning"); * start is beg of the month;
end_window=intnx("month",&start.,i-1,"end");* end is end of the month;
if not((startdate > end_window) or (enddate < start_window)) then do;
    itl_id_(i)=1;
	* measuring the days overseas;
				if (startdate <= start_window) and  (enddate > end_window) then
					days=(end_window-start_window)+1;
				else if (startdate <= start_window) and  (enddate <= end_window) then
					days=(enddate-start_window)+1;
				else if (startdate > start_window) and  (enddate <= end_window) then
					days=(enddate-startdate)+1;
				else if (startdate > start_window) and  (enddate > end_window) then
					days=(end_window-startdate)+1;
				itl_da_[i]=days*itl_id_(i);				
end;
end;
run;

* Industry training data are available from Jan 2003 ;
proc summary data=ITL_MTH_TEMP nway;
class snz_uid ;
var itl_id_&first-itl_id_&last itl_da_&first-itl_da_&last; 
output out=MTH_ITL_ENR (drop=_:) sum=;
run;

proc means data=MTH_ITL_ENR;
run;

data MTH_IT_ENR_2(drop=i);
set MTH_ITL_ENR ;
array itl_id_(*) itl_id_&first-itl_id_&last; 
do i=1 to dim(itl_id_);
   if itl_id_(i)>=1 then itl_id_(i)=1;
   end;
run;

**If person was not employed in a given month, set their IT activity measures to zero;

data combine;
merge MTH_IT_ENR_2(in=a) &folder..mth_emp(keep=snz_uid emp&first-emp&last);
by snz_uid;
if a;
array ita(*) itl_id_&first-itl_id_&last;
array itb(*) itl_da_&first-itl_da_&last; 
array emp(*) emp&first-emp&last;
do i=1 to dim(ita);
  if emp(i)~=1 then do;
     ita(i)=0;
	 itb(i)=0;
	 end;
	 end;
run;

proc means data=combine;
run;

data &folder..mth_it_enrol;
set combine(keep=snz_uid itl_id_&first-itl_id_&last itl_da_&first-itl_da_&last);  
run;


*****************************************************;
**Monthly vectors of adult or youth benefit receipt -using Sarah's code;
**This code draws on the benefit spell datasets;
**An alternative and simpler method is to use the first tier benefit expenditure dataset - but the results won't be the same;
**************************************************;

proc format ;
VALUE $bengp_pre2013wr                  /* Jane suggest to add the old format */
    '020','320' = "Invalid's Benefit"
    '030','330' = "Widow's Benefit"
    '040','044','340','344'
                = "Orphan's and Unsupported Child's benefits"
    '050','350','180','181'
    = "New Zealand Superannuation and Veteran's and Transitional Retirement Benefit"
    '115','604','605','610'
                = "Unemployment Benefit and Unemployment Benefit Hardship"
    '125','608' = "Unemployment Benefit (in Training) and Unemployment Benefit Hardship (in Training)"
    '313','613','365','665','366','666','367','667'
                = "Domestic Purposes related benefits"
    '600','601' = "Sickness Benefit and Sickness Benefit Hardship"
    '602','603' = "Job Search Allowance and Independant Youth Benefit"
    '607'       = "Unemployment Benefit Student Hardship"
    '609','611' = "Emergency Benefit"
    '839','275' = "Non Beneficiary"
    'YP ','YPP' = "Youth Payment and Young Parent Payment"
        ' '     = "No Benefit"
 ;

value $bennewgp 

'020'=	"Invalid's Benefit"
'320'=	"Invalid's Benefit"

'330'=	"Widow's Benefit"
'030'=	"Widow's Benefit"

'040'=	"Orphan's and Unsupported Child's benefits"
'044'=	"Orphan's and Unsupported Child's benefits"
'340'=	"Orphan's and Unsupported Child's benefits"
'344'=	"Orphan's and Unsupported Child's benefits"

'050'=	"New Zealand Superannuation and Veteran's and Transitional Retirement Benefit"
'180'=	"New Zealand Superannuation and Veteran's and Transitional Retirement Benefit"
'181'=	"New Zealand Superannuation and Veteran's and Transitional Retirement Benefit"
'350'=	"New Zealand Superannuation and Veteran's and Transitional Retirement Benefit"

'115'=	"Unemployment Benefit and Unemployment Benefit Hardship"
'604'=	"Unemployment Benefit and Unemployment Benefit Hardship"
'605'=	"Unemployment Benefit and Unemployment Benefit Hardship"
'610'=	"Unemployment Benefit and Unemployment Benefit Hardship"
'607'=	"Unemployment Benefit Student Hardship"
'608'=	"Unemployment Benefit (in Training) and Unemployment Benefit Hardship (in Training)"
'125'=	"Unemployment Benefit (in Training) and Unemployment Benefit Hardship (in Training)"


'313'=  "Domestic Purposes related benefits"
'365'=	"Sole Parent Support "					/* renamed */
'366'=	"Domestic Purposes related benefits"
'367'=	"Domestic Purposes related benefits"
'613'=	"Domestic Purposes related benefits"
'665'=	"Domestic Purposes related benefits"
'666'=	"Domestic Purposes related benefits"
'667'=	"Domestic Purposes related benefits"

'600'=	"Sickness Benefit and Sickness Benefit Hardship"
'601'=	"Sickness Benefit and Sickness Benefit Hardship"

'602'=	"Job Search Allowance and Independant Youth Benefit"
'603'=	"Job Search Allowance and Independant Youth Benefit"

'611'=	"Emergency Benefit"

'315'=	"Family Capitalisation"
'461'=	"Unknown"
'000'=	"No Benefit"
'839'=	"Non Beneficiary"

/* new codes */
'370'=  "Supported Living Payment related"
'675'=  "Job Seeker related"
'500'=  "Work Bonus"
;
run  ;

proc format;
value $ADDSERV
'YP'	='Youth Payment'
'YPP'	='Young Parent Payment'
'CARE'	='Carers'
'FTJS1'	='Job seeker Work Ready '
'FTJS2'	='Job seeker Work Ready Hardship'
'FTJS3'	='Job seeker Work Ready Training'
'FTJS4'	='Job seeker Work Ready Training Hardship'
'MED1'	='Job seeker Health Condition and Disability'
'MED2'	='Job seeker Health Condition and Disability Hardship'
'PSMED'	='Health Condition and Disability'
''		='.';
run;

%let sensor=30Jun2015;

data msd_spel; 
   set msd.msd_spell;
* Formating dates and sensoring;
	format startdate enddate spellfrom spellto date9.;
	spellfrom=input(compress(msd_spel_spell_start_date,"-"),yymmdd10.);
	spellto=input(compress(msd_spel_spell_end_date,"-"),yymmdd10.);
	if spellfrom<"&sensor"d;
	if spellto>"&sensor"d then spellto="&sensor"d;
	if spellto=. then spellto="&sensor"d;
	startdate=spellfrom;
	enddate=spellto;
* TRANSLATING POST REFORM SERVF INTO PRE REFORM FOR OLD TIME SERIES******;

	if msd_spel_prewr3_servf_code='' then prereform=put(msd_spel_servf_code, $bengp_pre2013wr.); 
	else prereform=put(msd_spel_prewr3_servf_code,$bengp_pre2013wr.);	

* applying wider groupings;
if prereform in ("Domestic Purposes related benefits", "Widow's Benefit","Sole Parent Support ") then ben='dpb';
else if prereform in ("Invalid's Benefit", "Supported Living Payment related") then ben='ib';
else if prereform in ("Unemployment Benefit and Unemployment Benefit Hardship",
   "Unemployment Benefit Student Hardship", "Unemployment Benefit (in Training) and Unemployment Benefit Hardship (in Training)") then ben='ub';
else if prereform in ("Job Search Allowance and Independant Youth Benefit") then ben='iyb';
else if prereform in ("Sickness Benefit and Sickness Benefit Hardship") then ben='sb';
else if prereform in ("Orphan's and Unsupported Child's benefits") then ben='ucb';
else ben='oth';

* TRANSLATING PREREFORM SERVF INTO POST REFORM SERVF FOR NEW TIME SEIRES*****;
length benefit_desc_new $50;
servf=msd_spel_servf_code;
additional_service_data=msd_spel_add_servf_code;
	if  servf in ('602', /* Job Search Allowance - a discontinued youth benefit */
				 '603') /* IYB then aft 2012 Youth/Young Parent Payment */	 
		and additional_service_data ne 'YPP' then benefit_desc_new='1: YP Youth Payment Related' ;/* in 2012 changes some young DPB-SP+EMA moved to YPP */

	else if servf in ('313')   /* EMA(many were young mums who moved to YPP aft 2012) */
		or additional_service_data='YPP' then benefit_desc_new='1: YPP Youth Payment Related' ;
  
	else if  (servf in (
				   '115', /* UB Hardship */
                   '610', /* UB */
                   '611', /* Emergency Benefit (UB for those that did not qualify)*/
				   '030', /* B4 2012 was MOST WB, now just WB paid overseas) */ 
				   '330', /* Widows Benefit (weekly, old payment system) */ 
				   '366', /* DPB Woman Alone (weekly, old payment system) */
				   '666'))/* DPB Woman Alone */
		or (servf in ('675') and additional_service_data in (
					'FTJS1', /* JS Work Ready */
					'FTJS2')) /* JS Work Ready Hardship */
			
		then benefit_desc_new='2: Job Seeker Work Ready Related'; 

	else if  (servf in ('607', /* UB Student Hardship (mostly over summer holidays)*/ 
				   '608')) /* UB Training */
        or (servf in ('675') and additional_service_data in (
					'FTJS3', /* JS Work Ready Training */
					'FTJS4'))/* JS Work Ready Training Hardship */
		then benefit_desc_new='2: Job Seeker Work Ready Training Related'; 


	else if (servf in('600', /* Sickness Benefit */
				  '601')) /* Sickness Benefit Hardship */ 
		or (servf in ('675') and additional_service_data in (
				'MED1',   /* JS HC&D */
				'MED2'))  /* JS HC&D Hardship */
		then benefit_desc_new='3: Job Seeker HC&D Related' ;

	else if servf in ('313',   /* Emergency Maintenance Allowance (weekly) */
				   
				   '365',   /* B4 2012 DPB-SP (weekly), now Sole Parent Support */
				   '665' )  /* DPB-SP (aft 2012 is just for those paid o'seas)*/
		then benefit_desc_new='4: Sole Parent Support Related' ;/*NB young parents in YPP since 2012*/

	else if (servf in ('370') and additional_service_data in (
						'PSMED', /* SLP */
						'')) /* SLP paid overseas(?)*/ 
		or (servf ='320')    /* Invalids Benefit */
		or (servf='020')     /* B4 2012 020 was ALL IB, now just old IB paid o'seas(?)*/
		then benefit_desc_new='5: Supported Living Payment HC&D Related' ;

	else if (servf in ('370') and additional_service_data in ('CARE')) 
		or (servf in ('367',  /* DPB - Care of Sick or Infirm */
					  '667')) /* DPB - Care of Sick or Infirm */
		then benefit_desc_new='6: Supported Living Payment Carer Related' ;

	else if servf in ('999') /* merged in later by Corrections... */
		then benefit_desc_new='7: Student Allowance';

	else if (servf = '050' ) /* Transitional Retirement Benefit - long since stopped! */
		then benefit_desc_new='Other' ;

	else if benefit_desc_new='Unknown'   /* hopefully none of these!! */;

* applying wider groupings;
if prereform in ("Domestic Purposes related benefits", "Widow's Benefit","Sole Parent Support ") then ben='DPB';
else if prereform in ("Invalid's Benefit", "Supported Living Payment related") then ben='IB';
else if prereform in ("Unemployment Benefit and Unemployment Benefit Hardship",
   "Unemployment Benefit Student Hardship", "Unemployment Benefit (in Training) and Unemployment Benefit Hardship (in Training)") then ben='UB';
else if prereform in ("Job Search Allowance and Independant Youth Benefit") then ben='IYB';
else if prereform in ("Sickness Benefit and Sickness Benefit Hardship") then ben='SB';
else if prereform in ("Orphan's and Unsupported Child's benefits") then ben='UCB';
else ben='OTH';

if benefit_desc_new='2: Job Seeker Work Ready Training Related' then ben_new='JSWR_TR';
else if benefit_desc_new='1: YP Youth Payment Related' then ben_new='YP';
else if benefit_desc_new='1: YPP Youth Payment Related' then ben_new='YPP';
else if benefit_desc_new='2: Job Seeker Work Ready Related' then ben_new='JSWR';

else if benefit_desc_new='3: Job Seeker HC&D Related' then ben_new='JSHCD';
else if benefit_desc_new='4: Sole Parent Support Related' then ben_new='SPSR';
else if benefit_desc_new='5: Supported Living Payment HC&D Related' then ben_new='SLP_HCD';
else if benefit_desc_new='6: Supported Living Payment Carer Related' then ben_new='SLP_C';
else if benefit_desc_new='7: Student Allowance' then ben_new='SA';

else if benefit_desc_new='Other' then ben_new='OTH';
if prereform='370' and ben_new='SLP_C' then ben='DPB';
if prereform='370' and ben_new='SLP_HCD' then ben='IB';

if prereform='675' and ben_new='JSHCD' then ben='SB';
if prereform='675' and (ben_new ='JSWR' or ben_new='JSWR_TR') then ben='UB';
run;


* BDD spell dataset;
data msd_spel;
	set msd_spel;
	spell=msd_spel_spell_nbr;
	keep snz_uid spell servf spellfrom spellto ben ben_new;
run;

proc sort data=msd_spel out=mainbenefits(rename=(spellfrom=startdate spellto=enddate));
	by snz_uid spell spellfrom spellto;
run;

* BDD partner spell table;
data icd_bdd_ptnr;
	set msd.msd_partner;
	format ptnrfrom ptnrto date9.;
	spell=msd_ptnr_spell_nbr;
	ptnrfrom=input(compress(msd_ptnr_ptnr_from_date,"-"), yymmdd10.);
	ptnrto=input(compress(msd_ptnr_ptnr_to_date,"-"), yymmdd10.);

	* Sensoring;
	if ptnrfrom>"&sensor"d then
		delete;

	if ptnrto=. then
		ptnrto="&sensor"d;

	if ptnrto>"&sensor"d then
		ptnrto="&sensor"d;
	keep snz_uid partner_snz_uid spell ptnrfrom ptnrto;
run;

* EXTRACTING MAIN BENEFIT AS PRIMARY;
proc sql;
	create table prim_mainben_prim_data as
		select
			s.snz_uid, s.spellfrom as startdate, s.spellto as enddate, s.ben, s.ben_new, s.spell,
			t.DOB
		from
			msd_spel  s inner join &population t
			on t.snz_uid= s.snz_uid;
run;

* MAIN BENEFITS AS PARTNER (relationship);
proc sql;
	create table prim_mainben_part_data as
		select
			s.partner_snz_uid, s.ptnrfrom as startdate, s.ptnrto as enddate,s.spell,
			s.snz_uid as main_snz_uid,
			t.DOB
		from  icd_bdd_ptnr  s inner join &population t
			on t.snz_uid = s.partner_snz_uid
		order by s.snz_uid, s.spell;

	**ADD benefit type to the partner's dataset;

	**Note that snz_uid+spell does not uniquely identify benefit spells therefore the start
	and enddate of each spell is also used below to correctly match partner spells to those of the main beneficiary;

	**This is done in two steps - (1) spells with fully matching start and end dates
	(2) partner spells that fall within the matching main benefit spell but are not as long;
proc sort data=mainbenefits out=main nodupkey;
	by snz_uid spell startdate enddate;
run;

proc sort data=prim_mainben_part_data out=partner(rename=(main_snz_uid=snz_uid)) nodupkey;
	by main_snz_uid spell startdate enddate;
run;

data fullymatched  unmatched(drop=ben ben_new servf);
	merge partner (in = a)
		main (in = b);
	by snz_uid spell startdate enddate;

	if a and b then
		output fullymatched;
	else if a and not b then
		output unmatched;
run;

proc sql;
	create table partlymatched as
		select a.partner_snz_uid, a.snz_uid, a.spell, a.dob, a.startdate, a.enddate,
			b.ben, b.ben_new, b.servf
		from unmatched a left join main b
			on a.snz_uid=b.snz_uid and a.spell=b.spell and a.startdate>=b.startdate and (a.enddate<=b.enddate or b.enddate=.) ;
quit;
run;

data prim_mainben_part_data_2;
	set fullymatched partlymatched;
run;

proc freq data=prim_mainben_part_data_2;
	tables ben_new ben;
run;

* CONSOLIDATING BENEFIT SPELLS AS PRIMARY AND PARTNER;
data prim_bennzs_data_1 del;
	set prim_mainben_prim_data (in=a)
		prim_mainben_part_data_2 (in=b);
	if b then
		snz_uid=partner_snz_uid;

	* Deleting benefit spells Before DOB of refrence person;
	if startdate<DOB then
		output del;
	else output prim_bennzs_data_1;
run;

proc sort data = prim_bennzs_data_1;
	by snz_uid startdate enddate;
run;

%overlap(prim_bennzs_data_1);

proc freq data=prim_bennzs_data_1;
tables enddate /list missing;


%let start='01Jan2006'd;
%let first=82;  *Jan2006;
%let last=195;  *Jun2015;  

data ben_TEMP; 
set PRIM_BENNZS_DATA_1_OR(keep=snz_uid startdate enddate);
format start_window end_window date9.;

array ben_id_(*) ben_id_&first-ben_id_&last; 
array ben_da_(*) ben_da_&first-ben_da_&last; 

do ind=&first to &last; i=ind-&first+1;
	ben_id_(i)=0;
* overwriting start and end window as interval equal to one month;
start_window=intnx("month",&start.,i-1,"beginning"); * start is beg of the month;
end_window=intnx("month",&start.,i-1,"end");* end is end of the month;
if not((startdate > end_window) or (enddate < start_window)) then do;
	ben_id_(i)=1; * creating inidcator of school enrolment;
	* measuring the days enrolled;
				if (startdate <= start_window) and  (enddate > end_window) then
					days=(end_window-start_window)+1;
				else if (startdate <= start_window) and  (enddate <= end_window) then
					days=(enddate-start_window)+1;
				else if (startdate > start_window) and  (enddate <= end_window) then
					days=(enddate-startdate)+1;
				else if (startdate > start_window) and  (enddate > end_window) then
					days=(end_window-startdate)+1;
				ben_da_[i]=days*ben_id_(i);
end;
end;
run;

proc summary data=ben_temp nway;
class snz_uid ;
var ben_id_&first-ben_id_&last ben_da_&first-ben_da_&last;
output out=ben_mon_enrol(drop=_:) sum=;
run;

data &folder..mth_ben(drop=i);
set ben_mon_enrol;
array ben_id_(*) ben_id_&first-ben_id_&last; 
do i=1 to dim(ben_id_);
   if ben_id_(i)>=1 then ben_id_(i)=1;
   end;
run;

proc means data=&folder..mth_ben;
run;

*****************************************************************;
***ANY CUSTODIAL OR COMMUNITY SENTENCES SERVED;
**by month - from Jan 2006 to June 2015;
***************************************************************;

proc sql;
Connect to sqlservr (server=WPRDSQL36\iLeed database=IDI_clean_&Version);
	create table COR as
		SELECT distinct 
		 snz_uid,
			input(cor_mmp_period_start_date,yymmdd10.) format date9. as startdate,
			input(cor_mmp_period_end_date, yymmdd10.)format date9. as enddate,
			cor_mmp_mmc_code,  
           /* Creating broader correction sentence groupings */
	    	(case when cor_mmp_mmc_code in ('PRISON','REMAND' ) then 'Custody'
			     
			when cor_mmp_mmc_code in ('HD_SENT','HD_SENT', 'HD_REL' 'COM_DET','CW','COM_PROG',
                 'COM_SERV' ,'OTH_COMM','INT_SUPER','SUPER','PERIODIC') then 'Comm'
                 else 'COR_OTHER' end) as sentence 
		FROM COR.ov_major_mgmt_periods 
		where snz_uid in (SELECT DISTINCT snz_uid FROM &population) 
		AND cor_mmp_mmc_code IN ('PRISON','REMAND','HD_SENT','HD_REL','PERIODIC',
			'COM_DET','CW','COM_PROG','COM_SERV','OTH_COMM','INT_SUPER','SUPER')        
		ORDER BY snz_uid,startdate;
quit;

proc means data=cor;
run;

proc sql;
create table COR_1 as select
a.* ,
b.DOB
from COR a left join &population b
on a.snz_uid=b.snz_uid;
quit;

data cor_2;
set cor_1;
**delete any spells that started before 14th birthday - probably wrong;
if startdate>=intnx('YEAR',DOB,14,'S');
run;


%OVERLAP (COR_2); 

%let m=82;  *Jan 2006;
%let n=195;   *June 2015;

data cor_spells(drop=i start_window end_window days);
set COR_2_OR;
start='01Jan2006'd; 
array custdays [*] cust_da_&m-cust_da_&n ; 
array commdays [*] comm_da_&m-comm_da_&n ; 
do i=1 to dim(custdays);
   start_window=intnx('month',start,i-1,'S');
   end_window=(intnx('month',start,i,'S'))-1;
   format start_window end_window date9.;  
   if not((startdate > end_window) or (enddate < start_window)) and sentence='Custody' then do;	              
		            if (startdate <= start_window) and  (enddate > end_window) then days=(end_window-start_window)+1;
		            else if (startdate <= start_window) and  (enddate <= end_window) then days=(enddate-start_window)+1;
		            else if (startdate > start_window) and  (enddate <= end_window) then days=(enddate-startdate)+1;
		            else if (startdate > start_window) and  (enddate > end_window) then days=(end_window-startdate)+1;     	     
		            custdays[i]=days;	                 
		         end;
   if not((startdate > end_window) or (enddate < start_window)) and sentence='Comm' then do;	              
		            if (startdate <= start_window) and  (enddate > end_window) then days=(end_window-start_window)+1;
		            else if (startdate <= start_window) and  (enddate <= end_window) then days=(enddate-start_window)+1;
		            else if (startdate > start_window) and  (enddate <= end_window) then days=(enddate-startdate)+1;
		            else if (startdate > start_window) and  (enddate > end_window) then days=(end_window-startdate)+1;     	     
		            commdays[i]=days;	                 
		         end;
	end;	          
run;

proc summary data=cor_spells nway;
class snz_uid;
var cust_da_&m-cust_da_&n comm_da_&m-comm_da_&n ; 
output out=corr(drop=_:)  sum=;
run;

data &folder..mth_corrections(drop=i);
set corr;
array custdays [*] cust_da_&m-cust_da_&n ; 
array commdays [*] comm_da_&m-comm_da_&n ; 
array cust [*] cust_id_&m-cust_id_&n ; 
array comm [*] comm_id_&m-comm_id_&n ; 
do i=1 to dim(custdays);
   if custdays(i)>0 then cust(i)=1; else cust(i)=0;
   if commdays(i)>0 then comm(i)=1; else comm(i)=0;
   end;
run;


proc means data=&folder..mth_corrections;
run;




******************************************;
****STUDENT ALLOWANCE RECEIPT;
**I choose to select data from 2006 onwards;
******************************************;

%macro select(year);
proc sql;
create table studall&year as 
   select snz_uid, 
    inc_cal_yr_year_nbr as year,
      sum(inc_cal_yr_mth_01_amt) as sa1,
	  sum(inc_cal_yr_mth_02_amt) as sa2,
      sum(inc_cal_yr_mth_03_amt) as sa3,
      sum(inc_cal_yr_mth_04_amt) as sa4,
      sum(inc_cal_yr_mth_05_amt) as sa5,
	  sum(inc_cal_yr_mth_06_amt) as sa6,
      sum(inc_cal_yr_mth_07_amt) as sa7,
      sum(inc_cal_yr_mth_08_amt) as sa8,
      sum(inc_cal_yr_mth_09_amt) as sa9,
      sum(inc_cal_yr_mth_10_amt) as sa10,
      sum(inc_cal_yr_mth_11_amt) as sa11,
      sum(inc_cal_yr_mth_12_amt) as sa12   
    from data.income_cal_yr
    where inc_cal_yr_income_source_code = 'STU' and inc_cal_yr_year_nbr=&year and snz_uid in (SELECT DISTINCT snz_uid FROM &population) 
    group by snz_uid, year
    order by snz_uid;
quit;

%mend select;

%select(2006);
%select(2007);
%select(2008);
%select(2009);
%select(2010);
%select(2011);
%select(2012);
%select(2013);
%select(2014);
%select(2015);

%let m=82;
%let n=195;
data mth_studall(keep=snz_uid sa&m-sa&n );
merge 
  studall2006(keep=snz_uid sa1-sa12 rename=(sa1-sa12=sa82-sa93))
  studall2007(keep=snz_uid sa1-sa12 rename=(sa1-sa12=sa94-sa105))
  studall2008(keep=snz_uid sa1-sa12 rename=(sa1-sa12=sa106-sa117)) 
  studall2009(keep=snz_uid sa1-sa12 rename=(sa1-sa12=sa118-sa129))
  studall2010(keep=snz_uid sa1-sa12 rename=(sa1-sa12=sa130-sa141))
  studall2011(keep=snz_uid sa1-sa12 rename=(sa1-sa12=sa142-sa153))
  studall2012(keep=snz_uid sa1-sa12 rename=(sa1-sa12=sa154-sa165)) 
  studall2013(keep=snz_uid sa1-sa12 rename=(sa1-sa12=sa166-sa177))
  studall2014(keep=snz_uid sa1-sa12 rename=(sa1-sa12=sa178-sa189))
  studall2015(keep=snz_uid sa1-sa6 rename=(sa1-sa6=sa190-sa195));
by snz_uid;
run;


data &folder..mth_studall(drop=i);
set mth_studall;
array stud(*) sa&m-sa&n;
do i=1 to dim(stud);
   if stud(i)>0 then stud(i)=1;
   else stud(i)=0;
end;

proc means data=&folder..mth_studall;
run;



