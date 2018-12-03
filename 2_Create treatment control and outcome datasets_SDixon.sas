/* This program creates study population, potential control and outcome datasets for the YS-NEET impact evaluation  */
/* Uses data from the 20160224 IDI refresh, which was made available in March 2016 */
/* Uses data for all birth cohorts from 1990 to 1999 */


/* Initially the program creates records for all people who were in the relevant age range (15-18) over the period from Aug 2012 to Dec 2014 */
/* With relatively few exclusions, namely must have survived until their 20th birthday, must have a NZ school enrolment 
   record during their lower high school years, must be in the population that was selected in the first data creation program */
/* So that we can use the datasets and variables to profile all YS participants and non-participants out to Dec 2014, 
   and also to calculate  risk measures for not achieving level 2 quals or being neet at 18, for the entire youth population*/

/* I create a record for each person in each calendar month, from Aug 2012 to Dec 2014, summarising their data up the start of that ref month*/
 
/* In this version of the program the study design exclusions for being overseas, for attending a nonNQF school, 
   and for having a missing school leaving date,
   are not applied until near the end, so we can more easily study the effect of variation in these restrictions;*/


**Part AA. Find YS:NEET start dates for the programme participants so I can use these as reference dates in the
    coding that follows (non-participants have randomly assigned reference dates);
**PART A. Create a record structure of 29 monthly records per person, giving each a reference date, and get some demographic variables;
**PART B: Construct a range of new activity and income variables that REQUIRE A REFERENCE DATE TO construct;
**PART C: Compile the variables on pre-programme history that will be used IN PS MODELLING and/or DESCRIPTIVE STATISTICS, ;
**PART D: Construct OUTCOME measures for records with a reference month in 2012 or 2013, which will be used in the impact analysis;
**PART E: Construct YS-NEET and YS-YPP/YP PARTICIPATION DATA;
**PART F: APPLY some FURTHER POPULATION RESTRICTIONS that are needed for THE MAIN YS:NEET IMPACT ANALYSIS;
**PART G: SELECT THE STUDY POPULATION AND POTENTIAL CONTROLS to be used in the PS REGRESSIONS;


%let VERSION=20160224;
%let date=20160224;
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
libname yst ODBC dsn=idi_clean_&VERSION._srvprd schema=yst_clean;
libname sandmoe ODBC dsn=idi_sandpit_srvprd schema="clean_read_moe";
libname sandcen ODBC dsn=idi_sandpit_srvprd schema="clean_read_cen";

libname Project "\\wprdSAS10\TreasuryData\MAA2013-16 Citizen pathways through human services\Social Investment_2016\1_Indicator_at_age_datasets\dataset_rerun_24022016";
libname basedata "\\wprdSAS10\TreasuryData\MAA2013-16 Citizen pathways through human services\Sylvia\Youth Service\Data 2017 revisions\Base datasets";
libname suppdata "\\wprdSAS10\TreasuryData\MAA2013-16 Citizen pathways through human services\Sylvia\Youth Service\Data 2017 revisions\Supp datasets";;
libname studdata "\\wprdSAS10\TreasuryData\MAA2013-16 Citizen pathways through human services\Sylvia\Youth Service\Data 2017 revisions\Impact evaluation";
libname schools "\\wprdSAS10\TreasuryData\MAA2013-16 Citizen pathways through human services\Sylvia\School data from MoE website";
libname ys4 "\\wprdSAS10\TreasuryData\MAA2013-16 Citizen pathways through human services\Sylvia\Youth Service\Data March refresh\Base datasets";
libname ys5 "\\wprdSAS10\TreasuryData\MAA2013-16 Citizen pathways through human services\Sylvia\Youth Service\Data March refresh\Impact evaluation";

**Macro variables to enable the project file locations to be easily changed;

%let folder = basedata ;   **specify the location of the base files created in the prev program;
%let folder2 = studdata;   **specify the location to save files created in this program;

**Include other standard A&I programs that will be called in the course of this program;

%let path=\\wprdsas10\TreasuryData\MAA2013-16 Citizen pathways through human services\sylvia\Youth Service\Programs to be shared;

* CALL AnI generic macros that not related to specific collections;
* Call macro that includes AnI formats;

%include "&path.\Stand_macro_new.sas";
%include "&path.\FORMATS_new.sas";

%let population=&folder..popn;



********************************************************;
**PART AA - GET YS-NEET first ENROLMENTS, PEOPLE AND their START DATES, in order to use
  these reference dates later in the program;
********************************************************;

data YSneet_starts(keep=snz_uid refmth YSstartdate ) ;
  set yst.yst_spells;
  YSstartdate  =input(compress(yst_spl_participation_start_date,"-"),yymmdd8.) ;
  enddate    =input(compress(yst_spl_participation_end_date,"-"),yymmdd8.) ;
  where yst_spl_programme_name_text='NOT IN EDUCATION, EMPLOYMENT OR TRAINING';
  format startdate enddate date9.;
refmth=(12*(year(YSstartdate) -1999) + (month(YSstartdate) -3)); 
run;

proc sort data=YSneet_starts; 
by snz_uid refmth YSstartdate;
run;

data YSneet_starts2;
set YSneet_starts;
by snz_uid refmth YSstartdate;
if first.refmth ;
run;


**********************************************************;
**PART A. Create the record structure;
***The starting population is created in the program 1_Create monthly indicator datasets and contains people born in 1990-99
   who are on the IDI spine and have IR and MoE numbers; 
**  Create some data sets that will be used later, when we restrict the study and control populations for various reasons;
**  Create the strata variable to be used in the PS models;
*********************************************************;

*Restrict popn to those who were enrolled in a NZ school at some point during years 9-11 
    (later for the earliest birth cohorts because enrolment records begin in 2006);

**Restrict the study population to people with school enrolments in the relevant years - lower secondary;

data popn(drop=keep sch_enr_da_2006-sch_enr_da_2014);
	merge &folder..popn(in=a) &folder.._ind_sch_enrol_&date(keep=snz_uid sch_enr_da_2006-sch_enr_da_2014);
	by snz_uid;
	if a;
    if birth_year=1990 and sum(of sch_enr_da_2006-sch_enr_da_2008)>0 then
		keep=1;
    if birth_year=1991 and sum(of sch_enr_da_2006-sch_enr_da_2008)>0 then
		keep=1;
    if birth_year=1992 and sum(of sch_enr_da_2006-sch_enr_da_2008)>0 then
		keep=1;
	if birth_year=1993 and sum(of sch_enr_da_2006-sch_enr_da_2008)>0 then
		keep=1;
	else if birth_year=1994 and sum(of sch_enr_da_2007-sch_enr_da_2009)>0 then
		keep=1;
	else if birth_year=1995 and sum(of sch_enr_da_2008-sch_enr_da_2010)>0 then
		keep=1;
	else if birth_year=1996 and sum(of sch_enr_da_2009-sch_enr_da_2011)>0 then
		keep=1;
	else if birth_year=1997 and sum(of sch_enr_da_2010-sch_enr_da_2012)>0 then
		keep=1;
	else if birth_year=1998 and sum(of sch_enr_da_2011-sch_enr_da_2013)>0 then
		keep=1;
	else if birth_year=1999 and sum(of sch_enr_da_2012-sch_enr_da_2014)>0 then
      	keep=1;
	if keep=1 then
		output;
run;


**DEMOGRAPHICS;
**Get ethnicities and drop individuals who died before 20 years of age;

proc contents data=&population; run;

proc sql;
	create table MOE_eth_nodupl as select distinct
		snz_uid
		,max(moe_spi_ethnic_grp1_snz_ind) as moe_spi_ethnic_grp1_snz_ind
		,max(moe_spi_ethnic_grp2_snz_ind) as moe_spi_ethnic_grp2_snz_ind
		,max(moe_spi_ethnic_grp3_snz_ind) as moe_spi_ethnic_grp3_snz_ind
		,max(moe_spi_ethnic_grp4_snz_ind) as moe_spi_ethnic_grp4_snz_ind
		,max(moe_spi_ethnic_grp5_snz_ind) as moe_spi_ethnic_grp5_snz_ind
		,max(moe_spi_ethnic_grp6_snz_ind) as moe_spi_ethnic_grp6_snz_ind
	from moe.student_per
	where snz_uid in (select distinct snz_uid from &population)
		group by snz_uid;


proc sql;
	create table MOE_ethnic_1 as select distinct
		a.*
		,b.moe_spi_ethnic_grp1_snz_ind
		,b.moe_spi_ethnic_grp2_snz_ind
		,b.moe_spi_ethnic_grp3_snz_ind
		,b.moe_spi_ethnic_grp4_snz_ind
		,b.moe_spi_ethnic_grp5_snz_ind
		,b.moe_spi_ethnic_grp6_snz_ind
	from &population a left join MOE_eth_nodupl  b
		on a.snz_uid=b.snz_uid;

data MOE_ethnic;
	set MOE_ethnic_1;
	if moe_spi_ethnic_grp1_snz_ind=. then
		moe_spi_ethnic_grp1_snz_ind=0;
	if moe_spi_ethnic_grp2_snz_ind=. then
		moe_spi_ethnic_grp2_snz_ind=0;
	if moe_spi_ethnic_grp3_snz_ind=. then
		moe_spi_ethnic_grp3_snz_ind=0;
	if moe_spi_ethnic_grp4_snz_ind=. then
		moe_spi_ethnic_grp4_snz_ind=0;
	if moe_spi_ethnic_grp5_snz_ind=. then
		moe_spi_ethnic_grp5_snz_ind=0;
	if moe_spi_ethnic_grp6_snz_ind=. then
		moe_spi_ethnic_grp6_snz_ind=0;
run;



data &folder2..demog(drop=snz_sex_code /*snz_deceased_year_nbr snz_deceased_month_nbr*/ moe_spi_ethnic: bday_20);
	merge popn(in=a) MOE_ethnic(keep=snz_uid moe_spi_ethnic: );
		/*project.population1988_2015(keep=snz_uid snz_sex_code snz_deceased_year_nbr)  
		data.personal_detail(keep=snz_uid snz_deceased_month_nbr)*/
		/*project.population_eth1988_2015(keep=snz_uid moe_spi_ethnic: )*/;
	by snz_uid;
	if a;
	if snz_sex_code='1' then
		female=0;
	else female=1;
	*dod=MDY(snz_deceased_month_nbr,15,snz_deceased_year_nbr);
	bday_20=intnx('YEAR',dob,20,'S');
	european=moe_spi_ethnic_grp1_snz_ind=1;
	maori=moe_spi_ethnic_grp2_snz_ind=1;
	pacific=moe_spi_ethnic_grp3_snz_ind=1;
	asian=moe_spi_ethnic_grp4_snz_ind=1;
	ethoth=(moe_spi_ethnic_grp5_snz_ind=1 or moe_spi_ethnic_grp6_snz_ind=1);
	if sum(of european, maori, pacific, asian, ethoth)=0 then
		ethmiss=1;
	else ethmiss=0;
	cohort=year(dob);
	if (dod=. or dod>=bday_20);
run;

proc means data=ys5.demog;
run;

proc means data=&folder2..demog;
run;



**Create 29 records for each person, one for each reference month from Aug 2012 to Dec 2014;
**The startdate within the reference month is randomly assigned;

%macro records;
     %DO i=0 %TO 28;	
		data all_mths%EVAL(&i+1)(drop= mindate maxdate difference rand_days t);
			set &folder2..demog(keep=snz_uid);
			format startdate mindate maxdate date9.;
			mindate=intnx('MONTH','01Aug2012'd, &i, 'S');
			maxdate=intnx('MONTH','01Aug2012'd, &i+1, 'S')-1;
			difference=maxdate-mindate+1;
			t=ranuni(&i);
			rand_days=floor(t*difference);
			startdate=mindate+rand_days;
			refmth=%EVAL(&i+1)+160;
		run;
	%end;
%mend;

%records;

data records;
	set all_mths1-all_mths29;
run;

**If a record's id-refmth pair coincides with an actual YS-NEET first enrolment date then substitute the actual date
  for the randomly generated one - which will lead to slightly more accurate generation of variables such as the last
  school attended etc, for YS-NEET study population members;

proc sort data=records ;
	by snz_uid refmth;
run;

data &folder2..records(drop=YSstartdate);
merge records(in=a) YSneet_starts2;
by snz_uid refmth;
if a;
if YSstartdate~=. then do;
   startdate=YSstartdate;
   revised=1;
   end;
run;

proc means data=&folder2..records;
var revised ;
run;

**Drop records that don't meet the broad study population criteria;
**Must have been aged 15-18 at the reference date and not dead before their 20th birthday;

data ages(drop=age_at_start rename=(age_at_start_int=age_at_start));
	merge &folder2..records(in=a drop=revised) &folder2..demog;
	by snz_uid;
	if a;
	age_at_start_int=floor((intck('month',dob,startdate)- (day(startdate) < day(dob)))/ 12);
	age_at_start=((intck('month',dob,startdate)- (day(startdate) < day(dob))) / 12);
	if (age_at_start-age_at_start_int)<.25 then age=age_at_start_int;
	else if .25<=(age_at_start-age_at_start_int)<.50 then age=age_at_start_int+.25;
    else if .50<=(age_at_start-age_at_start_int)<.75 then age=age_at_start_int+.50;
    else if .75<=(age_at_start-age_at_start_int)<1 then age=age_at_start_int+.75;
	if 15<=age_at_start_int<=18;
run;

proc freq data=ages;
	tables age age_at_start /list missing;
run;


data &folder2..popn_restricted(rename=(startdate=YSstartdate));
set ages;
run;

/*
**v similar but nbr obs slightly lower in latest version - 6981935 not 6981978;
proc means data=ys5.popn_restricted;
run;
proc means data=&folder2..popn_restricted;
run;
*/


**************************************************************************;
****CREATE STRATA FOR THIS SPECIFIC PROJECT*********************************;
***Stratify population into 3 subgroups;
**Still at school, enrolled in tertiary, all the rest;
**We can't determine the strata for refmths from Dec 2014 onwards due to having no tertiary enrolment data for 2015;
******************************************************************************;

%let m=82;
%let z=195;
%let n=106;
%let p=189;

data strata(keep=snz_uid refmth strata /*treat*/);
	merge &folder2..popn_restricted(in=a keep=snz_uid refmth YSstartdate /*treat*/) 
	&folder..mth_sch_enrol(in=b keep=snz_uid sch_enr_id_&m-sch_enr_id_&z)
    &folder..mth_ter_enrol(keep=snz_uid ter_enr_id_&n-ter_enr_id_&p) ;
    by snz_uid;
	if a;
    array sch(*) sch_enr_id_&m-sch_enr_id_&z;
	array tert(*) ter_enr_id_&n-ter_enr_id_&p;
	array nsch(*) sch1-sch4;
	array ntert(*) tert1-tert4;
    windowstart_leed=refmth-1;
	
if refmth<=187 then do;
        do i=1 to 4;
		nsch(i)=sch(i+(windowstart_leed-81)-1);
		if nsch(i)=. then nsch(i)=0;
		ntert(i)=tert(i+(windowstart_leed-105)-1);
			if ntert(i)=. then ntert(i)=0;
		end;		
   if sch1=1 and sch2=1 and (sch3=1 or sch4=1 ) then
		strata=1;
   else if tert1=1 and tert2=1 and tert3=1 then
		strata=2;
   else strata=3;
   end;
if refmth=188 then do;
     do i=1 to 3;
		nsch(i)=sch(i+(windowstart_leed-81)-1);
		if nsch(i)=. then nsch(i)=0;
		ntert(i)=tert(i+(windowstart_leed-105)-1);
			if ntert(i)=. then ntert(i)=0;
		end;
  if sch1=1 and sch2=1 and sch3=1 then
		strata=1;
  else if tert1=1 and tert2=1 and tert3=1 then
		strata=2;
else strata=3;
	end;
run;


data &folder2..modelS;
merge &folder2..popn_restricted(in=a) strata(keep=snz_uid refmth strata );
by snz_uid refmth;
if a;
run;

proc contents data=&folder2..modelS;
run;

proc freq data=&folder2..modelS;
	tables age age_at_start /list missing;
run;


************************************************************;
***************************************************************;
**PART B:  VARIABLES REQUIRING A REFERENCE DATE TO CONSTRUCT - represeting the start of the intervention;
**eg Characteristics of the last school attended before the reference date;
**eg Information about any tertiary study that was done before the reference date;
**************************************************************;

**STUDY POPULATION RESTRICTIONS FOR OVERSEAS TIME, non-NQF schools and missing school enrolment enddates - created here but not applied yet;

**Find records where person was overseas for a total of 6 months or longer during the 4 years immed before the reference month, 
  the reference month plus the main follow-up period of  18 months = 67 months;
**Note this follow-up window is censored for reference dates in 2014 and is a minimum of 6 months;

proc contents data=&folder..mth_os;
run;


%let n=67;
data &folder2..not_os(keep=snz_uid refmth)
     &folder2..overseas_6mthsplus(keep=snz_uid refmth);
    merge &folder2..popn_restricted(in=a keep=snz_uid refmth) 
          &folder..mth_os(keep=snz_uid os_da_112-os_da_195); 
	by snz_uid;
	if a;
	windowstart_leed=refmth-48;
	array os(*) os_da_112-os_da_195;
	array nos(*) nos1-nos&n;
    array nosb(*) nosb1-nosb&n;
    avail_mths=&n;
    do i=1 to 12;
	  if refmth=(i+177) then avail_mths=(&n-i);
      end;
	do i=1 to avail_mths;
        nos(i)=os(i+(windowstart_leed-111)-1);
        if nos(i)>=7 then nos(i)=1;
        else nos(i)=0;	
	end;
    do i=1 to avail_mths;
		nosb(i)=os(i+(windowstart_leed-112)-1);
		if nosb(i)=. then nosb(i)=0;
	end;
	if sum(of nosb1-nosb&n)<=180 then output &folder2..not_os;
	  else output &folder2..overseas_6mthsplus;
run;


**LAST SCHOOL enrolment before reference date - NEEded here purely to exclude people with missing school enrolment enddates***********;
**Also look for attendance at any non-NQF school - these kids also to be dropped;
proc sql;
	create table enrol
		as select 
			snz_uid
			,input(compress(moe_esi_start_date,"-"),yymmdd10.) format date9. as startdate
			,case when moe_esi_end_date is not null then input(compress(moe_esi_end_date,"-"),yymmdd10.) 
			      else input(compress(moe_esi_extrtn_date,"-"),yymmdd10.) 
		      end 
		format date9. as enddate
		,input(moe_esi_provider_code, 10.) as schoolnbr
		,moe_esi_domestic_status_code as domestic_status
		,case when moe_esi_end_date='  ' then 1 
		    else 0 end as sch_enddate_imputed
	from moe.student_enrol
		where snz_uid in (select distinct snz_uid from &folder2..popn_restricted) and moe_esi_start_date is not null 
			order by snz_uid, startdate, enddate;
quit;

**Removing any overlaps in enrolment;
%OVERLAP (enrol);

data enrol_R;
merge enrol_OR(in=a) &folder2..demog(keep=snz_uid dob );
by snz_uid;
if a;
bday_15=intnx('YEAR',dob,15,'S');
run;


**Find non-NQF schools so chn who attended them can be identified;
**Only pick children who attended non-NQF schools when aged about 15 or older;

data nonnqf(drop=ingp);
	set enrol_R;
if enddate>=bday_15 then ingp=1;
if ingp=1 and schoolnbr in 
		(29,41,52,54,62,78,81,89,130,141,278,281,436,439,440,441,456,459,460,484,571,620,1132,1139,1605,1626,1655,2085,4152,
		37,60,67,333,387,617,1606,1640) then output;
run;


proc sql;
	create table &folder2..kids_nonnqf
		as select distinct snz_uid
			from nonnqf
				order by snz_uid;
quit;


**Select the last school enrolment spell starting before the reference date, to be used later;

proc sql;
	create table keep
		as select a.snz_uid, a.refmth, a.dob, a.YSstartdate,
			b.*
		from &folder2..popn_restricted a 
			left join enrol_or b
				on a.snz_uid=b.snz_uid
			    where b.startdate<a.YSstartdate
				order by a.snz_uid, a.refmth, a.YSstartdate, b.startdate;
quit;

data &folder2..last_sch_rec;
	set keep;
	by snz_uid refmth YSstartdate startdate;
	if last.refmth;
run;

**If the last school enrolment spell hadn't ended by data load date in mid 2016 - is this because the end date was never supplied or
  could the students legitimately still be enrolled in mid 2016?;
**If missing without good reason, these people could be excluded from the study population later, since the design relies on
  accurate assignment of YS participants to one of the three strata;

/*
data enddate_imputed(keep=snz_uid refmth);
	set &folder2..last_sch_rec;
	if sch_enddate_imputed=1; *and year(startdate)<2011;
run;

proc sort data=enddate_imputed out=&folder2..enddate_imputed;
by snz_uid refmth;
run;
*/

**Look at the school types of the last school enrolment records that have imputed end dates and lasted for >5 years;
/*
data test;
set &folder2..last_sch_rec;
if (enddate-startdate)>(5*365);
year=year(startdate);
run;

**251395 records;

proc sql;
create table unique_schools
as select unique schoolnbr
from test
order by schoolnbr;
quit;
**693 unique schools;


proc sql;
create table schoolnbr
as select unique schoolnbr, year 
from test
order by schoolnbr, year;
quit;

proc sql;
create table type
as select a.schoolnbr, a.year, b.type
from schoolnbr a
left join schools.school_directories_2005_to_2016 b
on a.schoolnbr=b.school_ID and a.year=b.year
order by schoolnbr, year;
quit;

data nonmiss;
set type;
if type~='   ';
run;
data single;
set nonmiss;
by schoolnbr year;
if last.schoolnbr;
run;

proc freq data=single;
tables type;
run;

**We only have 'type' data for 523/693 of these unique schools;

proc sql;
create table students
as select a.*, b.type
from test a
left join single b
on a.schoolnbr=b.schoolnbr;
quit;

proc freq data=students;
tables type /list missing;
title 'Known and unknown school types for the uncompleted school enrolment records with durations of 5+ years';
run;

**91% of the uncompleted school enrolment records with durations of 5+ years are enrolments at composite schools
  or secondaries covering years 7-15;
**Suggests retaining these students in the study popn is best;
*/

*********************************************************;
*Characterisitics of the last school attended before the refdate, 
**Time gap between last school attended and the refdate,
************************************************************;

**Formats for recoding the school profile data in the IDI sandpit;

proc format;
value decile
		43000='1'
		43001='2'
		43002='3'
		43003='4'
		43004='5'
		43005='6'
		43006='7'
		43007='8'
		43008='9'
		43009='10'
		43010=' '
		9999='  ';

	value insttype
		10000	=	'Casual-Education and Care'
		10001	=	'Free Kindergarten'
		10002	=	'Playcentre'
		10003	=	'Education & Care Service'
		10004	=	'Homebased Network'
		10005	=	'Te Kohanga Reo'
		10007	=	'Licence Exempt Kohanga Reo'
		10008	=	'Hospitalbased'
		10009	=	'Playgroup'
		10010	=	'Private Training Establishment'
		10011	=	'Government Training Establishment'
		10012	=	'Polytechnic'
		10013	=	'College of Education'
		10014	=	'University'
		10015	=	'Wananga'
		10016	=	'Other Tertiary Education Provider'
		10017	=	'Industry Training Organisation'
		10018	=	'Other Certifying Authorities'
		10019	=	'OTEP Resource Centre'
		10020	=	'OTEP RS24(Completes RS24)'
		10021	=	'Government Agency'
		10022	=	'Peak Body'
		10023	=	'Full Primary'
		10024	=	'Contributing'
		10025	=	'Intermediate'
		10026	=	'Special School'
		10027	=	'Centre for Extra Support'
		10028	=	'Correspondence Unit'
		10029	=	'Secondary (Year 7-15)'
		10030	=	'Composite (Year 1-15)'
		10031	=	'Correspondence School'
		10032	=	'Restricted Composite (Year 7-10)'
		10033	=	'Secondary (Year 9-15)'
		10034	=	'Teen Parent Unit'
		10035	=	'Alternative Education Provider'
		10036	=	'Activity Centre'
		10037	=	'Kura Teina - Primary'
		10038	=	'Side-school'
		10039	=	'Special Unit'
		10040	=	'Kura Teina - Composite'
		10041	=	'Land Site'
		10042	=	'Manual Training Centre (stand alone)'
		10043	=	'Community Education/Resource/Youth Learning Centre'
		10044	=	'Rural Education Activities Programme (REAP)'
		10045	=	'Special Education Service Centre'
		10047	=	'Examination Centre'
		10048	=	'School cluster (for NZQA)'
		10049	=	'School Camp'
		10050	=	'Subsidiary Provider'
		10051	=	'Miscellaneous'
		10052	=	'Kindergarten Association'
		10053	=	'Playcentre Association'
		10054	=	'Commercial ECE Service Provider'
		10055	=	'Other ECE Service Provider'
		10056	=	'Board of Trustees'
		10057	=	'Private School Provider'
		10058	=	'Campus'
		10059	=	'Local Office'
		10060	=	'Special Unit Funded';
run;

proc format;
	value authority
		42000='State'
		42001='StateIntegrated'
		42002,42003='Private'
		42004,42010,42011,42012='Other'
		42005='Public Tertiary Institution'
		42006='Privately Owned Tertiary Institution'
		42007='Tertiary prov est under own Act of Parliament'
		42008='Tertiary inst owned by a Trust'
		42009='Tertiary inst owned by an Incorporated Society';
run;



proc sql;
	create table prof
		as select 
			schoolnumber as schoolnbr
			,SchoolAuthorityID
			,SchoolTypeID
			,decileID
		from sandmoe.moe_school_profile
			where schoolnumber in (select distinct schoolnbr from &folder2..last_sch_rec)
				order by schoolnbr;
quit;


data profile(keep=schoolnbr schooltype authority decile );
set prof;
schooltype=put(SchoolTypeID,insttype. );
authority=put(SchoolAuthorityID,authority. );
decile=put(DecileID, decile.)*1;
run;

proc freq data=profile;
tables schooltype authority decile ;
run;


**Link these additional school characteristics to the enrolment records;
proc sort data=&folder2..last_sch_rec out=last_sch_rec;
	by schoolnbr;
run;

data lastsch(keep=snz_uid refmth decile schtype authority /*nonnqf */
      last_sch_enddate age_when_left_sch sch_enddate_imputed);
	merge last_sch_rec(in=a) 
          profile;
	by schoolnbr;
	if a;
	last_sch_enddate=enddate;
	age_when_left_sch= floor((intck('month',dob,enddate)- (day(enddate) < day(dob))) / 12);

	if authority='StateIntegrated' then
		authority='StateInt';
	if authority=' ' then
		authority='Other';	
	if authority in ('Private', 'Other' ) then
		decile=.;
	format schtype $12.;
	if schooltype in ('Special School', 'Special Unit', 'Special Unit Funded') then
		schtype='Special';
	else if schooltype ='Correspondence School' then
		schtype='Corresp';
	else if schooltype in ('   ', 'Miscellaneous') then
		schtype='Other';
	else schtype='Standard';
run;

proc freq data=lastsch;
	tables authority  decile  schtype age_when_left_sch /missing;
run;

proc sort data=lastsch ;
	by snz_uid refmth;
run;


**Time gap between last school enrolment and YS enrolment;

data timegap(keep=snz_uid refmth YSstartdate enddate months_since_last_school);
set &folder2..last_sch_rec;
if enddate<YSstartdate and sch_enddate_imputed~=1;
months_since_last_school=intck('month',enddate,YSstartdate,'D');
run;

proc sort data=timegap;
by snz_uid refmth;
run;

data lastsch2;
merge lastsch(in=a) timegap(keep=snz_uid refmth months_since_last_school);
by snz_uid refmth;
if a;
run;

data &folder2..lastsch(drop=YSstartdate /*treat*/ strata);
merge &folder2..models(in=a keep=snz_uid refmth YSstartdate /*treat*/ strata) lastsch2(in=b);
by snz_uid refmth;
if a;
**Set to missing if person has not yet left school ;
if last_sch_enddate>=YSstartdate or strata=1 then do;
    age_when_left_sch=.;
    months_since_last_school=.;
    end; 
run;



*******************************************************;
****NUMBER OF SCHOOLS ATTENDED BEFORE THE REFERENCE DATE;
**keep school enrolment spells that started before the YS startdate;
****************************************;

proc sql;
create table schenrol
as select 
snz_uid
,input(compress(moe_esi_start_date,"-"),yymmdd10.) format date9. as startdate
,case when moe_esi_end_date is not null then input(compress(moe_esi_end_date,"-"),yymmdd10.) 
   else input(compress(moe_esi_extrtn_date,"-"),yymmdd10.) end format date9. as enddate
,input(moe_esi_provider_code, 10.) as schoolnbr
,moe_esi_domestic_status_code as domestic_status
,case when moe_esi_end_date='  ' then 1 else 0 end as sch_enddate_imputed
from moe.student_enrol
where snz_uid in (select distinct snz_uid from &folder2..models) and moe_esi_start_date is not null 
order by snz_uid, startdate, enddate;
quit;

**Removing any overlaps in enrolment;
%OVERLAP (schenrol);


proc sql;
	create table schenrol_1 as
		select a.snz_uid,
			a.refmth,
			a.YSstartdate,
			b.*
		from &folder2..models a inner join schenrol_or b
			on a.snz_uid=b.snz_uid
			order by snz_uid, refmth, startdate;
quit;

data before;
set schenrol_1;
if startdate<YSstartdate;
count=1;
run;


**First summarise records to school level - we only want one record per person id + refmth and school id;

proc summary data=before nway;
class snz_uid refmth schoolnbr;
var count;
output out=stats(keep=snz_uid refmth schoolnbr count) sum=count;
run;

**Count number of distinct schools attended;

proc summary data=stats nway;
class snz_uid refmth;
var count;
output out=&folder2..nschools(keep=snz_uid refmth nschools) n=nschools;
run;


*******************************************************;
**NATURE OF ANY TERTIARY STUDY THAT WAS DONE BEFORE THE REFERENCE DATE;
******************************************************;
**Get all tertiary enrolment records for people who are in the treatment and potential controls samples;

%let first_anal_yr=2004;
%let last_anal_yr=2015;

proc sql;
	create table enrolt as
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
				(SELECT DISTINCT snz_uid FROM &folder2..modelS) and moe_enr_year_nbr>=&first_anal_yr and moe_enr_year_nbr<=&last_anal_yr 
					group by snz_uid, moe_enr_prog_start_date, moe_enr_prog_end_date, qual, NZSCED
						order by snz_uid;
quit;


data enrol_clean_formal;
	set enrolt;
	if EFTS_consumed>0;
	dur=enddate-startdate;
	if dur>0;
	start_year=year(startdate);
	if start_year>=&first_anal_yr and start_year<=&last_anal_yr;
	if qual_type="D" then Formal=1;
	if formal=1 then
		output;
run;


proc sql;
	create table enrol_1 as
		select 
			a.refmth,
			a.YSstartdate,
			b.*
		from &folder2..models a inner join enrol_clean_formal b
			on a.snz_uid=b.snz_uid
			where b.startdate<a.YSstartdate
			order by snz_uid, refmth, startdate;
quit;


* Occupational skills programmes versus general skills programmes;

data enrol_clean_formal2;
	set enrol_1;
	if NZSCED >'120000' then
		genskills_prog=1;
	else genskills_prog=0;
	if NZSCED <'120000' then
		occskills_prog=1;
	else occskills_prog=0;
run;

proc summary data=enrol_clean_formal2 nway;
	class snz_uid refmth;
	var genskills_prog occskills_prog;
	output out=stats sum=;
run;

data &folder2..tert_prog_preYS(compress=y keep=snz_uid refmth genskills_only occskills_only gen_n_occ_skills);
	set stats;
	if genskills_prog>=1 then
		genskills_prog=1;
	else genskills_prog=0;
	if occskills_prog>=1 then
		occskills_prog=1;
	else occskills_prog=0;
	if genskills_prog=1 and occskills_prog=0 then
		genskills_only=1;
	else genskills_only=0;
	if genskills_prog=1 and occskills_prog=1 then
		gen_n_occ_skills=1;
	else gen_n_occ_skills=0;
	if genskills_prog=0 and occskills_prog=1 then
		occskills_only=1;
	else occskills_only=0;
run;

proc means data=&folder2..tert_prog_preYS;
run;


**Place of residence at end of the month immediately before the reference date;

%let m=160;
%let n=189;

data lastmth(keep=snz_uid refmth nzdep tla reg);
	merge &folder2..modelS(in=a keep=snz_uid refmth YSstartdate)  &folder..residence(keep=snz_uid
        nzdep&m-nzdep&n ta&m-ta&n reg&m-reg&n)    /*ys.nschools*/;
	by snz_uid;
	if a;
	array nzdepV(*) nzdep&m-nzdep&n;
	array tlaV(*) ta&m-ta&n;
	array regV(*) reg&m-reg&n;
	do i=1 to dim(nzdepV);
		if i+159=refmth-1 then
			do;
				nzdep=nzdepV(i);
				tla=tlaV(i);
				reg=regV(i);
			end;
	end;   
run;

proc means data=lastmth;
run;

proc freq data=lastmth;
	tables nzdep tla reg /*nschools*/ /list missing;
run;

proc sort data=lastmth out=&folder2..lastmth;
	by snz_uid refmth;
run;



***********************************************;
**NCEA credits and NCEA qualifications awarded;
**As at the end of the year before the reference date;
**Any tertiary qualifications that were awarded before the reference date;
***********************************************;


proc sort data=&folder..ncea_credit_sum;
by snz_uid;
run;


data quals(keep=snz_uid refmth hqual hothqual hterqual ncea_cr_l1-ncea_cr_l3 prop_cr_ext_cat );
	merge &folder2..models(in=a keep=snz_uid refmth YSstartdate) 
        &folder..highest_quals_achieved
		&folder..ncea_credit_sum;
	by snz_uid;
	if a;
	if YSstartdate<'01Jan2013'd then
		do;
			hqual=hncea_2011;
			hothqual=hnonncea_2011;
            hterqual=max(of hterqual_2011, hitqual_2011);
			ncea_cr_l1=tot_ncea_cr_L1_by_2011;
			ncea_cr_l2=tot_ncea_cr_L2_by_2011;
			ncea_cr_l3=tot_ncea_cr_L3_by_2011;
			prop_cr_ext_cat=prop_cr_ext_by_2011_cat;
		end;
	else if '01Jan2013'd<=YSstartdate<='31Dec2013'd then
		do;
			hqual=hncea_2012;
			hothqual=hnonncea_2012;
			hterqual=max(of hterqual_2012, hitqual_2012);
			ncea_cr_l1=tot_ncea_cr_L1_by_2012;
			ncea_cr_l2=tot_ncea_cr_L2_by_2012;
			ncea_cr_l3=tot_ncea_cr_L3_by_2012;
            prop_cr_ext_cat=prop_cr_ext_by_2012_cat;
		end;
    else if '01Jan2014'd<=YSstartdate<='31Dec2014'd then
		do;
			hqual=hncea_2013;
			hothqual=hnonncea_2013;
			hterqual=max(of hterqual_2013, hitqual_2013);
			ncea_cr_l1=tot_ncea_cr_L1_by_2013;
			ncea_cr_l2=tot_ncea_cr_L2_by_2013;
			ncea_cr_l3=tot_ncea_cr_L3_by_2013;
            prop_cr_ext_cat=prop_cr_ext_by_2013_cat;
		end;
array quals(*) hqual hothqual hterqual  ncea_cr_l1-ncea_cr_l3;
	do i=1 to dim(quals);
		if quals(i)=. then
			quals(i)=0;
	end;
run;


proc sort data=quals out=&folder2..pre_YS_quals;
	by snz_uid refmth;
run;


****************************************;
**Monthly employment, enrolment history, benefit receipt history and inactivity history prior to the refmth;
****************************************;


proc contents data=&folder..mth_studall;
run;


%let m=82;
%let n=106;
%let y=112;

%let p=189;
%let z=195;

**Target vector length;
%let b=79;

data &folder2..monthly(compress=y drop=i windowstart_leed  avail_mths avail_mths2  
         sch_enr_id_&y-sch_enr_id_&z  rearn&y-rearn&z ter_enr_id_&y-ter_enr_id_&p emp&y-emp&z 
         ben_id_&y-ben_id_&z os_da_&y-os_da_&z  itl_id_&y-itl_id_&z
         sa&y-sa&z  rename=(nemp1-nemp&b=emp1-emp&b nsa1-nsa&b=sa1-sa&b));
	merge &folder2..models(in=a keep=snz_uid refmth YSstartdate ) 
		&folder..mth_sch_enrol(in=b keep=snz_uid sch_enr_id_&y-sch_enr_id_&z)
		&folder..mth_ter_enrol(keep=snz_uid ter_enr_id_&y-ter_enr_id_&p) 
		&folder..mth_it_enrol(keep=snz_uid itl_id_&y-itl_id_&z)
		&folder..mth_emp(keep=snz_uid emp&y-emp&z rearn&y-rearn&z)
		&folder..mth_ben(keep=snz_uid ben_id_&y-ben_id_&z)
        &folder..mth_os(keep=snz_uid os_da_&y-os_da_&z)
        &folder..mth_studall(keep=snz_uid sa&y-sa&z);
	by snz_uid;
	if a;
	array sch(*) sch_enr_id_&y-sch_enr_id_&z;
	array emp(*) emp&y-emp&z;
	array it(*)  itl_id_&y-itl_id_&z;
	array tert(*) ter_enr_id_&y-ter_enr_id_&p;
	array ben(*) ben_id_&y-ben_id_&z;
	array earn(*) rearn&y-rearn&z;
	array os(*) os_da_&y-os_da_&z;
    array sa(*) sa&y-sa&z;

	array nsch(*) sch1-sch&b;
	array nemp(*) nemp1-nemp&b;
	array ntert(*) tert1-tert&b;
	array nit(*) it1-it&b;
	array nben(*) ben1-ben&b;
	array nearn(*) earn1-earn&b;
    array nos(*) nos1-nos&b;	
    array nsa(*) nsa1-nsa&b;

	windowstart_leed=refmth-48;
	**Available months is less than 18 months for people who
      started in 2014 as we are only using data for first 6 months of 2015; 
	avail_mths=&b;
    do i=1 to 12;
	  if refmth=(i+165) then avail_mths=(avail_mths-i);
      end;
    do i=1 to 12;
	  if refmth=(i+177) then avail_mths=(avail_mths-12-i);
      end;
    **Tertiary study and NEET measures can't be constructed for 2015 or 2016;
    avail_mths2=&b;
    do i=1 to 29;
	  if refmth=(i+160) then avail_mths2=78-i;
      end;
	do i=1 to avail_mths;
		nemp(i)=emp(i+(windowstart_leed-111)-1);
		   if nemp(i)=. then  nemp(i)=0;
        nearn(i)=earn(i+(windowstart_leed-111)-1);
		nsch(i)=sch(i+(windowstart_leed-111)-1);
		   if nsch(i)=. then nsch(i)=0;
        nsa(i)=sa(i+(windowstart_leed-111)-1);
		   if nsa(i)=. then nsa(i)=0;
		nben(i)=ben(i+(windowstart_leed-111)-1);
		if nben(i)=. then
			nben(i)=0;
        nos(i)=os(i+(windowstart_leed-111)-1);
          if nos(i)>=7 then nos(i)=1;
          else nos(i)=0;
        nit(i)=it(i+(windowstart_leed-111)-1);
		   if nit(i)=. then nit(i)=0;
		end;
        
	do i=1 to avail_mths2;
		ntert(i)=tert(i+(windowstart_leed-111)-1);
			if ntert(i)=. then
					ntert(i)=0;
	    end;
	array study(*) study1-study&b;
	array neet(*) neet1-neet&b;
	array eet(*) eet1-eet&b;
	**NEET if not enrolled at school or tertiary and not employed in a W&S position
	   and not overseas during the month;
	do i=1 to avail_mths2;
		if (nsch(i)>0 or ntert(i)>0) then study(i)=1;
		    else study(i)=0;
		if (nsch(i)>0 or ntert(i)>0 or nemp(i)>0)  then eet(i)=1;
		  else eet(i)=0;
		if (nsch(i)=0 and ntert(i)=0 and nemp(i)=0 and nos(i)=0) then neet(i)=1;
		   else neet(i)=0;
	end;  
run;

proc means data=&folder2..monthly;
var tert1-tert67;
run;


**Vectors showing whether any custodial or community sentence was served in each calendar month;

proc contents data=&folder..mth_corrections;
run;

%let b=79;
%let m=106;  *Jan 2008;
%let n=195;   *June 2015;
data &folder2..monthly_corrections(compress=y drop= i j windowstart_leed avail_mths
     cust_id_&m-cust_id_&n comm_id_&m-comm_id_&n);
merge &folder2..models(in=a  keep=snz_uid refmth YSstartdate) 
      &folder..mth_corrections(keep=snz_uid cust_id_&m-cust_id_&n comm_id_&m-comm_id_&n);
by snz_uid;
	if a;
array corr [2,90] cust_id_&m-cust_id_&n  comm_id_&m-comm_id_&n  ;  
array ncorr [2,79] cust1-cust&b comm1-comm&b;
windowstart_leed=refmth-48;
avail_mths=&b;
    do i=1 to 12;
	  if refmth=(i+165) then avail_mths=(avail_mths-i);
      end;
    do i=1 to 12;
	  if refmth=(i+177) then avail_mths=(avail_mths-12-i);
      end;
do j=1 to 2;
do i=1 to avail_mths;
    ncorr(j,i)=corr(j,i+(windowstart_leed-105)-1);
		if ncorr(j,i)=. then ncorr(j,i)=0;
  end;
  end;
array corrsum(*) corr1-corr&b;
  do i=1 to &b;
  if ncorr[1,i]=1 or ncorr[2,i]=1 then corrsum(i)=1;
  else if ncorr[1,i]=0 or ncorr[2,i]=0 then corrsum(i)=0;
  end;
run;


proc means data=&folder2..monthly_corrections;
run;


**Construct some pre-history summary variables that will be used as explanatory variables in the PS models; 
**The "last5" varibles do not include the month immediately before the reference or YS start month, while the "last6" variables do include it;

data pre_YS_monthly(keep=snz_uid refmth mths_sch_last6 mths_sch_prior12 
    mths_sch_first30 mths_sch_last18
	mths_emp_last6 mths_emp_prior12 mths_emp_first30 mths_emp_last18
	mths_tert_last6 mths_tert_prior12 mths_tert_first30 mths_tert_last18
	mths_neet_last6 mths_neet_prior12 mths_neet_first30 mths_neet_last18
	mths_ben_last6 mths_ben_prior12 mths_ben_first30   mths_ben_last18
    mean_earn_last6 
    sch48 emp48 tert48 neet48 ben48
    mths_sch_last5 mths_emp_last5 mths_tert_last5 mths_neet_last5 mths_ben_last5);

    set &folder2..monthly;
	**summing the data for 2-6 months before the reference month;
    mths_sch_last5=sum(of sch43-sch47);
	mths_emp_last5=sum(of emp43-emp47);
	mths_tert_last5=sum(of tert43-tert47);
	mths_neet_last5=sum(of neet43-neet47);
	mths_ben_last5=sum(of ben43-ben47);
    ** 1-6 months before the reference month;
	mths_sch_last6=sum(of sch43-sch48);
	mths_emp_last6=sum(of emp43-emp48);
	mths_tert_last6=sum(of tert43-tert48);
	mths_neet_last6=sum(of neet43-neet48);
	mths_ben_last6=sum(of ben43-ben48);
	** 7-18 months before the reference month;
	mths_sch_prior12=sum(of sch31-sch42);
	mths_emp_prior12=sum(of emp31-emp42);
	mths_tert_prior12=sum(of tert31-tert42);
	mths_neet_prior12=sum(of neet31-neet42);
	mths_ben_prior12=sum(of ben31-ben42);
	** 19-48 months before the refence month;
	mths_sch_first30=sum(of sch1-sch30);
	mths_emp_first30=sum(of emp1-emp30);
	mths_tert_first30=sum(of tert1-tert30);
	mths_neet_first30=sum(of neet1-neet30);
	mths_ben_first30=sum(of ben1-ben30);
	** 18 months before the reference month;
    mths_sch_last18=sum(of sch31-sch48);
	mths_emp_last18=sum(of emp31-emp48);
	mths_tert_last18=sum(of tert31-tert48);
	mths_neet_last18=sum(of neet31-neet48);
	mths_ben_last18=sum(of ben31-ben48);
	mean_earn_last6=mean(of earn43-earn48);
run;

proc sort data=pre_YS_monthly out=&folder2..pre_YS_monthly;
	by snz_uid refmth;
run;

/*
proc means data=ys5.pre_YS_monthly_revised;

proc means data=&folder2..pre_YS_monthly;
run;
*/



*********************************************************;
**ANNUAL EXPLANATORY VARIABLES - containing information on a child's parental and own life time history before the reference date;
**USES VARIABLES COVERING EACH YEAR OF AGE UP TO THE BIRTHDAY BEFORE THE REFERENCE DATE, from datasets constructed by A&I;
*********************************************************;

***Compile data on life histories by year of age;

proc contents data=&folder2..models;
run;
proc means data=&folder2..models;
run;
proc contents data=&folder..mat_educ_comb_&date;
run;
proc contents data=&folder.._ind_os_spells_at_age_&date; 
run;

/***Compare mother unqualified measures;*/
/*
%let firstage=0;
%let lastage=17;

**Dataset I actually used;
data testA;
merge &folder2..models(in=a keep=snz_uid refmth YSstartdate dob)
	  suppdata.mat_educ_comb_&date(keep=snz_uid maternal_edu_prior_birth maternal_edu_at_age_&firstage-maternal_edu_at_age_&lastage);
by snz_uid;
if a;
highest_mat_edu_at_age_0=max(of maternal_edu_prior_birth, maternal_edu_at_age_&firstage);
highest_mat_edu_at_age_1=max(of maternal_edu_prior_birth, maternal_edu_at_age_&firstage-maternal_edu_at_age_1);
highest_mat_edu_at_age_2=max(of maternal_edu_prior_birth, maternal_edu_at_age_&firstage-maternal_edu_at_age_2);
highest_mat_edu_at_age_3=max(of maternal_edu_prior_birth, maternal_edu_at_age_&firstage-maternal_edu_at_age_3);
highest_mat_edu_at_age_4=max(of maternal_edu_prior_birth, maternal_edu_at_age_&firstage-maternal_edu_at_age_4);
highest_mat_edu_at_age_5=max(of maternal_edu_prior_birth, maternal_edu_at_age_&firstage-maternal_edu_at_age_5);
highest_mat_edu_at_age_6=max(of maternal_edu_prior_birth, maternal_edu_at_age_&firstage-maternal_edu_at_age_6);
highest_mat_edu_at_age_7=max(of maternal_edu_prior_birth, maternal_edu_at_age_&firstage-maternal_edu_at_age_7);
highest_mat_edu_at_age_8=max(of maternal_edu_prior_birth, maternal_edu_at_age_&firstage-maternal_edu_at_age_8);
highest_mat_edu_at_age_9=max(of maternal_edu_prior_birth, maternal_edu_at_age_&firstage-maternal_edu_at_age_9);
highest_mat_edu_at_age_10=max(of maternal_edu_prior_birth, maternal_edu_at_age_&firstage-maternal_edu_at_age_10);
highest_mat_edu_at_age_11=max(of maternal_edu_prior_birth, maternal_edu_at_age_&firstage-maternal_edu_at_age_11);
highest_mat_edu_at_age_12=max(of maternal_edu_prior_birth, maternal_edu_at_age_&firstage-maternal_edu_at_age_12);
highest_mat_edu_at_age_13=max(of maternal_edu_prior_birth, maternal_edu_at_age_&firstage-maternal_edu_at_age_13);
highest_mat_edu_at_age_14=max(of maternal_edu_prior_birth, maternal_edu_at_age_&firstage-maternal_edu_at_age_14);
highest_mat_edu_at_age_15=max(of maternal_edu_prior_birth, maternal_edu_at_age_&firstage-maternal_edu_at_age_15);
highest_mat_edu_at_age_16=max(of maternal_edu_prior_birth, maternal_edu_at_age_&firstage-maternal_edu_at_age_16);
highest_mat_edu_at_age_17=max(of maternal_edu_prior_birth, maternal_edu_at_age_&firstage-maternal_edu_at_age_17);
array mother1(*) highest_mat_edu_at_age_&firstage-highest_mat_edu_at_age_&lastage;
array mother2(*) mother_unqual_at_age_&firstage-mother_unqual_at_age_&lastage;
	do i=1 to dim(mother1);
		if mother1(i)=0 then
			mother2(i)=1;
		else mother2(i)=0;
	end;
run;

proc means data=testA;
var mother_unqual_at_age_&firstage-mother_unqual_at_age_&lastage;
run;


**Reconstructed version;

data testB;
merge &folder2..models(in=a keep=snz_uid refmth YSstartdate dob)
      &folder..mat_educ_&date(keep=snz_uid highest_mat_edu_at_age_&firstage-highest_mat_edu_at_age_&lastage);
by snz_uid;
if a;
array mother1(*) highest_mat_edu_at_age_&firstage-highest_mat_edu_at_age_&lastage;
array mother2(*) mother_unqual_at_age_&firstage-mother_unqual_at_age_&lastage;
	do i=1 to dim(mother1);
		if mother1(i)=0 then
			mother2(i)=1;
		else mother2(i)=0;
	end;
run;

proc means data=testA;
var mother_unqual_at_age_&firstage-mother_unqual_at_age_&lastage;
run;

proc means data=testB;
var mother_unqual_at_age_&firstage-mother_unqual_at_age_&lastage;
run;

*/



%let firstage=0;
%let lastage=17;

data annual;
	merge &folder2..models(in=a keep=snz_uid refmth YSstartdate age age_at_start 
		dob female european maori pacific  asian ethoth ethmiss  cohort) 
	&folder.._ind_cyf_at_age_&date(keep=snz_uid 
		child_any_fdgs_abuse_at_age_&firstage-Child_any_fdgs_abuse_at_age_&lastage
		child_not_at_age_&firstage-child_not_at_age_&lastage
		child_Pol_FV_not_at_age_&firstage-child_Pol_FV_not_at_age_&lastage
		child_YJ_referral_at_age_&firstage-child_YJ_referral_at_age_&lastage
		child_CYF_place_at_age_&firstage-child_CYF_place_at_age_&lastage
		child_YJ_place_at_age_&firstage-child_YJ_place_at_age_&lastage)
	&folder.._ind_cg_corr_at_age_&date(keep=snz_uid cg_comm_at_age_&firstage-cg_comm_at_age_&lastage 
		cg_cust_at_age_&firstage-cg_cust_at_age_&lastage)
	&folder.._ind_bdd_child_at_age_&date(keep=snz_uid ch_total_da_onben_at_age_&firstage-ch_total_da_onben_at_age_&lastage
		ch_da_SPSR: )
	&folder.._ind_os_spells_at_age_&date(keep=snz_uid os_da:)
	&folder.._ind_interven_at_age_&date(keep=snz_uid trua_da_at_age_&firstage-trua_da_at_age_&lastage   
		susp_da_at_age_&firstage-susp_da_at_age_&lastage   sedu_da_at_age_&firstage-sedu_da_at_age_&lastage
		stand_da_at_age_&firstage-stand_da_at_age_&lastage)
    &folder.._ind_mhealth_at_age_&date(keep=snz_uid any_mh_at_age_&firstage-any_mh_at_age_&lastage)
	&folder..mat_educ_&date(keep=snz_uid highest_mat_edu_at_age_&firstage-highest_mat_edu_at_age_&lastage);
	by snz_uid;
	if a;  
	array mother1(*) highest_mat_edu_at_age_&firstage-highest_mat_edu_at_age_&lastage;
	array mother2(*) mother_unqual_at_age_&firstage-mother_unqual_at_age_&lastage;
	do i=1 to dim(mother1);
		if mother1(i)=0 then
			mother2(i)=1;
		else mother2(i)=0;
	end;
	array comm(*) cg_comm_at_age_&firstage-cg_comm_at_age_&lastage;
	array cust(*) cg_cust_at_age_&firstage-cg_cust_at_age_&lastage;
	array corr(*) cg_corr_at_age_&firstage-cg_corr_at_age_&lastage;

	do i=1 to dim(comm);
		if comm(i)=1 or cust(i)=1 then
			corr(i)=1;
		else corr(i)=0;
	end;
	model_age=age_at_start-1;	
run;

proc means data=annual;
var mother_unqual_at_age_&firstage-mother_unqual_at_age_&lastage;
run;

%macro sumbyage (prefix,lower_age1,to_age,ind);
	%if &ind=1 %then
		X_&prefix.=sum(of &prefix._at_age_&lower_age1.-&prefix._at_age_&to_age.)>0;
	%else X_&prefix._sum=sum(of &prefix._at_age_&lower_age1.-&prefix._at_age_&to_age.);;
%mend;


proc format;
	value bendur
		.,low-0='anone  '
		0<-.10='b1-9%'
		.10<-.25='c10-24%'
		.25<-.50='d25-49%'
		.50<-.75='e50-74%'
		.75<-high='f75+%';

proc format;
	value osdur
		.,low-<.10='a<10%  '
		.10-<.50='b10-<50%'
		.50-<.74999='c50-<75%'
		.74999<-high='d75%+ ';
run;

proc format;
   value nbrnot
       .,low-0='aNone '
       1-2='b1-2'
	   3-9='c3-9'
	   10-high='d10+';
run;

%macro compile2(lower_age1,to_age,curr_age);
	data grp&curr_age(keep=snz_uid
		dob
		refmth
		YSstartdate
		x_age
		x_female
		x_european
		x_maori
		x_pacific
		x_asian
		x_ethoth
		x_ethmiss
		x_cohort

		x_ch_total_da_onben_sum
		X_prop_onben_aschild_cat
		X_onben50_aschild
		X_onben75_aschild
		X_ever_onben_aschild
		x_ch_da_SPSR
		x_child_bentype
		x_years_onben_as_child

		x_os_da_sum
		x_prop_os_aschild_cat
		x_years_os_as_child

		x_mother_unqual
		X_cg_cust
		X_cg_comm
		x_cg_corr

		X_child_not
		X_child_yj_referral
		X_child_cyf_place
		X_child_yj_place
		X_child_any_fdgs_abuse      
        X_any_mh 

		x_sedu_da
		x_trua_da
		x_susp_da
		x_stand_da

        X_child_not_cat
		X_child_yj_referral_cat
		X_susp_da_cat
        X_stand_da_cat
		);

		set annual;

		if model_age=&curr_age;
		x_age=age_at_start;
		x_female=female;
		x_european=european;
		x_maori=maori;
		x_pacific=pacific;
		x_asian=asian;
		x_ethoth=ethoth;
		x_ethmiss=ethmiss;
		x_cohort=cohort;

		x_mother_unqual=mother_unqual_at_age_&to_age;	

		%sumbyage(child_not,&lower_age1,&to_age,1);
        %sumbyage(child_not,&lower_age1,&to_age,0);
            X_child_not_cat = put(X_child_not_sum,nbrnot.); 
		 
		%sumbyage(child_any_fdgs_abuse,&lower_age1,&to_age,1);
		%sumbyage(child_CYF_place,&lower_age1,&to_age,1);
		%sumbyage(child_Pol_FV_not,&lower_age1,&to_age,1);
		%sumbyage(cg_comm,&lower_age1,&to_age,1);
		%sumbyage(cg_cust,&lower_age1,&to_age,1);
		%sumbyage(cg_corr,&lower_age1,&to_age,1);
		%sumbyage(ch_total_da_onben,&lower_age1,&to_age,0);
	       currentdate=intnx('YEAR',dob,&to_age+1,'S')-1;
		   days_since_birth=currentdate-dob;
		X_prop_onben_aschild = (X_ch_total_da_onben_sum) /days_since_birth;
		X_prop_onben_aschild_cat=put(X_prop_onben_aschild,bendur.);
		X_onben50_aschild = (X_ch_total_da_onben_sum /days_since_birth) >.50;
		X_onben75_aschild = (X_ch_total_da_onben_sum /days_since_birth) >.75;
		X_years_onben_as_child =round((X_ch_total_da_onben_sum/365),0.1);
		X_ever_onben_aschild=X_ch_total_da_onben_sum>0;

		%sumbyage(ch_da_SPSR,&lower_age1,&to_age,1);
		if X_ch_da_SPSR then
			X_child_bentype='SPSR ';
		else if X_ch_total_da_onben_sum>0 then
			X_child_bentype='Other';
		else  X_child_bentype='None ';

		%sumbyage(sedu_da,&lower_age1,&to_age,1);
		%sumbyage(trua_da,&lower_age1,&to_age,1);
		%sumbyage(stand_da,&lower_age1,&to_age,1);
		    %sumbyage(stand_da,&lower_age1,&to_age,0);
            X_stand_da_cat = put(X_stand_da_sum,nbrnot.);
		%sumbyage(susp_da,&lower_age1,&to_age,1);
            %sumbyage(susp_da,&lower_age1,&to_age,0);
            X_susp_da_cat = put(X_susp_da_sum,nbrnot.);
		
		%sumbyage(child_YJ_referral,&lower_age1,&to_age,1);
        %sumbyage(child_YJ_referral,&lower_age1,&to_age,0);
            X_child_YJ_referral_cat = put(X_child_YJ_referral_sum,nbrnot.);

		*%sumbyage(YC_appearances,15,&to_age,1);
		%sumbyage(child_YJ_place,&lower_age1,&to_age,1);
		
        %sumbyage(any_mh,&lower_age1,&to_age,1);

		%sumbyage(os_da,&lower_age1,&to_age,0);
		X_prop_os_aschild=(X_os_da_sum) /days_since_birth;
		X_prop_os_aschild_cat=put(X_prop_os_aschild, osdur.);
		X_years_os_as_child =round((X_os_da_sum/365),0.1);
	run;

%mend compile2;

%compile2(0,14,14);
%compile2(0,15,15);
%compile2(0,16,16);
%compile2(0,17,17);

data annual2;
	set grp14 grp15 grp16 grp17;
run;

proc sort data=annual2 out=&folder2..annual;
	by snz_uid refmth;
run;

/*
proc means data=ys5.annual;
proc means data=&folder2..annual;
run;
*/

**Percent with the unqualified mother indicator is much lower than before because I have fixed the mistake I orginally made;
**Otherwise means are almost exactly the same;


**********************************************************************************;
**PART C: COMPILE SUMMARY DATASETS TO USE IN PS MODELS or descriptive statistics;
*********************************************************************;

proc format;
	value mth6m
		0-1=1
		2-3=2
		4-6=3;
run;

proc format;
	value mth12m
		0-3=1
		4-6=2
		7-9=3
		10-12=4;
run;

proc format;
	value mth30m
		0-6=1
		7-12=2
		13-18=3
		19-24=4
		25-30=5;
run;


proc format;
	value nceacred
		.,0-4=1
		5-49=2
		50-59=3
		60-79=4
		80-high=5;
run;

proc format;
 value schgap
   .='hNA'
   0,1='a<2mths'
   2-3='b2-3mths'
   4-6='c4-6mths'
   7-12='d7-12mths'
   13-24='e1-2yrs'
   25-36='f2-3yrs'
   37-high='g>3yrs';

value nschcat
.,1,2=2
3=3
4=4
5=5
6-high=6;
run;


proc contents data=&folder2..lastsch;
run;
proc contents data=&folder2..nschools;
run;
proc contents data=&folder2..lastmth;
run;
proc contents data=&folder2..tert_prog_preYS;
run;

proc contents data=&folder2..pre_YS_quals;
run;
proc means data=&folder2..pre_YS_quals;
var prop_cr_ext_cat;
run;


proc freq data=&folder2..lastsch;
tables authority;
run;

proc contents data=&folder2..models; 
run;

proc freq data=&folder2..models;
tables cohort;
run;


**Start bringing together the data for the PS model and convert some continuous variables into categorical;
**Also aggregate up some variable categories that are uncommon; 

data modelb(keep=snz_uid refmth YSstartdate strata dob dod 
	cohort age female european maori pacific asian ethoth ethmiss X_nzdep X_tla x_region x_decile x_schtype x_schauth
	ncea_cr_l1-ncea_cr_l3 /*prop_cr_ext_cat*/
	x_hqual x_hothqual x_hterqual x_ncea_cr_l1_cat x_ncea_cr_l2_cat x_ncea_cr_l3_cat  nschools x_nsch_cat months_since_last_school 
    time_since_school age_when_left_sch
    sch48 emp48 tert48 neet48 ben48 /*These are variables capturing status in the month immediately before the start month*/
    x_sch_last5 x_emp_last5 x_tert_last5 x_neet_last5 x_ben_last5
	x_sch_last6 x_emp_last6 x_tert_last6 x_neet_last6 x_ben_last6
	x_sch_prior12 x_emp_prior12 x_tert_prior12 x_neet_prior12 x_ben_prior12
	x_sch_first30 x_emp_first30 x_tert_first30 x_neet_first30 x_ben_first30
	X_prop_onben_aschild_cat  X_ever_onben_aschild  x_child_bentype 
	x_prop_os_aschild_cat
	x_mother_unqual X_cg_cust X_cg_comm x_cg_corr
	X_child_not X_child_yj_referral X_child_cyf_place X_child_yj_place X_child_any_fdgs_abuse X_any_mh 
	x_sedu_da x_trua_da x_susp_da x_stand_da
    X_child_not_cat X_child_yj_referral_cat X_susp_da_cat X_stand_da_cat
    genskills_only occskills_only gen_n_occ_skills
    /*mths_sch_last18 mths_emp_last18 mths_tert_last18 mths_neet_last18 mths_ben_last18 mean_earn_last6*/
    );
	merge &folder2..models(in=a keep=snz_uid refmth YSstartdate dob dod strata 
	          cohort age_at_start female european maori pacific asian ethoth ethmiss) 
		&folder2..lastsch(keep=snz_uid refmth decile schtype authority months_since_last_school age_when_left_sch)
		&folder2..nschools(keep=snz_uid refmth nschools)
		&folder2..lastmth(keep=snz_uid refmth nzdep tla reg) 
		&folder2..pre_YS_quals
		&folder2..pre_YS_monthly
		&folder2..annual(keep=snz_uid refmth x_ch_total_da_onben_sum
        X_prop_onben_aschild_cat
		X_onben50_aschild
		X_onben75_aschild
		X_ever_onben_aschild
		x_ch_da_SPSR
		x_child_bentype
		x_years_onben_as_child
		x_os_da_sum
		x_prop_os_aschild_cat
		x_years_os_as_child
		x_mother_unqual
		X_cg_cust
		X_cg_comm
		x_cg_corr
		X_child_not
		X_child_yj_referral
		X_child_cyf_place
		X_child_yj_place
		X_child_any_fdgs_abuse      
        X_any_mh 
		x_sedu_da
		x_trua_da
		x_susp_da
		x_stand_da
        X_child_not_cat
		X_child_yj_referral_cat
		X_susp_da_cat
        X_stand_da_cat)
        &folder2..tert_prog_preYS(keep=snz_uid refmth genskills_only occskills_only gen_n_occ_skills);
	by snz_uid refmth;
	if a;
	age=age_at_start;
	x_sch_last6=(put(mths_sch_last6,mth6m.))*1;
	x_emp_last6=(put(mths_emp_last6,mth6m.))*1;
	x_tert_last6=(put(mths_tert_last6,mth6m.))*1;
	x_neet_last6=(put(mths_neet_last6,mth6m.))*1;
	x_ben_last6=(put(mths_ben_last6,mth6m.))*1;
	
    x_sch_last5=(put(mths_sch_last5,mth6m.))*1;
	x_emp_last5=(put(mths_emp_last5,mth6m.))*1;
	x_tert_last5=(put(mths_tert_last5,mth6m.))*1;
	x_neet_last5=(put(mths_neet_last5,mth6m.))*1;
	x_ben_last5=(put(mths_ben_last5,mth6m.))*1;

	x_sch_prior12=(put(mths_sch_prior12,mth12m.))*1;
	x_emp_prior12=(put(mths_emp_prior12,mth12m.))*1;
	x_tert_prior12=(put(mths_tert_prior12,mth12m.))*1;
	x_neet_prior12=(put(mths_neet_prior12,mth12m.))*1;
	x_ben_prior12=(put(mths_ben_prior12,mth12m.))*1;
	x_sch_first30=(put(mths_sch_first30,mth30m.))*1;
	x_emp_first30=(put(mths_emp_first30,mth30m.))*1;
	x_tert_first30=(put(mths_tert_first30,mth30m.))*1;
	x_neet_first30=(put(mths_neet_first30,mth30m.))*1;
	x_ben_first30=(put(mths_ben_first30,mth30m.))*1;

	x_hqual=hqual;
    if hqual=. then x_hqual=0;
	**create a new category of the highest qual variable for people whose highest
	   qual is not an ncea qualification;
	if hothqual>hqual or hterqual>hqual then x_hqual=6; 
    if hothqual>0 then x_hothqual=1; else x_hothqual=0; 
	
	if hterqual>0 then x_hterqual=1; else x_hterqual=0; 
	array credits(*) ncea_cr_l1-ncea_cr_l3;
	do i=1 to 3;
		if credits(i)=. then
			credits(i)=0;
	end;
	x_ncea_cr_l1_cat=(put(ncea_cr_l1,nceacred.))*1;
	x_ncea_cr_l2_cat=(put(ncea_cr_l2,nceacred.))*1;
	x_ncea_cr_l3_cat=(put(ncea_cr_l3,nceacred.))*1;
	x_decile=decile;

	if decile in (., 99) then
		x_decile=99;
	x_schtype=schtype;
	x_schauth=authority;
    if x_schtype='Special' then
		x_schtype='Other';
    if x_schtype='   ' then
		x_schtype='Other';
	if x_schauth='Private' then
		x_schauth='Other';
    if x_schauth='   ' then
		x_schauth='Other';
    if months_since_last_school~=. then do;
      time_since_school=put(months_since_last_school,schgap.);
      end;
    **Note a small number of cases have no school enrolment before their first reference date in this 
	   dataset, probably because they  just arrived from overseas, therefore they have a missing value for nschools;
    **I am recoding them here to the base category of nschools;
    **These cases are not actually selected into the final randomly-selected potential control samples used in the modelling;
    x_nsch_cat=(put(nschools,nschcat.))*1;
    if nschools=. then nschools=0;
	x_nzdep=nzdep;
	if nzdep in (., 99) then x_nzdep=99;
	x_region=reg;
	  if reg=. then x_region=99;
	x_tla=tla;
      if tla=. then x_tla=99;
    **Very few persons in this sample have a birth year of 1993;	
	*if cohort=1993 then cohort=1994;
	**Not going to use the ethmiss category;
  if ethmiss=1 then  ethoth=1;
  if genskills_only=. then genskills_only=0;
  if occskills_only=. then occskills_only=0;
  if gen_n_occ_skills=. then gen_n_occ_skills=0; 
run;

proc means data=modelb;
var X_any_mh;
run;



**Rename vars - get rid of Xs;
**Create alternative, more aggregated, versions of the age and region variables;

data &folder2..model_final(compress=y keep=snz_uid refmth dob dod YSstartdate strata 
	cohort age age2 female european maori pacific asian ethoth ethmiss 
    nzdep tla region region2 region3 decile x_schtype x_schauth
	hqual hothqual hterqual ncea_cr_l1-ncea_cr_l3 /*prop_cr_ext_cat*/
    x_ncea_cr_l1_cat x_ncea_cr_l2_cat x_ncea_cr_l3_cat nschools nsch_cat time_since_school age_when_left_sch
    sch48 emp48 tert48 neet48 ben48
    sch_last5 emp_last5 tert_last5 neet_last5 ben_last5
    sch_last6 emp_last6 tert_last6 neet_last6 ben_last6
	sch_prior12 emp_prior12 tert_prior12 neet_prior12 ben_prior12
	sch_first30 emp_first30 tert_first30 neet_first30 ben_first30
	genskills_only occskills_only gen_n_occ_skills 
	x_prop_onben_aschild_cat  ever_onben_aschild  x_child_bentype 
	x_prop_os_aschild_cat   
	mother_unqual cg_cust cg_comm cg_corr
	child_not child_yj_referral child_cyf_place child_yj_place child_any_fdgs_abuse any_mh
	sedu_da trua_da susp_da stand_da
	/*mths_sch_last18 mths_emp_last18 mths_tert_last18 mths_neet_last18 mths_ben_last18 mean_earn_last6*/
    X_child_not_cat X_child_yj_referral_cat X_susp_da_cat X_stand_da_cat
	rename=(x_schtype=schtype x_schauth=schauth X_prop_onben_aschild_cat=prop_onben_aschild_cat   
	    x_child_bentype=child_bentype x_prop_os_aschild_cat=prop_os_aschild_cat
	    x_ncea_cr_l1_cat=ncea_cr_l1_cat   x_ncea_cr_l2_cat=ncea_cr_l2_cat   x_ncea_cr_l3_cat=ncea_cr_l3_cat  
        X_child_not_cat=child_not_cat  X_child_yj_referral_cat=child_yj_referral_cat
        X_susp_da_cat=susp_da_cat X_stand_da_cat=stand_da_cat  ));
	set modelb;
	array old(*) x_nzdep x_region x_decile x_tla	    
	    x_hqual x_hothqual x_hterqual x_nsch_cat
        x_sch_last5 x_emp_last5 x_tert_last5 x_neet_last5 x_ben_last5
		x_sch_last6 x_emp_last6 x_tert_last6 x_neet_last6 x_ben_last6
		x_sch_prior12 x_emp_prior12 x_tert_prior12 x_neet_prior12 x_ben_prior12
		x_sch_first30 x_emp_first30 x_tert_first30 x_neet_first30 x_ben_first30
	    X_ever_onben_aschild X_any_mh
	    x_mother_unqual X_cg_cust X_cg_comm x_cg_corr
		X_child_not X_child_yj_referral X_child_cyf_place X_child_yj_place X_child_any_fdgs_abuse 
		x_sedu_da x_trua_da x_susp_da x_stand_da;
	array new(*) nzdep region decile tla   
	    hqual hothqual hterqual nsch_cat
        sch_last5 emp_last5 tert_last5 neet_last5 ben_last5
		sch_last6 emp_last6 tert_last6 neet_last6 ben_last6
		sch_prior12 emp_prior12 tert_prior12 neet_prior12 ben_prior12
		sch_first30 emp_first30 tert_first30 neet_first30 ben_first30
	    ever_onben_aschild  any_mh
	    mother_unqual cg_cust cg_comm cg_corr
		child_not child_yj_referral child_cyf_place child_yj_place child_any_fdgs_abuse 
		sedu_da trua_da susp_da stand_da;
	do i=1 to dim(old);
		new(i)=old(i);
	end;
	**Aggregating some region and age categories to assist with better exact matching;
	region2=region;
	if region in (5,6,7,8) then
		region2=10;
	*rest of north island;
	if region in (12,13,14,15,16,17,18,99) then
		region2=19;
	*rest of south island;
	region3=region;
	if region in (7,12,14,15,16,17,18,99) then
		region3=19;
	*Taranaki and rest of south island;
	age2=age;
	if age=15 then
		age2=16;
	if age=18 then
		age2=17;
    if time_since_school='   ' then time_since_school='hNA';
run;


proc freq data=&folder2..model_final;
tables age age2 /list missing;
run;

proc freq data=ys5.model_final_revised;
tables strata /list missing;
run;


proc freq data=&folder2..model_final;
tables
    refmth cohort  female age age2 european maori pacific asian ethoth ethmiss 
    nzdep tla region region2 region3 decile schtype schauth
	hqual hothqual hterqual ncea_cr_l1-ncea_cr_l3 /*prop_cr_ext_cat*/
    ncea_cr_l1_cat ncea_cr_l2_cat ncea_cr_l3_cat nschools nsch_cat time_since_school age_when_left_sch
    sch48 emp48 tert48 neet48 ben48
    sch_last5 emp_last5 tert_last5 neet_last5 ben_last5
    sch_last6 emp_last6 tert_last6 neet_last6 ben_last6
	sch_prior12 emp_prior12 tert_prior12 neet_prior12 ben_prior12
	sch_first30 emp_first30 tert_first30 neet_first30 ben_first30
	genskills_only occskills_only gen_n_occ_skills 
	prop_onben_aschild_cat  ever_onben_aschild  child_bentype 
	prop_os_aschild_cat   
	mother_unqual cg_cust cg_comm cg_corr
	child_not child_yj_referral child_cyf_place child_yj_place child_any_fdgs_abuse any_mh
	sedu_da trua_da susp_da stand_da
    child_not_cat child_yj_referral_cat susp_da_cat stand_da_cat  /list missing;
run;


/*

proc freq data=&folder2..model_final;
tables mother_unqual;
run;

proc freq data=ys5.model_final_revised;
tables mother_unqual /list missing;
run;
*/


****************************************************;
**PART D: Construct post-reference month OUTCOME VARIABLES for all records with reference months in 2012 and 2013;
**Using monthly data on enrolment status, employment, benefit receipt and NEET status
  and annual data on educational achievement;
*******************************************************;

**Mainly overseas in each month - use this dataset in the final analytical phase to censor months in which
  the person was mainly overseas;
 
%let m=82;
**Target vector length;
%let b=79;

data &folder2..mainly_overseas(compress=y drop=i windowstart_leed  avail_mths os_da_82-os_da_195  rename=(nos1-nos&b=os1-os&b ));          
  merge &folder2..models(in=a keep=snz_uid refmth YSstartdate ) &folder..mth_os(keep=snz_uid os_da_82-os_da_195); 
by snz_uid;
if a;
windowstart_leed=refmth-48;
avail_mths=&b;
    do i=1 to 12;
	  if refmth=(i+165) then avail_mths=(avail_mths-i);
      end;
    do i=1 to 12;
	  if refmth=(i+177) then avail_mths=(avail_mths-12-i);
      end;
array os(*) os_da_82-os_da_195;
array nos(*) nos1-nos&b;    
    do i=1 to avail_mths;
		nos(i)=os(i+(windowstart_leed-81)-1);
		if nos(i)>=25 then nos(i)=1; 
        else nos(i)=0; 
end;
run;


%let m=55;
%let n=61;
%let o=67;
%let p=73;
%let q=79;

*proc contents data=ys.model_final; 
*run;

data birth;
set &folder2..model_final(in=a keep=snz_uid refmth YSstartdate /*treat*/ strata dob );
if 161<=refmth<=177;
run;


data &folder2..depvars_1(compress=y keep=snz_uid refmth dob 
               emp_post6 emp_post12 emp_post18 emp_post24 emp_post30 
               study_post6 study_post12 study_post18 study_post24 study_post30
               neet_post6 neet_post12 neet_post18 neet_post24 neet_post30   
                eet_post6 eet_post12 eet_post18  eet_post24 eet_post30  
               ben_post6 ben_post12 ben_post18 ben_post24 ben_post30
               tert_post6 tert_post12 tert_post18 tert_post24 tert_post30
               sa_post6 sa_post12 sa_post18 sa_post24 sa_post30
               it_post6 it_post12 it_post18 it_post24 it_post30
               cust_in_followup  comm_in_followup 
               );
 merge birth(in=a keep=snz_uid refmth dob)
		&folder2..monthly(keep=snz_uid refmth 
               emp&m emp&n emp&o emp&p emp&q 
               study&m study&n study&o study&p study&q
               neet&m neet&n neet&o neet&p neet&q    
               eet&m eet&n eet&o  eet&p eet&q 
               ben&m ben&n ben&o ben&p ben&q 
               tert&m tert&n tert&o tert&p tert&q 
               sa&m sa&n sa&o sa&p sa&q
               it&m it&n it&o it&p it&q
                )
        &folder2..monthly_corrections(keep=snz_uid refmth cust50-cust67 comm50-comm67 );      
  by snz_uid refmth;
  if a; 
  array base(*) emp&m emp&n emp&o emp&p emp&q 
               study&m study&n study&o study&p study&q
               neet&m neet&n neet&o neet&p neet&q    
               eet&m eet&n eet&o  eet&p eet&q 
               ben&m ben&n ben&o ben&p ben&q 
               tert&m tert&n tert&o tert&p tert&q 
               sa&m sa&n sa&o sa&p sa&q
                it&m it&n it&o it&p it&q;
  array outcome(*) emp_post6 emp_post12 emp_post18 emp_post24 emp_post30 
               study_post6 study_post12 study_post18 study_post24 study_post30
               neet_post6 neet_post12 neet_post18 neet_post24 neet_post30   
                eet_post6 eet_post12 eet_post18  eet_post24 eet_post30  
               ben_post6 ben_post12 ben_post18 ben_post24 ben_post30
               tert_post6 tert_post12 tert_post18 tert_post24 tert_post30
               sa_post6 sa_post12 sa_post18 sa_post24 sa_post30
               it_post6 it_post12 it_post18 it_post24 it_post30;
  do i=1 to dim(base);
     outcome(i)=base(i);
      end; 
if sum(of cust50-cust67)>0 then cust_in_followup=1; else cust_in_followup=0;
if sum(of comm50-comm67)>0 then comm_in_followup=1; else comm_in_followup=0;
run;

/*
proc means data=ys5.depvars_revised_1;
run; 
proc means data=&folder2..depvars_1;
run; 
*/


data &folder2..depvars_2(compress=y drop=i j sch_enr_id_106-sch_enr_id_189 ter_enr_id_106-ter_enr_id_189 
	 emp106-emp189 ben_id_106-ben_id_195 os_da_106-os_da_189
             hncea_2009-hncea_2014 hnonncea_2009-hnonncea_2014
             hterqual_2009-hterqual_2014  hqual_2009-hqual_2014
			 cust_id_106-cust_id_195 
             emp_3mths_post  sch_3mths_post tert_3mths_post os_3mths_post);
	merge birth(in=a keep=snz_uid refmth dob)
		&folder..mth_sch_enrol(keep=snz_uid sch_enr_id_106-sch_enr_id_189)
		&folder..mth_ter_enrol(keep=snz_uid ter_enr_id_106-ter_enr_id_189)
		&folder..mth_emp(keep=snz_uid emp106-emp189) 
		&folder..mth_os(keep=snz_uid os_da_106-os_da_189) 
		&folder..mth_ben(keep=snz_uid ben_id_106-ben_id_195)	
		&folder..highest_quals_achieved(keep=snz_uid hncea_2009-hncea_2014 hnonncea_2009-hnonncea_2014
             hterqual_2009-hterqual_2014 hqual_2009-hqual_2014)
        &folder..mth_corrections(keep=snz_uid cust_id_106-cust_id_195 ); 
	by snz_uid;
	if a; 
	array highqual(*) hncea_2009-hncea_2014 hnonncea_2009-hnonncea_2014
             hterqual_2009-hterqual_2014  hqual_2009-hqual_2014;    
	do i=1 to dim(highqual);
		if highqual(i)=. then highqual(i)=0;
	   end;
	bday_18=intnx('YEAR',dob,18,'S');
	format bday_18 date9.;
	year_18=year(bday_18);
	refmth_18=(12*(year(bday_18)-1999) + (month(bday_18) -3));
	refmth_17_75=refmth_18-3;
**Highest qualifications completed, ordered by reference year where 0 =year before YS participation
	  and 1= year of first YS participation;
array old(4,6) hncea_2009-hncea_2014 hnonncea_2009-hnonncea_2014
             hterqual_2009-hterqual_2014 hqual_2009-hqual_2014;
array new(4,4) hncea_y0-hncea_y3 hnonncea_y0-hnonncea_y3
             hterqual_y0-hterqual_y3 hqual_y0-hqual_y3;
if refmth<166 then do j=1 to 4;
             do i=1 to 4;
           new(j,i)=old(j,i+2);
           end;
		   end;
else if refmth>=166 then do j=1 to 4;
             do i=1 to 3;
           new(j,i)=old(j,i+3);
           end;
		   end;
**Indicators capturing whether person has a level 1 qualification at each time point, based on the above;
array newind(4,4) i1hncea_y0-i1hncea_y3 i1hnonncea_y0-i1hnonncea_y3
             i1hterqual_y0-i1hterqual_y3 i1hqual_y0-i1hqual_y3;
    do j=1 to 4;
	do i=1 to 4;
	   if new(j,i)>=1 then newind(j,i)=1; 
       else if new(j,i)~=. then newind(j,i)=0;
     end;
	 end;
**Indicators capturing whether person has a level 2 qualification at each time point, based on the above;
array newindb(4,4) i2hncea_y0-i2hncea_y3 i2hnonncea_y0-i2hnonncea_y3
             i2hterqual_y0-i2hterqual_y3 i2hqual_y0-i2hqual_y3;
    do j=1 to 4;
	do i=1 to 4;
	   if new(j,i)>=2 then newindb(j,i)=1; 
       else if new(j,i)~=. then newindb(j,i)=0;
     end;
	 end;
**Indicators capturing whether person has a level 3 or higher qualification at each time point, based on the above;
array newindc(4,4) i3hncea_y0-i3hncea_y3 i3hnonncea_y0-i3hnonncea_y3
             i3hterqual_y0-i3hterqual_y3 i3hqual_y0-i3hqual_y3;
    do j=1 to 4;
	do i=1 to 4;
	   if new(j,i)>=3 then newindc(j,i)=1; 
       else if new(j,i)~=. then newindc(j,i)=0;
     end;
	 end;
**Create a marker variable to identify people who turned 18 at least 3 months after
	 they enrolled in YS for the first time and have educ outcome data available;
	if refmth<refmth_17_75 and year_18<=2014 then
		lev2_yr18_avail=1;
	    else lev2_yr18_avail=0;

*Highest qualification attained by end of year person turned 18;
**Attainment data ends in 2014 so we can't observe this outcome for everyone yet;
	if lev2_yr18_avail=1 then
		do;
			lev2_yr18=0;
		end;
    array hqual2(*) hqual_2009-hqual_2014;
	do i=1 to dim(hqual2);
		if i+2008=year_18 and hqual2(i)>=2 and lev2_yr18_avail=1 then
			lev2_yr18=1;
	end;
**Create a BEN variable providing an indicator for receiving a benefit in any of the first 3 months after 18th birthday;
**for people who enrolled in YS at least 3 months before turning 18
	   and have benefit data for at least some of the 3 months after they
	    turned 18;
	if refmth<refmth_17_75 and (refmth_18+1<=195) then
		do;
			ben_3mths_aft_18=0;
			cust_3mths_aft_18=0;
			*comm_3mths_aft_18=0;
		end;
	array ben(*) ben_id_106-ben_id_195;
   	array cust(*) cust_id_106-cust_id_195;
	do i=1 to dim(ben);
		if refmth<refmth_17_75 and (refmth_18+1<=195)  
			and (i+105=(refmth_18+1) or i+105=(refmth_18+2) or i+105=(refmth_18+3)) 
			and ben(i)=1 then
			do;
				ben_3mths_aft_18=1;
			end;
        if refmth<refmth_17_75 and (refmth_18+1<=195)  
			and (i+105=(refmth_18+1) or i+105=(refmth_18+2) or i+105=(refmth_18+3)) 
			and cust(i)=1 then
			do;
				cust_3mths_aft_18=1;
			end;    
    end;
	**Create a NEET variable to capture being NEET for ALL of the first 3 months after the month of turning 18,
	 for people who enrolled in YS at least 3 months before turning 18, and also have enrolment data for the 3 months after they
	    turned 18;
	if refmth<refmth_17_75 and (refmth_18+3<=189) then
		do;
			neet_3mths_aft_18=0;
		end;

	emp_3mths_post=0;
	sch_3mths_post=0;
	tert_3mths_post=0;
	os_3mths_post=0;
	array ter(*) ter_enr_id_106-ter_enr_id_189;
	array sch(*) sch_enr_id_106-sch_enr_id_189;
	array emp(*) emp106-emp189;
    array os(*)  os_da_106-os_da_189;
	do i=1 to dim(emp);
		if (i+105=(refmth_18+1) or i+105=(refmth_18+2) or i+105=(refmth_18+3)) 
			and (refmth_18+3<=189) then
			do;
				if ter(i)=1 then
					tert_3mths_post+1;

				if sch(i)=1 then
					sch_3mths_post+1;

				if emp(i)=1 then
					emp_3mths_post+1;
                if os(i)>=7 then os_3mths_post+1; 
			end;
	end;
	if emp_3mths_post=0 and sch_3mths_post=0 and tert_3mths_post=0 and os_3mths_post=0 and neet_3mths_aft_18~=. then
		do;
			neet_3mths_aft_18=1;
		end;
run;

proc contents data=&folder2..depvars_2;
run;
/*
proc means data=ys5.depvars_revised_2;
run; 
proc means data=&folder2..depvars_2;
run; 
*/


***Additional earnings variables for individuals with W&S earnings both before (in the prior 6 months)
   and after (in the post 13-18 months) their YS enrolment;

data pre_post_earnings(drop=earn43-earn67 emp43-emp67);
set &folder2..monthly(keep=snz_uid refmth earn43-earn67 emp43-emp67 );
if sum(of earn43-earn48)>0 and sum(of emp43-emp48)>0 then do;
   mean_mthly_earn_prior6=mean(of earn43-earn48);
   mths_emp_prior6=sum(of emp43-emp48);
   tot_pre_earn=sum(of earn43-earn48);
   mean_mthly_earn_prior6b=tot_pre_earn/mths_emp_prior6;
   end;
if sum(of earn62-earn67)>0 and sum(of emp62-emp67)>0 then do;
   mean_mthly_earn_post=mean(of earn62-earn67);
   mths_emp_final6=sum(of emp62-emp67);
   tot_post_earn=sum(of earn62-earn67);
   mean_mthly_earn_postb=tot_post_earn/mths_emp_final6;
   end;
if mean_mthly_earn_prior6>0 and mean_mthly_earn_post>0 then do;
   chg_mean_earn=mean_mthly_earn_post-mean_mthly_earn_prior6;
   rel_chg_mean_earn=((mean_mthly_earn_post-mean_mthly_earn_prior6)/mean_mthly_earn_prior6)*100;
   end;
run;

proc means data=pre_post_earnings;
run;

proc print data=pre_post_earnings(obs= 20);
var mean_mthly_earn_prior6  mean_mthly_earn_post chg_mean_earn  rel_chg_mean_earn   ;
where mean_mthly_earn_prior6>0 and mean_mthly_earn_post>0;
run;

data &folder2..depvars_mthly_earn(keep=snz_uid refmth chg_mean_earn  mean_mthly_earn_prior6  mean_mthly_earn_post
     rel_chg_mean_earn);
set pre_post_earnings;
if chg_mean_earn~=.;
run;

/*
proc means data=ys5.depvars_mthly_earn;
run; 
proc means data=&folder2..depvars_mthly_earn;
run; 
*/


proc contents data=&folder2..depvars_mthly_earn;
run;





***********************************************************************************************;
**PART E: MEASURES CAPTURING YS-NEET and YS-YP/YPP PARTICIPATION;
***Also create an indicator of having participated in YTS, the predecessor program, for at least 30 days; 
***************************************************************************************;


**Select all who FIRST enrolled in YS-NEET (not first in YP or YPP) and remained enrolled in YS NEET for at least 3 months during their first year;

data YS(keep=snz_uid provider prog startdate enddate refmth) ;
  set yst.yst_spells;
  startdate  =input(compress(yst_spl_participation_start_date,"-"),yymmdd8.) ;
  enddate    =input(compress(yst_spl_participation_end_date,"-"),yymmdd8.) ;
  provider= yst_spl_provider_name_text;
  if yst_spl_programme_name_text='NOT IN EDUCATION, EMPLOYMENT OR TRAINING' then prog='Neet';
  if yst_spl_programme_name_text='YOUTH PAYMENT' then prog='YP';
  if yst_spl_programme_name_text='YOUNG PARENT PAYMENT' then prog='YPP';
  where yst_spl_programme_name_text in ('NOT IN EDUCATION, EMPLOYMENT OR TRAINING', 
     'YOUTH PAYMENT', 'YOUNG PARENT PAYMENT');
  *startyr=year(startdate);
  format startdate enddate date9.;
  refmth=(12*(year(startdate) -1999) + (month(startdate) -3));
run;

**One record per person for those who first enrolled in YS-NEET;

proc sort data=YS;
by snz_uid startdate;
run;

%overlap(YS);

data neetfirst ;
set YS_or ;
by snz_uid startdate;
if first.snz_uid and prog='Neet' and '01Aug2012'd<=startdate<='31Dec2013'd;
run;

proc freq data=neetfirst;
tables refmth /list missing;
run;



**select those who were enrolled in any type of YS for at least 90 days in their first year;
data firstyr;
merge neetfirst(in=a keep= snz_uid startdate rename=(startdate=YSstartdate))  YS_or;
by snz_uid;
if a;
windowstart=YSstartdate;
windowend=YSstartdate+365;
format windowstart windowend date9.;
run;

proc sort data=firstyr;
by snz_uid startdate; 
run;

data count(keep=snz_uid daysfirstyr);
set firstyr;
by snz_uid startdate;
retain daysfirstyr; 
if first.snz_uid then do;
       daysfirstyr=0; end;
   if startdate<windowstart and enddate>=windowstart then startdate=windowstart;
   if enddate>windowend and startdate<=windowend then enddate=windowend;
   ***only count days that fall within the obs window;
   if startdate<=windowend and enddate>=windowstart then do;
       days=enddate - startdate +1;
       daysfirstyr=daysfirstyr+days;  end;
if last.snz_uid then output;
run;

**Restrict to those who were enrolled for at least 90 days in their first year ;
**who first enrolled in 2012 or 2013;

data study1(drop=daysfirstyr);
merge neetfirst(in=a keep=snz_uid provider startdate refmth rename=(startdate=YSstartdate)) count /*other_YS(in=b)*/ ; 
by snz_uid;
if a and daysfirstyr>=90;
run;


data study1b(drop=daysfirstyr);
merge neetfirst(in=a keep=snz_uid provider startdate refmth rename=(startdate=YSstartdate)) count /*other_YS(in=b)*/ ; 
by snz_uid;
if a and daysfirstyr>=1;
run;


**Vector  of calendar month YS participation;


data neet_spells;
merge neetfirst(in=a keep=snz_uid startdate rename=(startdate=YSstartdate)) YS_OR;
by snz_uid;
if a and prog='Neet';
run;

%let start='01Jan2008'd;
%let first=106; *Jan2008;
%let last=199;  *Oct 2015 - but the data for this month appears incomplete;

data neet_month_temp;
	set neet_spells;
	format start_window end_window date9.;
	array ys_id_(*) ys_id_&first-ys_id_&last;
	array ys_da_(*) ys_da_&first-ys_da_&last;
	do ind=&first to &last;
		i=ind-&first+1;
		ys_id_(i)=0;
		ys_da_(i)=0;

		* overwriting start and end window as interval equal to one month;
		start_window=intnx("month",&start.,i-1,"beginning");
		* start is beg of the month;
		end_window=intnx("month",&start.,i-1,"end");
		* end is end of the month;
		if not((startdate > end_window) or (enddate < start_window)) then
			do;
				ys_id_(i)=1;
				* creating inidcator of school enrolment;
				* measuring the days enrolled;
				if (startdate <= start_window) and  (enddate > end_window) then
					days=(end_window-start_window)+1;
				else if (startdate <= start_window) and  (enddate <= end_window) then
					days=(enddate-start_window)+1;
				else if (startdate > start_window) and  (enddate <= end_window) then
					days=(enddate-startdate)+1;
				else if (startdate > start_window) and  (enddate > end_window) then
					days=(end_window-startdate)+1;
				ys_da_[i]=days*ys_id_(i);
			end;
	end;
run;

proc summary data=neet_month_temp  nway;
	class snz_uid ;
	var ys_id_&first-ys_id_&last ys_da_&first-ys_da_&last;
	output out=TEMP(drop=_:) sum=;
run;

proc means data=temp ;
run;

data temp2;
set temp;
array ys(*) ys_id_&first-ys_id_&last;
do i=1 to dim(ys);
  if ys(i)>=1 then ys(i)=1;
  else ys(i)=0;
  end;
run;

proc means data=temp2;
run;

*Create std monthly vectors of YS-NEET in relation to the assigned refmth;

%let b=79;

data ysneet_monthly(drop=i windowstart_leed ys_id_&first-ys_id_&last avail_mths);
merge study1(in=a) temp2(keep=snz_uid ys_id_&first-ys_id_&last);
by snz_uid;
if a;
    avail_mths=&b;
    do i=1 to 12;
	  if refmth=(i+165) then avail_mths=(avail_mths-i);
      end;
    do i=1 to 12;
	  if refmth=(i+177) then avail_mths=(avail_mths-12-i);
      end;
	windowstart_leed=refmth-48;
	array ys_id_(*) ys_id_&first-ys_id_&last;
	array nys(*) ys1-ys&b;
	do i=1 to avail_mths;
		nys(i)=ys_id_(i+(windowstart_leed-105)-1);
		if nys(i)=. then
			nys(i)=0;		
	end;
run;

**Same dataset but including all durations of YS enrolment;
/*
data ysneet_monthlyb(drop=i windowstart_leed ys_id_&first-ys_id_&last avail_mths);
merge study1b(in=a) temp2(keep=snz_uid ys_id_&first-ys_id_&last);
by snz_uid;
if a;
    avail_mths=&b;
    do i=1 to 12;
	  if refmth=(i+165) then avail_mths=(avail_mths-i);
      end;
    do i=1 to 12;
	  if refmth=(i+177) then avail_mths=(avail_mths-12-i);
      end;
	windowstart_leed=refmth-48;
	array ys_id_(*) ys_id_&first-ys_id_&last;
	array nys(*) ys1-ys&b;
	do i=1 to avail_mths;
		nys(i)=ys_id_(i+(windowstart_leed-105)-1);
		if nys(i)=. then
			nys(i)=0;		
	end;
run;
*/

**Calculate total days on any YS in the first 18 months, for the analysis of impacts by duration in YS programme;

data durn;
merge study1b(in=a keep=snz_uid YSstartdate)  YS_or;
by snz_uid;
if a;
windowstart=YSstartdate;
windowend=YSstartdate+365+182;
format windowstart windowend date9.;
run;

proc sort data=durn;
by snz_uid startdate;
run;

data durn2(keep=snz_uid days_first_18mths);
set durn;
by snz_uid startdate;
retain days_first_18mths ; 
if first.snz_uid then do;
days_first_18mths=0; end;
   if startdate<windowstart and enddate>=windowstart then startdate=windowstart;
   if enddate>windowend and startdate<=windowend then enddate=windowend;
   ***only count days overseas that fall within the obs window;
   if startdate<=windowend and enddate>=windowstart then do;
       days=enddate - startdate +1;
       days_first_18mths=days_first_18mths+days;  end;
if last.snz_uid then output;
run;

**Find first and last YS enrolment dates, counting all types of YS; 

data YS_first(keep=snz_uid startdate rename=(startdate=first_YS_enrol_date));
set YS_or;
by snz_uid startdate;
if first.snz_uid ;
run;

proc sort data=YS_or;
by snz_uid enddate;
run;

data YS_last(keep=snz_uid enddate rename=(enddate=last_YS_enrol_date));
set YS_or;
by snz_uid enddate;
if last.snz_uid ;
run;

**Bring all measures together;
***&folder2.ysneet_monthly defines the main study population, &folder2.ysneet_monthlyb defines the expanded study population with people who
   were enrolled for less than 90 days;

proc contents data=durn2;
run;


data &folder2..ysneet_monthly;
merge ysneet_monthly(in=a) durn2 YS_first YS_last ;
by snz_uid;
if a;
if days_first_18mths<90 then durn_gp=0;
else if 90<=days_first_18mths<182 then durn_gp=1;
else if days_first_18mths<365 then durn_gp=2;
else  durn_gp=3;
run;


**Same dataset for the expanded study population;
/*
data  &folder2.ysneet_monthlyb;
merge ysneet_monthlyb(in=a)  durn2 YS_first YS_last ;
by snz_uid;
if a;
if days_first_18mths<90 then durn_gp=0;
else if 90<=days_first_18mths<182 then durn_gp=1;
else if days_first_18mths<365 then durn_gp=2;
else  durn_gp=3;
*if durn_gp=. then durn_gp=0;
run;
*/

proc freq data=&folder2..ysneet_monthly;
tables durn_gp / missing;
run;

/*
proc freq data=&folder2..ysneet_monthlyb;
tables durn_gp / missing;
run;
*/


**Vector of participation in YP or YPP ;
**I have chosen to eliminate spell overlaps before constructing these measures of YP/YPP participation, which
  leads to slightly lower counts of YP/YPP participants and slightly less time in YP/YPP;

data YS(keep=snz_uid provider prog startdate enddate refmth) ;
  set yst.yst_spells;
  startdate  =input(compress(yst_spl_participation_start_date,"-"),yymmdd8.) ;
  enddate    =input(compress(yst_spl_participation_end_date,"-"),yymmdd8.) ;
  provider= yst_spl_provider_name_text;
  if yst_spl_programme_name_text='NOT IN EDUCATION, EMPLOYMENT OR TRAINING' then prog='Neet';
  if yst_spl_programme_name_text='YOUTH PAYMENT' then prog='YP';
  if yst_spl_programme_name_text='YOUNG PARENT PAYMENT' then prog='YPP';
  where yst_spl_programme_name_text in (/*'NOT IN EDUCATION, EMPLOYMENT OR TRAINING',*/ 
     'YOUTH PAYMENT', 'YOUNG PARENT PAYMENT');
  format startdate enddate date9.;
  refmth=(12*(year(startdate) -1999) + (month(startdate) -3));
run;

proc sort data=YS;
by snz_uid startdate;
run;

%overlap(YS);

data YP_spells;
set YS_or;
if prog in ('YP', 'YPP');
run;

%let start='01Jan2008'd;
%let first=106; *Jan2008;
%let last=199;  *Oct 2015 - but the data for this month appears incomplete;

data yp_month_temp;
	set yp_spells;
	format start_window end_window date9.;
	array ys_id_(*) ys_id_&first-ys_id_&last;
	array ys_da_(*) ys_da_&first-ys_da_&last;
	do ind=&first to &last;
		i=ind-&first+1;
		ys_id_(i)=0;
		ys_da_(i)=0;

		* overwriting start and end window as interval equal to one month;
		start_window=intnx("month",&start.,i-1,"beginning");
		* start is beg of the month;
		end_window=intnx("month",&start.,i-1,"end");
		* end is end of the month;
		if not((startdate > end_window) or (enddate < start_window)) then
			do;
				ys_id_(i)=1;
				* creating inidcator of school enrolment;
				* measuring the days enrolled;
				if (startdate <= start_window) and  (enddate > end_window) then
					days=(end_window-start_window)+1;
				else if (startdate <= start_window) and  (enddate <= end_window) then
					days=(enddate-start_window)+1;
				else if (startdate > start_window) and  (enddate <= end_window) then
					days=(enddate-startdate)+1;
				else if (startdate > start_window) and  (enddate > end_window) then
					days=(end_window-startdate)+1;
				ys_da_[i]=days*ys_id_(i);
			end;
	end;
run;

proc summary data=yp_month_temp  nway;
	class snz_uid ;
	var ys_id_&first-ys_id_&last ys_da_&first-ys_da_&last;
	output out=TEMP_yp(drop=_:) sum=;
run;

data temp_yp2;
set temp_yp;
array ys(*) ys_id_&first-ys_id_&last;
do i=1 to dim(ys);
  if ys(i)>=1 then ys(i)=1;
  else ys(i)=0;
  end;
run;

*Create std monthly vectors of YP/YPP for everyone in the total dataset who had YP or YPP participation;
%let b=79;

data &folder2..yp_monthly(drop=i windowstart_leed ys_id_&first-ys_id_&last avail_mths );
merge &folder2..models(in=a keep=snz_uid refmth) temp_yp2(keep=snz_uid ys_id_&first-ys_id_&last);
by snz_uid ;
if a;
    avail_mths=&b;
    do i=1 to 12;
	  if refmth=(i+165) then avail_mths=(avail_mths-i);
      end;
    do i=1 to 12;
	  if refmth=(i+177) then avail_mths=(avail_mths-12-i);
      end;
	windowstart_leed=refmth-48;
	array ys_id_(*) ys_id_&first-ys_id_&last;
	array nys(*) yp1-yp&b;
	do i=1 to avail_mths;
		nys(i)=ys_id_(i+(windowstart_leed-105)-1);
		if nys(i)=. then
			nys(i)=0;		
	end;
run;


***CREATE INDICATOR OF PARTICPATING IN YTS (Youth Transition Scheme) FOR AT LEAST 30 DAYS;

data YTS(keep=snz_uid refmth provider prog startdate enddate ) ;
  set yst.yst_spells;
  if yst_spl_programme_name_text='YOUTH TRANSITION SERVICE';
  startdate  =input(compress(yst_spl_participation_start_date,"-"),yymmdd8.) ;
  enddate    =input(compress(yst_spl_participation_end_date,"-"),yymmdd8.) ;
  provider= yst_spl_provider_name_text;
  prog='YTS';
  format startdate enddate date9.;
  refmth=(12*(year(startdate) -1999) + (month(startdate) -3));
run;

**One record per person for those who first enrolled in YS-NEET, and tot days enrolled in first 12 months;

proc sort data=YTS;
by snz_uid startdate;
run;

%OVERLAP (YTS);

proc sort data=YTS_or;
by snz_uid startdate;
run;

data ytsfirst ;
set YTS_or;
by snz_uid startdate;
if first.snz_uid;
run;

data firstyr_yts;
merge ytsfirst(in=a keep=snz_uid startdate rename=(startdate=YTSstartdate))  YTS_or;
by snz_uid;
if a;
windowstart=YTSstartdate;
windowend='31Dec2012'd;
format windowstart windowend date9.;
run;

proc sort data=firstyr_yts;
by snz_uid startdate; 
run;

data &folder2..prev_YTS(keep=snz_uid daystot);
set firstyr_yts;
by snz_uid startdate;
retain daystot; 
if first.snz_uid then do;
       daystot=0; end;
   if startdate<windowstart and enddate>=windowstart then startdate=windowstart;
   if enddate>windowend and startdate<=windowend then enddate=windowend;
   ***only count days that fall within the obs window;
   if startdate<=windowend and enddate>=windowstart then do;
       days=enddate - startdate +1;
       daystot=daystot+days;  
       end;
if last.snz_uid and daystot>=30 then output;
run;




**************************************************************************************;
**PART F:  APPLY FURTHER POPULATION RESTRICTIONS TO BE USED IN THE MAIN IMPACT ANALYSIS;
******************************************************************************;

**Retain those with missing enddates for their last school enrolment spell if the total enrolment period is
   less that 5 years in duration, but exclude if the final spell was 5+ years in duration ;
**This step was taken because some school enrolment spells without enddates are implausibly long and lead to children 
  appearing to be still at school when they are in their early 20s;
**However it's wrong because I'd forgotten about composite schools and year 7-15 secondary schools, and 
  most of the records I exclude turn out to be students at those two types of schools; 
**So I'm excluding far more people than should be excluded with this restriction - 33 persons from the final study pop, more than that from the
  the potential control sample; 

**Exclude if overseas for more than 6 months in the study period; 
**Append on my measure of prior YTS particpation;
**Exclude if attended a secondary school offering non-NCEA quals at ages 15 or above;
**Exclude those who were on a benefit in the reference month (the first YS-NEET start month) - they should have started on YP/YPP;

data drop(keep=snz_uid refmth);
set &folder2..last_sch_rec;
if sch_enddate_imputed=1 and (enddate-startdate)>(365*5);
run;
proc sort data=drop;
by snz_uid refmth;
run;

**These records are defined by snz_uid and refmth;

data first;
merge &folder2..model_final(in=a keep=snz_uid refmth) &folder2..overseas_6mthsplus(in=b keep=snz_uid refmth) /*drop(in=c) */;
by snz_uid refmth;
if a and not b /*and not c*/;
run;

**These records are defined by snz_uid only;

data second(compress=y  );
merge first(in=a ) &folder2..kids_nonnqf(in=b) &folder2..prev_YTS(in=c keep=snz_uid ) ; 
by snz_uid ;
if a and not b ;
if c then YTS=1; else YTS=0;
*if c and daystat>=1 then anyYTS=1; *else anyYTS=0;
run;

**Finally exclude those who were on a benefit in the reference month (the YS-NEET start month) - they should have started on YP/YPP;

data &folder2..model_final2R(compress=y drop=benrefmth);
merge second(in=a) 
    &folder2..monthly(keep=snz_uid refmth ben49 rename=(ben49=benrefmth)) 
    &folder2..model_final;
by snz_uid refmth;
if a and benrefmth~=1;
run;

proc means data=&folder2..model_final2R;
run;


***************************************************;
**PART G:  SELECT THE STUDY POPULATION AND POTENTIAL CONTROLS FOR THE MATCHING MODELS;
**Identify the YS participants we want to include in our study population, the YS participants
  we want to exclude from the analysis altogether, and the rest;
**Aggregating a few variable codes where there are too few people with a particular code in the treatment group;
*****************************************************;

**One record for each study population member;
**Get the study population and its data;

proc contents data=&folder2..ysneet_monthly;
run;

data studypopn ; 
merge &folder2..model_final2R(in=a)
      &folder2..ysneet_monthly(in=b keep=snz_uid refmth YSstartdate rename=(YSstartdate=MSDstartdate));
by snz_uid refmth;
if a and b;
run;



**Control group should exclude all YS-NEET participants and people who participated in YP or YPP before the reference month;

data YSneetall(keep=snz_uid provider prog startdate enddate refmth) ;
  set yst.yst_spells;
  startdate  =input(compress(yst_spl_participation_start_date,"-"),yymmdd8.) ;
  enddate    =input(compress(yst_spl_participation_end_date,"-"),yymmdd8.) ;
  provider= yst_spl_provider_name_text;
  if yst_spl_programme_name_text='NOT IN EDUCATION, EMPLOYMENT OR TRAINING' then prog='Neet';
  where yst_spl_programme_name_text in ('NOT IN EDUCATION, EMPLOYMENT OR TRAINING');
  *startyr=year(startdate);
  format startdate enddate date9.;
  refmth=(12*(year(startdate) -1999) + (month(startdate) -3));
run;

**One record per person for those who first enrolled in YS-NEET, and tot days enrolled in first 12 months;

proc sort data=YSneetall;
by snz_uid startdate;
run;

data YSneetall_persons ;
set YSneetall;
by snz_uid startdate;
if first.snz_uid ;
run;

**Drop records of people who became participants in  YS-NEET ;
**Convert age variable to an integer as it is used in matching;

data controla;
  merge &folder2..model_final2R(in=a) 
        YSneetall_persons(in=b keep=snz_uid );
  by snz_uid;
  if a and not b and 161<=refmth<=177 ; 
run;

data controlb(drop=yp1-yp48);
merge controla(in=a) &folder2..yp_monthly(keep=snz_uid refmth yp1-yp48);
by snz_uid refmth;
if a and sum(of yp1-yp48)<1;
run;

data model;
	set studypopn(in=a) controlb;	
	if a then treat=1;
	else treat=0;
	*age=int(age);	
run;

proc freq data=model;
tables treat /list missing;
run;

proc sort data=model out=&folder2..regressionsR;  **previously regressions_addrecs;
	by snz_uid refmth;
run;


/*
proc freq data=ys5.regressions_addrecs;
tables strata*treat /list missing;
run;

proc freq data=&folder2..regressions;
tables strata*treat /list missing;
run;

proc freq data=&folder2..regressionsR;
tables strata*treat /list missing;
run;

proc freq data=ys5.regressions_addrecs;
tables mother_unqual /list missing;
run;
proc freq data=&folder2..regressions;
tables mother_unqual;
run;
*/


**Divide sample and potential controls into the three strata for separate modelling;

data &folder2..allcases_1R(compress=y);
	set &folder2..regressionsR;
	if strata=1;
	if cohort=1993 then cohort=1994;
	if cohort=1998 then cohort=1997;
	*if hqual=3 then
		hqual=2;
	if ben_last6 in (2,3) then
		ben_last6=2;
	if ben_prior12 in (2,3,4) then
		ben_prior12=2;
	if ncea_cr_l3 in (4, 5) then
		ncea_cr_l3=3;
run;

data &folder2..allcases_2R(compress=y);
	set &folder2..regressionsR;
	if strata=2;
	if cohort=1998 then cohort=1997;
    if time_since_school='g>3yrs' then time_since_school='f2-3yrs';
	if ncea_cr_l3 in (4, 5) then
		ncea_cr_l3=3;   
	if ben_last6 in (2,3) then
		ben_last6=2;
	if ben_prior12 in (2,3,4) then
		ben_prior12=2;
  
	if sch_first30 in (1,2,3) then
		sch_first30=3;
	if neet_first30 in (3,4,5) then
		neet_first30=3;
	if prop_os_aschild_cat ='d75%+ ' then
		prop_os_aschild_cat='c50-<75%';
run;


data &folder2..allcases_3R(compress=y);
	set &folder2..regressionsR;
	if strata=3;
	if cohort=1998 then cohort=1997;
	if time_since_school='g>3yrs' then time_since_school='f2-3yrs';
    if ben_prior12 in (2,3,4) then
		ben_prior12=2;
	if sch_first30 in (1,2) then
		sch_first30=2;
	if neet_first30=5 then
		neet_first30=4;
run;


**Select a random sub-set of the potential controls, for model estimation;

%macro select(strata);
	data controls;
		set &folder2..allcases_&strata.R;
		if treat=0;
	run;

	data study;
		set &folder2..allcases_&strata.R;
		if treat=1;
	run;

	proc surveyselect data=controls method=srs reps=1
		sampsize=10000 seed=12345 out=random1;
	run;

	data &folder2..model_data_&strata.R(compress=y);
		set study random1;
	run;

%mend select;

%select(1);
%select(2);
%select(3);



proc means data=&folder2..model_data_2;
run;

















