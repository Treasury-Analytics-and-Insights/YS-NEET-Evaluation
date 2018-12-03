/* 1b_Create annual indicator datasets */

**Datasets are created here that are subsequently used in the program 
    "2_ Create treatment and control and outcome datasets" for the YS-NEET impact evaluation;
**The sample (or study population) is generated in "1a_Create monthly indicator datasets";

**The code in this program was largely compiled by Sarah Tumen in 2015, for general A&I purposes ;
**As of 2017, there are more up-to-date versions that could be used, in some cases incorporating minor improvements;


****************************************************************;
*****SECTIONS OF THIS PROGRAM - EACH RUNS INDEPENDENTLY;

**A. Nbr school enrolment days in each calendar year;
**B. CYF interactions BY YEAR OF AGE ;
**C. BENEFIT RECEIPT AS CHILD BY YEAR OF AGE ;
**D. PARENTAL CORRECTIONS HISTORY;
**E. MATERNAL EDUCATION;
**F. INTERVENTIONS AT SCHOOL - Truancy, SUSPENSIONS ETC BY YEAR OF AGE;
**G. DAYS OVERSEAS AT EACH YEAR OF AGE;
**H. USE OF MENTAL HEALTH SERVICES AT EACH YEAR OF AGE;
*****************************************************************;


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
libname sandmoh ODBC dsn=idi_sandpit_srvprd schema="clean_read_moh_PRIMHD";

libname Project "\\wprdfs08\TreasuryData\MAA2013-16 Citizen pathways through human services\Social Investment_2016\1_Indicator_at_age_datasets\dataset_rerun_24022016";
libname basedata "\\wprdSAS10\TreasuryData\MAA2013-16 Citizen pathways through human services\Sylvia\Youth Service\Data 2017 revisions\Base datasets";
libname suppdata "\\wprdSAS10\TreasuryData\MAA2013-16 Citizen pathways through human services\Sylvia\Youth Service\Data 2017 revisions\Supp datasets";;
libname studdata "\\wprdSAS10\TreasuryData\MAA2013-16 Citizen pathways through human services\Sylvia\Youth Service\Data 2017 revisions\Impact evaluation";

**Macro variables to enable the project file locations to be easily changed;

%let folder = basedata ;   **specify the location of the base files created in the prev program;
%let folder2 = studdata;   **specify the location to save files created in this program;

**Include other standard A&I programs that will be called in the course of this program;

%let path=\\wprdsas10\TreasuryData\MAA2013-16 Citizen pathways through human services\sylvia\Youth Service\Programs to be shared;
%include "&path.\Stand_macro_new.sas";
%include "&path.\FORMATS_new.sas";

%let population=&folder..popn;


***************************************************;
**A. Enrolment days in each calendar year - only ran these for years from 2005 onwards to cover
  secondary school years for my population of interest;
****************************************************;

%let sensor=31Dec2015; * Global censor data cut of date;
%let first_anal_yr=2005; /*1988*/* first calendar year of analysis, first year of birth for popualtion of interest;
%let last_anal_yr=2015;* Last calendar year of analysis;


%macro creating_clean_sch_enrol;
data student_enrol;
	set moe.student_enrol;
	format startdate enddate ExtractionDate date9.;
	startdate=input(compress(moe_esi_start_date,"-"),yymmdd10.);
	enddate=input(compress(moe_esi_end_date,"-"),yymmdd10.);
	ExtractionDate=input(compress(moe_esi_extrtn_date,"-"),yymmdd10.);
	schoolnumber=moe_esi_provider_code*1;
run;

proc sql;
	create table sch_enrol_clean
		as select distinct 
			a.snz_uid
			,a.snz_moe_uid
			,a.startdate
			,a.enddate
			,a.schoolnumber
			,a.ExtractionDate
			,b.DOB
			,year(b.DOB)+19 as year19_birth
		from student_enrol a inner join &population b
		on a.snz_uid=b.snz_uid
				order by snz_uid;
quit;

data sch_enrol_clean; set sch_enrol_clean;
* imputing enddates for those who are over 19;
if enddate=. and ExtractionDate>intnx('YEAR',DOB,19,'S') then enddate=MDY(12,31,year19_birth); 
else if enddate=. then	enddate=ExtractionDate;

if enddate>startdate;

	* cleaning for errorness records JL;
if startdate>0;

if startdate>"&sensor"d then
		delete;

if enddate>"&sensor"d then
		enddate="&sensor"d;
drop year19_birth;
run;

* lets make sure no duplicate records of enrolments;
proc sort data=sch_enrol_clean nodupkey;
	by snz_uid startdate enddate schoolnumber;
run;

proc sort data=sch_enrol_clean;
	by snz_uid startdate enddate;
run;

%mend;

%creating_clean_sch_enrol;

proc sort data= sch_enrol_clean nodupkey out=enrol;
	by snz_uid startdate enddate schoolnumber;
run;

proc sort data=enrol;
	by snz_uid startdate enddate;
run;

%OVERLAP (enrol);

data enrol_2;
	set enrol_OR;
	keep snz_uid schoolnumber startdate enddate;
run;

%aggregate_by_year(enrol_2,enrol_2_sum,&first_anal_yr,&last_anal_yr);

* creating final table;
proc sql;
	create table IND_SCH_ENROL_&date as
		select 
			SNZ_uid,
			year,
			sum(days) as sch_enr_da
		from enrol_2_sum
			group by snz_uid, year
				order by snz_uid, year;
quit;

*Creating final ENROL dataset long file;
data _ENROL;
	set;
	set IND_SCH_ENROL_&date;
	array sch_enr_da_(*) sch_enr_da_&first_anal_yr-sch_enr_da_&last_anal_yr;

	do i=&first_anal_yr to &last_anal_yr;
		ind=i-(&first_anal_yr-1);
		sch_enr_da_(ind)=0;

		if i=year then
			sch_enr_da_(ind)=sch_enr_da;
	end;
run;

proc summary data=_ENROL nway;
	class snz_uid;
	var sch_enr_da_&first_anal_yr-sch_enr_da_&last_anal_yr;
	output out=TEMP (drop=_type_ _freq_) sum=;
run;

data &folder.._IND_SCH_ENROL_&date;
	merge &population (keep=snz_uid DOB) TEMP;
	by snz_uid;
	array sch_enr_da_(*) sch_enr_da_&first_anal_yr-sch_enr_da_&last_anal_yr;

	do i=&first_anal_yr to &last_anal_yr;
		ind=i-(&first_anal_yr-1);

		if sch_enr_da_(ind)=. then
			sch_enr_da_(ind)=0;
		drop i ind;
	end;
run;

proc means data=&folder.._IND_SCH_ENROL_&date;
run;




************************************************;
**B. CYF ANNUAL INDICATORS;
************************************;

***********************************************************************************************************************************
***********************************************************************************************************************************
Creating standard CYF tables with all required variables
***********************************************************************************************************************************
***********************************************************************************************************************************;

%let sensor=31Dec2015; * Global censor data cut of date;
%let first_anal_yr=1990; /*1988*/* first calendar year of analysis, first year of birth for popualtion of interest;
%let last_anal_yr=2015;* Last calendar year of analysis;

%let firstage=0;
%let lastage=18;


%macro Create_clean_CYF_tables;
proc sql;
create table CYF_intake as select 
	snz_uid,
	snz_composite_event_uid,
	input(compress(cyf_ine_event_from_date_wid_date,"-"),yymmdd10.) as event_start_date,
	input(compress(cyf_ine_event_to_date_wid_date,"-"),yymmdd10.) as event_end_date
from cyf.CYF_intakes_event 
order by snz_composite_event_uid;

proc sql;
create table CYF_intake1 as select 
	a.*,
	b.cyf_ind_business_area_type_code as Business_Area,
	b.cyf_ind_notifier_role_type_code as NOTIFIER_ROLE_GROUP				
from CYF_intake a left join cyf.cyf_intakes_details b
on a.snz_composite_event_uid=b.snz_composite_event_uid
order by snz_uid;

data CYF_intake_clean; 
set CYF_intake1; 
format event_start_date event_end_date date9.;
if event_start_date>"&sensor."D then delete;
if event_end_date>"&sensor."D then event_end_date="&sensor."D;
run;

* CYF abuse findings;
proc sql;
create table CYF_abuse as select 
	snz_uid,
	snz_composite_event_uid,
	cyf_abe_source_uk_var2_text as abuse_type,
	input(compress(cyf_abe_event_from_date_wid_date,"-"),yymmdd10.) as finding_date
from cyf.CYF_abuse_event 
order by snz_composite_event_uid;

* CYF notifications and YJ refferals, where  Police Family Violence notifications identfiied through notifier_role_type_code PFV;
proc sql;
create table CYF_abuse1 as select 
	a.*,
	b.cyf_abd_business_area_type_code as Business_Area
	from CYF_abuse a left join cyf.cyf_abuse_details b
on a.snz_composite_event_uid=b.snz_composite_event_uid
order by snz_uid;

data CYF_abuse_clean; set CYF_abuse1;
format finding_date date9.;
if finding_date>"&sensor."D then delete;
if abuse_type="NTF" then delete;* abuse not found;
drop snz_composite_event_uid;
run;

proc sort data=CYF_abuse_clean nodupkey; 
by snz_uid abuse_type finding_date Business_Area; run;

* CYF placements;

proc sql;
create table CYF_place as select 
	snz_uid,
	cyf_ple_event_type_wid_nbr,
	snz_composite_event_uid,
	cyf_ple_number_of_days_nbr as event_duration,
	input(compress(cyf_ple_event_from_date_wid_date,"-"),yymmdd10.) as event_start_date,
	input(compress(cyf_ple_event_to_date_wid_date,"-"),yymmdd10.) as event_end_date
from cyf.cyf_placements_event 
order by snz_composite_event_uid;

proc sql;
create table CYF_place1 as select 
	a.*,
	b.cyf_pld_business_area_type_code as Business_Area,
	b.cyf_pld_placement_type_code as placement_type				
from CYF_place a left join cyf.cyf_placements_details b
on a.snz_composite_event_uid=b.snz_composite_event_uid
order by snz_uid;

data CYF_place_clean; 
set CYF_place1;
format event_start_date event_end_date date9.;
if event_start_date>"&sensor."D then delete;
if event_end_date>"&sensor."D then event_end_date="&sensor."D;
run;
%mend;

%Create_clean_CYF_tables;


**Creating summary of indicators by year
**Creating count of notifications and abuse and placements ( duration)

* BUSINESS RULES: Ignore records past 18th birthday;
* NOTIFICATIONS;
data CYF_intake3;
	merge CYF_intake_clean(in=a) &population(in=b keep=snz_uid DOB);
	by snz_uid;
	if a and b;
	format event_start_date event_end_date date9.;

	if event_start_date>intnx('YEAR',DOB, 18,'sameday') then
		delete;

	if event_end_date>intnx('YEAR',DOB, 18,'sameday') then
		event_end_date=intnx('YEAR',DOB, 18,'sameday');
	year=year(event_start_date);
	child_not=0;
	child_Pol_FV_not=0;
	child_YJ_referral=0;

	if Business_Area in ('CNP','UNK') then
		child_not=1;

	if Business_Area in ('CNP','UNK') and  NOTIFIER_ROLE_GROUP	 in ('PFV') then
		child_Pol_FV_not=1;

	if Business_Area in ('YJU') then
		child_YJ_referral=1;
run;

* inlcudes CYF and YJ notifications
Notifications can overlap-not running overlap macro;
proc summary data=CYF_intake3 nway;
	class snz_uid DOB year;
	var child_not child_Pol_FV_not child_YJ_referral;
	output out=CYF_intake_year(drop=_type_ _freq_) sum=;
run;

* BUSINESS RULES: Ignore records past 18th birthday;
* PLACEMENTS;
data CYF_place3;
	merge CYF_place_clean(in=a) &population(in=b keep=snz_uid DOB);
	by snz_uid;

	if a and b;
	format event_start_date event_end_date date9.;

	* ignoring all notification after 18th DOB;
	if event_start_date>intnx('YEAR',DOB, 18,'sameday') then
		delete;

	if event_end_date>intnx('YEAR',DOB, 18,'sameday') then
		event_end_date=intnx('YEAR',DOB, 18,'sameday');
	year=year(event_start_date);
	child_CYF_place=0;
	child_YJ_place=0;

	if event_duration ge 28 and business_area='CNP' then
		do;
			child_CYF_place=1;
		end;

	if event_duration ge 28 and business_area='YJU' then
		do;
			child_YJ_place=1;
		end;
run;

proc summary data=CYF_place3 nway;
	class snz_uid DOB year;
	var child_CYF_place child_YJ_place;
	output out=CYF_place_year(drop=_type_ _freq_) sum=;
run;

* BUSINESS RULES: Ignore records past 18th birthday;
* ABUSE FINDINGS;
data CYF_abuse3;
	merge CYF_abuse_clean(in=a) &population(in=b keep=snz_uid DOB);
	by snz_uid;

	if a and b;
	format finding_date date9.;

	if finding_date>intnx('YEAR',DOB, 18,'sameday') then
		delete;
	year=year(finding_date);
	Child_fdgs_neglect=0;
	Child_fdgs_phys_abuse=0;
	Child_fdgs_sex_abuse=0;
	Child_fdgs_emot_abuse=0;
	Child_fdgs_behav_relat=0;
	Child_fdgs_selfh_suic=0;

	if abuse_type='BRD' then
		Child_fdgs_behav_relat=1;

	if abuse_type='EMO' then
		Child_fdgs_emot_abuse=1;

	if abuse_type='NEG' then
		Child_fdgs_neglect=1;

	if abuse_type='PHY' then
		Child_fdgs_phys_abuse=1;

	if abuse_type='SEX' then
		Child_fdgs_sex_abuse=1;

	if abuse_type in ('SHS', 'SHM','SUC') then
		Child_fdgs_selfh_suic=1;
run;

proc summary data=CYF_abuse3 nway;
	class snz_uid DOB year;
	var Child_fdgs_neglect
		Child_fdgs_phys_abuse
		Child_fdgs_sex_abuse
		Child_fdgs_emot_abuse
		Child_fdgs_behav_relat
		Child_fdgs_selfh_suic;
	output out=CYF_abuse_year(drop=_type_ _freq_) sum=;
run;

* Consolidated datasets;
proc sort data=CYF_intake_year;
	by snz_uid year;
run;

proc sort data=CYF_abuse_year;
	by snz_uid year;
run;

proc sort data=CYF_place_year;
	by snz_uid year;
run;

DATA &folder..IND_CYF_&date;
	merge CYF_intake_year  CYF_abuse_year CYF_place_year;
	by snz_uid year;

	IF child_not=. Then
		child_not=0;

	IF child_Pol_FV_not=. Then
		child_Pol_FV_not=0;

	IF child_YJ_referral=. Then
		child_YJ_referral=0;

	IF Child_fdgs_neglect=. Then
		Child_fdgs_neglect=0;

	IF Child_fdgs_phys_abuse=. Then
		Child_fdgs_phys_abuse=0;

	IF Child_fdgs_sex_abuse=. Then
		Child_fdgs_sex_abuse=0;

	IF Child_fdgs_emot_abuse=. Then
		Child_fdgs_emot_abuse=0;

	IF Child_fdgs_behav_relat=. Then
		Child_fdgs_behav_relat=0;

	IF Child_fdgs_selfh_suic=. Then
		Child_fdgs_selfh_suic=0;

	IF child_CYF_place=. Then
		child_CYF_place=0;

	IF child_YJ_place=. Then
		child_YJ_place=0;
run;

* creating dataset of CYF indicators by year wide dataset;

**Creating at age variables;

* NOtifications at age;
data  CYF_intake4;
	set  CYF_intake3;
	dateofbirth=DOB;
	array child_not_at_age_(*) child_not_at_age_&firstage-child_not_at_age_&lastage;
	array child_Pol_FV_not_at_age_(*) child_Pol_FV_not_at_age_&firstage-child_Pol_FV_not_at_age_&lastage;
	array child_YJ_referral_at_age_(*) child_YJ_referral_at_age_&firstage-child_YJ_referral_at_age_&lastage;

	do ind = &firstage to &lastage;
		i=ind-(&firstage-1);
		child_not_at_age_(i)=0;
		child_Pol_FV_not_at_age_(i)=0;
		child_YJ_referral_at_age_(i)=0;

		* events by selected birthdays;
		start_window=intnx('YEAR',dateofbirth,i-1,'S');
		end_window=intnx('YEAR',dateofbirth,i,'S');

		if Business_Area in ('CNP','UNK') and ((event_Start_Date <end_window) and (event_Start_Date>=start_window)) then
			child_not_at_age_(i)=1;

		if Business_Area in ('CNP','UNK') and ((event_Start_Date <end_window) and (event_Start_Date>=start_window)) then
			do;
				if NOTIFIER_ROLE_GROUP	 in ('PFV') then
					do;
						child_Pol_FV_not_at_age_(i)=1;
					end;
			end;

		if Business_Area in ('YJU') and ((event_Start_Date <end_window) and (event_Start_Date>=start_window)) then
			child_YJ_referral_at_age_(i)=1;
	end;
run;

proc summary data=CYF_intake4 nway;
	class snz_uid;
	var child_not_at_age_&firstage-child_not_at_age_&lastage
		child_Pol_FV_not_at_age_&firstage-child_Pol_FV_not_at_age_&lastage
		child_YJ_referral_at_age_&firstage-child_YJ_referral_at_age_&lastage;
	output out=CYF_intake_at_age (drop=_type_ _freq_) sum=;
run;

* Placements at age;
data  CYF_place4;
	set  CYF_place3;
	dateofbirth=DOB;
	array child_CYF_place_at_age_(*) child_CYF_place_at_age_&firstage-child_CYF_place_at_age_&lastage;
	array child_YJ_place_at_age_(*) child_YJ_place_at_age_&firstage-child_YJ_place_at_age_&lastage;

	do ind = &firstage to &lastage;
		i=ind-(&firstage-1);
		child_CYF_place_at_age_(i)=0;
		child_YJ_place_at_age_(i)=0;
		start_window=intnx('YEAR',dateofbirth,i-1,'S');
		end_window=intnx('YEAR',dateofbirth,i,'S');

		* events by selected birthdays;
		if business_area='CNP' and ((event_Start_Date <end_window) and (event_Start_Date>=start_window)) and event_duration ge 28 then
			child_CYF_place_at_age_(i)=1;

		if business_area='YJU' and ((event_Start_Date <end_window) and (event_Start_Date>=start_window))  and event_duration ge 28 then
			child_YJ_place_at_age_(i)=1;
	end;
run;

proc summary data=CYF_place4 nway;
	class snz_uid;
	var child_CYF_place_at_age_&firstage-child_CYF_place_at_age_&lastage child_YJ_place_at_age_&firstage-child_YJ_place_at_age_&lastage;
	output out=CYF_place_at_age(drop=_type_ _freq_) sum=;
run;

* Abuse findings;
data CYF_abuse4;
	set CYF_abuse3;
	dateofbirth=DOB;
	array Child_fdgs_neglect_at_age_(*)Child_fdgs_neglect_at_age_&firstage - Child_fdgs_neglect_at_age_&lastage;
	array Child_fdgs_phys_abuse_at_age_(*)Child_fdgs_phys_abuse_at_age_&firstage- Child_fdgs_phys_abuse_at_age_&lastage;
	array Child_fdgs_emot_abuse_at_age_(*)Child_fdgs_emot_abuse_at_age_&firstage - Child_fdgs_emot_abuse_at_age_&lastage;
	array Child_fdgs_sex_abuse_at_age_(*)Child_fdgs_sex_abuse_at_age_&firstage - Child_fdgs_sex_abuse_at_age_&lastage;
	array Child_fdgs_behav_relat_at_age_(*)Child_fdgs_behav_relat_at_age_&firstage - Child_fdgs_behav_relat_at_age_&lastage;
	array Child_fdgs_selfh_suic_at_age_(*) Child_fdgs_selfh_suic_at_age_&firstage-Child_fdgs_selfh_suic_at_age_&lastage;
	array child_any_fdgs_abuse_at_age_(*) child_any_fdgs_abuse_at_age_&firstage-child_any_fdgs_abuse_at_age_&lastage;

	do ind = &firstage to &lastage;
		i=ind-(&firstage-1);
		Child_fdgs_neglect_at_age_(i)=0;
		Child_fdgs_phys_abuse_at_age_(i)=0;
		Child_fdgs_sex_abuse_at_age_(i)=0;
		Child_fdgs_emot_abuse_at_age_(i)=0;
		Child_fdgs_behav_relat_at_age_(i)=0;
		Child_fdgs_selfh_suic_at_age_(i)=0;
		Child_any_fdgs_abuse_at_age_(i)=0;
		start_window=intnx('YEAR',dateofbirth,i-1,'S');
		end_window=intnx('YEAR',dateofbirth,i,'S');

		* events by selected birthdays;
		if ((finding_date <end_window) and (finding_date>=start_window)) then
			do;
				if abuse_type='BRD' then
					do;
						Child_fdgs_behav_relat_at_age_(i)=1;
					end;

				if abuse_type='EMO' then
					do;
						Child_fdgs_emot_abuse_at_age_(i)=1;
					end;

				if abuse_type='NEG' then
					do;
						Child_fdgs_neglect_at_age_(i)=1;
					end;

				if abuse_type='PHY' then
					do;
						Child_fdgs_phys_abuse_at_age_(i)=1;
					end;

				if abuse_type='SEX' then
					do;
						Child_fdgs_sex_abuse_at_age_(i)=1;
					end;

				if abuse_type in ('SHS', 'SHM','SUC') then
					do;
						Child_fdgs_selfh_suic_at_age_(i)=1;
					end;

				if abuse_type in ('EMO','NEG','PHY','SEX') then
					do;
						child_any_fdgs_abuse_at_age_(i)=1;
					end;
			end;
	end;
run;

proc summary data=CYF_abuse4 nway;
	class snz_uid;
	var 	Child_fdgs_neglect_at_age_&firstage--Child_fdgs_neglect_at_age_&lastage
		Child_fdgs_phys_abuse_at_age_&firstage--Child_fdgs_phys_abuse_at_age_&lastage
		Child_fdgs_sex_abuse_at_age_&firstage--Child_fdgs_sex_abuse_at_age_&lastage
		Child_fdgs_emot_abuse_at_age_&firstage--Child_fdgs_emot_abuse_at_age_&lastage
		Child_fdgs_behav_relat_at_age_&firstage--Child_fdgs_behav_relat_at_age_&lastage
		Child_fdgs_selfh_suic_at_age_&firstage--Child_fdgs_selfh_suic_at_age_&lastage
		child_any_fdgs_abuse_at_age_&firstage-child_any_fdgs_abuse_at_age_&lastage;
	output out=CYF_abuse_at_age(drop=_type_ _freq_) sum=;
run;

* Consolidated datasets;
proc sort data=CYF_intake_at_age;
	by snz_uid;
run;

proc sort data=CYF_abuse_at_age;
	by snz_uid;
run;

proc sort data=CYF_place_at_age;
	by snz_uid;
run;


proc contents data=&folder.._IND_CYF_at_age_&date;
run;

DATA &folder.._IND_CYF_at_age_&date;
	merge &population(keep=snz_uid DOB) CYF_intake_at_age CYF_abuse_at_age CYF_place_at_age;
	by snz_uid;
	array Child_fdgs_neglect_at_age_(*)Child_fdgs_neglect_at_age_&firstage- Child_fdgs_neglect_at_age_&lastage;
	array Child_fdgs_phys_abuse_at_age_(*)Child_fdgs_phys_abuse_at_age_&firstage - Child_fdgs_phys_abuse_at_age_&lastage;
	array Child_fdgs_emot_abuse_at_age_(*)Child_fdgs_emot_abuse_at_age_&firstage - Child_fdgs_emot_abuse_at_age_&lastage;
	array Child_fdgs_sex_abuse_at_age_(*)Child_fdgs_sex_abuse_at_age_&firstage - Child_fdgs_sex_abuse_at_age_&lastage;
	array Child_fdgs_behav_relat_at_age_(*)Child_fdgs_behav_relat_at_age_&firstage - Child_fdgs_behav_relat_at_age_&lastage;
	array Child_fdgs_selfh_suic_at_age_(*) Child_fdgs_selfh_suic_at_age_&firstage-Child_fdgs_selfh_suic_at_age_&lastage;
	array Child_any_fdgs_abuse_at_age_(*) Child_any_fdgs_abuse_at_age_&firstage-Child_any_fdgs_abuse_at_age_&lastage;
	array child_not_at_age_(*) child_not_at_age_&firstage-child_not_at_age_&lastage;
	array child_Pol_FV_not_at_age_(*) child_Pol_FV_not_at_age_&firstage-child_Pol_FV_not_at_age_&lastage;
	array child_YJ_referral_at_age_(*) child_YJ_referral_at_age_&firstage-child_YJ_referral_at_age_&lastage;
	array child_CYF_place_at_age_(*) child_CYF_place_at_age_&firstage-child_CYF_place_at_age_&lastage;
	array child_YJ_place_at_age_(*) child_YJ_place_at_age_&firstage-child_YJ_place_at_age_&lastage;

	do ind = &firstage to &lastage;
		i=ind-(&firstage-1);

		if child_not_at_age_(i)=. Then
			child_not_at_age_(i)=0;

		IF child_Pol_FV_not_at_age_(i)=. Then
			child_Pol_FV_not_at_age_(i)=0;

		IF child_YJ_referral_at_age_(i)=. Then
			child_YJ_referral_at_age_(i)=0;

		IF Child_fdgs_neglect_at_age_(i)=. Then
			Child_fdgs_neglect_at_age_(i)=0;

		IF Child_fdgs_phys_abuse_at_age_(i)=. Then
			Child_fdgs_phys_abuse_at_age_(i)=0;

		IF Child_fdgs_emot_abuse_at_age_(i)=. Then
			Child_fdgs_emot_abuse_at_age_(i)=0;

		IF Child_fdgs_sex_abuse_at_age_(i)=. Then
			Child_fdgs_sex_abuse_at_age_(i)=0;

		IF Child_fdgs_behav_relat_at_age_(i)=. Then
			Child_fdgs_behav_relat_at_age_(i)=0;

		IF Child_fdgs_selfh_suic_at_age_(i)=. Then
			Child_fdgs_selfh_suic_at_age_(i)=0;

		IF child_CYF_place_at_age_(i)=. Then
			child_CYF_place_at_age_(i)=0;

		IF child_YJ_place_at_age_(i)=. Then
			child_YJ_place_at_age_(i)=0;

		if Child_any_fdgs_abuse_at_age_(i)=. then
			Child_any_fdgs_abuse_at_age_(i)=0;

		* Now sensoring for not fully observed data;
		start_window=intnx('YEAR',DOB,i-1,'S');
		end_window=intnx('YEAR',DOB,i,'S');

		if ((end_window>"&sensor"d) or (start_window>"&sensor"d)) then
			child_not_at_age_(i)=.;

		if ((end_window>"&sensor"d) or (start_window>"&sensor"d)) then
			child_Pol_FV_not_at_age_(i)=.;

		if ((end_window>"&sensor"d) or (start_window>"&sensor"d)) then
			child_YJ_referral_at_age_(i)=.;

		if ((end_window>"&sensor"d) or (start_window>"&sensor"d)) then
			Child_fdgs_neglect_at_age_(i)=.;

		if ((end_window>"&sensor"d) or (start_window>"&sensor"d)) then
			Child_fdgs_phys_abuse_at_age_(i)=.;

		if ((end_window>"&sensor"d) or (start_window>"&sensor"d)) then
			Child_fdgs_emot_abuse_at_age_(i)=.;

		if ((end_window>"&sensor"d) or (start_window>"&sensor"d)) then
			Child_fdgs_sex_abuse_at_age_(i)=.;

		if ((end_window>"&sensor"d) or (start_window>"&sensor"d)) then
			Child_fdgs_behav_relat_at_age_(i)=.;

		if ((end_window>"&sensor"d) or (start_window>"&sensor"d)) then
			Child_fdgs_selfh_suic_at_age_(i)=.;

		if ((end_window>"&sensor"d) or (start_window>"&sensor"d)) then
			child_CYF_place_at_age_(i)=.;

		if ((end_window>"&sensor"d) or (start_window>"&sensor"d)) then
			child_YJ_place_at_age_(i)=.;

		if ((end_window>"&sensor"d) or (start_window>"&sensor"d)) then
			Child_any_fdgs_abuse_at_age_(i)=.;
		drop i ind start_window end_window;
	end;
run;



************************************************************;
**C. BENEFIT RECEIPT AS A CHILD ANNUAL INDICATORS;
************************************************************;

%macro creating_MSD_SPEL;
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


data msd_spel; set msd.msd_spell;
* Formating dates and sensoring;
	format startdate enddate spellfrom spellto date9.;
	spellfrom=input(compress(msd_spel_spell_start_date,"-"),yymmdd10.);
	spellto=input(compress(msd_spel_spell_end_date,"-"),yymmdd10.);
	if spellfrom<"&sensor"d;
	if spellfrom<"01Jan1993"d then spellfrom="01Jan1993"d;* BDD left censor;
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
%mend;

%creating_MSD_spel;

/*
proc freq data=msd_spel;
	tables BEN*ben_new/list missing;
run;

proc freq data=msd_spel;
	where prereform='675';
	tables BEN*ben_new/list missing;
run;

proc freq data=msd_spel;
	where prereform='370';
	tables BEN*ben_new/list missing;
run;
*/


* child spell dataset;

%macro create_BDD_child_spell;

* child spell dataset;
data ICD_BDD_chd;
	set msd.msd_child;
	format chto chfrom chdob date9.;
	spell=msd_chld_spell_nbr;
	chfrom=input(compress(msd_chld_child_from_date,"-"),yymmdd10.);
	chto=input(compress(msd_chld_child_to_date,"-"),yymmdd10.);
	chdob=mdy(msd_chld_child_birth_month_nbr,15,msd_chld_child_birth_year_nbr);

	* SENSORING;
	if chfrom>"&sensor"d then
		delete;

	if chto>"&sensor"d then
		chto="&sensor"d;

	if chto=. then
		chto="&sensor"d;
	keep snz_uid spell child_snz_uid chfrom chto chdob;
run;
%mend;

%create_BDD_child_spell;

* BDD spell dataset;
data msd_spel;
	set msd_spel;
	spell=msd_spel_spell_nbr;
	keep snz_uid spellfrom spellto spell servf ben ben_new;
run;


*Linking child parent relationship to parent's spell;
proc sort data = ICD_BDD_chd;
	by snz_uid spell;
run;

proc sort data = msd_spel;
	by snz_uid spell;
run;

data chd_mainben;
	merge    ICD_BDD_chd (in = y) msd_spel (keep = snz_uid ben ben_new spell);
	by snz_uid spell;

	if y;
run;

proc sort data=chd_mainben;
	by child_snz_uid chfrom chto;
run;

data feed;
	set chd_mainben;
	format startdate enddate date9.;
	rename snz_uid=adult_snz_uid;
	rename child_snz_uid=snz_uid;
	startdate=chfrom;
	enddate=chto;
run;

* sometimes both parents can claim main benefit on the same child, MSD did not check it in the past.
* We are randomly removing overlaping spells;
%overlap(feed);

/*proc freq data=examine; format servf $bennewgp.; tables servf*delete_flag; run;*/

* some children have different chdob, we will trust our Dateofbirth variable from tsy_b15_04_cohort dataset
* limiting to records of related ref children;
proc sql;
	create table feed_new1
		as select 
			a.*,
			b.DOB
		from feed_OR a inner join &population b
			on a.snz_uid=b.snz_uid where startdate>(DOB-45)
		order by snz_uid, startdate 
	;

	* Excluding all records 45 days prior to DOB of the child;
quit;


* Creating at age variables;


%macro create_BDD_at_age_child(infile, outfile);

	data _IND_BDD_NEW;
		set &infile.(drop=adult_snz_uid);
		by  snz_uid startdate;
		array supp_YP_at_age_ [*] supp_dpb_at_age_&firstage-supp_dpb_at_age_&lastage;
		array supp_YPP_at_age_ [*] supp_ub_at_age_&firstage-supp_ub_at_age_&lastage;
		array supp_SPSR_at_age_ [*] supp_sb_at_age_&firstage-supp_sb_at_age_&lastage;
		array supp_JSWR_at_age_ [*] supp_ib_at_age_&firstage-supp_ib_at_age_&lastage;
		array supp_JSWR_TR_at_age_ [*] supp_JSWR_TR_at_age_&firstage-supp_JSWR_TR_at_age_&lastage;
		array supp_JSHCD_at_age_ [*] supp_JSHCD_at_age_&firstage-supp_JSHCD_at_age_&lastage;
		array supp_SLP_C_at_age_ [*] supp_SLP_C_at_age_&firstage-supp_SLP_C_at_age_&lastage;
		array supp_SLP_HCD_at_age_ [*] supp_SLP_HCD_at_age_&firstage-supp_SLP_HCD_at_age_&lastage;
		array supp_OTH_at_age_ [*] supp_OTH_at_age_&firstage-supp_OTH_at_age_&lastage;
		array da_YP_at_age_ [*] da_YP_at_age_&firstage-da_YP_at_age_&lastage;
		array da_YPP_at_age_ [*] da_YPP_at_age_&firstage-da_YPP_at_age_&lastage;
		array da_SPSR_at_age_ [*] da_SPSR_at_age_&firstage-da_SPSR_at_age_&lastage;
		array da_JSWR_at_age_ [*] da_JSWR_at_age_&firstage-da_JSWR_at_age_&lastage;
		array da_JSWR_TR_at_age_ [*] da_JSWR_TR_at_age_&firstage-da_JSWR_TR_at_age_&lastage;
		array da_JSHCD_at_age_ [*] da_JSHCD_at_age_&firstage-da_JSHCD_at_age_&lastage;
		array da_SLP_C_at_age_ [*] da_SLP_C_at_age_&firstage-da_SLP_C_at_age_&lastage;
		array da_SLP_HCD_at_age_ [*] da_SLP_HCD_at_age_&firstage-da_SLP_HCD_at_age_&lastage;
		array da_OTH_at_age_ [*] da_OTH_at_age_&firstage-da_OTH_at_age_&lastage;

		do i=&firstage to &lastage;
			age=i-(&firstage-1);
			supp_YP_at_age_[age]=0;
			supp_YPP_at_age_[age]=0;
			supp_SPSR_at_age_ [age]=0;
			supp_JSWR_at_age_ [age]=0;
			supp_JSWR_TR_at_age_ [age]=0;
			supp_JSHCD_at_age_[age]=0;
			supp_SLP_C_at_age_[age]=0;
			supp_SLP_HCD_at_age_[age]=0;
			supp_OTH_at_age_[age]=0;
			da_YP_at_age_[age]=0;
			da_YPP_at_age_[age]=0;
			da_SPSR_at_age_ [age]=0;
			da_JSWR_at_age_ [age]=0;
			da_JSWR_TR_at_age_ [age]=0;
			da_JSHCD_at_age_[age]=0;
			da_SLP_C_at_age_[age]=0;
			da_SLP_HCD_at_age_[age]=0;
			da_OTH_at_age_[age]=0;
			supp_YP_atbirth=0;
			supp_YPP_atbirth=0;
			supp_SPSR_atbirth=0;
			supp_JSWR_atbirth=0;
			supp_JSWR_TR_atbirth=0;
			supp_JSHCD_atbirth=0;
			supp_SLP_C_atbirth=0;
			supp_SLP_HCD_atbirth=0;
			supp_OTH_atbirth=0;
			start_window=intnx('YEAR',DOB,age-1,'S');
			end_window=intnx('YEAR',DOB,age,'S');

			if not((startdate > end_window) or (enddate < start_window)) then
				do;
					if ben_new='YP' then
						supp_YP_at_age_[age]=1;

					if ben_new='YPP' then
						supp_YPP_at_age_[age]=1;

					if ben_new='SPSR' then
						supp_SPSR_at_age_[age]=1;

					if ben_new='JSWR' then
						supp_JSWR_at_age_[age]=1;

					if ben_new='JSWR_TR' then
						supp_JSWR_TR_at_age_[age]=1;

					if ben_new='JSHCD' then
						supp_JSHCD_at_age_[age]=1;

					if ben_new='SLP_C' then
						supp_SLP_C_at_age_[age]=1;

					if ben_new='SLP_HCD' then
						supp_SLP_HCD_at_age_[age]=1;

					if ben_new='OTH' then
						supp_OTH_at_age_[age]=1;

					if (startdate <= start_window) and  (enddate > end_window) then
						days=(end_window-start_window)+1;
					else if (startdate <= start_window) and  (enddate <= end_window) then
						days=(enddate-start_window)+1;
					else if (startdate > start_window) and  (enddate <= end_window) then
						days=(enddate-startdate)+1;
					else if (startdate > start_window) and  (enddate > end_window) then
						days=(end_window-startdate)+1;

					* calcualting days;
					da_YP_at_age_[age]=days*supp_YP_at_age_[age];
					da_YPP_at_age_[age]=days*supp_YPP_at_age_[age];
					da_SPSR_at_age_ [age]=days*supp_SPSR_at_age_[age];
					da_JSWR_at_age_ [age]=days*supp_JSWR_at_age_[age];
					da_JSWR_TR_at_age_ [age]=days*supp_JSWR_TR_at_age_[age];
					da_JSHCD_at_age_[age]=days*supp_JSHCD_at_age_[age];
					da_SLP_C_at_age_[age]=days*supp_SLP_C_at_age_[age];
					da_SLP_HCD_at_age_[age]=days*supp_SLP_HCD_at_age_[age];
					da_OTH_at_age_[age]=days*supp_OTH_at_age_[age];
				end;

			if not((startdate> (DOB+45) or (enddate < (DOB-45) ) ) ) then
				do;
					if ben_new='YP' then
						supp_YP_atbirth=1;
					else if ben_new='YPP' then
						supp_YPP_atbirth=1;
					else if ben_new='SPSR' then
						supp_SPSR_atbirth=1;
					else if ben_new='JSWR' then
						supp_JSWR_atbirth=1;
					else if ben_new='JSWR_TR' then
						supp_JSWR_TR_atbirth=1;
					else if ben_new='JSHCD' then
						supp_JSHCD_atbirth=1;
					else if ben_new='SLP_C' then
						supp_SLP_C_atbirth=1;
					else if ben_new='SLP_HCD' then
						supp_SLP_HCD_atbirth=1;
					else supp_OTH_atbirth=1;
				end;
		end;

		keep snz_uid ben_new chdob chfrom chto enddate startdate 

			da_YP_at_age_&firstage-da_YP_at_age_&lastage
			da_YPP_at_age_&firstage-da_YPP_at_age_&lastage
			da_SPSR_at_age_&firstage-da_SPSR_at_age_&lastage
			da_JSWR_at_age_&firstage-da_JSWR_at_age_&lastage
			da_JSWR_TR_at_age_&firstage-da_JSWR_TR_at_age_&lastage
			da_JSHCD_at_age_&firstage-da_JSHCD_at_age_&lastage
			da_SLP_C_at_age_&firstage-da_SLP_C_at_age_&lastage
			da_SLP_HCD_at_age_&firstage-da_SLP_HCD_at_age_&lastage
			da_OTH_at_age_&firstage-da_OTH_at_age_&lastage

			supp_YP_atbirth
			supp_YPP_atbirth
			supp_SPSR_atbirth
			supp_JSWR_atbirth
			supp_JSWR_TR_atbirth
			supp_JSHCD_atbirth
			supp_SLP_C_atbirth
			supp_SLP_HCD_atbirth
			supp_OTH_atbirth;
	run;

	* Creating at age variables;
	data _IND_BDD_OLD;
		set &infile.(drop=adult_snz_uid);
		by  snz_uid startdate;
		array supp_dpb_at_age_ [*] supp_dpb_at_age_&firstage-supp_dpb_at_age_&lastage;
		array supp_ub_at_age_ [*] supp_ub_at_age_&firstage-supp_ub_at_age_&lastage;
		array supp_sb_at_age_ [*] supp_sb_at_age_&firstage-supp_sb_at_age_&lastage;
		array supp_ib_at_age_ [*] supp_ib_at_age_&firstage-supp_ib_at_age_&lastage;
		array supp_iyb_at_age_ [*] supp_iyb_at_age_&firstage-supp_iyb_at_age_&lastage;
		array supp_othben_at_age_ [*] supp_othben_at_age_&firstage-supp_othben_at_age_&lastage;
		array supp_ucb_at_age_ [*] supp_ucb_at_age_&firstage-supp_ucb_at_age_&lastage;
		array total_da_onben_at_age_ [*] total_da_onben_at_age_&firstage-total_da_onben_at_age_&lastage;
		array da_dpb_at_age_ [*] da_DPB_at_age_&firstage-da_DPB_at_age_&lastage;
		array da_ub_at_age_ [*] da_UB_at_age_&firstage-da_UB_at_age_&lastage;
		array da_sb_at_age_ [*] da_SB_at_age_&firstage-da_SB_at_age_&lastage;
		array da_ib_at_age_ [*] da_IB_at_age_&firstage-da_IB_at_age_&lastage;
		array da_iyb_at_age_ [*] da_IYB_at_age_&firstage-da_IYB_at_age_&lastage;
		array da_othben_at_age_ [*] da_OTHBEN_at_age_&firstage-da_OTHBEN_at_age_&lastage;
		array da_ucb_at_age_ [*] da_UCB_at_age_&firstage-da_UCB_at_age_&lastage;

		do i=&firstage to &lastage;
			age=i-(&firstage-1);
			total_da_onben_at_age_[age]=0;
			supp_dpb_at_age_[age]=0;
			supp_ub_at_age_[age]=0;
			supp_sb_at_age_ [age]=0;
			supp_ib_at_age_ [age]=0;
			supp_iyb_at_age_ [age]=0;
			supp_othben_at_age_[age]=0;
			supp_ucb_at_age_[age]=0;
			da_dpb_at_age_[age]=0;
			da_ub_at_age_[age]=0;
			da_sb_at_age_ [age]=0;
			da_ib_at_age_ [age]=0;
			da_iyb_at_age_ [age]=0;
			da_othben_at_age_[age]=0;
			da_ucb_at_age_[age]=0;
			supp_dpb_atbirth=0;
			supp_ub_atbirth=0;
			supp_sb_atbirth=0;
			supp_ib_atbirth=0;
			supp_othben_atbirth=0;
			supp_ucb_atbirth=0;
			supp_onben_atbirth=0;
			start_window=intnx('YEAR',DOB,age-1,'S');
			end_window=intnx('YEAR',DOB,age,'S');

			if not((startdate > end_window) or (enddate < start_window)) then
				do;
					if ben='DPB' then
						supp_dpb_at_age_[age]=1;

					if ben='IB' then
						supp_ib_at_age_[age]=1;

					if ben='UB' then
						supp_ub_at_age_[age]=1;

					if ben='IYB' then
						supp_iyb_at_age_[age]=1;

					if ben='SB' then
						supp_sb_at_age_[age]=1;

					if ben='UCB' then
						supp_ucb_at_age_[age]=1;

					if ben='OTH' then
						supp_othben_at_age_[age]=1;

					if (startdate <= start_window) and  (enddate > end_window) then
						days=(end_window-start_window)+1;
					else if (startdate <= start_window) and  (enddate <= end_window) then
						days=(enddate-start_window)+1;
					else if (startdate > start_window) and  (enddate <= end_window) then
						days=(enddate-startdate)+1;
					else if (startdate > start_window) and  (enddate > end_window) then
						days=(end_window-startdate)+1;

					* calcualting days;
					total_da_onben_at_age_[age]=days;
					da_dpb_at_age_[age]=days*supp_dpb_at_age_[age];
					da_ub_at_age_[age]=days*supp_ub_at_age_[age];
					da_sb_at_age_ [age]=days*supp_sb_at_age_[age];
					da_ib_at_age_ [age]=days*supp_ib_at_age_[age];
					da_iyb_at_age_ [age]=days*supp_iyb_at_age_[age];
					da_othben_at_age_[age]=days*supp_othben_at_age_[age];
					da_ucb_at_age_[age]=days*supp_ucb_at_age_[age];
				end;

			if not((startdate> (DOB+45) or (enddate < (DOB-45) ) ) ) then
				do;
					supp_onben_atbirth=1;

					if ben='DPB' then
						supp_dpb_atbirth=1;
					else if ben='SB' then
						supp_sb_atbirth=1;
					else if ben='IB' then
						supp_ib_atbirth=1;
					else if ben='UB' then
						supp_ub_atbirth=1;
					else if  ben='UCB' then
						supp_ucb_atbirth=1;
					else supp_othben_atbirth=1;
				end;
		end;

		keep snz_uid ben chdob chfrom chto dob startdate enddate 

			total_da_onben_at_age_&firstage-total_da_onben_at_age_&lastage
			da_DPB_at_age_&firstage-da_DPB_at_age_&lastage
			da_UB_at_age_&firstage-da_UB_at_age_&lastage
			da_SB_at_age_&firstage-da_SB_at_age_&lastage
			da_IB_at_age_&firstage-da_IB_at_age_&lastage
			da_IYB_at_age_&firstage-da_IYB_at_age_&lastage
			da_OTHBEN_at_age_&firstage-da_OTHBEN_at_age_&lastage
			da_UCB_at_age_&firstage-da_UCB_at_age_&lastage

			supp_dpb_atbirth
			supp_ub_atbirth
			supp_sb_atbirth
			supp_ib_atbirth
			supp_othben_atbirth
			supp_ucb_atbirth
			supp_onben_atbirth;
	run;

	proc sort data=_IND_BDD_OLD;
		by snz_uid;
	run;

	proc sort data=_IND_BDD_NEW;
		by snz_uid;
	run;

	data _IND_BDD_NEW_OLD;
		merge _IND_BDD_OLD _IND_BDD_NEW;
		by snz_uid;
	run;

	proc summary data=_IND_BDD_NEW_OLD nway;
		var 
			total_da_onben_at_age_&firstage-total_da_onben_at_age_&lastage
			da_dpb_at_age_&firstage-da_dpb_at_age_&lastage
			da_ub_at_age_&firstage-da_ub_at_age_&lastage
			da_sb_at_age_&firstage-da_sb_at_age_&lastage
			da_ib_at_age_&firstage-da_ib_at_age_&lastage
			da_iyb_at_age_&firstage-da_iyb_at_age_&lastage
			da_othben_at_age_&firstage-da_othben_at_age_&lastage
			da_ucb_at_age_&firstage-da_ucb_at_age_&lastage

			da_YP_at_age_&firstage-da_YP_at_age_&lastage
			da_YPP_at_age_&firstage-da_YPP_at_age_&lastage
			da_SPSR_at_age_&firstage-da_SPSR_at_age_&lastage
			da_JSWR_at_age_&firstage-da_JSWR_at_age_&lastage
			da_JSWR_TR_at_age_&firstage-da_JSWR_TR_at_age_&lastage
			da_JSHCD_at_age_&firstage-da_JSHCD_at_age_&lastage
			da_SLP_C_at_age_&firstage-da_SLP_C_at_age_&lastage
			da_SLP_HCD_at_age_&firstage-da_SLP_HCD_at_age_&lastage
			da_OTH_at_age_&firstage-da_OTH_at_age_&lastage;
		by snz_uid;
		output out=Part1(drop=_type_ _freq_) sum=;

	proc summary data=_IND_BDD_NEW_OLD nway;
		var 
			supp_dpb_atbirth
			supp_ub_atbirth
			supp_sb_atbirth
			supp_ib_atbirth
			supp_othben_atbirth
			supp_ucb_atbirth

			supp_YP_atbirth
			supp_YPP_atbirth
			supp_SPSR_atbirth
			supp_JSWR_atbirth
			supp_JSWR_TR_atbirth
			supp_JSHCD_atbirth
			supp_SLP_C_atbirth
			supp_SLP_HCD_atbirth
			supp_OTH_atbirth
		;
		by snz_uid;
		output out=Part2(drop=_type_ _freq_) max=;
	run;

	data &outfile.;
		merge &population(keep=snz_uid DOB) part2 part1;
		by snz_uid;

		* setting missing to zero;
		array total_da_onben_at_age_ [*] total_da_onben_at_age_&firstage-total_da_onben_at_age_&lastage;
		array da_dpb_at_age_ [*] da_DPB_at_age_&firstage-da_DPB_at_age_&lastage;
		array da_ub_at_age_ [*] da_UB_at_age_&firstage-da_UB_at_age_&lastage;
		array da_sb_at_age_ [*] da_SB_at_age_&firstage-da_SB_at_age_&lastage;
		array da_ib_at_age_ [*] da_IB_at_age_&firstage-da_IB_at_age_&lastage;
		array da_iyb_at_age_ [*] da_IYB_at_age_&firstage-da_IYB_at_age_&lastage;
		array da_othben_at_age_ [*] da_OTHBEN_at_age_&firstage-da_OTHBEN_at_age_&lastage;
		array da_ucb_at_age_ [*] da_UCB_at_age_&firstage-da_UCB_at_age_&lastage;
		array da_YP_at_age_ [*] da_YP_at_age_&firstage-da_YP_at_age_&lastage;
		array da_YPP_at_age_ [*] da_YPP_at_age_&firstage-da_YPP_at_age_&lastage;
		array da_SPSR_at_age_ [*] da_SPSR_at_age_&firstage-da_SPSR_at_age_&lastage;
		array da_JSWR_at_age_ [*] da_JSWR_at_age_&firstage-da_JSWR_at_age_&lastage;
		array da_JSWR_TR_at_age_ [*] da_JSWR_TR_at_age_&firstage-da_JSWR_TR_at_age_&lastage;
		array da_JSHCD_at_age_ [*] da_JSHCD_at_age_&firstage-da_JSHCD_at_age_&lastage;
		array da_SLP_C_at_age_ [*] da_SLP_C_at_age_&firstage-da_SLP_C_at_age_&lastage;
		array da_SLP_HCD_at_age_ [*] da_SLP_HCD_at_age_&firstage-da_SLP_HCD_at_age_&lastage;
		array da_OTH_at_age_ [*] da_OTH_at_age_&firstage-da_OTH_at_age_&lastage;

		do i=&firstage to &lastage;
			age=i-(&firstage-1);

			if total_da_onben_at_age_[age]=. then
				total_da_onben_at_age_[age]=0;

			if da_dpb_at_age_[age]=. then
				da_dpb_at_age_[age]=0;

			if da_ub_at_age_[age]=. then
				da_UB_at_age_[age]=0;

			if da_SB_at_age_[age]=. then
				da_SB_at_age_[age]=0;

			if da_IB_at_age_[age]=. then
				da_IB_at_age_[age]=0;

			if da_IYB_at_age_[age]=. then
				da_IYB_at_age_[age]=0;

			if da_OTHBEN_at_age_[age]=. then
				da_OTHBEN_at_age_[age]=0;

			if da_UCB_at_age_[age]=. then
				da_UCB_at_age_[age]=0;

			if da_YP_at_age_[age]=. then
				da_YP_at_age_[age]=0;

			if da_YPP_at_age_[age]=. then
				da_YPP_at_age_[age]=0;

			if da_SPSR_at_age_[age]=. then
				da_SPSR_at_age_[age]=0;

			if da_JSWR_at_age_[age]=. then
				da_JSWR_at_age_[age]=0;

			if da_JSWR_TR_at_age_[age]=. then
				da_JSWR_TR_at_age_[age]=0;

			if da_JSHCD_at_age_[age]=. then
				da_JSHCD_at_age_[age]=0;

			if da_SLP_C_at_age_[age]=. then
				da_SLP_C_at_age_[age]=0;

			if da_SLP_HCD_at_age_[age]=. then
				da_SLP_HCD_at_age_[age]=0;

			if da_OTH_at_age_[age]=. then
				da_OTH_at_age_[age]=0;

			* Now sensoring for not fully observed data;
			start_window=intnx('YEAR',DOB,age-1,'S');
			end_window=intnx('YEAR',DOB,age,'S');

			if ((end_window>"&sensor"d) or (start_window>"&sensor"d)) then
				total_da_onben_at_age_[age]=.;

			if ((end_window>"&sensor"d) or (start_window>"&sensor"d)) then
				da_dpb_at_age_[age]=.;

			if ((end_window>"&sensor"d) or (start_window>"&sensor"d)) then
				da_UB_at_age_[age]=.;

			if ((end_window>"&sensor"d) or (start_window>"&sensor"d)) then
				da_SB_at_age_[age]=.;

			if ((end_window>"&sensor"d) or (start_window>"&sensor"d)) then
				da_IB_at_age_[age]=.;

			if ((end_window>"&sensor"d) or (start_window>"&sensor"d)) then
				da_IYB_at_age_[age]=.;

			if ((end_window>"&sensor"d) or (start_window>"&sensor"d)) then
				da_OTHBEN_at_age_[age]=.;

			if ((end_window>"&sensor"d) or (start_window>"&sensor"d)) then
				da_YP_at_age_[age]=.;

			if ((end_window>"&sensor"d) or (start_window>"&sensor"d)) then
				da_YPP_at_age_[age]=.;

			if ((end_window>"&sensor"d) or (start_window>"&sensor"d)) then
				da_SPSR_at_age_[age]=.;

			if ((end_window>"&sensor"d) or (start_window>"&sensor"d)) then
				da_JSWR_at_age_[age]=.;

			if ((end_window>"&sensor"d) or (start_window>"&sensor"d)) then
				da_JSWR_TR_at_age_[age]=.;

			if ((end_window>"&sensor"d) or (start_window>"&sensor"d)) then
				da_JSHCD_at_age_[age]=.;

			if ((end_window>"&sensor"d) or (start_window>"&sensor"d)) then
				da_SLP_C_at_age_[age]=.;

			if ((end_window>"&sensor"d) or (start_window>"&sensor"d)) then
				da_SLP_HCD_at_age_[age]=.;

			if ((end_window>"&sensor"d) or (start_window>"&sensor"d)) then
				da_OTH_at_age_[age]=.;
		end;

		drop i age start_window end_window;

		if supp_dpb_atbirth=. then
			supp_dpb_atbirth=0;

		if supp_ub_atbirth=. then
			supp_ub_atbirth=0;

		if supp_sb_atbirth=. then
			supp_sb_atbirth=0;

		if supp_ib_atbirth=. then
			supp_ib_atbirth=0;

		if supp_othben_atbirth=. then
			supp_othben_atbirth=0;

		if supp_ucb_atbirth=. then
			supp_ucb_atbirth=0;

		if supp_YP_atbirth=. then
			supp_YP_atbirth=0;

		if supp_YPP_atbirth=. then
			supp_YPP_atbirth=0;

		if supp_SPSR_atbirth=. then
			supp_SPSR_atbirth=0;

		if supp_JSWR_atbirth=. then
			supp_JSWR_atbirth=0;

		if supp_JSWR_TR_atbirth=. then
			supp_JSWR_TR_atbirth=0;

		if supp_JSHCD_atbirth=. then
			supp_JSHCD_atbirth=0;

		if supp_SLP_C_atbirth=. then
			supp_SLP_C_atbirth=0;

		if supp_SLP_HCD_atbirth=. then
			supp_SLP_HCD_atbirth=0;

		if supp_OTH_atbirth=. then
			supp_OTH_atbirth=0;
	run;

%mend create_BDD_at_age_child;

%create_BDD_at_age_child(infile=feed_new1, outfile=_IND_BDD_child_at_age_&date);


**Global renaming as a child BDD variables;

data &folder.._IND_BDD_child_at_age_&date;
	set _IND_BDD_child_at_age_&date;
	rename total_da_onben_at_age_&firstage-total_da_onben_at_age_&lastage=ch_total_da_onben_at_age_&firstage-ch_total_da_onben_at_age_&lastage;
	rename da_DPB_at_age_&firstage-da_DPB_at_age_&lastage=ch_da_DPB_at_age_&firstage-ch_da_DPB_at_age_&lastage;
	rename da_UB_at_age_&firstage-da_UB_at_age_&lastage=ch_da_UB_at_age_&firstage-ch_da_UB_at_age_&lastage;
	rename da_SB_at_age_&firstage-da_SB_at_age_&lastage=ch_da_SB_at_age_&firstage-ch_da_SB_at_age_&lastage;
	rename da_IB_at_age_&firstage-da_IB_at_age_&lastage=ch_da_IB_at_age_&firstage-ch_da_IB_at_age_&lastage;
	rename da_IYB_at_age_&firstage-da_IYB_at_age_&lastage=ch_da_IYB_at_age_&firstage-ch_da_IYB_at_age_&lastage;
	rename da_OTHBEN_at_age_&firstage-da_OTHBEN_at_age_&lastage=ch_da_OTHBEN_at_age_&firstage-ch_da_OTHBEN_at_age_&lastage;
	rename da_UCB_at_age_&firstage-da_UCB_at_age_&lastage=ch_da_UCB_at_age_&firstage-ch_da_UCB_at_age_&lastage;
	rename da_YP_at_age_&firstage-da_YP_at_age_&lastage=ch_da_YP_at_age_&firstage-ch_da_YP_at_age_&lastage;
	rename da_YPP_at_age_&firstage-da_YPP_at_age_&lastage=ch_da_YPP_at_age_&firstage-ch_da_YPP_at_age_&lastage;
	rename da_SPSR_at_age_&firstage-da_SPSR_at_age_&lastage=ch_da_SPSR_at_age_&firstage-ch_da_SPSR_at_age_&lastage;
	rename da_JSWR_at_age_&firstage-da_JSWR_at_age_&lastage=ch_da_JSWR_at_age_&firstage-ch_da_JSWR_at_age_&lastage;
	rename da_JSWR_TR_at_age_&firstage-da_JSWR_TR_at_age_&lastage=ch_da_JSWR_TR_at_age_&firstage-ch_da_JSWR_TR_at_age_&lastage;
	rename da_JSHCD_at_age_&firstage-da_JSHCD_at_age_&lastage=ch_da_JSHCD_at_age_&firstage-ch_da_JSHCD_at_age_&lastage;
	rename da_SLP_C_at_age_&firstage-da_SLP_C_at_age_&lastage=ch_da_SLP_C_at_age_&firstage-ch_da_SLP_C_at_age_&lastage;
	rename da_SLP_HCD_at_age_&firstage-da_SLP_HCD_at_age_&lastage=ch_da_SLP_HCD_at_age_&firstage-ch_da_SLP_HCD_at_age_&lastage;
	rename da_OTH_at_age_&firstage-da_OTH_at_age_&lastage=ch_da_OTH_at_age_&firstage-ch_da_OTH_at_age_&lastage;
run;




*********************************************************************************************;
***D PARENTAL CORRECTIONS HISTORY;
***the PARENT TO CHILD MAP dataset was created by the program by Chris Ball titled 
   "Creating_integrated_relationships_24022016" which is saved
   along with the other YS-NEET evaluation programs;
**But I haven't rerun it in 2017. In future, the best available approach for mapping relationships should be used - eg
  the A&I program titled "Relationships_macro_new", also saved in the project folder ;
********************************************************************************************;


**Derive highest child age for use later;
proc sql;
	select max(floor(yrdif(dob,"&sensor."d))) into: maxage separated by "" from &population.;
quit;

*%put *** The maximum age in population is &maxage.;

* Sort the parent to child event file by eventdate (= first date at which parent and child connected);
proc sort data=suppdata.parenttochildmap_&date out=parenttochildmap_&date;
	by snz_uid parent event_date;
run;

* Now eliminate the multiple event parent event records (keeping the first recorded eventdate);
proc sort data=parenttochildmap_&date out=parenttochildmap nodupkey;
	by snz_uid parent;
run;

* Now merge with the current population file and only keep the children we are interested in;
* On this dataset we have child DOB and eventdate = first time they are associated with parent;
data population_children_parent_map child_parentcount;
	merge &population(in=inpop) parenttochildmap;
	by snz_uid;

	if inpop;

	if first.snz_uid then
		parent_count = 0;
	retain parent_count;

	if parent ne . then
		parent_count + 1;
	noparent = parent_count = 0;

	if last.snz_uid then
		output child_parentcount;
	output population_children_parent_map;
run;

/*proc sql; select sum(noparent)/sum(1) as prop_no_parent from population_children_parent_map;quit; **7.4% with no parent;*/

* Count how many children each parent is associated with;
proc sort data=population_children_parent_map;
	by parent;
run;

data parents_single_record parents_children;
	set population_children_parent_map;
	by parent;
	child_snz_uid = snz_uid;
	snz_uid = parent; 

	if first.parent then
		countchild = 1;
	retain countchild;

	if first.parent = 0 and parent ne . then
		countchild + 1;

	if parent ne . then
		output parents_children;

	if last.parent and  parent ne . then
		output parents_single_record;
run;

* Max count is 4  --> commit this to a macro variable for use in loop later;
proc sql;
	select max(countchild) into: maxchild separated by "" from parents_single_record;
quit;

*%put *** The maximum number of children per parent is &maxchild.;

* Extracting Correction Records;
* Add a couple of variables to corrections sentence data;

data cor_0;
	set cor.ov_major_mgmt_periods;
	format period_start_date period_end_date date9.;
	period_start_date = input(compress(cor_mmp_period_start_date,"-"),yymmdd10.);
	period_end_date = input(compress(cor_mmp_period_end_date,"-"),yymmdd10.);
	mmc_code = cor_mmp_mmc_code;

run;

proc sql;
	create table cor as
		select distinct 
			snz_uid,		
			period_start_date  as startdate,
			period_end_date as enddate,
			mmc_code

		from cor_0

		where 
			mmc_code in 
			('PRISON','REMAND','HD_SENT','HD_REL','ESO','PAROLE',
			'ROC','PDC','PERIODIC', 'COM_DET','CW','COM_PROG','COM_SERV','OTH_COMM',
			'INT_SUPER','SUPER') 
			and 
			snz_uid in 
		(select distinct snz_uid from parents_single_record)
			order by snz_uid;
quit;

* Create wider correction sentence groupings;
* Delete any very recent records i.e. past the censoring date;
data cor2;
	set cor;
	offence = 'COR_COMM';

	if mmc_code = 'PRISON' or mmc_code='REMAND' or mmc_code = 'HD_SENT' 
		or mmc_code = 'HD_REL' then
		offence = 'COR_INTE';

	if startdate > "&sensor."d then
		delete;
if enddate>"&sensor"d then enddate="&sensor"d;

	drop mmc_code;

	* grouping for sentencing of serious type of crime;
run;
*/

** Merging clean corrections with parent_children map;
proc sort data = cor2;
	by snz_uid;
run;

proc sort data = parents_children;
	by snz_uid;
run;

* This macro merges the corrections spells of each parent to one of their children;
* It will be run separately for each child using a loop (and the maxchild macro created earlier);
%macro perchild(num);

	data cor_child_&num.;
		merge parents_children (in = a where = (countchild = &num.)) 
			cor2(in = b );
		by snz_uid;

		if a and b;

		* Only count sentences served with 5 year window before adult was associated with child;
		if intnx('YEAR',enddate,5,'sameday') < (event_date) then
			delete;
	run;

%mend;

%macro run_perchild;
	%do i = 1 %to &maxchild.;
		%perchild(&i.);
	%end;
%mend;

%run_perchild;

* Append these datasets together;
data cor_all_children;
	set cor_child_1-cor_child_&maxchild.;
run;

**************************************************************************************************************************
**************************************************************************************************************************
Creating indicators

**************************************************************************************************************************
**************************************************************************************************************************
*** Create correction vars by age of child - per child caregiver;
proc sort data = cor_all_children;
	by snz_uid enddate;
run;

**** By age;
%macro cor_at_age(x);

	data cor_time_age_&x.;
		set cor_all_children;
		by snz_uid enddate;
		format date_age_&x. date_age_&x._plus1 date9.;
		date_age_&x. = intnx('YEAR',dob,&x.,'sameday');

		* Initialise;

		cg_cust_da_at_age_&x. = 0;
		cg_comm_da_at_age_&x. = 0;

		* Amend enddate so that we create a 1 year follow-on window;
		date_age_&x._plus1 = intnx('YEAR',date_age_&x.,+1,'sameday');

		if startdate > "&sensor."d then
			delete;

		if enddate > "&sensor."d then
			enddate = "&sensor."d;

		%overlap_days(startdate,enddate,date_age_&x.,date_age_&x._plus1,days);

		* Only count the days if child and adult associated before age x;
		* Note: variable eventdate here is when child and adult first associated;

		* Usually this is birth date but sometimes it is when new guardian or partner 
				is linked in via MSD;
		if offence = 'COR_COMM' and event_date < date_age_&x._plus1 then
			cg_comm_da_at_age_&x. = days;

		if offence = 'COR_INTE' and event_date < date_age_&x._plus1 then
			cg_cust_da_at_age_&x. = days;
		drop days;
	run;

	proc summary data = cor_time_age_&x. nway;
		class snz_uid child_snz_uid;
		var cg_cust_da_at_age_&x. cg_comm_da_at_age_&x.;
		output out = cor_at_age_&x. sum=;
	run;

	data cor_at_age_&x.;
		set cor_at_age_&x.(drop = _: );
	run;

	proc sql;
		drop table cor_time_age_&x.;
	quit;

%mend;

***NOTE: firstage and lastage not set in core program yet -- using maxage macro for now;
%macro do_all_ages;
	/*	%do i=&firstage. %to &lastage.;*/
	%do i = &firstage. %to &maxage.;
		%cor_at_age(&i.);
	%end;
%mend;

%do_all_ages;

* Also do at birth;
data cor_time_birth;
	set cor_all_children;
	format minus5 date9.;
	by snz_uid enddate;
	cg_cust_da_at_birth = 0;
	cg_comm_da_at_birth = 0;

	* Amend enddate so that we create 5 year follow-on window;
	minus5 = intnx('YEAR',dob,-5,'sameday');

	if startdate > "&sensor."d then
		delete;

	if enddate > "&sensor."d then
		enddate = "&sensor."d;

	%overlap_days(startdate,enddate,minus5,dob,days);

	* Only count the days if child and adult associated before age x;
	* Note: varianle eventdate here is when child and adult first associated;
	* Usually this is birth date but sometimes it is when new guardian or partner is linked in via MSD;
	if offence = 'COR_COMM' and event_date < dob then
		cg_comm_da_at_birth = days;

	if offence = 'COR_INTE' and event_date < dob then
		cg_cust_da_at_birth = days;
	drop days;
run;

proc summary data = cor_time_birth nway;
	class snz_uid child_snz_uid;
	var cg_cust_da_at_birth cg_comm_da_at_birth;
	output out = cor_at_birth sum=;
run;

data cor_at_birth;
	set cor_at_birth(drop = _: );
run;

* Put together all of the the 'at age' variables;
%macro include_all;
	/*	%do i=&firstage. %to &lastage.;*/
	%do i = &firstage. %to &maxage.;
		cor_at_age_&i.
	%end;
%mend;

*%include_all;

data _summary;
	merge cor_at_birth cor_at_age_0-cor_at_age_&maxage.;
	by snz_uid child_snz_uid;
run;

* Aggregate data down to become child records;
proc summary data = _summary nway;
	class child_snz_uid;
	var cg_cust_da_at_birth cg_comm_da_at_birth cg_cust_da_at_age_0-cg_cust_da_at_age_&maxage. 
		cg_comm_da_at_age_0-cg_comm_da_at_age_&maxage.;
	;
	output out = cor_final(rename= (child_snz_uid=snz_uid) drop = _FREQ_ _TYPE_) sum=;
run;

****  Permanenet datasets created from here*********************;
data &folder.._ind_cg_corr_at_age_&date.;
	merge  
		cor_final (in=b) 
		&population. (in=a);
	by snz_uid;

	if a;
	array cg_cust_da_at_age_(*) cg_cust_da_at_age_&firstage.-cg_cust_da_at_age_&maxage.;
	array cg_comm_da_at_age_(*) cg_comm_da_at_age_&firstage.-cg_comm_da_at_age_&maxage.;
	array cg_cust_at_age_(*) cg_cust_at_age_&firstage.-cg_cust_at_age_&maxage.;
	array cg_comm_at_age_(*) cg_comm_at_age_&firstage.-cg_comm_at_age_&maxage.;

	if b then
		do;
			cg_cust_at_birth = cg_cust_da_at_birth > 0;
			cg_comm_at_birth = cg_comm_da_at_birth > 0;

			do ind = &firstage. to &maxage.;
				i = ind-(&firstage.-1);
				cg_cust_at_age_(i) = cg_cust_da_at_age_(i) > 0;
				cg_comm_at_age_(i) = cg_comm_da_at_age_(i) > 0;
			end;
		end;

	if not b then
		do;
			cg_cust_at_birth = 0;
			cg_comm_at_birth = 0;

			do ind = &firstage. to &maxage.;
				i = ind-(&firstage.-1);
				cg_cust_at_age_(i) = 0;
				cg_comm_at_age_(i) = 0;
			end;
		end;

	* Set to missing if these people  have not reached these ages yet;
	do ind = &firstage. to &maxage.;
		i = ind-(&firstage.-1);

		if intnx('YEAR', dob,(i-1),'sameday') > "&sensor."d then
			cg_cust_at_age_(i)= .;

		if intnx('YEAR', dob,(i-1),'sameday') > "&sensor."d then
			cg_comm_at_age_(i)= .;
	end;

	*Set missing records to zero;
	array zero _numeric_;

	do over zero;
		if zero = . then
			zero = 0;
	end;

	keep snz_uid DOB cg_cust_at_age_&firstage.-cg_cust_at_age_&maxage.
		cg_comm_at_age_&firstage.-cg_comm_at_age_&maxage.
		cg_cust_at_birth cg_comm_at_birth;
run;











********************************************************;
**E MATERNAL EDUCATION;
**Note this requires the dataset "childtoparentmap_&date" which is generated separately;
**See the program by Chris Ball titled 
   "Creating_integrated_relationships_24022016" which is saved
    along with the other YS-NEET evaluation programs;
******************************************************;

%macro get_all_mother_new;
	
	data ChildParentMap;
		set suppdata.childtoparentmap_&date;
	P1Sex=parent1_sex;
	P2Sex=Parent2_sex;
	run;

	data TEMP;
		set &population.
			(keep=snz_uid DOB);
	run;

	proc sql;
		create table birth_cohort as
			select  a.*, 
					b.event_end,
					b.source,
					b.parent1_snz_uid,
					b.parent1_spine,
					b.parent1_snz_sex_code,
					b.parent2_snz_uid,
					b.parent2_spine,
					b.parent2_snz_sex_code,
					b.event_date,
					b.parent1,
					b.parent2,
					b.parent1_sex,
					b.parent1_onspine,
					b.parent2_sex,
					b.parent2_onspine,
					b.P1Sex,
					b.P2Sex
			from TEMP a LEFT JOIN ChildParentMap b
					on a.snz_uid = b.snz_uid;
	quit;

	data ChildMother;
		set birth_cohort;
		if P1Sex = 2 then
			Mother = Parent1;
		if P2Sex = 2 then
			Mother = Parent2;
		if MISSING(Mother) then
			DELETE;
	run;

%mend;

%let population=&folder..popn;

%get_all_mother_new;

* this macro gets mothers for popualtion;

data mother; set childmother; keep mother DOB; DOB=.; *Creating fake variable for mother's DOB;
rename mother=snz_uid;
run;

%let population=mother;

**************************************************************************************************************************************
**************************************************************************************************************************************
SECONDARY SCHOOL QUALIFICATIONS SECONDARY SCHOOL QUALIFICATIONS SECONDARY SCHOOL QUALIFICATIONS SECONDARY SCHOOL QUALIFICATIONS
NCEA QUAL NCEA QUAL NCEA QUAL NCEA QUAL NCEA QUAL
**************************************************************************************************************************************
**************************************************************************************************************************************;

* FORMATING DATES;
* NQZA Attainment code;
* Will create qualification table SEC_QUAL for reference population;
* Will contain DOB of ref person;

%macro creating_clean_qual_table;
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

* FORMATING DATES;

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

proc sql;
create table sec_qual as
select distinct
      a.*
      ,b.DOB
from student_qual a right join &population b
on a.snz_uid=b.snz_uid
order by snz_uid, year;
quit;

%mend;

%creating_clean_qual_table;


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

* BUSINESS RULES TO DEFINE NCEA ATTAINMENT;

DATA sec_qual_event;
	merge sec_qual(in=a drop=DOB) qual_lookup(in=b);
	by qual;

	if a;
HA=0;
* Allows 2 years for loading qualifications;
/*if year=load_year or load_year-year<=2 or load_year=.; */
if NQFlevel in (0,.) then delete;

if year < 2003 then delete; 
if year>=&first_anal_yr and year<=&last_anal_yr;

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
* Highest school qualification , not necessarily NCEA ;
level=0;
if HA in (19,18,17,16,15) then level=1;
if HA in (29,28,27,26,25) then level=2;
if HA in (39,38,37,36,35) then level=3;
if HA in (42,41,40) then level=4;

	qual_type='SCH';
	format startdate enddate date9.;
	startdate=MDY(12,31,year);
	enddate=startdate;

	* Allows 2 years for loading qualifications;
	if year=load_year or load_year-year<=2 or load_year=.;
	keep snz_uid year startdate enddate qual level qual_type;
run;

proc sort data=sec_qual_event nodupkey;
	by snz_uid year startdate enddate qual level qual_type;
run;

**************************************************************************************************************************************************************************************
**************************************************************************************************************************************************************************************
TERTIARY QUAL
Creating event table 
**************************************************************************************************************************************************************************************
**************************************************************************************************************************************************************************************;
proc format;
	value $lv8id
		"40","41","46", "60", "96", "98"      ="1"
		"36"-"37","43"                        ="2"
		"30"-"35"                       ="3"
		"20","25","21"                  ="4"
		"12"-"14"                       ="6"
		"11"                            ="7"
		"01","10"                       ="8"
		"90", "97", "99"                ="9"
		Other                           ="E";
run;

proc sql;
	create table TER_compl as
		select  snz_uid,
			moe_com_year_nbr,
			put(moe_com_qacc_code,$lv8id.) as att_TER_qual_type,
			moe_com_qual_level_code as raw_level,
			moe_com_qual_nzsced_code
		from moe.completion
			where snz_uid in
				(select distinct snz_uid from &population)
					and MDY(12,31,moe_com_year_nbr)<="&sensor"d;
quit;

proc freq data=ter_compl;
	tables att_TER_qual_type*raw_level/list missing;
run;
* There are some levels are missing ( not many) and some levels seems to be too high;
* Giving priority to qual type classification by MOE and overwriting Level variable;
data Ter_qual_event;
	set Ter_compl;
	ter_qual=att_TER_qual_type*1;
	Ter_level=raw_level*1;
* Level 1-3 Tertiary rtificates;
	IF att_ter_qual_type=1 and (raw_level=. or raw_level=1) then
		level=1; * few missing set to level 1;

	IF att_ter_qual_type=1 and raw_level=2 then
		level=2;

	IF att_ter_qual_type=1 and raw_level>=3 then
		level=3; * some have level 4,5 and 10... setting them to Level 3;

* Level 4 Tertiary certificates;

	IF att_ter_qual_type=2 and (raw_level=. or raw_level<=4)  then
		level=4;

	IF att_ter_qual_type=2 and raw_level>4 then
		level=4;

* Tertiary diplomas;
	IF att_ter_qual_type=3 and (raw_level=. or raw_level<=5)  then
		level=5;

	IF att_ter_qual_type=3 and raw_level>=6 then
		level=6;

* Bachelor degrees;
	IF att_ter_qual_type=4 and (raw_level=. or raw_level<=7) then
		level=7;

* Postgraduate degrees ;

	IF att_ter_qual_type=6  then
		level=8;

* Masters and PHDs;
	IF att_ter_qual_type=7 then
		level=9;

	IF att_ter_qual_type=8  then
		level=10;


	qual_type='TER';
	format startdate enddate date9.;
	startdate=MDY(12,31,moe_com_year_nbr);
	enddate=startdate;
	if moe_com_year_nbr>=&first_anal_yr or moe_com_year_nbr<=&last_anal_yr;
year=moe_com_year_nbr;
keep snz_uid year startdate enddate level qual_type;
run;


************************************************************************************************************************************************************
************************************************************************************************************************************************************
Industry training qualifications
************************************************************************************************************************************************************
************************************************************************************************************************************************************;

* Calculating credits in a year, which will be used as filter for participation;
* FORMATING, SENSORING AND CLEANING;

data it deletes;
	set moe.tec_it_learner;

	if moe_itl_programme_type_code in ("NC","TC");

	format startdate enddate date9.;
	startdate=input(compress(moe_itl_start_date,"-"),yymmdd10.);

	if moe_itl_end_date ne '' then
		enddate=input(compress(moe_itl_end_date,"-"),yymmdd10.);

	if moe_itl_end_date='' then
		enddate="&sensor"d;

	if startdate>"&sensor"d then
		output deletes;

	if enddate>"&sensor"d then
		enddate="&sensor"d;

	if startdate>enddate then
		output deletes;
	else output it;
run;

proc sql;
	create table it_qual as 
		SELECT distinct
			snz_uid
			,moe_itl_year_nbr as year 
			,startdate 
			,enddate
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
				ORDER by snz_uid, year,startdate;
quit;
* most of the quals 1-4, some 5 and 6, setting them to 4;
data IT_qual_event; set it_qual;
level=0;
	if L1=1 then level=1;
	if L2=1 then level=2;
	if L3=1 then level=3;
	if L4=1 then level=4;
	if L5=1 then level=4;
	if L6=1 then level=4;
	if L7=1 then level=4;
	if L8=1 then level=4;
if level>0;
* We want qualification to be counted once so using enddate as qualification attainment date;
startdate=enddate;
qual_type='ITL';
keep snz_uid startdate enddate level qual_type year;
run;

proc freq data=it_qual_event; tables level;run;

***************************************************************************************************************************************************************
***************************************************************************************************************************************************************;
proc sort data=SEC_QUAL_EVENT; by snz_uid;
proc sort data=TER_QUAL_EVENT; by snz_uid;
proc sort data=IT_QUAL_EVENT; by snz_uid;

* This is event based table for all qualifications;
* We assuming that qualifications attained in one day rather than period, hence it should fall once within or outside the "at age" window';


* For year long file, choosing the highest qualification in a year;
data Qual_event;
	set SEC_QUAL_EVENT TER_QUAL_EVENT IT_QUAL_EVENT;
	by snz_uid;
drop qual;
run;

* now we have event table , lets bring information of children;
proc sql;
create table Mother_qual_event
as select
	a.snz_uid as mother,
	a.startdate,
	a.enddate,
	a.qual_type,
	a.level,
	b.snz_uid,
	b.DOB
from  Qual_event a inner join childmother b
on a.snz_uid=b.mother;

* creating at age variable;

data qual_event_at_age;
	set Mother_qual_event;
	array high_qual_at_age_(*) high_qual_at_age_&firstage- high_qual_at_age_&lastage;

	do ind = &firstage to &lastage;
		i=ind-(&firstage-1);

		start_window=intnx('YEAR',DOB,i-1,'S');
		end_window=intnx('YEAR',DOB,i,'S');

		* events by selected birthdays;
		if ((startdate <end_window) and (startdate>=start_window)) then
			do;
				high_qual_at_age_(i)=level;
			end;
		if startdate<DOB then high_qual_before_birth=level;
	end;
run;

proc summary data=qual_event_at_age nway;
class snz_uid DOB;
var high_qual_before_birth high_qual_at_age_&firstage- high_qual_at_age_&lastage;
output out= TEMP (drop=_type_ _freq_) max=;
run; 

data Mat_Edu_Ter_NCEA_&date;
	set TEMP;
	array high_qual_at_age_(*) high_qual_at_age_&firstage- high_qual_at_age_&lastage;

	do ind=&firstage to &lastage;
		i=ind-(&firstage-1);

		* Now sensoring for not fully observed data;
		start_window=intnx('YEAR',DOB,i-1,'S');
		end_window=intnx('YEAR',DOB,i,'S');

		if ((end_window>"&sensor"d) or (start_window>"&sensor"d)) then
			high_qual_at_age_(i)=.;
	end;

drop i ind start_window end_window;
run;



* create education level format;
* Sarah Tumen: Separated Degree and professional qual to level 4 ;
proc format;
	value $BDD_edu
		'A', 'B', 'I' = 0
		'C', 'J' = 1
		'D', 'F', 'K' = 2
		'E', 'L','M','G'= 3
		'H' = 4
		other = .
	;
run;
data birth_cohort;
	set &population;
	/*	(keep=snz_uid DOB snz_birth_year_nbr snz_dia_uid
		 where=(snz_birth_year_nbr>=&cohort_start and snz_dia_uid));*/
run;

proc sql;
	create table birth_cohort as
	select a.*, b.*
	from birth_cohort a LEFT JOIN ChildParentMap b
	on a.snz_uid = b.snz_uid;
quit;

* Import the msd education data set;
proc sql;
	connect to odbc(dsn="idi_clean_&version._srvprd");
	create table work.MSDEducation as
	select *
	from connection to odbc
	( select * from msd_clean.msd_education_history order by snz_uid);
	disconnect from odbc;
quit;

* create a simplified education level;
data MSDEducation;
	set MSDEducation;
	education_level = put(msd_edh_education_code, $BDD_edu.);
run;
* Go through the Education dataset and take the most recent observation for each person;
* This is an assumption that avoids nasty event overlap math;
data MSDEducation;
	format StartDate EndDate ddmmyy10.;
	set MSDEducation;

	StartDate = input(msd_edh_educ_lvl_start_date_text, ddmmyy10.);
	EndDate = input(msd_edh_educ_lvl_end_date_text, ddmmyy10.);
run;


* Merge on the MSD education data;
* 32% are missing...;
proc sql;
	create table MaternalEducation as
	select a.*, 
		b.StartDate, b.EndDate, b.msd_edh_education_code, 
		b.msd_edh_education_desc_text, b.education_level
	from ChildMother a left join MSDEducation b
	on a.Mother = b.snz_uid
/*	group by Mother*/
	order by snz_uid;
quit;


* Work out maternal education at age;
*** FIX!! ****;
* If the spell fall between bdates, it won't be considered;
* probably very small portion of cases;
* way to fix, should do a loop from age at start and age at the end of the spell;
* or maybe just consider the age at the start;
data MaternalEducationLongWide;
	set MaternalEducation;
	array Maternal_Edu_at_age_(*) Maternal_Edu_at_age_&firstage-Maternal_Edu_at_age_&lastage;

	DO ind = &firstage to &lastage; i=ind-(&firstage-1);
		if StartDate <=intnx('YEAR',DOB,i,'S')  and (intnx('YEAR',DOB,i,'S')<= EndDate) 
			then Maternal_Edu_at_age_(i) = education_level*1;
		else Maternal_Edu_at_age_(i) = .;
	END;

	if EndDate lt DOB then maternal_edu_prior_birth = education_level*1;
drop i ind;
run;

proc summary data=MaternalEducationLongWide nway;
	class snz_uid;
	var maternal_edu_prior_birth 
		Maternal_Edu_at_age_&firstage-Maternal_Edu_at_age_&lastage;
	output out=MaternalEducationWide(drop=_type_ _freq_) max=;
run;

* Finally output a data set;
data Mat_Edu_BDD_&date;
	set MaternalEducationWide; 
run;


* aggregate all sources of information;

data MaternalEducationWide;
	set Mat_Edu_BDD_&date (in=a)
	    Mat_Edu_Ter_NCEA_&date (in=b);

array maternal_edu_at_age_(*) maternal_edu_at_age_&firstage-maternal_edu_at_age_&lastage;
array high_qual_at_age_(*) high_qual_at_age_&firstage- high_qual_at_age_&lastage;

do ind=&firstage to &lastage;
   i=ind-(&firstage-1);

	if b then do;
		maternal_edu_at_age_(i)=high_qual_at_age_(i);
		maternal_edu_prior_birth=high_qual_before_birth;
	end;
end;
run;

* keeping the highest education level per age;
* 60% missing - only 0.5% of increased coverage from BDD data only!!;
* if more education information is available, we could prioritise
  the source (BDD vs MoE). At the moment it is not worth the effort...;
proc summary data=MaternalEducationWide nway;
	class snz_uid;
	var maternal_edu_prior_birth 
		Maternal_Edu_at_age_&firstage-Maternal_Edu_at_age_&lastage;
	output out=MaternalEducationWide1(drop=_type_ _freq_) max=;
run;


**Get mother's highest qualification at each year of age for child;
**Missing for a high proportion of mothers with no MSD record or tertiary qualifications data;
**We are only going to use the information to identify the mothers that MSD recorded as having no quals (and our 
   other qualifications data do not contradict this); 

%let firstage=0;
%let lastage=17;

data &folder..Mat_Educ_&date(keep=snz_uid highest_mat_edu_at_age_&firstage-highest_mat_edu_at_age_&lastage);
set MaternalEducationWide1; 
array maternal_edu_at_age (*) maternal_edu_at_age_&firstage-maternal_edu_at_age_&lastage;
array highest_mat_edu_at_age (*) highest_mat_edu_at_age_&firstage-highest_mat_edu_at_age_&lastage;
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
run;

proc freq data=&folder..Mat_Educ_&date;
tables highest_mat_edu_at_age_&firstage-highest_mat_edu_at_age_&lastage;
run;

%let population=&folder..popn;






*********************************************************;
**F INTERVENTIONS AT SCHOOL - SUSPENSIONS ETC;
**********************************************************;

%macro creating_clean_interv_table; 
data STUDENTINTERVENTIONS;
	set moe.student_interventions;
	format startdate enddate extractiondate date9.;
	startdate=input(compress(moe_inv_start_date,"-"),yymmdd10.);
	enddate=input(compress(moe_inv_end_date,"-"),yymmdd10.);
	extractiondate=input(compress(moe_inv_extrtn_date,"-"),yymmdd10.);
	InterventionID=moe_inv_intrvtn_code*1;

	if enddate='31Dec9999'd then
		enddate=Extractiondate;

	if enddate=. then
		enddate=ExtractionDate;

	if enddate>=startdate;

	* cleaning for errorness records;
	if startdate>"&sensor"d then
		delete;

	if enddate>"&sensor"d then
		enddate="&sensor"d;
run;


proc format;
	value interv_grp
		5='ESOL'
		6,17='AlTED'
		7='SUSP'
		8='STAND'
		9,32='TRUA'
		12,26,29,24,25,27,28,30='SEDU'
		10='EARLEX'
		11='HOMESCH'
		13,14='BOARD'
		16,31='OTHINT'
		33='HEALTH'
		34,35='SECTER'
		37='IRF';
run;

proc sql;
	create table interventions as select 
		b.snz_uid
		,b.DOB
		,a.InterventionID as interv
		,a.startdate
		,a.enddate
		,put(a.InterventionID,interv_grp.) as interv_grp	  	 
	from STUDENTINTERVENTIONS a inner join &population b 
		on a.snz_uid=b.snz_uid
	order by b.snz_uid;
quit;

%mend;
%creating_clean_interv_table;


* Grouping interventions into groader categores
5	ESOL (English for Speakers of Other Languages)- should not appear for our domestic students
6	Alternative Education- for kids that dont fit into mainstream schools
7	Suspensions-suspections
8	Stand downs-suspentions
9	Non Enrolment Truancy Services-TRUANCY
10	Early Leaving exemptions-other
11	Homeschooling-Homeschooling
12	Section 9-enrolment over legal age
13	Mapihi Pounamu-similar to Boarding burs
14	Boarding bursaries
16	Reading Recovery
17	Off Sites Centres (teen parenting, altern education centres )
24	Special Education Service-SPECIAL EDU ( Physical and mental disabilities)
25	ORRS					-SPECIAL EDU ( Physical and mental disabilities)
26	Over 19 at Secondary	
27	High Health				-SPECIAL EDU ( Physical and mental disabilities)
28	Special School			-SPECIAL EDU ( Physical and mental disabilities)
29	Over 14 at Primary-		-LEARNING DIFF
30	SE Other
31	Resource Teachers: Literacy
32	Truancy (Unjustified Absence)-TRUANCY
36	ENROL- no records
33 HEaring and eye check-HEALTH 
34 Gateway - SECTER
35 Trade academies-SECTER
37 Interium response fund-IRF

******************************************************;
proc sort data=interventions;
	by  snz_uid interv_grp startDate;
run;

* Spliting dataset by each intervention type;
%macro interv(interv);

	data &interv;
		set interventions;

		if interv_grp="&interv";
		keep snz_uid DOB interv_grp startDate enddate;
	run;

%mend;

%interv(AlTED);
%interv(SUSP);
%interv(STAND);
%interv(TRUA);
%interv(SEDU);
%interv(ESOL);
%interv(EARLEX);
%interv(HOMESCH);
%interv(BOARD);
%interv(OTHINT);
%interv(HEALTH);
%interv(SECTER);
%interv(IRF);

* checking for overlap;
%overlap(AlTED);
%overlap(SUSP);
%overlap(STAND);
%overlap(TRUA);
%overlap(SEDU);
%overlap(ESOL);
%overlap(EARLEX);
%overlap(HOMESCH);
%overlap(BOARD);
%overlap(OTHINT);
%overlap(HEALTH);
%overlap(SECTER);
%overlap(IRF);

%let firstage=0;
%let lastage=19;

* Calcualting Interventions at age 1-24;
%macro at_age_calc(intervention);
	data &intervention;
		set &intervention._OR;
		array &intervention._da_at_age_(*) &intervention._da_at_age_&firstage-&intervention._da_at_age_&lastage;

		do ind=&firstage to &lastage;
			i=ind-(&firstage-1);
			&intervention._da_at_age_(i)=0;
			start_window=intnx('YEAR',DOB,i-1,'S');
			end_window=intnx('YEAR',DOB,i,'S');

			if not((startdate > end_window) or (enddate < start_window)) then
				do;
					if (startdate <= start_window) and  (enddate > end_window) then
						days=(end_window-start_window)+1;
					else if (startdate <= start_window) and  (enddate <= end_window) then
						days=(enddate-start_window)+1;
					else if (startdate > start_window) and  (enddate <= end_window) then
						days=(enddate-startdate)+1;
					else if (startdate > start_window) and  (enddate > end_window) then
						days=(end_window-startdate)+1;
					&intervention._da_at_age_[i]=days;
				end;
		end;

		keep snz_uid &intervention._da_at_age_&firstage-&intervention._da_at_age_&lastage;
	run;

	proc summary data=&intervention nway;
		class snz_uid;
		var &intervention._da_at_age_&firstage-&intervention._da_at_age_&lastage;
		output out=temp (drop=_type_ _freq_) sum=;
	run;

	data &intervention._at_age;
		merge &population(keep=snz_uid DOB) temp;
		by snz_uid;
		array &intervention._da_at_age_(*) &intervention._da_at_age_&firstage-&intervention._da_at_age_&lastage;

		do ind=&firstage to &lastage;
			i=ind-(&firstage-1);

			if &intervention._da_at_age_(i)=. then
				&intervention._da_at_age_(i)=0;

			* Now sensoring for not fully observed data;
			start_window=intnx('YEAR',DOB,i-1,'S');
			end_window=intnx('YEAR',DOB,i,'S');

			if ((end_window>"&sensor"d) or (start_window>"&sensor"d)) then
				&intervention._da_at_age_(i)=.;
		end;

		drop ind i start_window end_window;
	run;

%mend;

%at_age_calc(AlTED);
%at_age_calc(SUSP);
%at_age_calc(STAND);
%at_age_calc(TRUA);
%at_age_calc(SEDU);
%at_age_calc(ESOL);
%at_age_calc(EARLEX);
%at_age_calc(HOMESCH);
%at_age_calc(BOARD);
%at_age_calc(OTHINT);
%at_age_calc(HEALTH);
%at_age_calc(SECTER);
%at_age_calc(IRF);


data &folder.._IND_INTERVEN_at_age_&date;
	merge AlTED_at_age SUSP_at_age STAND_at_age TRUA_at_age SEDU_at_age ESOL_at_age
		EARLEX_at_age  HOMESCH_at_age BOARD_at_age OTHINT_at_age HEALTH_at_age SECTER_at_age IRF_at_age;
	by snz_uid;
run;


*************************************************;
**G DAYS OVERSEAS AT EACH AGE;
**************************************************;


%let firstage=0;
%let lastage=19;

proc sql;
	Connect to sqlservr (server=WPRDSQL36\iLeed database=IDI_clean_&Version );
	/*	create table mbie as select * from connection to  sqlservr (*/
	create table Overseas as 
		SELECT 
			a.snz_uid,
			pos_applied_date as startdate,
			pos_ceased_date as enddate
		FROM data.person_overseas_spell a
			join (SELECT DISTINCT snz_uid FROM &population) b
				on a.snz_uid = b.snz_uid
			ORDER BY a.snz_uid, pos_applied_date;
quit;

data OS;
	set Overseas;
	format startdate1 enddate1 date9.;
	startdate1=datepart(startdate);
	enddate1=datepart(enddate);
	drop startdate enddate;
	rename startdate1=startdate enddate1=enddate;
run;

proc sql;
	create table 
		OS_1 as select
		a.*, 
		b.DOB
	from OS a left join &population b
		on a.snz_uid=b.snz_uid;

data OS_2;
	set OS_1;
	if startdate<"&sensor"d;

	if enddate>"&sensor"d then
		enddate="&sensor"d;

	if startdate<DOB and enddate>DOB then
		startdate=DOB;

	* making sure no records appear prior to person's DOB;
	if startdate <DOB and enddate<DOB then
		delete;
run;

%overlap(OS_2);

* creating at age variables;
data OS_3_OR;
	set OS_2_OR;
	array OS_da_at_age_(*)	OS_da_at_age_&firstage-OS_da_at_age_&lastage;

	do ind=&firstage to &lastage;
		i=ind-(&firstage-1);
		OS_da_at_age_(i)=0;
		start_window=intnx('YEAR',DOB,i-1,'S');
		end_window=intnx('YEAR',DOB,i,'S')-1;

		if not((startdate > end_window) or (enddate < start_window)) then
			do;
				if (startdate <= start_window) and  (enddate > end_window) then
					days=(end_window-start_window)+1;
				else if (startdate <= start_window) and  (enddate <= end_window) then
					days=(enddate-start_window)+1;
				else if (startdate > start_window) and  (enddate <= end_window) then
					days=(enddate-startdate)+1;
				else if (startdate > start_window) and  (enddate > end_window) then
					days=(end_window-startdate)+1;
				OS_da_at_age_(i)=days;
			end;
	end;
run;

proc summary data=OS_3_OR nway;
	class snz_uid;
	var OS_da_at_age_&firstage-OS_da_at_age_&lastage;
	output out= TEMP (drop=_TYPE_ _FREQ_) sum=;
run;

data &folder.._ind_OS_spells_at_age_&date;
	merge &population(keep=snz_uid DOB) TEMP;
	by snz_uid;
	array OS_da_at_age_(*)	OS_da_at_age_&firstage-OS_da_at_age_&lastage;

	do ind=&firstage to &lastage;
		i=ind-(&firstage-1);

		if OS_da_at_age_(i)=. then
			OS_da_at_age_(i)=0;

		* Now sensoring for not fully observed data;
		start_window=intnx('YEAR',DOB,i-1,'S');
		end_window=intnx('YEAR',DOB,i,'S')-1;

		if ((end_window>"&sensor"d) or (start_window>"&sensor"d)) then
			OS_da_at_age_(i)=.;
	end;

	drop i ind start_window end_window;
run;


***************************************************************;
**H. USE OF MENTAL HEALTH SERVICES AT EACH YEAR OF AGE;
*****************************************************************;

%let firstage=0;
%let lastage=19;

**Get the snz_moh_uid for this population of interest;

proc sql;
create table extend
as select a.*, b.snz_moh_uid
from &population a
left join security.concordance b
on a.snz_uid=b.snz_uid
order by a.snz_uid;
quit;

proc means data=extend;
run;

**93.5 records have an MoH ID number;


** Extract PRIMHD records for the people in the cohort **;
proc sql;
	create table activity_days_1 as
	select 
		 a.snz_moh_uid
		,input(compress(b.activity_start_date,"-"),yymmdd8.) format date9. as start_date
		,input(compress(b.activity_end_date,"-"),yymmdd8.) format date9. as end_date
		,b.activity_type_code
		,b.team_code
	from extend a
		inner join sandmoh.moh_PRIMHD_201505 b
			on a.snz_moh_uid = b.snz_moh_uid
	where a.snz_moh_uid>0
	order by snz_moh_uid
	;
quit;


** Link to team lookup table to get team type, but first have to convert team_code from character to numeric **;
data team_lookup (keep=team_code team_type);
	set sandmoh.moh_PRIMHD_team_lookup;
	if team_code = 'Unknow' then delete;
	temp = 1*team_code;
	drop team_code;
	rename temp=team_code;
run;

proc sql;
	create table activity_days_2 as
	select 
		 a.*
		,b.team_type
	from activity_days_1 a
		inner join team_lookup b
			on a.team_code = b.team_code
	order by snz_moh_uid, start_date
	;
quit;

** Turn this into the input dataset the Health Tracker code expects **;
data activity_20082014;
	set activity_days_2 (rename=(snz_moh_uid = enc_hcu
								 start_date = activity_start_date
								 end_date = activity_end_date
								 team_type = team_type_code
								 )
						 );
run;


** Health Tracker code... **;

* Team type - activity days table;
*Creates a SAS dataset with variables: enc_hcu code source indicator date;
data primhd_team (rename=(team_type_code=code));
   set activity_20082014(keep=enc_hcu activity_start_date team_type_code);
   source = 'primhd_team';
   format indicator $20.;
   format date DDMMYY10.;
   date = activity_start_date;

   * Eating disorders;
   if team_type_code = '16' then do indicator = 'Eating'; output; end;

   * Substance use;
   if team_type_code in ('03','10','11','21','23') then do indicator = 'Substance_use'; output; end;

   keep enc_hcu date team_type_code indicator source ;
run;

* Activity code - activity days table (also keep all services as they can be used for an overall MH indicator;
*Creates a SAS dataset with variables: enc_hcu code source indicator date;
data primhd_act (rename=(activity_type_code=code));
   set activity_20082014(keep=enc_hcu activity_start_date activity_type_code);
   source = 'primhd_act';
   format indicator $20.;
   format date DDMMYY10.;
   date = activity_start_date;

   * Psychotic disorders;
   if activity_type_code = 'T09' then indicator = 'Psychotic';

   * Substance use;
   if activity_type_code in ('T16','T17','T18','T19','T20') then indicator = 'Substance_use';

   * Any mental health disorder - for input into an any MH disorder category;
   if activity_type_code not in ('T09','T16','T17','T18','T19','T20') then indicator = 'Any_MH_disorder';

   keep enc_hcu date activity_type_code indicator source ;
run;


** Combine mental health indicators from all data sources **;
data mh1;
   length indicator $20;
   set /*labs_mentalhealth (keep=enc_hcu date indicator)
       nmds_mentalhealth (keep=enc_hcu date indicator)
       pharms            (keep=enc_hcu date indicator)*/
       primhd_act        (keep=enc_hcu date indicator)
       primhd_team       (keep=enc_hcu date indicator);
run;


** Join with cohort to add snz_uid and date of birth **;

proc sql;
   create table mh2 as
   select a.snz_uid
	 ,a.snz_moh_uid
	 ,a.dob
         ,b.date
	 ,b.indicator
   from extend a
	inner join mh1 b
		on a.snz_moh_uid = b.enc_hcu and a.snz_moh_uid>0
   order by snz_uid;
quit;

** Group indicators into substance use and everything else **;
data mh3;
	set mh2;
	year = year(date);
	if not(indicator = 'Substance_use') then indicator = 'Other_MH';
run;

proc means data=mh3;
run;


*** Create indicators by calendar year ***;

proc sort data=&population(keep=snz_uid DOB) out=Population; 
by snz_uid; 
run;


** Create indicators by year of age **;

data mh7;
	set mh3;
	array substance_use_(*) substance_use_at_age_&firstage-substance_use_at_age_&lastage;
	array other_mh_(*) other_mh_at_age_&firstage-other_mh_at_age_&lastage;
	array any_mh_(*) any_mh_at_age_&firstage-any_mh_at_age_&lastage;

	do ind=&firstage to &lastage;
		i=ind-(&firstage-1);
        substance_use_(i) = 0;
		other_mh_(i) = 0;
		any_mh_(i) = 0;
		start_window=intnx('YEAR',DOB,i-1,'S');
		end_window=intnx('YEAR',DOB,i,'S');
		if ((date < end_window) and (date >= start_window)) then do;
			if indicator = 'Substance_use' then substance_use_(i) = 1;
			if indicator = 'Other_MH'      then other_mh_(i) = 1;
			any_mh_(i) = 1;
		end;
	end;
run;

proc summary data=mh7 nway;
	class snz_uid;
	id dob;
	var substance_use_at_age_&firstage-substance_use_at_age_&lastage
		other_mh_at_age_&firstage-other_mh_at_age_&lastage
		any_mh_at_age_&firstage-any_mh_at_age_&lastage;
	output out=mh8 (drop=_type_ _freq_) sum=;
run;

proc sort data=mh8; by snz_uid; run;


proc sort data=&population; by snz_uid; run;

data &folder.._ind_mhealth_at_age_&date (keep=snz_uid dob
                                              substance_use_at_age_&firstage-substance_use_at_age_&lastage
                                              other_mh_at_age_&firstage-other_mh_at_age_&lastage
                                              any_mh_at_age_&firstage-any_mh_at_age_&lastage
										 );
	merge mh8        (in=a keep=snz_uid DOB 
                                substance_use_at_age_&firstage-substance_use_at_age_&lastage
		                        other_mh_at_age_&firstage-other_mh_at_age_&lastage
	                            any_mh_at_age_&firstage-any_mh_at_age_&lastage)
          &Population (in=b keep=snz_uid DOB);
	by snz_uid;
    if b;
	array substance_use_(*) substance_use_at_age_&firstage-substance_use_at_age_&lastage;
	array other_mh_     (*) other_mh_at_age_&firstage-other_mh_at_age_&lastage;
	array any_mh_       (*) any_mh_at_age_&firstage-any_mh_at_age_&lastage;

	do ind=&firstage to &lastage;
		i=ind-(&firstage-1);

		* Where have greater than 1 count make it one;
	 	if substance_use_(i) > 1 then substance_use_(i) = 1;
		if other_mh_(i) > 1         then other_mh_(i) = 1;
		if any_mh_(i) > 1           then any_mh_(i) = 1;

        * Initially set all missing to zero values;
		if substance_use_(i)=. then substance_use_(i)=0;
		if other_mh_(i)=. then other_mh_(i)=0;
		if any_mh_(i)=. then any_mh_(i)=0;

  		* Sensor for not fully observed data (set to missing);
 		start_window=intnx('YEAR',DOB,i-1,'S');
		end_window=intnx('YEAR',DOB,i,'S');
		if ((end_window>"&sensor"d) or (start_window>"&sensor"d)) then  substance_use_(i) = .;
		if ((end_window>"&sensor"d) or (start_window>"&sensor"d)) then  other_mh_(i) = .;
		if ((end_window>"&sensor"d) or (start_window>"&sensor"d)) then  any_mh_(i) = .;
	end;
	drop i ind start_window end_window;
run;

proc means data=&folder.._ind_mhealth_at_age_&date;
run;


proc datasets lib=work kill nolist memtype=data;
quit;




