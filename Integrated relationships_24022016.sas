
******************************************************************************************************************************************************************
******************************************************************************************************************************************************************

DICLAIMER:
This code has been created for research purposed by Analytics and Insights Team, The Treasury. 
The business rules and decisions made in this code are those of author(s) not Statistics New Zealand and New Zealand Treasury. 
This code can be modified and customised by users to meet the needs of specific research projects and in all cases, 
Analytics and Insights Team, NZ Treasury must be acknowledged as a source. While all care and diligence has been used in developing this code, 
Statistics New Zealand and The Treasury gives no warranty it is error free and will not be liable for any loss or damage suffered by the use directly or indirectly

******************************************************************************************************************************************************************
******************************************************************************************************************************************************************;

/*
Author: Christopher Ball

Purpose:
To create an integrated workflow which creates child-caregiver maps and regional information
which can be imputed for children when we know their caregivers...

NOTES:
Sole Parent Benefit Information is no longer included.  This is picked up through an entry 
in the child database not having a partner entry.
More than 99% of the applications considered in the Migration data can identify at most two caregivers for residents.
Changed Migration to include rejected applications and temporary applications.

Section 1: Import/Link Caregiver Data Sets
Section 2: Clean Up Caregiver Indicators
Section 3: Import/Link Regional Information
Section 4: Clean Up Regional Information
Section 5: Impute Regional Information for Children
Section 6: Prioritise Regional Information (Experimental)
*/
* Clear the work directory when the full program is run;
proc datasets lib=work kill nolist memtype=data;
quit;

***************************************************************************************************************************************************
***************************************************************************************************************************************************
PART 0: CONCORDANCE LOOK UP TABLE

* Obtaining all records from admin datasets using SNZ concordance table;
* It will help us to plug in gender of parents and whether they have been linked to the spine;
***************************************************************************************************************************************************
***************************************************************************************************************************************************;

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
			snz_jus_uid from security.concordance)
order by snz_uid;
quit;

proc SQL;
	Connect to sqlservr (server=WPRDSQL36\iLeed database=IDI_clean_&version);
	create table PERS as select * from connection to  sqlservr
		(select 
			snz_uid, 
			snz_sex_code, snz_birth_month_nbr,snz_birth_year_nbr,
			snz_person_ind,
			snz_spine_ind
		from data.personal_detail)
order by snz_uid;
quit;

data project.cons_pers; merge conc pers; by snz_uid;
DOB=MDY(snz_birth_month_nbr,15,snz_birth_year_nbr);
run;

***************************************************************************************************************************************************
***************************************************************************************************************************************************
PART 1: Using DIA realtionships;
* fixing DIA parent UID mess that SNZ created;

* DID relationships are pretty straight forward. One or two parent per child and 
* Relationship event starts from DOB of the child, where day of the borth is set to 15th day to be consistent with A&I definitions of DOB;

*******************************************************************************************************************************************;
*******************************************************************************************************************************************;

proc sql;
connect to odbc(dsn="idi_clean_&version._srvprd");
	create table ParentChildMap_0 as
		select * from connection to odbc (select 
			a.snz_uid, 
			a.dia_bir_birth_year_nbr, 
			a.dia_bir_birth_month_nbr, 
			a.parent1_snz_dia_uid,
			a.dia_bir_parent1_sex_snz_code,
			a.parent2_snz_dia_uid,
			a.dia_bir_parent2_sex_snz_code
		
		from  dia_clean.births a )
order by parent1_snz_dia_uid;
disconnect from odbc;
quit;

proc sort data=project.cons_pers nodupkey out=cons_pers_1; by snz_dia_uid; run;
data cons_pers_1; set cons_pers_1; if snz_dia_uid>0; run;
proc sql;
	create table ParentChildMap_1
		as select 
			a.*,
			'dia' as source,
			b.snz_uid as parent1_snz_uid,
			b.snz_spine_ind as parent1_spine,
			b.snz_sex_code as parent1_snz_sex_code
	from ParentChildMap_0 a left join cons_pers_1 b
			on a.parent1_snz_dia_uid=b.snz_dia_uid
		
order by a.parent2_snz_dia_uid;

proc sql;
	create table ParentChildMap_2 as select
			a.*,
			c.snz_uid as parent2_snz_uid,
			c.snz_spine_ind as parent2_spine,
			c.snz_sex_code as parent2_snz_sex_code
		from ParentChildMap_1 a left join cons_pers_1 c
			on a.parent2_snz_dia_uid=c.snz_dia_uid;
quit;


* Now after mess created by SNZ we have correct snz_uid for parents;

proc freq data=ParentChildMap_2; where parent1_snz_uid>0; tables dia_bir_parent1_sex_snz_code*parent1_snz_sex_code parent1_spine/list missing;
proc freq data=ParentChildMap_2; where parent2_snz_uid>0; tables dia_bir_parent2_sex_snz_code*parent2_snz_sex_code parent2_spine/list missing;

data project.PART1; set ParentChildMap_2;
event_date=mdy(dia_bir_birth_month_nbr,15,dia_bir_birth_year_nbr); 
DOB=mdy(dia_bir_birth_month_nbr,15,dia_bir_birth_year_nbr);
format event_date DOB date9.;
if parent1_snz_sex_code=. and dia_bir_parent1_sex_snz_code ne . then parent1_snz_sex_code=dia_bir_parent1_sex_snz_code;
if parent2_snz_sex_code=. and dia_bir_parent2_sex_snz_code ne . then parent2_snz_sex_code=dia_bir_parent2_sex_snz_code;
if parent1_snz_uid>0 or parent2_snz_uid>0;
run;


*******************************************************************************************************************************************
*******************************************************************************************************************************************
PART 2: Using MSD realtionships

* Import the linked MSD Child/Partner data;
* MSD relationships are more complex, child can have multiple caregivers and these multiple caregivers can have multiple partners.
* first we plugging in partner information only where child relationship spell falls within partner reltionship window
* relationship event starts from the earlist of the two dates...
(i) date child was included in the main benefit
(ii) date when partnership reltaionship started

Note: It is event table and can have multiple records for same caregiver and child .

*******************************************************************************************************************************************
*******************************************************************************************************************************************;
proc sql;
	create table ChildPartner as
		select distinct 
				'msd' as source
				,a.snz_uid
				,a.child_snz_uid
				,a.msd_chld_child_birth_month_nbr as child_birth_month
				,a.msd_chld_child_birth_year_nbr as child_birth_year
				,a.msd_chld_spell_nbr
				,MDY(a.msd_chld_child_birth_month_nbr,15,a.msd_chld_child_birth_year_nbr) as DOB format date9.
				,b.partner_snz_uid
				,input(compress(a.msd_chld_child_from_date,"-"),yymmdd10.) as child_from format date9.
				,input(compress(a.msd_chld_child_to_date,"-"),yymmdd10.) as child_to format date9.
				,input(compress(b.msd_ptnr_ptnr_from_date,"-"),yymmdd10.) as ptnr_from format date9.
				,input(compress(b.msd_ptnr_ptnr_to_date,"-"),yymmdd10.) as ptnr_to format date9.
				,b.msd_ptnr_spell_nbr
			from msd.msd_child a LEFT JOIN msd.msd_partner b
				on a.snz_uid = b.snz_uid and a.msd_chld_spell_nbr=b.msd_ptnr_spell_nbr
				and b.msd_ptnr_ptnr_to_date >= a.msd_chld_child_from_date
				and b.msd_ptnr_ptnr_from_date <= a.msd_chld_child_to_date
order by a.snz_uid, a.child_snz_uid, child_from, ptnr_from			;
quit;

/** Trim it down by removing obviously incorrect cases;*/
/** 6.33 seconds with 3924350 rows and 5 columns;*/
proc sql;
	create table ChildExtended as
		select distinct child_snz_uid as snz_uid, 
				DOB,
				snz_uid as parent1_snz_uid,
				partner_snz_uid as parent2_snz_uid,
				source,
				ptnr_from, child_from, ptnr_to, child_to,
				child_from as event_date
/*				input(COALESCE(msd_ptnr_ptnr_from_date,msd_chld_child_from_date),yymmdd10.) as Event_Date format yymmdd10.*/
			from ChildPartner
				where child_snz_uid ne partner_snz_uid 
				and child_snz_uid ne snz_uid 
				and partner_snz_uid ne snz_uid
	order by snz_uid, child_snz_uid, partner_snz_uid;
quit;

* creating an event date, Chris chosen relationship event date as min of two dates, But i think we need to choose child_from date as event date ;

* attached spine indicators and gender of caregivers;

proc sql;
	create table project.PART2
		as select 
			a.*,
			b.snz_spine_ind as parent1_spine,
			b.snz_sex_code as parent1_snz_sex_code,
			c.snz_spine_ind as parent2_spine,
			c.snz_sex_code as parent2_snz_sex_code
		from ChildExtended a left join cons_pers b
			on a.parent1_snz_uid=b.snz_uid
			left join cons_pers c
			on a.parent2_snz_uid=c.snz_uid;

proc freq data=project.PART2; where parent1_snz_uid>0; tables parent1_spine;
proc freq data=project.PART2; where parent2_snz_uid>0; tables parent2_spine;

**********************************************************************************************************************************************************
**********************************************************************************************************************************************************

PART 3: Using IMMIGRATION data
* Relationship is based on application submitted for Immigration.
* limiting to applications that are approved for residency
			In most cases Parents and children but not necessarily in all cases.
* The decision on application is assumed event date of relationship
*******************************************************************************************************************************************;
*******************************************************************************************************************************************;
proc sql;
	create table Mig as
		select snz_uid, snz_application_uid, 
			input(dol_dec_decision_date,yymmdd10.) as Decision_Date format yymmdd10.,
			mdy(dol_dec_birth_month_nbr,15,dol_dec_birth_year_nbr) as DOB format yymmdd10.,
			yrdif(mdy(dol_dec_birth_month_nbr,15,dol_dec_birth_year_nbr),input(dol_dec_decision_date,yymmdd10.),'AGE') as Age,
			case when yrdif(mdy(dol_dec_birth_month_nbr,15,dol_dec_birth_year_nbr),input(dol_dec_decision_date,yymmdd10.),'AGE') >= 18 then 1
			else 0 end as Adult,
			case when yrdif(mdy(dol_dec_birth_month_nbr,15,dol_dec_birth_year_nbr),input(dol_dec_decision_date,yymmdd10.),'AGE') < 18 then 1
			else 0 end as Child
				from dol.decisions
					where dol_dec_nbr_applicants_nbr > 1 and dol_dec_decision_type_code = 'A' and dol_dec_reporting_cat_code = 'R'
						order by snz_application_uid;
quit;

* Summarise by application and merge back onto the data set;

proc sql;
	create table Mig_sum as
		select distinct
			snz_application_uid, decision_date,
			sum(Adult) as Adults, 
			sum(Child) as Children, 
			max(Age*Child) as Oldest,
			max(age*adult) as Oldest_p
			from Mig
			group by snz_application_uid, decision_date
			order by snz_application_uid;
quit;

data Mig_sum;
set Mig_sum; 
if Adults > 2 or Children=0 then delete;
if Oldest_p-oldest<14 then delete;
run;

* We excluded applications where there are two or more adults and applications where child is not included*
* excluded applications where oldest child was born at the age of 14 of the olderst adult;
%contents(Mig_sum);
proc sql;
	create table Mig_1 as
		select distinct 
			a.*,
			b.adults,
			b.children,
			b.oldest,
			b.oldest_p
			from Mig a right join Mig_sum b
on a.snz_application_uid = b.snz_application_uid and a.decision_date = b.decision_date
order by a.snz_application_uid, a.Decision_Date;
quit;

proc transpose data=Mig_1(rename=snz_uid=parent) out=Parents_dol(rename=(col1=Parent1_snz_uid col2=Parent2_snz_uid));
where Adult=1;
by snz_application_uid Decision_Date;
var parent;
run;

data PART3_temp; 
merge Mig_1(where=(child=1) in=a) Parents_dol(in=b); 
by snz_application_uid Decision_Date; 
if a and b;
event_date=decision_date;
format event_date date9.;
source='dol';
keep snz_uid DOB source event_date parent1_snz_uid Parent2_snz_uid;
run;


proc sql;
	create table project.PART3
		as select 
			a.*,
			b.snz_spine_ind as parent1_spine,
			b.snz_sex_code as parent1_snz_sex_code,
			c.snz_spine_ind as parent2_spine,
			c.snz_sex_code as parent2_snz_sex_code
		from PART3_temp a left join cons_pers b
			on a.parent1_snz_uid=b.snz_uid
			left join cons_pers c
			on a.parent2_snz_uid=c.snz_uid;


proc freq data=project.part3; where Parent1_snz_uid>0; tables parent1_spine;
proc freq data=project.part3; where Parent2_snz_uid>0; tables parent2_spine;

/***********************************************************************************************************************************/
/***********************************************************************************************************************************/
/*PART 4: Using Working for Families dataset in the sandpit to create family relationships*/
/** event date is set as a start date of parnet and child relationship*/
/***********************************************************************************************************************************/
/**********************************************************************************************************************************;*/
/**/
/*proc sql;*/
/*	create table WFFMap as select distinct*/
/*			snz_pcc_ird_uid, */
/*			snz_chd_ird_uid,*/
/*			MDY(birth_month,15,input(birth_year,4.)) as DOB format date9.,*/
/*			input(compress(DATE_START,"-"),yymmdd10.) as event_date format date9.,*/
/*			input(compress(DATE_END,"-"),yymmdd10.) as end_date format date9.*/
/**/
/*	from SANDWFF.wff_Fam_children  where snz_chd_ird_uid > 0 and snz_pcc_ird_uid>0*/
/*	order by snz_pcc_ird_uid;*/
/*quit;*/
/**/
/*data WFFMap; set WFFMap;if end_date=. then end_date="&sensor"d;run;*/
/**/
/*proc sql;*/
/*create table WFFMap_0*/
/*as select */
/*c.snz_uid,*/
/*a.*,*/
/*b.snz_uid as parent1_snz_uid,*/
/*b.snz_spine_ind as parent1_spine,*/
/*b.snz_sex_code as parent1_snz_sex_code*/
/*from WFFMap a left join cons_pers b*/
/*on a.snz_pcc_ird_uid=b.snz_ird_uid */
/**/
/*left join cons_pers c*/
/*on a.snz_chd_ird_uid=c.snz_ird_uid*/
/*where b.snz_uid>0*/
/*order by parent1_snz_uid;*/
/**/
/** Let us try to add the snz_uid and partner_uid to the data;*/
/**/
/** lets create single record of parent 1 and their partners*/
/** note people can change partners and there are more than 1 partner reported;*/
/**/
/*proc sql; */
/*create table WFF_partners as select distinct*/
/*	snz_uid,*/
/*	snz_partner_ird_uid,*/
/*	partner_snz_uid as Parent2_snz_uid,*/
/*	min(input(compress(wff_spe_spell_start_date,"-"),yymmdd10.)) as part_start format date9.,*/
/*	max(input(compress(wff_spe_spell_end_date,"-"),yymmdd10.)) as part_end format date9.*/
/*from wff.spells where partner_snz_uid is not Null and snz_partner_ird_uid ne 9432389*/
/*group by snz_uid, partner_snz_uid*/
/*order by snz_uid;*/
/**/
/**/
/** we have to create many to many relationships, primary WFF parent 1 having many children and havng many partners at the same time;*/
/** will do the same as we did for MSD spells;*/
/**/
/*proc sql;*/
/*create table PART4*/
/*as select */
/*a.*,*/
/*'wff' as source,*/
/*b.Parent2_snz_uid,*/
/*b.part_start,*/
/*b.part_end,*/
/*c.snz_spine_ind as parent2_spine,*/
/*c.snz_sex_code as parent2_snz_sex_code*/
/*from WFFMap_0 a left join WFF_partners b*/
/*on a.parent1_snz_uid = b.snz_uid */
/*and b.part_end >= a.event_date	and b.part_start <= a.end_date*/
/*left join cons_pers c*/
/*on b.parent2_snz_uid=c.snz_uid*/
/*where a.snz_uid>0;*/
/**/
/*proc freq data=part4; where parent1_snz_uid>0; tables parent1_spine;*/
/*proc freq data=part4; where parent2_snz_uid>0; tables parent2_spine;run;*/


***********************************************************************************************************************************
***********************************************************************************************************************************
Part5: Census relationships
***********************************************************************************************************************************
***********************************************************************************************************************************;
%contents(cenpit.census_individual_updated);
proc sql;
create table Cen_fam as select 
snz_uid_new,
snz_uid_old,
snz_cen_uid,
snz_cen_fam_uid,
cen_ind_sex_code,
MDY(cen_ind_birth_month_nbr,15,cen_ind_birth_year_nbr) as DOB format date9.,
MDY(cen_ind_birth_month_nbr,15,cen_ind_birth_year_nbr) as event_date format date9.,
cen_ind_fam_grp_code,
cen_ind_fam_role_code
from cenpit.census_individual_updated where snz_uid_new>0 and snz_cen_fam_uid>0 and cen_ind_fam_grp_code not in ('00','50') and cen_ind_fam_role_code ne '00';

proc sql;
create table Cen_fam as select 
snz_uid,
snz_cen_uid,
snz_cen_fam_uid,
cen_ind_sex_code,
MDY(cen_ind_birth_month_nbr,15,cen_ind_birth_year_nbr) as DOB format date9.,
MDY(cen_ind_birth_month_nbr,15,cen_ind_birth_year_nbr) as event_date format date9.,
cen_ind_fam_grp_code,
cen_ind_fam_role_code
from cen.census_individual
where snz_uid>0 and snz_cen_fam_uid>0 and cen_ind_fam_grp_code not in ('00','50') and cen_ind_fam_role_code not in ('00','50');

* where cen_ind_fam_grp_code equal to 00 "guest or visitor" 50 is Unable to code;
* where cen_ind_fam_role_code =00-not in family nucleus and 50 is unable to code";

data cen_fam; set cen_fam; 
child=0; CG_parent=0; Grand_parent=0; other_cg=0;
if cen_ind_fam_role_code in ('02','12') then child=1;
if cen_ind_fam_role_code='01' then CG_parent=1;
if cen_ind_fam_role_code='03' then Grand_parent=1;
if cen_ind_fam_role_code='11' then other_cg=1;
run;
proc sort data=cen_fam; by snz_cen_fam_uid;run;

proc summary data=cen_fam nway;
class snz_cen_fam_uid;
var child CG_parent Grand_parent other_cg;
output out=fam_sum(drop=_:) sum=;
run;
* Dont notice any weird family relationships in census data;

proc transpose data=cen_fam(rename=snz_uid=parent) out=Parents_cen(rename=(col1=Parent1_snz_uid col2=Parent2_snz_uid));
where child=0;
by snz_cen_fam_uid;
var parent;
run;

data PART5_temp; 
merge cen_fam(where=(child=1) in=a keep=snz_uid DOB event_date snz_cen_fam_uid child) Parents_cen(in=b); 
by snz_cen_fam_uid; 
if a and b;
source='cen'; 
keep snz_uid DOB source event_date parent1_snz_uid Parent2_snz_uid;
run;
* about 7-8 thousand children have no DOB, we will populate using spine info
* personal details table does not have DOB for them;

proc sql;
	create table project.PART5
		as select 
			a.*,
			b.snz_spine_ind as parent1_spine,
			b.snz_sex_code as parent1_snz_sex_code,
			c.snz_spine_ind as parent2_spine,
			c.snz_sex_code as parent2_snz_sex_code
		from PART5_temp a left join project.cons_pers b
			on a.parent1_snz_uid=b.snz_uid
			left join project.cons_pers c
			on a.parent2_snz_uid=c.snz_uid;

proc freq data=part5; where Parent1_snz_uid>0; tables parent1_spine;
proc freq data=part5; where Parent2_snz_uid>0; tables parent2_spine;


***********************************************************************************************************************************
***********************************************************************************************************************************
Integrating relationship events from 4 sources: 
	Part1-DIA
	Part2-MSD,
	Part3-DOL( immigration applications and decisions)
	Part4-WFF ( not used anymore)
	Part5-Census 2013

Event dates before DOB can happen for the reasons listed below:

	no exact DOB of people
	mother applied for DPB or sickness benefit prior to birth of the child( during the pregancy) 
	immigration decision was made while mother was pregnant ( child unborn)

***********************************************************************************************************************************
***********************************************************************************************************************************;
DATA EVENTS; set project.Part1 project.part2 project.part3 project.part5;
	keep 
		snz_uid
		DOB
		parent1_snz_uid
		parent2_snz_uid
		source
		Event_Date
		parent1_spine
		parent1_snz_sex_code
		parent2_spine
		parent2_snz_sex_code;

	if DOB ne .;

	* about 7500 events will be deleted;
/*	if source='wff' and year(event_date)<year(DOB) then*/
/*		event_date=DOB;*/

	* about 4k cases where event_date is way too early, i assume they are missing relationship start dates;
	* keeping these records and resetting event_date to DOB of the child;
run;

* Now we will try to remove non-informative events, where the caregivers do not change;
* Start by reordering the parents in a unique way;

data parentchildmap_5 
/*(drop = parent1_snz_uid parent2_snz_uid );*/
;	set events;

* cases where both parents exist; 
if parent1_snz_uid ne . and parent2_snz_uid ne .  then do;

	parent1= min(parent1_snz_uid, Parent2_snz_uid);
	parent2=max(parent1_snz_uid, Parent2_snz_uid);

		* lets carry characteristics of parent1;
		if parent1=parent1_snz_uid then do;
			parent1_sex=parent1_snz_sex_code;
			parent1_onspine=parent1_spine;
		end;

		if parent1=parent2_snz_uid then do;
			parent1_sex=parent2_snz_sex_code;
			parent1_onspine=parent2_spine;
		end;
		* lets carry characteristics of parent2;
		if parent2=parent1_snz_uid then do;
			parent2_sex=parent1_snz_sex_code;
			parent2_onspine=parent1_spine;
		end;

		if parent2=parent2_snz_uid then do;
			parent2_sex=parent2_snz_sex_code;
			parent2_onspine=parent2_spine;
		end;
end;

* cases where one of the parents is missing, non missing parent becoming parent 1 and missing parent is parent2;
if parent1_snz_uid= . or parent2_snz_uid= . then do;
	parent1= max(parent1_snz_uid, Parent2_snz_uid);
	parent2=.;

* lets carry characteristics of parent1;
		if parent1=parent1_snz_uid then do;
			parent1_sex=parent1_snz_sex_code;
			parent1_onspine=parent1_spine;
		end;

		if parent1=parent2_snz_uid then do;
			parent1_sex=parent2_snz_sex_code;
			parent1_onspine=parent2_spine;
		end;
		
end;
run;


proc sort data=parentchildmap_5 nodup out=events_nodup;
/*	by snz_uid parent1 parent2 eventdate;*/
	by snz_uid DOB event_date source parent1 parent2;
run;
*************************************************************************************************************************************
*************************************************************************************************************************************;

data project.child_parent_events_&date; set events_nodup; 
format event_end date9.;
event_end = ifn(lag(snz_uid) = snz_uid, lag(event_date)-1, .);
run;

*************************************************************************************************************************************
*************************************************************************************************************************************;

* Now remove all records which have the same parents as the previous record;

data parentchildmap_5;
	set parentchildmap_5;
	if event_date = lag(event_date) and snz_uid = lag(snz_uid) then DELETE; /* DIA DOL MSD WFF priority order */
	if (snz_uid=lag(snz_uid) and parent1 = lag(parent1) and parent2 = lag(parent2)) then
		delete;
run;

* Finally add a to date where relevant;
proc sort data=parentchildmap_5 nodup;
	by snz_uid descending event_date;
run;

data parentchildmap_6;
	format event_end ddmmyy10.;
	set parentchildmap_5;
	event_end = ifn(lag(snz_uid) = snz_uid, lag(event_date)-1, .);
run;

proc sort data= parentchildmap_6;
	by snz_uid event_date;
run;


/* Section 6: Create a parent to child version */
data PC1;
	set parentchildmap_6;
	parent = parent1;
	if parent = . then
		delete;
	drop  parent1 parent2 ;
run;

data PC2;
	set parentchildmap_6;
	parent = parent2;
	if parent = . then
		delete;
	drop  parent1 parent2 ;
run;

* Stick them together;
data PC3; set PC1 PC2;run;

/*proc append base = PC1 data =  PC2 force;*/
/*run;*/

* Sort and remove duplicate records ;
proc sort data=PC3 noduprecs;
	by parent snz_uid event_date;
run;

* Output the data. 
  8,062,135 reords;
data Project.ChildToParentMap_&date ;
	set parentchildmap_6;
run;

data Project.ParentToChildMap_&date;
	set PC3;
run;


proc sql;
	create table SiblingBase as
		select a.snz_uid as snz_uid,
			b.snz_uid as sibling, 
			a.parent as parent,
			a.event_date as start1, 
			a.event_end as end1, 
			b.event_date as start2, 
			b.event_end as end2, 
			a.source as Source, 
			b.source as SibSource 
		from PC3 a 
		left join PC3 b
			on a.parent = b.parent
		where a.snz_uid ne b.snz_uid;

	create table SiblingExtended as
		select distinct snz_uid, sibling, 1 as CNT
		from SiblingBase
		order by snz_uid;

	create table  SiblingEvent as
		select distinct snz_uid, sibling, parent, 
				max(start1,start2) as startdate format = ddmmyy10., 
				min(end1,end2) as enddate format = ddmmyy10., 
				Source, SibSource
		from SiblingBase
		where (end2 = . or start1 <= end2) 
		and (end1 = . or start2 <= end1)
		order by snz_uid;

	create table SiblingEvCnt as
		select distinct snz_uid, sibling, 1 as CNT
		from SiblingEvent
		order by snz_uid;
quit;

data Project.ChildSiblingMapExtended_&date;
	set SiblingExtended;
run;

data Project.ChildSiblingMapvent_&date;
	set SiblingEvent;
run;



%relationship(diarel,dia); *Creating DIA relationship tables;

proc freq data=diarel.PARENTTOCHILDMAP_20160224; 
tables 
parent1_snz_sex_code*parent2_snz_sex_code/list;
run;

%relationship(msdrel,msd); *Creating MSD relationship tables;
%relationship(cenrel,cen); *Creating MSD relationship tables;



