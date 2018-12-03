/* 3. MATCHING AND ANALYSIS   */

**PART A: PS MODELS AND MATCHING;
**PART B: PROFILE OF THE MATCHED YS PARTICIPANTS AND THEIR COMPARISIONS;
**PART C: ACTIVITY and INCOME GRAPHS USING THE MATCHED SAMPLES;
**PART D: MAIN IMPACT ESTIMATES AND THEIR STARDARD ERRORS;
**PART F: subgroup impact estimates - GENDER AND HIGHEST QUALIFICATION;

%let VERSION=20160224;
%let date=20160224;

libname Project "\\wprdSAS10\TreasuryData\MAA2013-16 Citizen pathways through human services\Social Investment_2016\1_Indicator_at_age_datasets\dataset_rerun_24022016";
libname basedata "\\wprdSAS10\TreasuryData\MAA2013-16 Citizen pathways through human services\Sylvia\Youth Service\Data 2017 revisions\Base datasets";
libname suppdata "\\wprdSAS10\TreasuryData\MAA2013-16 Citizen pathways through human services\Sylvia\Youth Service\Data 2017 revisions\Supp datasets";;
libname studdata "\\wprdSAS10\TreasuryData\MAA2013-16 Citizen pathways through human services\Sylvia\Youth Service\Data 2017 revisions\Impact evaluation";
libname schools "\\wprdSAS10\TreasuryData\MAA2013-16 Citizen pathways through human services\Sylvia\School data from MoE website";
libname ys4 "\\wprdSAS10\TreasuryData\MAA2013-16 Citizen pathways through human services\Sylvia\Youth Service\Data March refresh\Base datasets";
libname ys5 "\\wprdSAS10\TreasuryData\MAA2013-16 Citizen pathways through human services\Sylvia\Youth Service\Data March refresh\Impact evaluation";
libname boot1 "\\wprdSAS10\TreasuryData\MAA2013-16 Citizen pathways through human services\Sylvia\Youth Service\Data 2017 revisions\Replicate weights\Strata1";
libname boot2 "\\wprdSAS10\TreasuryData\MAA2013-16 Citizen pathways through human services\Sylvia\Youth Service\Data 2017 revisions\Replicate weights\Strata2";
libname boot3 "\\wprdSAS10\TreasuryData\MAA2013-16 Citizen pathways through human services\Sylvia\Youth Service\Data 2017 revisions\Replicate weights\Strata3";

**Macro variables to enable the project file locations to be easily changed;

%let folder = basedata ;   **specify the location of the base files created in the prev program;
%let folder2 = studdata;   **specify the location to save files created in this program;
%let boot1= boot1;
%let boot2= boot2;
%let boot3= boot3;


**************************************************;
**PART A: PROPENSITY SCORE MODELS AND MATCHING;
**In this case we estimate separate models for the members of three subgroups or strata, 
    based on enrolment status in the reference month;
***********************************************;


**Strata 1;
%let classvars_1=
  refmth(ref='166') cohort(ref='1996') age(ref='17') 
  nzdep(ref='5') region(ref='2') decile(ref='5') schtype(ref='Standard') /*schauth(ref='State')*/ hqual(ref='1')
    ncea_cr_l1_cat(ref='4') ncea_cr_l2_cat(ref='2') ncea_cr_l3_cat(ref='1') nsch_cat(ref='2')
  sch_last6(ref=first) emp_last5(ref=first) tert_last5(ref=first) neet_last6(ref=first) /*ben_last6(ref=first)*/
  sch_prior12(ref=first) emp_prior12(ref=first) tert_prior12(ref=first) neet_prior12(ref=first)  /* ben_prior12(ref=first)*/
  /*sch_first30(ref=first)*/ emp_first30(ref=first) /*tert_first30(ref=first) neet_first30(ref=first) ben_first30(ref=first)*/
  prop_onben_aschild_cat(ref=first) /* child_bentype(ref=first)*/
  prop_os_aschild_cat(ref=first) 
  child_not_cat(ref=first)  susp_da_cat(ref=first) stand_da_cat(ref=first);
%let xvars_1=
  refmth cohort yts age female maori pacific asian ethoth
  nzdep region decile schtype  hqual hothqual hterqual ncea_cr_l1_cat ncea_cr_l2_cat ncea_cr_l3_cat nsch_cat
  genskills_only gen_n_occ_skills
  emp48 tert48 /* ben48*/
  sch_last6 emp_last5 tert_last5 neet_last6 /*ben_last6*/
  sch_prior12 emp_prior12 tert_prior12 neet_prior12 /*ben_prior12*/
  /*sch_first30*/ emp_first30 /*tert_first30 neet_first30 ben_first30 */
  prop_onben_aschild_cat /*child_bentype */
  prop_os_aschild_cat mother_unqual cg_cust cg_comm
  child_not_cat child_any_fdgs_abuse child_cyf_place child_yj_referral child_yj_place any_mh
  sedu_da trua_da susp_da_cat stand_da_cat;

**Strata 2;
%let classvars_2=
  refmth(ref='166') cohort(ref='1996') age(ref='17') 
  nzdep(ref='5') region(ref='2') decile(ref='5') schtype(ref='Standard') hqual(ref='1') hterqual(ref='0')
     ncea_cr_l1_cat(ref='4') ncea_cr_l2_cat(ref='2') ncea_cr_l3_cat(ref='1') nsch_cat(ref='2') time_since_school(ref='a<2mths')
  sch_last6(ref=first) emp_last5(ref=first) tert_last6(ref=first) neet_last6(ref=first) ben_last6(ref=first)
  sch_prior12(ref=first) emp_prior12(ref=first) tert_prior12(ref=first) neet_prior12(ref=first) ben_prior12(ref=first)
  sch_first30(ref=first) emp_first30(ref=first) /*tert_first30(ref=first)*/ neet_first30(ref=first) ben_first30(ref=first)
  prop_onben_aschild_cat(ref=first) /* child_bentype(ref=first)*/
  prop_os_aschild_cat(ref=first) 
  child_not_cat(ref=first)  child_yj_referral_cat(ref=first)  susp_da_cat(ref=first) stand_da_cat(ref=first);
%let xvars_2=
  refmth cohort yts age female maori pacific asian ethoth
  nzdep region decile schtype  hqual hothqual hterqual ncea_cr_l1_cat ncea_cr_l2_cat ncea_cr_l3_cat nsch_cat time_since_school
  genskills_only gen_n_occ_skills
  emp48 /*sch48*/  /*ben48*/
  sch_last6 emp_last5 tert_last6 neet_last6 ben_last6
  sch_prior12 emp_prior12 tert_prior12 neet_prior12 ben_prior12
  sch_first30 emp_first30 /*tert_first30*/ neet_first30 ben_first30
  prop_onben_aschild_cat /*child_bentype */
  prop_os_aschild_cat mother_unqual cg_cust cg_comm 
  child_not_cat child_any_fdgs_abuse child_cyf_place child_yj_referral_cat child_yj_place any_mh
  sedu_da trua_da susp_da_cat stand_da_cat;


**Strata 3;
%let classvars_3=
  refmth(ref='166') cohort(ref='1996') age(ref='17') 
  nzdep(ref='5') region(ref='2') decile(ref='5') schtype(ref='Standard') schauth(ref='State') hqual(ref='1') hterqual(ref='0')
     ncea_cr_l1_cat(ref='4') ncea_cr_l2_cat(ref='2') ncea_cr_l3_cat(ref='1') nsch_cat(ref='2') time_since_school(ref='a<2mths')
  sch_last6(ref=first) emp_last5(ref=first) tert_last5(ref=first) neet_last5(ref=first) ben_last6(ref=first)
  sch_prior12(ref=first) emp_prior12(ref=first) tert_prior12(ref=first) neet_prior12(ref=first) ben_prior12(ref=first)
  sch_first30(ref=first) emp_first30(ref=first) /*tert_first30(ref=first)*/ neet_first30(ref=first) ben_first30(ref=first)
  prop_onben_aschild_cat(ref=first) /*child_bentype(ref=first)*/
  prop_os_aschild_cat(ref=first) 
  child_not_cat(ref=first)  child_yj_referral_cat(ref=first)  susp_da_cat(ref=first) stand_da_cat(ref=first);
%let xvars_3=
  refmth cohort yts age female maori pacific asian ethoth
  nzdep region decile schtype schauth hqual hothqual hterqual ncea_cr_l1_cat ncea_cr_l2_cat ncea_cr_l3_cat nsch_cat time_since_school
  genskills_only gen_n_occ_skills
  /*sch48*/ emp48 tert48 neet48 /*ben48*/
  sch_last6 emp_last5 tert_last5 neet_last5 ben_last6  
  sch_prior12 emp_prior12 tert_prior12 neet_prior12 ben_prior12
  sch_first30 emp_first30 /*tert_first30*/ neet_first30 ben_first30
  prop_onben_aschild_cat /*child_bentype*/ 
  prop_os_aschild_cat mother_unqual cg_cust cg_comm 
  child_not_cat child_any_fdgs_abuse child_cyf_place child_yj_referral_cat child_yj_place any_mh
  sedu_da trua_da susp_da_cat stand_da_cat;


%macro main(file,classvars,xvars,strata,band);

ods output ParameterEstimates=&file..PE_&strata.R
           oddsratios=&file..OR_&strata.R;
proc logistic data=&file..model_data_&strata.R;
class &classvars;
model treat(event='1') = &xvars
	  / firth rsquare; roc;
	  score data=&file..allcases_&strata.R out=score1;
run;
ods html close;

********MATCHING******;
**Get the variables needed for the matching;

data study_m(drop=treat rename=(snz_uid=_snz_uid refmth=_refmth female=_female age =_age age2 =_age2 
        hqual=_hqual region=_region region2=_region2
        p_1=_pscore )) 
      comparison_m(drop=treat rename=(p_1=pscore )) ;
set score1(keep=snz_uid treat refmth female age age2 hqual region region2 p_1 );
if p_1~=.;
if treat=1 then output study_m ; 
                 else output comparison_m ; 
run ;


**Select exact matches and pscore matches within a range of +/- 0.03;
	
proc sql ;
  create table matchesa as
  select *
  from study_m a, comparison_m b
  where a._refmth=b.refmth and 
		a._female=b.female and	
		a._age = b.age and 
        a._hqual=b.hqual and  
		a._region = b.region and
		a._pscore+ &band >= b.pscore  and    
        a._pscore- &band <= b.pscore
  order by _snz_uid, snz_uid;
quit ;

**Second pass using less exact matching - less exact age and regional matching;

data unmatched;
merge study_m(in=a ) matchesa(in=b keep=_snz_uid);
by _snz_uid;
if a and not b;
run;

proc sql ;
  create table matchesb as
  select *
  from unmatched a, comparison_m b
  where a._refmth=b.refmth and 
        a._female=b.female and	
		a._age2 = b.age2 and 
        a._hqual=b.hqual and  
		a._region2 = b.region2 and
		a._pscore+ &band >= b.pscore  and    
        a._pscore- &band <= b.pscore
order by _snz_uid, snz_uid;
quit ;

data matchesc;
set matchesa(in=a) matchesb(in=b);
if a then match_run=1;
else if b then match_run=2;
run;


**Third pass using less exact matching - drop region altogether;

proc sort data=matchesc;
by _snz_uid;
run;

data unmatched2;
merge study_m(in=a ) matchesc(in=b keep=_snz_uid);
by _snz_uid;
if a and not b;
run;

proc sql ;
  create table matchesd as
  select *
  from unmatched2 a, comparison_m b
  where a._refmth=b.refmth and 
        a._female=b.female and	
		a._age2 = b.age2 and 
        a._hqual=b.hqual and  
		/*a._&region2 = b.&region2 and*/
		a._pscore+ &band >= b.pscore  and    
        a._pscore- &band <= b.pscore
order by _snz_uid, snz_uid;
quit ;

data matches_final;
set matchesc matchesd(in=c);
if c then match_run=3;
run;

****CALCULATING THE MATCH RATE; 
	
proc freq data=matches_final noprint ; 
tables _snz_uid / out=out ;
run ;

proc sort data=study_m ;  
by _snz_uid  ;
run ;

data matchrate_&strata ;
merge study_m (in=a )
        out (in=b keep=  _snz_uid  count) ;
  by _snz_uid ;
  if a and b  then flag='Matched  ' ;
  if a and not b then flag='Unmatched  ' ;
  if a and not b then count=0 ;
run ;

proc freq data=matchrate_&strata ;
tables flag /*count*/ / nocol norow;
title "Match rate for &strata"; 
run;

***SELECTING UP TO 10 BEST MATCHES TO KEEP AND CALCULATING WEIGHTS FOR THE MATCHED CONTROL GROUP;

data matches2 ;  
set matches_final;
 distance=abs(_pscore-pscore);
 rn=rand('uniform')  ;
run ;

proc sort data=matches2 ; 
by _snz_uid distance rn ; 
run; * take closest matches ;

data matches3 ; 
set matches2;
 by _snz_uid  distance rn ;
 if first._snz_uid   then n=0 ;
 n+1 ;
if n<=10 then output ; * keep max of 10 matches ;
run ;

proc freq data=matches3 noprint ;
  tables _snz_uid / out=out2 ;
run ;

 * Weight per each matched non participant;
data out3 (keep=_snz_uid  weight) ;  
  set out2 ;
  weight=1/count ;
run ;

*One record per matched control person;

data comparison_gp(keep=snz_uid refmth _snz_uid weight pscore );   
  merge matches3 out3 ;
  by _snz_uid  ;
run ;

**One record per matched study grp mbr;
data study_gp (rename=(_snz_uid=snz_uid  _refmth=refmth  _pscore=pscore count=nbr_matched /*flag=match_flag*/ )) ;
   set matchrate_&strata(keep=_snz_uid _refmth count _pscore   ) ;
   if count=0 then weight=0 ; 
   else weight=1 ;
   if weight>0;
 run ;

 **Combine these study group and control group records;
 **The persons own id is stored in snz_uid but if they are in the control group, _snz_uid gives the person they were matched to;

data matched ;
  set study_gp comparison_gp (in=a) ;
  if a then treat=0 ; else treat=1 ;  
run ;

proc sort data=matched ; 
by snz_uid refmth;  
run ;

data &file..rep0_&strata.R;
set matched;
run;

%mend main;

%main(&&folder2, &&classvars_1, &&xvars_1, 1, 0.01);
%main(&&folder2, &&classvars_2, &&xvars_2, 2, 0.03);
%main(&&folder2, &&classvars_3, &&xvars_3, 3, 0.03);





**Who is unmatched? ;

%let strata=2;
proc means data=matchrate_&strata;
class flag;
var _pscore;
run;

proc freq data=matchrate_&strata;
tables _female _age _region  _pscore /list missing;
where flag='Unmatched  ' ;
run;



******CHECK MODELS ARE BALANCED ;
******Take the matched records, merge on the full set of model variables 
    and then re-run the model using the weights, to check that nothing is statistically significant;


%macro matches(strata);
proc sort data=&folder2..rep0_&strata; by snz_uid refmth; run;
proc sort data=&folder2..allcases_&strata; by snz_uid refmth; run;

data &folder2..matched_modelrerun_&strata;
merge &folder2..rep0_&strata(in=a keep=snz_uid treat refmth pscore weight)
   &folder2..allcases_&strata;
by snz_uid refmth;
if a;
run;
%mend;
%matches(1);
%matches(2);
%matches(3);


proc contents data=&folder2..matched_modelrerun_2;
run;

%macro rerun(classvars,xvars,strata);
proc logistic data=&folder2..matched_modelrerun_&strata;
class &classvars;
model treat(event='1') = &xvars
	  / firth rsquare; roc;	
	  weight weight;
title "Model re-run for &strata";
run;
%mend;

%rerun(&&classvars_1, &&xvars_1, 1);
%rerun(&&classvars_2, &&xvars_2, 2);
%rerun(&&classvars_3, &&xvars_3, 3);



****************************************************;
**PART B: DESCRIPTIVE PROFILE OF THE MATCHED STUDENTS AND THEIR COMPARISIONS;
**Takes individual characteristics from the earliest model dataset, before any aggregation of categories is done;
**************************************************;

proc contents data=&folder2..model_final2;
run;
/*
data matches_old;
set ys5..rep0_1(in=a keep=snz_uid treat refmth pscore weight)
    ys5..rep0_2(in=b keep=snz_uid treat refmth pscore weight)
	ys5..rep0_3(in=c keep=snz_uid treat refmth pscore weight);
if a then strata=1;
else if b then strata=2;
else if c then strata=3;
run;
*/
data matches;
set &folder2..rep0_1(in=a keep=snz_uid treat refmth pscore weight)
    &folder2..rep0_2(in=b keep=snz_uid treat refmth pscore weight)
	&folder2..rep0_3(in=c keep=snz_uid treat refmth pscore weight);
if a then strata=1;
else if b then strata=2;
else if c then strata=3;
run;


proc freq data=matches_old;
tables strata*treat /list missing;
run;
proc freq data=matches;
tables strata*treat /list missing;
run;

proc freq data=matches_old;
tables treat /list missing;
run;
proc freq data=matches;
tables treat /list missing;
run;

**I have 1.3% more matched study population cases after fixing the error in the age2 variable;


proc sort data=matches;
by snz_uid refmth;
run;

data allmodel(drop=age rename=(ageb=age));
merge matches(in=a) &folder2..model_final2;
by snz_uid refmth;
if a;
if strata=1 and treat=1 then gp=1;
else if strata=2 and treat=1 then gp=2;
else if strata=3 and treat=1 then gp=3;
else if strata=1 and treat=0 then gp=4;
else if strata=2 and treat=0 then gp=5;
else if strata=3 and treat=0 then gp=6;
year_started=year(YSstartdate);
ageb=int(age);
run;

proc means data=allmodel;
run;


**Tables for publication;

proc format;
value agelsch
.='NA'
1-15='LT16'
16='16'
17='17'
18='18'
19-high='19+';

value nzdep
1-2='1-2'
3-4='3-4'
5-6='5-6'
7-8='7-8'
9-10='9-10'
99='NA';

value nschool
.='NA'
1,2='1-2'
3,4='3-4'
5-high='5+';

value months
0,.='aNone'
1-6='b1-6mths'
7-12='c7-12mths'
13-18='d13-18mths';

value monthsb
0,.='aNone'
1-6='b1-6mths'
7-18='c7-18mths';

value earnings
.,0='aNone'
.0001-<500='bLT$500'
500-<1000='cLT$1000'
1000-<2500='dLT$2500'
2500-high='dGT$2500';
run;

proc format;
value credits
.,0='None'
1-<40='LT40'
40-<60='40-59'
60-<80='60-79'
80-high='80+';

value creds
.,0='None'
1-<40='LT40'
40-high='40+';

value region
1='Northland'
2='Auckland' 
3='Waikato' 
4='Bay of Plenty'
5='Gisborne' 
6='Hawkes Bay' 
7='Taranaki' 
8=	'Manawatu-Wanganui' 
9=	'Wellington' 
12=	'West Coast' 
13=	'Canterbury' 
14=	'Otago' 
15=	'Southland' 
16=	'Tasman' 
17=	'Nelson' 
18=	'Marlborough' 
99=	'NA';
run;

proc format;
 value $schgap
   'hNA'='NA'
   'a<2mths'='<2 months'
   'b2-3mths'='2-3 months'
   'c4-6mths'='4-6 months'
   'd7-12mths'='7-12 months'
   'e1-2yrs' = '1-2 years'
   'f2-3yrs','g>3yrs' ='2+ years';
run;

proc format;
   value $nbrnot
       'aNone'='aNone'
	   'b1-2'='b1-2'
	   'c3-9','d10+'='c3+';   
run;

proc format;
value $standgp
'aNone'='aNone'
'b1-2'='b1-2'
'c3-9','d10+'='c3+';  
run;


proc format;
value $schtype
'Standard'='aStandard'
'Corresp','Other' = 'bOther';
run;

proc format;
value $schauth
'State'='aState'
'StateInt'='bState Integrated'
'Other'='cPrivate/Other';
run;


proc freq data=allmodel;
tables (year_started age female european maori pacific asian ethoth nzdep
    region  mother_unqual cg_cust cg_comm 
    prop_os_aschild_cat prop_onben_aschild_cat child_not_cat child_any_fdgs_abuse child_cyf_place child_yj_referral_cat 
    all_act)*gp / missing norow nocol nopercent;
format nzdep nzdep. region region.  child_not_cat child_yj_referral_cat $nbrnot.; 
weight weight;
title "Matched sample means for personal and childhood ";
run;

proc freq data=allmodel;
tables (decile /*schtype schauth*/ nschools 
    sedu_da trua_da stand_da_cat susp_da age_when_left_sch 
    hqual ncea_cr_l1-ncea_cr_l3 hothqual hterqual 
    )*gp / missing norow nocol nopercent;
format /*schtype $schtype.  schauth $schauth.*/ age_when_left_sch agelsch.  decile nzdep. stand_da_cat $standgp.
    nschools nschool.  ncea_cr_l1-ncea_cr_l2  credits. ncea_cr_l3 creds.;
weight weight;
title "Matched sample means for school and tertiary achievement";
run;

proc freq data=allmodel;
tables (time_since_school
    mths_sch_last18 mths_emp_last18 mths_tert_last18 mths_neet_last18 mths_ben_last18 mean_earn_last6
    occskills_only genskills_only gen_n_occ_skills)*gp / missing norow nocol nopercent;
weight weight;
format time_since_school $schgap. mths_sch_last18 months. mths_emp_last18 mths_tert_last18 mths_neet_last18 mths_ben_last18 monthsb.
       mean_earn_last6 earnings.;
title "Matched sample means for other activities before enrolment";
run;



********************************************************;
**PART C: Pre and post YS activity and benefit GRAPHS, using matched samples;
**In the second data step  includes an adjustment for being overseas - monthly obs are set to missing 
  if the person was out of NZ for > 25 days of that month;

**Note that the reference month in the vectors is 49;
**Months 25-48 contain data on the 24 months prior;
**Months 1-48 contain data on the 48 months prior;
**Months 50-73 contain data on the 24 months following the reference month;
*********************************************************;

proc contents data=&folder2..monthly;
run;

%let z=25;
%let a=73;
%macro graphdata(strata);
data &folder2..graphs_&strata;
     merge &folder2.rep0_addrecs_revised_&strata(in=a keep=snz_uid treat refmth pscore weight)
     &folder2..monthly(keep=snz_uid refmth sch&z-sch&a tert&z-tert&a study&z-study&a neet&z-neet&a 
          emp&z-emp&a ben&z-ben&a sa&z-sa&a)
	 &folder2..ysneet_monthly(keep=snz_uid refmth ys&z-ys&a)
     &folder2..yp_monthly(keep=snz_uid refmth yp&z-yp&a);
by snz_uid refmth;
if a;
array ys(*) ys&z-ys&a;
  do i=1 to dim(ys);
  if ys(i)=. then ys(i)=0;
  end;
run;
%mend graphdata;

%graphdata(1);
%graphdata(2);
%graphdata(3);


**Censor obs that were OS in that specfic month;

%let b=73;
%macro graphdata2(strata);
data &folder2..graphs_&strata(drop=os1-os&b);
merge &folder2..graphs_&strata(in=a) &folder2..mainly_overseas(keep=snz_uid refmth os1-os&b);
by snz_uid refmth;
if a;
array vectors(9,73) os1-os&b sch1-sch&b tert1-tert&b study1-study&b ben1-ben&b neet1-neet&b 
          emp1-emp&b ys1-ys&b yp1-yp&b;
do j=1 to 1; 
do i=1 to 73;
   if vectors(1,i)=1 then do;
      vectors(2,i)=.; vectors(3,i)=.; vectors(4,i)=.; vectors(5,i)=.; vectors(6,i)=.; 
       vectors(7,i)=.; vectors(8,i)=.; vectors(9,i)=.;
   end;
end;
end;
run;
%mend graphdata2;

%graphdata2(1);
%graphdata2(2);
%graphdata2(3);


**Get means of each  measure to rapidly construct and check the graphs;

%macro graphs(data,strata,xvar);
proc summary data=&data nway;
class treat;
var &xvar;
weight weight;
output out=stats(drop=_type_) mean=;
proc print data=stats;
title "&strata &xvar";
run;
%mend graphs;


%graphs(&folder2..graphs_1,1,ys25-ys73);
%graphs(&folder2..graphs_1,1,yp25-yp73);
%graphs(&folder2..graphs_1,1,sch25-sch73);
%graphs(&folder2..graphs_1,1,tert25-tert73);
%graphs(&folder2..graphs_1,1,study25-study73);
%graphs(&folder2..graphs_1,1,neet25-neet73);
%graphs(&folder2..graphs_1,1,emp25-emp73);
%graphs(&folder2..graphs_1,1,ben25-ben73);

%graphs(&folder2..graphs_2,2,ys25-ys73);
%graphs(&folder2..graphs_2,2,yp25-yp73);
%graphs(&folder2..graphs_2,2,sch25-sch73);
%graphs(&folder2..graphs_2,2,tert25-tert73);
%graphs(&folder2..graphs_2,2,study25-study73);
%graphs(&folder2..graphs_2,2,neet25-neet73);
%graphs(&folder2..graphs_2,2,emp25-emp73);
%graphs(&folder2..graphs_2,2,ben25-ben73);

%graphs(&folder2..graphs_3,3,ys25-ys73);
%graphs(&folder2..graphs_3,3,yp25-yp73);
%graphs(&folder2..graphs_3,3,sch25-sch73);
%graphs(&folder2..graphs_3,3,tert25-tert73);
%graphs(&folder2..graphs_3,3,study25-study73);
%graphs(&folder2..graphs_3,3,neet25-neet73);
%graphs(&folder2..graphs_3,3,emp25-emp73);
%graphs(&folder2..graphs_3,3,ben25-ben73);

data all;
set &folder2.graphs_1 &folder2.graphs_2 &folder2.graphs_3;
run; 

proc contents data=all;
run;


%graphs(all,All,ys25-ys73);
%graphs(all,All,yp25-yp73);
%graphs(all,All,sch25-sch73);
%graphs(all,All,tert25-tert73);
%graphs(all,All,study25-study73);
%graphs(all,All,neet25-neet73);
%graphs(all,All,emp25-emp73);
%graphs(all,All,ben25-ben73);



**************************************************;
**PART D: MAIN IMPACT ESTIMATES AND THEIR STANDARD ERRORS;
**I include an adjustment for being overseas - monthly obs are set to missing 
  if the person was out of NZ for > 25 days of that month;
************************************************;


data matches;
set &folder2..rep0_1r(in=a keep=snz_uid _snz_uid treat refmth pscore weight)
    &folder2..rep0_2r(in=b keep=snz_uid _snz_uid treat refmth pscore weight)
	&folder2..rep0_3r(in=c keep=snz_uid _snz_uid treat refmth pscore weight);
if a then strata=1;
if b then strata=2;
if c then strata=3;
run;

proc sort data=matches;
by snz_uid refmth;
run;

data depvars;
merge matches(in=a)     
    &folder2..depvars_1(keep=snz_uid refmth 
           study_post6 study_post12 study_post18 study_post24
           ben_post6 ben_post12 ben_post18 ben_post24
           neet_post6 neet_post12 neet_post18  neet_post24
           emp_post6 emp_post12 emp_post18 emp_post24
           cust_in_followup comm_in_followup
           sa_post6 sa_post12 sa_post18 sa_post24
           it_post6 it_post12 it_post18 it_post24
		   )
	  &folder2..depvars_2(keep=snz_uid refmth	 i1hqual_y2  i2hqual_y2  i3hqual_y2 lev2_yr18 ben_3mths_aft_18  cust_3mths_aft_18
           i1hncea_y2  i2hncea_y2  i3hncea_y2
           i1hqual_y0  i2hqual_y0  i3hqual_y0
              i1hqual_y1  i2hqual_y1  i3hqual_y1 ) 
      &folder2..depvars_mthly_earn(keep=snz_uid refmth  /*mean_mthly_earn_prior6  mean_mthly_earn_post*/
            chg_mean_earn   )
      &folder2..mainly_overseas(keep=snz_uid refmth os50-os79 );
by snz_uid refmth;
if a;
all=1;
array os(*) os50-os79;
array m6(*)  study_post6 ben_post6 neet_post6 emp_post6 sa_post6 it_post6;
array m12(*) study_post12 ben_post12 neet_post12 emp_post12 sa_post12 it_post12;
array m18(*) study_post18 ben_post18 neet_post18 emp_post18 sa_post18 it_post18;
array m24(*) study_post24 ben_post24 neet_post24 emp_post24 sa_post24 it_post24;
do j=1 to dim(os);
if j=6 and os(j)=1 then do i=1 to 6;
    m6(i)=.;
	end;
if j=12 and os(j)=1 then do i=1 to 6;
    m12(i)=.;
	end;
if j=18 and os(j)=1 then do i=1 to 6;
    m18(i)=.;
	end;
if j=24 and os(j)=1 then do i=1 to 6;
    m24(i)=.;
	end;
end;
run;

%let vlist= study_post6 study_post12 study_post18 study_post24
           i1hqual_y2  i2hqual_y2  i3hqual_y2  
           ben_post6 ben_post12 ben_post18 ben_post24
           lev2_yr18 ben_3mths_aft_18  cust_3mths_aft_18
           neet_post6 neet_post12 neet_post18  neet_post24
           emp_post6 emp_post12 emp_post18 emp_post24
 
           cust_in_followup comm_in_followup
           sa_post6 sa_post12 sa_post18 sa_post24
           it_post6 it_post12 it_post18 it_post24
           chg_mean_earn ;

%let classvar = all;
%let ntreat=n1-n33;
%let vtreat = t1-t33;
%let vcontrol = c1-c33;
%let vdiff = diff1-diff33;
%let vse = sd1-sd33;
%let format=   ;


%macro impacts(data, vlist, classvar, ntreat, vcontrol, vtreat, vdiff); 
proc summary data=&data nway;
var   &vlist;
class &classvar;
where treat=1;
weight weight;
format &classvar &format.;
output out=samples(drop=_type_ ) n=&ntreat;
run;

proc summary data=&data nway;
var   &vlist;
class &classvar;
where treat=0;
weight weight;
format &classvar &format.;
output out=results0(drop=_type_ ) mean=&vcontrol;
run;

proc summary data=&data nway;
var  &vlist;
class &classvar;
where treat=1;
weight weight;
format &classvar &format.;
output out=results1(drop=_type_ ) mean=&vtreat;
run;

data diff;
merge samples(rename=_FREQ_=ntreat) results0(rename=_FREQ_=ncontrol) results1(rename=_FREQ_=ntreat);
by &classvar;
array treat (*) &vtreat;
array control (*) &vcontrol;
array diff (*) &vdiff;
**Here I multiply all proportions by 100 so they will be displayed as percentages, 
  but not the mean monthly earnings varible, which is the last item in the vector of outcome measures;
do i=1 to dim(treat)-1;
   treat(i)= treat(i)*100;
   control(i)=control(i)*100;
   end;
do i=1 to dim(treat);
   diff(i)=(treat(i)-control(i));
   end;
run;

data results_main_&classvar(keep=i &classvar varlabel ntreat treat_mean control_mean diff ); 
set diff;
array sample (*) &ntreat;
array treat (*) &vtreat;
array control (*) &vcontrol;
array differ(*) &vdiff;
do i= 1 to dim(treat); 
 ntreat=sample(i);
 treat_mean=treat[i];
 control_mean=control[i];
 diff=differ[i];
 varlabel=scan("&vlist.", i);
 output;
 end;
run ;

proc print; 
var &classvar varlabel ntreat treat_mean control_mean diff ;
title "Main estimates &classvar.";
run;

%mend impacts;


**Current results to compare with the published ones;

ods excel file="\\wprdSAS10\TreasuryData\MAA2013-16 Citizen pathways through human services\Sylvia\Youth Service\Outputs 2017 revisions\Main_impacts.xlsx"
   options(sheet_interval='none');

%impacts(depvars, &&vlist, all,  &&ntreat, &&vcontrol, &&vtreat, &&vdiff);
%impacts(depvars, &&vlist, strata,  &&ntreat, &&vcontrol, &&vtreat, &&vdiff);

ods excel close;




**STANDARD ERRORS FOR MAIN IMPACT ESTIMATES ;
***THE REPLICATE WEIGHTS ARE CREATED IN THE STANDARD ERROR PROG;

** Compile the necessary data;

%macro sedata(strata, boot);
data long(keep=nsnz_uid n_snz_uid ntreat nrefmth i newweight  rename=(  
    nsnz_uid=snz_uid n_snz_uid=_snz_uid  ntreat=treat nrefmth=refmth i=rep newweight=weight));
set &boot..allreps_&strata;
nsnz_uid=snz_uid;
n_snz_uid=_snz_uid;
ntreat=treat;
nrefmth=refmth;
array wgts[100] wgt1-wgt100;
do i= 1 to 100;
   newweight=wgts[i];
   if newweight^=. then output;
   end;
drop snz_uid _snz_uid treat refmth weight;
run;

proc sort data=long;
by snz_uid refmth; 
run;

data &boot..sedata_&strata;
merge long(in=a)  
           &folder2..depvars_1(keep=snz_uid refmth 
           study_post6 study_post12 study_post18 study_post24
           ben_post6 ben_post12 ben_post18 ben_post24
           neet_post6 neet_post12 neet_post18  neet_post24
           emp_post6 emp_post12 emp_post18 emp_post24

           cust_in_followup comm_in_followup
           sa_post6 sa_post12 sa_post18 sa_post24
           it_post6 it_post12 it_post18 it_post24)
	  &folder2..depvars_2(keep=snz_uid refmth	 i1hqual_y2  i2hqual_y2  i3hqual_y2 
            lev2_yr18 ben_3mths_aft_18  cust_3mths_aft_18
              i1hncea_y2  i2hncea_y2  i3hncea_y2
              i1hqual_y0  i2hqual_y0  i3hqual_y0
              i1hqual_y1  i2hqual_y1  i3hqual_y1
              i1hqual_y3  i2hqual_y3  i3hqual_y3) 
      &folder2..depvars_mthly_earn(keep=snz_uid refmth  
            chg_mean_earn   )
      &folder2..mainly_overseas(keep=snz_uid refmth os50-os79 );
by snz_uid refmth; 
if a;
all=1;
array os(*) os50-os79;
array m6(*)  study_post6 ben_post6 neet_post6 emp_post6 sa_post6 it_post6;
array m12(*) study_post12 ben_post12 neet_post12 emp_post12 sa_post12 it_post12;
array m18(*) study_post18 ben_post18 neet_post18 emp_post18 sa_post18 it_post18;
array m24(*) study_post24 ben_post24 neet_post24 emp_post24 sa_post24 it_post24;
do j=1 to dim(os);
if j=6 and os(j)=1 then do i=1 to 6;
    m6(i)=.;
	end;
if j=12 and os(j)=1 then do i=1 to 6;
    m12(i)=.;
	end;
if j=18 and os(j)=1 then do i=1 to 6;
    m18(i)=.;
	end;
if j=24 and os(j)=1 then do i=1 to 6;
    m24(i)=.;
	end;
end;
run;

%mend sedata;

%sedata(1, boot1);
%sedata(2, boot2);
%sedata(3, boot3);



data combined;
set boot1.sedata_1(in=a) boot2.sedata_2(in=b) boot3.sedata_3(in=c) ;
if a then strata=1;
if b then strata=2;
if c then strata=3;
run;

proc contents data=combined;
run;


***CALCULATE STANDARD ERRORS;
**CLASSVAR IS THE SUBGROUP VARIABLE(S). IF SET TO 'ALL' THEN CODE GENERATES AGGREGATE RESULTS;

%macro fastboot(data, vlist, classvar, format, vtreat, vcontrol, vdiff, vse);

proc sort data=&data;
by rep;
run;

proc means data=&data noprint nway;
by rep;
weight weight;
class &classvar. treat;
var &vlist;
output out=outall mean=;
run;

* split the participants from non-participants and then merge back together into one wide record
for each category of the SPLIT variable;

data treatgp(drop= &vlist);
set outall;
if treat=1;
array oldname (*) &vlist;
array newname (*) &vtreat;
do i=1 to dim(oldname);
  newname(i)=oldname(i);
  end;
run;

data controlgp(drop=&vlist);
set outall;
if treat=0;
array oldname (*) &vlist;
array newname (*) &vcontrol;
do i=1 to dim(oldname);
  newname(i)=oldname(i);
  end;
run;

data diff;
merge treatgp(drop=treat _type_ _freq_) controlgp(drop=treat _type_ _freq_);
by rep &classvar;
array treat (*) &vtreat;
array control (*) &vcontrol;
array diff (*) &vdiff;
do i=1 to dim(treat);
   diff(i)=treat(i)-control(i);
   end;
run;

proc means data=diff noprint nway ;
var &vdiff &vtreat &vcontrol;
class &classvar;
output out=se(keep= &classvar &vdiff &vtreat &vcontrol &vse)
   mean=&vdiff &vtreat &vcontrol    
   stddev(&vdiff) = &vse;
run;

/*
proc univariate data=diff noprint;
	class &classvar;
	var &vdiff;
	output out=lcl pctlpts=2.5 pctlpre=diff_lcl
			pctlname=d;
run;

proc univariate data=diff noprint;
	class &classvar;
	var &vdiff;
	output out=ucl pctlpts=97.5 pctlpre=diff_ucl
			pctlname=d;
run;
*/
data results_se_&classvar(keep=i &classvar varlabel treat_mean control_mean diff std_error u_ci l_ci t sig); 
/*merge se lcl ucl;*/ set se;
/*by &classvar;*/
array treat (*) &vtreat;
array control (*) &vcontrol;
array difference (*) &vdiff;
array se (*) &vse;
do i= 1 to dim(treat);
 std_error=se[i];
 treat_mean=treat[i];
 control_mean=control[i];
 U_CI = difference[i] + 1.96*se[i] ;
 L_CI = difference[i] - 1.96*se[i] ;
 *u_ci2=ucl[i];
 *l_ci2=lcl[i];
 t = difference[i]/se[i]  ;
 if t > 1.96 or t < -1.96 then sig = '*' ; else sig =' ' ;
 diff=difference[i];
 varlabel=scan("&vlist.", i);
 output;
 end;
run ;

proc print; 
var &classvar varlabel treat_mean control_mean diff std_error u_ci l_ci t sig ;
title "Standard Errors Group= &classvar.";
run;

%mend fastboot;


***Join the impact estimates and standard errors and find significance using the ratio of the SE to the impact estimate;


%macro results(classvar);

data results_&classvar;
merge results_main_&classvar(keep=&classvar  i varlabel ntreat treat_mean control_mean  diff)       
      results_se_&classvar(keep=&classvar i std_error treat_mean control_mean diff 
      rename=(  treat_mean=treat_mean_SE control_mean=control_mean_SE  diff=diff2));
by &classvar i;
if i<=32 then do;
   treat_SE=treat_mean_SE*100;
   control_SE=control_mean_SE*100;
   diff_SE=diff2*100;
   SE=std_error*100;
   end;
else if i>=33 then do;
   treat_SE=treat_mean_SE;
   control_SE=control_mean_SE;
   diff_SE=diff2;
   SE=std_error;
   end;
U_CI = diff + 1.96*SE ;
L_CI = diff - 1.96*SE ;
t = diff/SE ;
if t > 1.96 or t < -1.96 then sig = '*' ; else sig =' ' ;
run;

proc print data=results_&classvar;
var &classvar varlabel ntreat treat_mean control_mean diff SE sig t l_ci u_ci treat_SE control_SE diff_SE;
*format &classvar  &format.;
title "Classvar = &classvar.";
run;

%mend results;


%impacts(depvars, &&vlist, all,  &&ntreat, &&vcontrol, &&vtreat, &&vdiff);
%fastboot(combined,&&vlist, all,    ,&&vtreat, &&vcontrol, &&vdiff, &&vse);
%results(all);


%impacts(depvars, &&vlist, strata,  &&ntreat, &&vcontrol, &&vtreat, &&vdiff);
%fastboot(combined,&&vlist, strata,   ,&&vtreat, &&vcontrol, &&vdiff, &&vse);
%results(strata);



**************************************************;
**PART F:  SUBGROUP IMPACT ESTIMATES - GENDER AND HIGHEST QUALIFICATION;
*********************************************;

**Impact estimates by GENDER and ETHNIC group;
**First create a general-purpose dataset that can be used for the sub-group estimates below;
**Exact matching by female and hqual in the PS models means all in control group will have same gender and highest qual
  in the following step;
**Not so for ethnicities and other groups - in those cases I had to rerun PS matching with exacting matching on ethnic group etc;


data depvars_demog;
merge 
    &folder2..regressions(keep=snz_uid refmth female european maori pacific hqual)
           depvars(in=a keep=snz_uid refmth weight treat _snz_uid
           study_post6 study_post12 study_post18 study_post24
           i1hqual_y2  i2hqual_y2  i3hqual_y2  
           ben_post6 ben_post12 ben_post18 ben_post24
           lev2_yr18 ben_3mths_aft_18  cust_3mths_aft_18
           neet_post6 neet_post12 neet_post18  neet_post24
           emp_post6 emp_post12 emp_post18 emp_post24  ); 
by snz_uid refmth;
if a;
run;



proc sort data=combined;
by snz_uid refmth;
run;

data combined_demog;
merge combined(in=a drop=cust_in_followup comm_in_followup
           sa_post6 sa_post12 sa_post18 sa_post24
           it_post6 it_post12 it_post18 it_post24 chg_mean_earn  ) 
   &folder2..regressions(keep=snz_uid refmth female european maori pacific hqual); 
by snz_uid refmth;
if a;
run;


%let vlist=  
 		   study_post6 study_post12 study_post18 study_post24
           i1hqual_y2  i2hqual_y2  i3hqual_y2  
           ben_post6 ben_post12 ben_post18 ben_post24
           lev2_yr18 ben_3mths_aft_18  cust_3mths_aft_18
           neet_post6 neet_post12 neet_post18  neet_post24
           emp_post6 emp_post12 emp_post18 emp_post24 ;
%let ntreat=n1-n22;
%let vtreat = t1-t22;
%let vcontrol = c1-c22;
%let vdiff = diff1-diff22;
%let vse = sd1-sd22;
%let format=   ;

**Gender;

%impacts(depvars_demog, &&vlist, female,  &&ntreat, &&vcontrol, &&vtreat, &&vdiff  ); 
%fastboot(combined_demog,&&vlist, female,    ,&&vtreat, &&vcontrol, &&vdiff, &&vse);  

%results(female);


**Highest qual ;

%impacts(depvars_demog, &&vlist, hqual,  &&ntreat, &&vcontrol, &&vtreat, &&vdiff      ); 
%fastboot(combined_demog,&&vlist, hqual,    ,&&vtreat, &&vcontrol, &&vdiff, &&vse);  

%results(hqual);


