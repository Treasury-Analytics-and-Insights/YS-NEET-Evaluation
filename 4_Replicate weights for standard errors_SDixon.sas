
/**Standard errors  */
/** This program creates replicate weights for the calculation of SEs using bootstrap sampling */
/* Note the structure of the programme using three population sub-strata reflects the design of the YS:NEET evaluation */

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



%macro SEweight(folder, strata, sampsize_s, sampsize_c, classvars, xvars, boot, band);

data treatment control;
set &folder2..allcases_&strata;
if treat=1 then output treatment;
else output control;
run;

proc surveyselect data=treatment  
   method=urs reps=1 outhits sampsize=&sampsize_s out=treatment2;
  run ;

proc sort data=treatment2;
by snz_uid ;
run;

data samplea;
set treatment2;
by snz_uid ;
if first.snz_uid then unique_id = 0 ;
  unique_id + 1 ;
run ;

proc surveyselect data=control method=urs reps=1 outhits
   sampsize=&sampsize_c out=control2;
run;

proc sort data=control2;
by snz_uid refmth;
run;

data sampleb;
set control2;
by snz_uid refmth;
if first.snz_uid then unique_id = 0 ;
  unique_id + 1 ;
run ;

data allcases;
set samplea sampleb ;
run;

**Subsample 10000 controls to use in the regression; 

proc surveyselect data=sampleb(drop=replicate) method=srs reps=1 
   sampsize=10000 out=control3;

data regression;
set samplea control3;
run;

proc logistic data=regression;
class &classvars;
model treat(event='1') = &xvars
	  / firth rsquare; roc;
	  score data=allcases out=score1;
run;

********MATCHING******;
**Get the variables needed for the matching;

data study_m(drop=treat rename=(snz_uid=_snz_uid refmth=_refmth unique_id=_unique_id 
        female=_female age =_age age2 =_age2 
        hqual=_hqual region=_region region2=_region2
        p_1=_pscore )) 
      comparison_m(drop=treat rename=(p_1=pscore )) ;
set score1(keep=snz_uid treat refmth unique_id female age age2 hqual region region2 p_1 );
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
  order by _snz_uid, _unique_id, snz_uid;
quit ;

**Second pass using less exact matching - less exact age and regional matching;

/*
data unmatched;
merge study_m(in=a ) matchesa(in=b keep=_snz_uid );
by _snz_uid ;
if a and not b;
run;
*/

data unmatched;
merge study_m(in=a ) matchesa(in=b keep=_snz_uid _unique_id);
by _snz_uid _unique_id;
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
order by _snz_uid, _unique_id, snz_uid;
quit ;

data matchesc;
set matchesa(in=a) matchesb(in=b);
*if a then match_run=1;
*else if b then match_run=2;
run;

**Third pass using less exact matching - drop region altogether;

proc sort data=matchesc;
by _snz_uid _unique_id;
run;

data unmatched2;
merge study_m(in=a ) matchesc(in=b keep=_snz_uid _unique_id);
by _snz_uid _unique_id;
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
set matchesc matchesd;
run;

****CALCULATING THE MATCH RATE; 
	
proc freq data=matches_final noprint ; 
tables _snz_uid*_unique_id / out=out ;
run ;

proc sort data=study_m ;  
by _snz_uid _unique_id  ;
run ;

data matchrate ;
merge study_m (in=a )
        out (in=b keep= _snz_uid _unique_id   count) ;
  by _snz_uid _unique_id ;
  if a and b  then flag='Matched  ' ;
  if a and not b then flag='Unmatched  ' ;
  if a and not b then count=0 ;
run ;

***SELECTING UP TO 10 BEST MATCHES TO KEEP AND CALCULATING WEIGHTS FOR THE MATCHED CONTROL GROUP;

data matches2 ;  
set matches_final;
 distance=abs(_pscore-pscore);
 rn=rand('uniform')  ;
run ;

proc sort data=matches2 ; 
by _snz_uid _unique_id distance rn ; 
run; * take closest matches ;

data matches3 ; 
set matches2;
 by _snz_uid _unique_id  distance rn ;
 if first._unique_id  then n=0 ;
 n+1 ;
if n<=10 then output ; * keep max of 10 matches ;
run ;

proc freq data=matches3 noprint ;
  tables _snz_uid*_unique_id/ out=out2 ;
run ;

 * Weight per each matched non participant;
data out3 (keep=_snz_uid _unique_id weight) ;  
  set out2 ;
  weight=1/count ;
run ;

*One record per matched control person;

data comparison_gp(keep=refmth _snz_uid _unique_id snz_uid unique_id weight pscore );   
  merge matches3 out3 ;
  by _snz_uid _unique_id  ;
run ;

**One record per matched study grp mbr;

data study_gp (rename=(_snz_uid=snz_uid  _refmth=refmth _unique_id=unique_id  _pscore=pscore count=nbr_matched /*flag=match_flag*/ )) ;
   set matchrate(keep=_snz_uid _unique_id   _refmth count _pscore   ) ;
   if count=0 then weight=0 ; 
   else weight=1 ;
   if weight>0;
 run ;


 **Combine these study group and control group records;
 **The persons own id is stored in snz_uid but if they are in the control group, _snz_uid gives the person they were matched to;

data matched(keep=snz_uid refmth unique_id _snz_uid _unique_id treat pscore weight nbr_matched rename=(weight=wgt&i));
  set study_gp comparison_gp(in=a) ;
  if a then treat=0 ; 
  else treat=1 ;  
run ;

**Sum up records and weights for people who have been selected more than once into the sample for this iteration
    and matched more than once to the same study sample person;

proc summary data = matched nway MISSING ;    * need to specify missing or the records of the treatment group wont remain intact; 
  class snz_uid _snz_uid treat refmth ;    
  var wgt&i ;
  output out=&boot..rep&i(drop=_type_ _freq_ ) sum= ;
 run ;

%mend SEweight;


****CREATING REPLICATE WEIGHTS ;
**You need to plug in the actual sizes of the source datasets in place of 'n_study', 'n_control';

**Strata 1;

%macro replicate ; 
 %DO i =1 %TO 100;
  %SEweight(&&folder2, 1, n_study, n_control, &&classvars_1, &&xvars_1, &&boot1 , 0.01);
  %END;
%mend replicate;
%replicate;

****strata 2;

%macro replicate ; 
 %DO i =1 %TO 100;
  %SEweight(&&folder2, 2, n_study, n_control, &&classvars_2, &&xvars_2, &&boot2, 0.03 );
  %END;
%mend replicate;
%replicate;

****Strata 3;

%macro replicate ; 
 %DO i =1 %TO 100;
  %SEweight(&&folder2, 3, n_study, n_control, &&classvars_3, &&xvars_3, &&boot3, 0.03 );
  %END;
%mend replicate;
%replicate;


**TEST - running this code in the latest iteration;

proc freq data=&folder2..allcases_1;
tables treat;
run;
proc freq data=&folder2..allcases_2;
tables treat;
run;
proc freq data=&folder2..allcases_3;
tables treat;
run;




%macro replicate ; 
 %DO i =1 %TO 100;
  %SEweight(&&folder2, 1, 3258, 1924000,  &&classvars_1, &&xvars_1, &&boot1 , 0.01);
  %END;
%mend replicate;
%replicate;

****strata 2;

%macro replicate ; 
 %DO i =1 %TO 100;
  %SEweight(&&folder2, 2, 1761, 260589, &&classvars_2, &&xvars_2, &&boot2, 0.03 );
  %END;
%mend replicate;
%replicate;

****Strata 3;

%macro replicate ; 
 %DO i =1 %TO 100;
  %SEweight(&&folder2, 3, 4275, 624723, &&classvars_3, &&xvars_3, &&boot3, 0.03 );
  %END;
%mend replicate;
%replicate;



**Append the base and replicate datasets to form a single  dataset of weights;

%macro append(folder, strata, boot);

data allreps ;
 set &folder2..rep0_&strata(keep=snz_uid _snz_uid treat refmth weight );
run;

proc sort data=allreps;
by snz_uid _snz_uid treat refmth;
run;

%macro merge ;
 %DO i =1 %TO 100;
   
 data allreps(compress=Y) ;
  merge allreps 
        &boot..rep&i  ;
  by snz_uid _snz_uid treat refmth ;
  run ;
 
%END ;

%mend merge ;
%merge;

data &boot..allreps_&strata;
set allreps;
run;

%mend append;

%append(&&folder2, 1, &&boot1);
%append(&&folder2, 2, &&boot2);
%append(&&folder2, 3, &&boot3);


