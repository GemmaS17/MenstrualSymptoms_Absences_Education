********************************************************************************
**Adolescent HMB, menstrual pain, absences, and educational attainment
**GRS, 6th September 2023
**Stata version 18
********************************************************************************

********************************************************************************
*****************************OPEN DATASET AND PREP******************************
********************************************************************************
clear all
set maxvar 30000

use "S:\B4175_howe - B4175_howe\20231123_b4175_howe_built.dta", replace

*remove males
drop if kz021==1

*install 
search rmiss2
search mdesc
search eststo
search mvpatterns
ssc install heatplot
ssc install palettes
ssc install colrspace
ssc install coefplot

**************************
****EXPOSURE TIMEPOINT****
**************************

**Checking the ks4year variable that will be used
tab ks4year
*12 is 2006/2007, 13 is 2007/2008, and 14 is 2008/2009 (academic year 11)

**Deriving an exposure time variable (number of puberty questionnaire ppt complete in the two years before Jan of year 11)
*12=06/05; 13=07/06; 12=08/07
gen exp_time=.
replace exp_time=8 if ks4year==14 & pub896b==2008 | pub896b==2007
replace exp_time=7 if ks4year==14 & pub796b==2008 & exp_time==. | pub796b==2007 & exp_time==. 
replace exp_time=6 if ks4year==14 & pub696b==2007 & exp_time==.
replace exp_time=8 if ks4year==13 & pub896b==2007 & exp_time==. 
replace exp_time=7 if ks4year==13 & pub796b==2007 & exp_time==. | pub796b==2006 & exp_time==. 
replace exp_time=6 if ks4year==13 & pub696b==2007 & exp_time==. | pub696b==2006 & exp_time==. 
replace exp_time=5 if ks4year==13 & pub596b==2006 & exp_time==.
replace exp_time=4 if ks4year==13 & pub496b==2007 & exp_time==. | pub496b==2006 & exp_time==. 
replace exp_time=7 if ks4year==12 & pub796b==2006 & exp_time==. 
replace exp_time=6 if ks4year==12 & pub696b==2006 & exp_time==. 
replace exp_time=5 if ks4year==12 & pub596b==2006 & exp_time==. | pub596b==2005 & exp_time==. 
replace exp_time=4 if ks4year==12 & pub496b==2006 & exp_time==. | pub496b==2005 & exp_time==.
*check exp_time
tab exp_time
*4=9, 5=456, 6=684, 7=1347, 8=1349

*************************
*****DEFINE EXPOSURE*****
*************************

***PRIMARY EXPOSURES

**Heavy or prolonged (1=yes; 2=no; -2=not started periods)
tab1 pub420 pub520 pub620 pub720 pub820 
label define heavy_prolonged_lbl 0"Not heavy or prolonged" 1"Heavy or prolonged bleeding" 
gen heavy=.
replace heavy=1 if pub420==1 & exp_time==4 | pub520==1 & exp_time==5 | pub620==1 & exp_time==6 | pub720==1 & exp_time==7 | pub820==1 & exp_time==8
replace heavy=0 if pub420==2 & exp_time==4 | pub520==2 & exp_time==5 | pub620==2 & exp_time==6 | pub720==2 & exp_time==7 | pub820==2 & exp_time==8
label values heavy heavy_prolonged_lbl
tab heavy

**Days bleeding 
tab1 pub415 pub515 pub615 pub715 pub815
*exact number (-2=not started periods; responses from 1 to 60) 
tab1 pub416 pub516 pub616 pub716 pub816 
*categorical (-2=not started periods; 1=3 or less; 2=4 to 6; 3=7 or more)
label define days_bleed_lbl 0"Less than 7 days" 1"7 days or more" 
gen days_bleed=.
replace days_bleed=1 if inrange(pub415,7,60) & exp_time==4 | pub416==3 & exp_time==4 | inrange(pub515,7,60) & exp_time==5 | pub516==3 & exp_time==5 | inrange(pub615,7,60) & exp_time==6 | pub616==3 & exp_time==6 | inrange(pub715,7,60) & exp_time==7 | pub716==3 & exp_time==7 | inrange(pub815,7,60) & exp_time==8 | pub816==3 & exp_time==8
replace days_bleed=0 if inrange(pub415,1,6) & exp_time==4 | inrange(pub416,1,2) & exp_time==4 | inrange(pub515,1,6) & exp_time==5 | inrange(pub516,1,2) & exp_time==5 | inrange(pub615,1,6) & exp_time==6 | inrange(pub616,1,2) & exp_time==6 | inrange(pub715,1,6) & exp_time==7 | inrange(pub716,1,2) & exp_time==7 | inrange(pub815,1,6) & exp_time==8 | inrange(pub816,1,2) & exp_time==8
label values days_bleed days_bleed_lbl
tab days_bleed

**Pain (-2=not started period; 1=yes; 2=no except pub723 2 and 3 are moderate or severe; 1 is mild and pub722 1 is yes and 2 is no)
tab1 pub422 pub522 pub622 pub722 pub723 pub822
label define pain_lbl 0"No pain" 1"Pain" 
gen pain=.
replace pain=1 if pub422==1 & exp_time==4 | pub522==1 & exp_time==5 | pub622==1 & exp_time==6 | inrange(pub723,2,3) & exp_time==7 | pub822==1 & exp_time==8
replace pain=0 if pub422==2 & exp_time==4 | pub522==2 & exp_time==5 | pub622==2 & exp_time==6 | pub722==2 & exp_time==7 | pub723==1 & exp_time==7 | pub822==2 & exp_time==8
label values pain pain_lbl
tab pain

**Only severe pain
gen sev_pain=.
replace sev_pain=1 if pub422==1 & exp_time==4 | pub522==1 & exp_time==5 | pub622==1 & exp_time==6 | pub723==3 & exp_time==7 | pub822==1 & exp_time==8
replace sev_pain=0 if pub422==2 & exp_time==4 | pub522==2 & exp_time==5 | pub622==2 & exp_time==6 | pub722==2 & exp_time==7 | inrange(pub723,1,2) & exp_time==7 | pub822==2 & exp_time==8
label values sev_pain pain_lbl
tab sev_pain

***SENSITIVTY EXPOSURES
**4-level heavy or prolonged
label define heavycat_lbl 0"Neither" 1"Prolonged only" 2"Heavy only" 3"Both"
gen heavy_prolonged_cat=.
replace heavy_prolonged_cat=3 if heavy==1 & days_bleed==1
replace heavy_prolonged_cat=2 if heavy==1 & days_bleed==0
replace heavy_prolonged_cat=1 if heavy==0 & days_bleed==1
replace heavy_prolonged_cat=0 if heavy==0 & days_bleed==0
label values heavy_prolonged_cat heavycat_lbl
tab heavy_prolonged_cat

*heavy/prolonged doctor (-3=no heavy bleeding; -2=not started period; 1=yes; 2=no)
tab1 pub421 pub521 pub621 pub721 pub821 
label define heavy_doctor_lbl 2"Went to doctor" 1"No doctor" 0"No heavy bleeding" 
gen heavy_doctor=.
replace heavy_doctor=2 if pub421==1 & exp_time==4 | pub521==1 & exp_time==5 | pub621==1 & exp_time==6 | pub721==1 & exp_time==7 | pub821==1 & exp_time==8
replace heavy_doctor=1 if pub421==2 & exp_time==4 | pub521==2 & exp_time==5 | pub621==2 & exp_time==6 | pub721==2 & exp_time==7 | pub821==2 & exp_time==8 
replace heavy_doctor=0 if pub421==-3 & exp_time==4 | pub521==-3 & exp_time==5 | pub621==-3 & exp_time==6 |pub721==-3 & exp_time==7 | pub821==-3 & exp_time==8
replace heavy_doctor=1 if heavy==1 & heavy_doctor==. 
label values heavy_doctor heavy_doctor_lbl
tab heavy_doctor

*pain doctor (-3=no severe cramps/pain; -2=not started period; 1=yes; 2=no)
tab1 pub423 pub523 pub623 pub724 pub823 
label define pain_doctor_lbl 2"Went to doctor" 1"No doctor" 0"No pain" 
gen pain_doctor=.
replace pain_doctor=2 if pub423==1 & exp_time==4 | pub523==1 & exp_time==5 | pub623==1 & exp_time==6 | pub724==1 & exp_time==7 | pub823==1 & exp_time==8
replace pain_doctor=1 if pub423==2 & exp_time==4 | pub523==2 & exp_time==5 | pub623==2 & exp_time==6 | pub724==2 & exp_time==7 & pain==1 | pub823==2 & exp_time==8 | pain==1 & pain_doctor==.
replace pain_doctor=0 if pub423==-3 & exp_time==4 | pub523==-3 & exp_time==5 | pub623==-3 & exp_time==6 | pub724==2 & exp_time==7 & pain==0 | pub724==-3 & exp_time==7 | pub823==-3 & exp_time==8 | pain==0 & pain_doctor==.
label values pain_doctor pain_doctor_lbl
tab pain_doctor

*severe pain doctor
gen sevpain_doctor=.
replace sevpain_doctor=2 if pub423==1 & exp_time==4 | pub523==1 & exp_time==5 | pub623==1 & exp_time==6 | pub724==1 & exp_time==7 | pub823==1 & exp_time==8
replace sevpain_doctor=1 if pub423==2 & exp_time==4 | pub523==2 & exp_time==5 | pub623==2 & exp_time==6 | pub724==2 & exp_time==7 & sevpain_doctor==1 | pub823==2 & exp_time==8 | sevpain_doctor==1 & sevpain_doctor==.
replace sevpain_doctor=0 if pub423==-3 & exp_time==4 | pub523==-3 & exp_time==5 | pub623==-3 & exp_time==6 | pub724==2 & exp_time==7 & sevpain_doctor==0 | pub724==-3 & exp_time==7 | pub823==-3 & exp_time==8 | sevpain_doctor==0 & sevpain_doctor==.
label values sevpain_doctor pain_doctor_lbl
tab sevpain_doctor

**4-level heavy or pain 
label define heavy_pain 0"Neither" 1"Pain only" 2"Heavy only" 3"Both"
gen heavy_pain_cat=.
replace heavy_pain_cat=0 if heavy==0 & pain==0
replace heavy_pain_cat=1 if heavy==0 & pain==1
replace heavy_pain_cat=2 if heavy==1 & pain==0
replace heavy_pain_cat=3 if heavy==1 & pain==1
label values heavy_pain_cat heavy_pain 
tab heavy_pain_cat

************************
*******COVARIATES*******
************************

***Adjustment set 1 (early childhood SEP)
**Ethnicity c804 (white or non-white; derived, non-white if either mother or father ethncitiy was anything other than white)
label define ethnicity_lbl 1"Non-white" 0"White"
recode c804 (-1=.) (1=0) (2=1), gen (ethnicity)
label values ethnicity ethnicity_lbl
tab ethnicity

**Mat ed c645 c645a
label define mated_lbl 1"CSE/Vocational" 2"O level" 3"A level" 4"Degree"
recode c645 (-1=.) (1/2=1) (3=2) (4=3) (5=4), gen (mated)
label values mated mated_lbl
tab mated

**Parental social class c755 c765
gen mat_binary_social_class_1=.
replace mat_binary_social_class_1=0 if c755==1 | c755==2 | c755==3  
replace mat_binary_social_class_1=1 if c755==4 | c755==5 | c755==6
gen pat_binary_social_class_1=.
replace pat_binary_social_class_1=0 if c765==1 | c765==2 | c765==3 
replace pat_binary_social_class_1=1 if c765==4 | c765==5 | c765==6
gen social_class=.
replace social_class=0 if (mat_binary_social_class_1==0 & pat_binary_social_class_1==0) | (mat_binary_social_class_1==. & pat_binary_social_class_1==0) | (mat_binary_social_class_1==0 & pat_binary_social_class_1==.)
replace social_class=1 if (mat_binary_social_class_1==1 & pat_binary_social_class_1==1) | (mat_binary_social_class_1==. & pat_binary_social_class_1==1) | (mat_binary_social_class_1==1 & pat_binary_social_class_1==.) | (mat_binary_social_class_1==0 & pat_binary_social_class_1==1) | (mat_binary_social_class_1==1 & pat_binary_social_class_1==0)
label define socclass_lbl 0"Non-manual" 1"Manual"
label values social_class socclass_lbl
tab social_class

**Financial difficulties c525 (derived score based on difficulties in food, clothing, heating, rent or mortgage, things you need for baby (lower score for more difficulty) where they subtracted value for each from 20 so higher value meant more difficulty and score ranged from 0 to 15)
*binary version
label define finance_lbl 0"None" 1"Any"
gen financediff_bin=.
replace financediff_bin=0 if c525==0
replace financediff_bin=1 if inrange(c525,1,15)
label values financediff_bin finance_lbl

**Home ownership a006 (mortgaged, owned with no mortgage, rented from council, rented from private landlord furnished, rented from private landlord unfurnished, rented from housing association, or other)
label define housing_lbl 0"Owner or private renter" 1"Renter or non-homeowner"
recode a006 (-7 -1=.) (0 1 3 4=0) (2 5 6=1), gen (housing)
label values housing housing_lbl
tab housing

**Maternal smoking b663 (pre-pregnancy) (resonse options are no, yes cigarettes, yes cigars, yes pipe, or yes other)
label define smoking 0"No" 1"Yes"
recode b663 (1=0) (2/5=1), gen (matsmoke_prepreg)
tab1 matsmoke_prepreg 

***Adjustment set 2 (ACEs - only including those who have answered 50% of questions for that construct - and mums mental health)

**Parental separation (16 variables - divore or separation from partner f228 f237 g308 g317 h218 h227 j308 j317 k4008 k4017 l4008 l4017 p2008 p2017 r5008 r5017)
recode f228 f237 g308 g317 h218 h227 j308 j317 k4008 k4017 l4008 l4017 p2008 p2017 r5008 r5017 (-10 -8 -1=.)
*comment out below to install if needed
*search rmiss2
*create variable that counts how many of varlist are missing (ranges from 0 to 16)
egen parentsep_miss=rmiss2(f228 f237 g308 g317 h218 h227 j308 j317 k4008 k4017 l4008 l4017 p2008 p2017 r5008 r5017)
tab parentsep_miss
*4741 (65.17) have 8 or less variables missing so want to use these people (original version had 3364 people)
label define parentsep_lbl 0"No separation" 1"Any separation"
gen parentsep=.
*yes if ever say yes, no if ever say no and haven't already been coded for saying yes, then put back to mising anyone who has too many missing values (more than 8)
replace parentsep=1 if inrange(f228,1,4) | inrange(f237,1,4) | inrange(g308,1,4) | inrange(g317,1,4) | inrange(h218,1,4) | inrange(h227,1,4) | inrange(j308,1,4) | inrange(j317,1,4) | inrange(k4008,1,4) | inrange(k4017,1,4) | inrange(l4008,1,4) | inrange(l4017,1,4) | inrange(p2008,1,3) | inrange(p2017,1,3) | inrange(r5008,1,3) | inrange(r5017,1,3)
replace parentsep=0 if f228==5 & parentsep==. | f237==5 & parentsep==. | g308==5 & parentsep==. | g317==5 & parentsep==. | h218==5 & parentsep==. | h227==5 & parentsep==. | j308==5 & parentsep==. | j317==5 & parentsep==. | k4008==5 & parentsep==. | k4017==5 & parentsep==. | l4008==5 & parentsep==. | l4017==5 & parentsep==. | p2008==4 & parentsep==. | p2017==4 & parentsep==. | r5008==4 & parentsep==. | r5017==4 & parentsep==. 
replace parentsep=. if inrange(parentsep_miss,9,16)
label values parentsep parentsep_lbl
tab parentsep

**Sexual abuse (8 variables - kf455a kj465 kl475 kn4005 kq365 kt5005 ypb8030 ypb8040)
recode kf455a kj465 kl475 kn4005 kq365 kt5005 ypb8030 ypb804 (-10 -9 -6 -1=.)
*create variable that counts how many of varlist are missing (ranges from 0 to 8)
egen sexabuse_miss=rmiss2(kf455a kj465 kl475 kn4005 kq365 kt5005 ypb8030 ypb804)
tab sexabuse_miss
*4443 (61.07) have 4 or less variables missing (original version of variable only had 1703 obs)
label define abuse_lbl 0"No abuse" 1"Any abuse"
gen sexualabuse=.
*yes if ever say yes, no if they have ever said no and haven't already been coded for saying yes, and then put back to missing anyone who has too many missing values (more than 4)
replace sexualabuse=1 if kf455a==1 | inrange(kj465,1,4) | inrange(kl475,1,3) | inrange(kn4005,1,4) | inrange(kq365,1,4) | inrange(kt5005,1,3) | inrange(ypb8030,2,3) | inrange(ypb8040,2,3)
replace sexualabuse=0 if kf455a==2 & sexualabuse==. | kj465==5 & sexualabuse==. | kl475==5 & sexualabuse==. | kn4005==5 & sexualabuse==. | kq365==5 & sexualabuse==. | kt5005==5 & sexualabuse==. | ypb8030==1 & sexualabuse==. | ypb8040==1 & sexualabuse==.
replace sexualabuse=. if inrange(sexabuse_miss,5,8)
label values sexualabuse abuse_lbl
tab sexualabuse

**Physical abuse (18 variables - f246 f247 g326 g327 h236 h237 j327 k4026 k4027 l4026 l4027 p2026 p2027 r5026 r5027 ypb8002 ypb8003 ypb8007)
recode f246 f247 g326 g327 h236 h237 j327 k4026 k4027 l4026 l4027 p2026 p2027 r5026 r5027 ypb8002 ypb8003 ypb8007 (-10 -9 -1=.)
*create var that counts how many of varlist are missing (ranges from 0 to 18)
egen physabuse_miss=rmiss2(f246 f247 g326 g327 h236 h237 j327 k4026 k4027 l4026 l4027 p2026 p2027 r5026 r5027 ypb8002 ypb8003 ypb8007)
tab physabuse_miss
*4490 (61.72) have 9 or less variables missing (original version only had 2280)
gen physicalabuse=.
*yes if ever say yes, no if ever say no and not yet coded as yes, then put back to missing anyone with too many missing values (more than 9)
replace physicalabuse=1 if inrange(f246,1,4) | inrange(f247,1,4) | inrange(g326,1,4) | inrange(g327,1,4) | inrange(h236,1,3) | inrange(h237,1,3) | inrange(j327,1,4) | inrange(k4026,1,4) | inrange(k4027,1,4) | inrange(l4026,1,4) | inrange(l4027,1,4) | inrange(p2026,1,3) | inrange(p2027,1,3) | inrange(r5026,1,3) | inrange(r5027,1,3) | inrange(ypb8002,2,5) | inrange(ypb8003,2,5) | inrange(ypb8007,2,5)
replace physicalabuse=0 if f246==5 & physicalabuse==. | f247==5 & physicalabuse==. | g326==5 & physicalabuse==. | g327==5 & physicalabuse==. | h236==5 & physicalabuse==. | h237==5 & physicalabuse==. | j327==5 & physicalabuse==. | k4026==5 & physicalabuse==. | k4027==5 & physicalabuse==. | l4026==5 & physicalabuse==. | l4027==5 & physicalabuse==. | p2026==4 & physicalabuse==. | p2027==4 & physicalabuse==. | r5026==4 & physicalabuse==. | r5027==4 & physicalabuse==. | ypb8002==1 & physicalabuse==. | ypb8003==1 & physicalabuse==. | ypb8007==1 &physicalabuse==. 
replace physicalabuse=. if inrange(physabuse_miss,10,18)
label values physicalabuse abuse_lbl
tab physicalabuse

**Maternal depression (s1011; in last 2 years - yes doctor; yes no doctor; no)
label define yes_no_lbl 0"No" 1"Yes"
recode s1011 (-10 -1=.) (1 2=1) (3=0), gen (mat_dep12)
label values mat_dep12 yes_no_lbl
tab mat_dep12

***Adjustment set 3
**Age at menarche 
*clon070 (continuous age) clon071 (early, normal, and late onset) - focus on clon070
recode clon070 (-10 -1=.), gen (menarche)
summ menarche, detail

***Adjustment set 4 
**BMI - ff2039 (weight over height squared)
gen bmi12=ff2039
replace bmi12=. if bmi12<0
summ bmi12, detail

***Adjustment set 5 (child's mental health) - age 9
**Internalising problems from SDQ (combine the emotional and peer problems scales - ku707b ku709b; continuous from 0 to 10 each (although no one scored 10 on peer problems)) 
tab1 ku707b ku709b
recode ku707b ku709b (-10 -6 -5=.)
gen sdq_internal9=ku707b + ku709b
summ sdq_internal9, detail
**Externalising problems from SDQ (combine the hyperactivity and conduct problems scales - ku706b ku708b; continuous from 0 to 10 each)
tab1 ku706b ku708b
recode ku706b ku708b (-10 -6 -5=.)
gen sdq_external9=ku706b + ku708b
summ sdq_external9, detail 

***Contraception 
tab1 pub427 pub527 pub627 pub727 pub827
*-2=not started periods, 1=yes, 2=no, 9=don't know 
label define cont_lbl 0"No or not started periods" 1"Yes" 
gen contraception=.
replace contraception=1 if pub427==1 & exp_time==4 | pub527==1 & exp_time==5 | pub627==1 & exp_time==6 | pub727==1 & exp_time==7 | pub827==1 & exp_time==8
replace contraception=0 if pub427==2 & exp_time==4 | pub527==2 & exp_time==5 | pub627==2 & exp_time==6 | pub727==2 & exp_time==7 | pub827==2 & exp_time==8 | pub427==-2 & exp_time==4 | pub527==-2 & exp_time==5 | pub627==-2 & exp_time==6 | pub727==-2 & exp_time==7 | pub827==-2 & exp_time==8
label values contraception cont_lbl
tab contraception 

***Competing exposure
**IQ age 8 - 110 is verbal IQ (cont), 111 is performance IQ (cont), 112 is total IQ (cont), and 115 is categorical total IQ
tab1 f8ws110 f8ws111 f8ws112 f8ws115 
*Use total IQ for confounder
recode f8ws112 (-3 -2=.), gen (iq)
summ iq, detail 

************************
********OUTCOMES********
************************

**GCSEs
recode ks4_ptscnewe (-10 -1=.), gen (capped_gcse)
summ capped_gcse, detail
*range from 0 to 540, mean 331.78 (88.09), skewness -1.26 (5.06)

**Sensitivity GCSE outcome - variables to come
recode ks4_level2_em (-10=.), gen (gcse_fiveac)
label values gcse_fiveac yes_no_lbl

**ABSENCES
*arranged by calendar year

*Recode -10 to missing
foreach year in 2007 2008 2009 {
foreach var in lab_totalabsence`year' lab_totalsessionsposs`year' lab_percentofsessionsmissed`year' {
recode `var' -10=.
	}
}
*Don't have absence data at each year for all 3 'cohorts' so we are only looking in their year 11 - C1/0607=2007; C2/0708=2008; C3/0809=2009
*ks4year variable is coded so that 12 is 2006/2007, 13 is 2007/2008, and 14 is 2008/2009

**ISSUES WITH PERCENTAGE DERIVED VARIABLE 
**Firstly, they coded to missing anyone with a 0 for number of sessions absent or total sessions (even though presumably there would have been some)
foreach year in 2007 2008 2009 {
count if lab_percentofsessionsmissed`year'==. & lab_totalsessionsposs`year'==0
count if lab_percentofsessionsmissed`year'==. & lab_totalabsence`year'==0 
}
*13 for 2007, 1197 for 2008, and 4369 for 2009
**Secondly, includes implausible values for possible sessions (some values up to 604)
*Weird outliers, not about exam period in year 11:
foreach year in 2007 2008 2009 {
summ lab_totalsessionsposs`year' if ks4year==12, det
summ lab_totalsessionsposs`year' if ks4year==13, det
summ lab_totalsessionsposs`year' if ks4year==14, det
}
*rename the existing derived vars (proportion instead of percentage):
rename lab_percentofsessionsmissed2007 lab_propofsessionsmissed2007
rename lab_percentofsessionsmissed2008 lab_propofsessionsmissed2008
rename lab_percentofsessionsmissed2009 lab_propofsessionsmissed2009

**ALIGN WITH SCHOOL YEARS
foreach varstem in lab_totalabsence lab_totalsessionsposs lab_propofsessionsmissed {
gen `varstem'year11=`varstem'2007 if ks4year==12
replace `varstem'year11=`varstem'2008 if ks4year==13
replace `varstem'year11=`varstem'2009 if ks4year==14
summ `varstem'year11, det
}
summ lab_totalabsenceyear11 lab_totalsessionspossyear11 lab_propofsessionsmissedyear11
*5186 obs for total absences and total sessions, 5138 for proportion (48 people with 0 for absences/sessions are missing here)

**MAKE NEW PERCENTAGE VARIABLES
**Exploring possible sessions variables 
summ lab_totalsessionspossyear11, det
hist lab_totalsessionspossyear11
*range from 0 to 604, really skewed (-6.03), kurtosis 49.84
*Look at sessions variables where the values don't include 0
summ lab_totalsessionspossyear11 if lab_totalsessionspossyear11!=0, det
*ranges from 2 to 604, still skewed etc.
*How many observations are outside of typical/plausible values (i.e. less than 230 or more than 320)
count if lab_totalsessionspossyear11!=0 & lab_totalsessionspossyear11!=. & (lab_totalsessionspossyear11<230 | lab_totalsessionspossyear11>320)
*39
*Look at key variables amongst those with low total session values (less than 230)
list lab_totalsessionspossyear11 lab_totalabsenceyear11 lab_propofsessionsmissedyear11 ks4_nftype ks4_toe_code if lab_totalsessionspossyear11!=0 & lab_totalsessionspossyear11!=. & lab_totalsessionspossyear11<230 
*Look at key variables amongst those with low total session values (more than 320)
list lab_totalsessionspossyear11 lab_totalabsenceyear11 lab_propofsessionsmissedyear11 ks4_nftype ks4_toe_code if lab_totalsessionspossyear11!=0 & lab_totalsessionspossyear11!=. &  lab_totalsessionspossyear11>320
*Possible that some school types report differently here (PRUs and further education have a much lower mean number of total sessions than the others - 37 and 29 compared to 284-380)
tab ks4_toe_code, summ (lab_totalsessionspossyear11)
*There is still useful information in the low values, so just trim the top outliers (above plausible values - record to end of May = 33 weeks/330 sessions -> only 6)
count if lab_totalsessionspossyear11!=0 & lab_totalsessionspossyear11!=. &  lab_totalsessionspossyear11>330
**Derive a variable to flag outliers in total sessions (zeros or high values)
*Zeros for possible sessions contain no info
gen totalsessionsposs_outlieryear11=.
replace totalsessionsposs_outlieryear11=lab_totalsessionspossyear11 if lab_totalsessionspossyear11==0
replace totalsessionsposs_outlieryear11=lab_totalsessionspossyear11 if lab_totalsessionspossyear11>330 & lab_totalsessionspossyear11!=.
*Examine what's left
summ lab_totalsessionspossyear11 if totalsessionsposs_outlieryear11==., det
*Fine - range from 2 to 320.
**Derive new percentage variable (missing if total sessions variable is an outlier)
gen percentabsent=(lab_totalabsenceyear11/lab_totalsessionspossyear11)*100 if totalsessionsposs_outlieryear11==.
*Check that those who have a value for the outlier var are missing for percentage
list percentabsent if totalsessionsposs_outlieryear11!=.
*Check variable (range from 0 to 100, mean 9.18 (10.97), very right skewed (3.65))
summ percentabsent, det
hist percentabsent

**Log-transform for imputation and analysis, adding 1 for the zeros now this is in percent: 
gen log_percentabsent=log(percentabsent+1)
summ log_percentabsent, det
hist log_percentabsent 

**Check against existing derived vars (where proportion var was missing - there are no obs who are missing original prop var and have a value for percentabsent, because we didn't use those who had zeros for total sessions)
list lab_totalabsenceyear11 lab_totalsessionspossyear11 lab_propofsessionsmissedyear11 percentabsent if lab_propofsessionsmissedyear11==. & percentabsent!=. 
count if lab_propofsessionsmissedyear11==. & percentabsent!=. 
*Post-imputation, untransform and remove the 0.01

**SENSITIVTY ABSENCE OUTCOME
*Binary <10% absent versus 10% or more absent (persistent)
label define abs_lbl 0"<10% Absence" 1"Persistent Absence"
gen persistent_abs=.
replace persistent_abs=0 if inrange(percentabsent,0,9.99)
replace persistent_abs=1 if inrange(percentabsent,10,100)
label values persistent_abs abs_lbl
*Check
tab persistent_abs
summ percentabsent if persistent_abs==0
summ percentabsent if persistent_abs==1

********************************************************************************************************************************************************************************************************************
*COMPLETE CASE

********************************************************************************
*********************************DESCRIPTIVES***********************************
********************************************************************************
*Restrict to sample with complete exposure and outcome data 
mark exp_out_use
markout exp_out_use heavy days_bleed pain capped_gcse percentabsent
*2698 with complete exposure and outcome data 

log using "S:\B4175_howe - B4175_howe\CompleteCase_descriptives", replace

**Main exposures
tab1 heavy days_bleed pain if exp_out_use==1

**Main outcomes
summ capped_gcse if exp_out_use==1, detail
summ percentabsent if exp_out_use==1, detail

*Covarites according to main exposure variables
foreach exp of varlist heavy days_bleed pain {
	foreach cov of varlist ethnicity mated social_class financediff_bin housing matsmoke_prepreg mat_dep12 parentsep sexualabuse physicalabuse contraception {
		tab `exp' `cov' if exp_out_use==1, row chi2
	}
}
foreach exp of varlist heavy days_bleed pain {
	foreach cov of varlist menarche bmi12 sdq_internal9 sdq_external9 iq {
		ttest `cov' if exp_out_use==1, by (`exp')
	}
}

**Sensitivity menstrual symptom patterns
*Prevalence
tab1 heavy_prolonged_cat heavy_pain_cat heavy_doctor pain_doctor

**Doctor variables - socioeconomic patterning 
foreach exp of varlist heavy_doctor pain_doctor {
	foreach sep of varlist mated social_class housing financediff_bin matsmoke_prepreg {
		tab `exp' `sep' if exp_out_use==1, row 
	}
}

**Sensitivity outcomes
tab persistent_abs if exp_out_use==1
tab gcse_fiveac if exp_out_use==1

********************************************************************************
***********************************ANALYSIS*************************************
********************************************************************************

**Explore data 
foreach exp of varlist heavy days_bleed pain {
	foreach out of varlist capped_gcse percentabsent {
		tab `exp' if exp_out_use==1, summ (`out')
		ttest `out' if exp_out_use==1, by (`exp') 
	}
}
*Dotplots
dotplot capped_gcse if exp_out_use==1, over (heavy) mean
dotplot log_percentabsent if exp_out_use==1, over (heavy) mean
dotplot capped_gcse if exp_out_use==1, over (days_bleed) mean
dotplot log_percentabsent if exp_out_use==1, over (days_bleed) mean
dotplot capped_gcse if exp_out_use==1, over (pain) mean
dotplot log_percentabsent if exp_out_use==1, over (pain) mean

**Regression models
*Restricting to exposures, outcomes, and confounders
mark touse
markout touse heavy days_bleed pain capped_gcse percentabsent ethnicity mated social_class financediff_bin housing matsmoke_prepreg mat_dep12 parentsep sexualabuse physicalabuse menarche bmi12 sdq_internal9 sdq_external9 iq
*1274 with complete data (1258 also have contraception)

**Results into excel file 
*Main analysis structure: 1) crude, 2) SEP adjusted 3) adversity adjusted 4) menarche adjusted 5) bmi adjusted 6) mental health adjusted 7) iq adjusted 
capture erase "main_results.xls"
eststo clear
foreach exp of varlist heavy days_bleed pain {
	eststo `exp': regress capped_gcse `exp' if touse==1
	eststo `exp'_sep: regress capped_gcse `exp' i.ethnicity i.mated i.social_class i.financediff_bin i.housing i.matsmoke_prepreg if touse==1
	eststo `exp'_adv: regress capped_gcse `exp' i.ethnicity i.mated i.social_class i.financediff_bin i.housing i.matsmoke_prepreg  i.mat_dep12 i.parentsep i.sexualabuse i.physicalabuse if touse==1
	eststo `exp'_men: regress capped_gcse `exp' i.ethnicity i.mated i.social_class i.financediff_bin i.housing i.matsmoke_prepreg  i.mat_dep12 i.parentsep i.sexualabuse i.physicalabuse menarche if touse==1
	eststo `exp'_bmi: regress capped_gcse `exp' i.ethnicity i.mated i.social_class i.financediff_bin i.housing i.matsmoke_prepreg  i.mat_dep12 i.parentsep i.sexualabuse i.physicalabuse menarche bmi12 if touse==1
	eststo `exp'_mh: regress capped_gcse `exp' i.ethnicity i.mated i.social_class i.financediff_bin i.housing i.matsmoke_prepreg  i.mat_dep12 i.parentsep i.sexualabuse i.physicalabuse menarche bmi12 sdq_internal9 sdq_external9 if touse==1
	eststo `exp'_iq: regress capped_gcse `exp' i.ethnicity i.mated i.social_class i.financediff_bin i.housing i.matsmoke_prepreg  i.mat_dep12 i.parentsep i.sexualabuse i.physicalabuse menarche bmi sdq_internal9 sdq_external9 iq if touse==1
	estout `exp' `exp'_sep `exp'_adv `exp'_men `exp'_bmi `exp'_mh `exp'_iq using "main_results.xls", append cells("b(fmt(4)) ci_l(fmt(4)) ci_u(fmt(4)) p(fmt(4)) se(fmt(4))") keep(*`exp'*) ///
	title (GCSE SCORE) note(crude; adjusted for early SEP; adjusted for adversity; adjusted for menarche; adjusted for bmi; adjusted for mental health; adjusted for iq)
}
eststo clear
foreach exp of varlist heavy days_bleed pain {
	eststo `exp': regress log_percentabsent `exp' if touse==1
	eststo `exp'_sep: regress log_percentabsent `exp' i.ethnicity i.mated i.social_class i.financediff_bin i.housing i.matsmoke_prepreg if touse==1
	eststo `exp'_adv: regress log_percentabsent `exp' i.ethnicity i.mated i.social_class i.financediff_bin i.housing i.matsmoke_prepreg  i.mat_dep12 i.parentsep i.sexualabuse i.physicalabuse if touse==1
	eststo `exp'_men: regress log_percentabsent `exp' i.ethnicity i.mated i.social_class i.financediff_bin i.housing i.matsmoke_prepreg  i.mat_dep12 i.parentsep i.sexualabuse i.physicalabuse menarche if touse==1
	eststo `exp'_bmi: regress log_percentabsent `exp' i.ethnicity i.mated i.social_class i.financediff_bin i.housing i.matsmoke_prepreg  i.mat_dep12 i.parentsep i.sexualabuse i.physicalabuse menarche bmi12 if touse==1
	eststo `exp'_mh: regress log_percentabsent `exp' i.ethnicity i.mated i.social_class i.financediff_bin i.housing i.matsmoke_prepreg  i.mat_dep12 i.parentsep i.sexualabuse i.physicalabuse menarche bmi12 sdq_internal9 sdq_external9 if touse==1
	eststo `exp'_iq: regress log_percentabsent `exp' i.ethnicity i.mated i.social_class i.financediff_bin i.housing i.matsmoke_prepreg  i.mat_dep12 i.parentsep i.sexualabuse i.physicalabuse menarche bmi12 sdq_internal9 sdq_external9 iq if touse==1
	estout `exp' `exp'_sep `exp'_adv `exp'_men `exp'_bmi `exp'_mh `exp'_iq using "main_results.xls", append cells("b(fmt(4)) ci_l(fmt(4)) ci_u(fmt(4)) p(fmt(4)) se(fmt(4))") keep(*`exp'*) eform ///
	title (ABSENCES) note(crude; adjusted for early SEP; adjusted for adversity; adjusted for menarche; adjusted for bmi; adjusted for mental health; adjusted for iq)
}

*Doctor sensitivity analysis structure: 1) doctor vars crude 2) doctor vars adjusted 3) doctor vars iq adjusted 
capture erase "sensitivity_results.xls"
eststo clear
foreach exp of varlist heavy_doctor pain_doctor {
	eststo `exp': regress capped_gcse i.`exp' if touse==1
	eststo `exp'_mh: regress capped_gcse i.`exp' i.ethnicity i.mated i.social_class i.financediff_bin i.housing i.matsmoke_prepreg  i.mat_dep12 i.parentsep i.sexualabuse i.physicalabuse menarche bmi12 sdq_internal9 sdq_external9 if touse==1
	eststo `exp'_iq: regress capped_gcse i.`exp' i.ethnicity i.mated i.social_class i.financediff_bin i.housing i.matsmoke_prepreg  i.mat_dep12 i.parentsep i.sexualabuse i.physicalabuse menarche bmi12 sdq_internal9 sdq_external9 iq if touse==1
	estout `exp' `exp'_mh `exp'_iq using "sensitivity_results.xls", append cells("b(fmt(4)) ci_l(fmt(4)) ci_u(fmt(4)) p(fmt(4)) se(fmt(4))") keep(*1.`exp'* *2.`exp'*) ///
	title (GCSE SCORE - `exp') note(1=no doctor; 2=doctor: crude; adjusted for early SEP, adversity, menarche, bmi, and mental health; adjusted for iq)
}
eststo clear
foreach exp of varlist heavy_doctor pain_doctor {
	eststo `exp': regress log_percentabsent i.`exp' if touse==1
	eststo `exp'_mh: regress log_percentabsent i.`exp' i.ethnicity i.mated i.social_class i.financediff_bin i.housing i.matsmoke_prepreg  i.mat_dep12 i.parentsep i.sexualabuse i.physicalabuse menarche bmi12 sdq_internal9 sdq_external9 if touse==1
	eststo `exp'_iq: regress log_percentabsent i.`exp' i.ethnicity i.mated i.social_class i.financediff_bin i.housing i.matsmoke_prepreg  i.mat_dep12 i.parentsep i.sexualabuse i.physicalabuse menarche bmi12 sdq_internal9 sdq_external9 iq if touse==1
	estout `exp' `exp'_mh `exp'_iq using "sensitivity_results.xls", append cells("b(fmt(4)) ci_l(fmt(4)) ci_u(fmt(4)) p(fmt(4)) se(fmt(4))") keep(*1.`exp'* *2.`exp'*) eform ///
	title (ABSENCES- `exp') note(1=no doctor; 2=doctor: crude; adjusted for early SEP, adversity, menarche, bmi, and mental health; adjusted for iq)
}

*4-level heavy variable sensitivity analysis structure: 1) heavy cat crude 2) heavy cat adjusted 3) heavy cat iq adjusted
eststo clear
eststo heavycat: regress capped_gcse i.heavy_prolonged_cat if touse==1
eststo heavycat_mh: regress capped_gcse i.heavy_prolonged_cat i.ethnicity i.mated i.social_class i.financediff_bin i.housing i.matsmoke_prepreg  i.mat_dep12 i.parentsep i.sexualabuse i.physicalabuse menarche bmi12 sdq_internal9 sdq_external9 if touse==1
eststo heavycat_iq: regress capped_gcse i.heavy_prolonged_cat i.ethnicity i.mated i.social_class i.financediff_bin i.housing i.matsmoke_prepreg  i.mat_dep12 i.parentsep i.sexualabuse i.physicalabuse menarche bmi12 sdq_internal9 sdq_external9 iq if touse==1
estout heavycat heavycat_mh heavycat_iq using "sensitivity_results.xls", append cells("b(fmt(4)) ci_l(fmt(4)) ci_u(fmt(4)) p(fmt(4)) se(fmt(4))") ///
keep(*1.heavy_prolonged_cat* *2.heavy_prolonged_cat* *3.heavy_prolonged_cat*) ///
title (GCSE SCORE - Categorical heavy) note(1=prolonged only; 2=heavy only; 3=both: crude; adjusted for early SEP, adversity, menarche, bmi, and mental health; adjusted for iq)
eststo clear
eststo heavycat: regress log_percentabsent i.heavy_prolonged_cat if touse==1
eststo heavycat_mh: regress log_percentabsent i.heavy_prolonged_cat i.ethnicity i.mated i.social_class i.financediff_bin i.housing i.matsmoke_prepreg  i.mat_dep12 i.parentsep i.sexualabuse i.physicalabuse menarche bmi12 sdq_internal9 sdq_external9 if touse==1
eststo heavycat_iq: regress log_percentabsent i.heavy_prolonged_cat i.ethnicity i.mated i.social_class i.financediff_bin i.housing i.matsmoke_prepreg  i.mat_dep12 i.parentsep i.sexualabuse i.physicalabuse menarche bmi12 sdq_internal9 sdq_external9 iq if touse==1
estout heavycat heavycat_mh heavycat_iq using "sensitivity_results.xls", append cells("b(fmt(4)) ci_l(fmt(4)) ci_u(fmt(4)) p(fmt(4)) se(fmt(4))") ///
keep(*1.heavy_prolonged_cat* *2.heavy_prolonged_cat* *3.heavy_prolonged_cat*) eform ///
title (ABSENCES - Categorical heavy) note(1=prolonged only; 2=heavy only; 3=both: crude; adjusted for early SEP, adversity, menarche, bmi, and mental health; adjusted for iq)

*Sensitivity combined heavy and pain analysis structure: 1) heavy pain cat crude 2) heavy pain cat adjusted 3) heavy pain cat iq adjusted 
eststo clear
eststo heavy_pain: regress capped_gcse i.heavy_pain_cat if touse==1
eststo heavy_pain_mh: regress capped_gcse i.heavy_pain_cat i.ethnicity i.mated i.social_class i.financediff_bin i.housing i.matsmoke_prepreg  i.mat_dep12 i.parentsep i.sexualabuse i.physicalabuse menarche bmi12 sdq_internal9 sdq_external9 if touse==1
eststo heavy_pain_iq: regress capped_gcse i.heavy_pain_cat i.ethnicity i.mated i.social_class i.financediff_bin i.housing i.matsmoke_prepreg  i.mat_dep12 i.parentsep i.sexualabuse i.physicalabuse menarche bmi12 sdq_internal9 sdq_external9 iq if touse==1
estout heavy_pain heavy_pain_mh heavy_pain_iq using "sensitivity_results.xls", append cells("b(fmt(4)) ci_l(fmt(4)) ci_u(fmt(4)) p(fmt(4)) se(fmt(4))") ///
keep(*1.heavy_pain_cat* *2.heavy_pain_cat* *3.heavy_pain_cat*) ///
title (GCSE SCORE - Heavy and pain combined) note(1=pain only; 2=heavy only; 3=both: crude; adjusted for early SEP, adversity, menarche, bmi, and mental health; adjusted for iq)
eststo clear
eststo heavy_pain: regress log_percentabsent i.heavy_pain_cat if touse==1
eststo heavy_pain_mh: regress log_percentabsent i.heavy_pain_cat i.ethnicity i.mated i.social_class i.financediff_bin i.housing i.matsmoke_prepreg  i.mat_dep12 i.parentsep i.sexualabuse i.physicalabuse menarche bmi12 sdq_internal9 sdq_external9 if touse==1
eststo heavy_pain_iq: regress log_percentabsent i.heavy_pain_cat i.ethnicity i.mated i.social_class i.financediff_bin i.housing i.matsmoke_prepreg  i.mat_dep12 i.parentsep i.sexualabuse i.physicalabuse menarche bmi12 sdq_internal9 sdq_external9 iq if touse==1
estout heavy_pain heavy_pain_mh heavy_pain_iq using "sensitivity_results.xls", append cells("b(fmt(4)) ci_l(fmt(4)) ci_u(fmt(4)) p(fmt(4)) se(fmt(4))") ///
keep(*1.heavy_pain_cat* *2.heavy_pain_cat* *3.heavy_pain_cat*) eform ///
title (ABSENCES - Heavy and pain combined) note(1=pain only; 2=heavy only; 3=both: crude; adjusted for early SEP, adversity, menarche, bmi, and mental health; adjusted for iq)

**Persistent absence binary outcome analysis structure: 1) crude 2) adjusted 3) iq adjusted  
eststo clear
foreach exp of varlist heavy days_bleed pain {
	eststo `exp': quietly logistic persistent_abs `exp' if touse==1
	eststo `exp'_mh: quietly logistic persistent_abs `exp' i.ethnicity i.mated i.social_class i.financediff_bin i.housing i.matsmoke_prepreg  i.mat_dep12 i.parentsep i.sexualabuse i.physicalabuse menarche bmi12 sdq_internal9 sdq_external9 if touse==1
	eststo `exp'_iq: quietly logistic persistent_abs `exp' i.ethnicity i.mated i.social_class i.financediff_bin i.housing i.matsmoke_prepreg  i.mat_dep12 i.parentsep i.sexualabuse i.physicalabuse menarche bmi12 sdq_internal9 sdq_external9 iq if touse==1
	estout `exp' `exp'_mh `exp'_iq using "sensitivity_results.xls", eform append cells("b(fmt(4)) ci_l(fmt(4)) ci_u(fmt(4)) p(fmt(4)) se(fmt(4))") keep(*`exp'*) ///
	title (Binary Persistent Absence - `exp') note(ODDS RATIOS: crude; adjusted for early SEP, adversity, menarche, bmi, and mental health; adjusted for iq)
}

**Five A*-C including maths and english binary outcome analysis structure: 1) crude 2) adjusted 3) iq adjusted  
eststo clear
foreach exp of varlist heavy days_bleed pain {
	eststo `exp': quietly logistic gcse_fiveac `exp' if touse==1
	eststo `exp'_mh: quietly logistic gcse_fiveac `exp' i.ethnicity i.mated i.social_class i.financediff_bin i.housing i.matsmoke_prepreg  i.mat_dep12 i.parentsep i.sexualabuse i.physicalabuse menarche bmi12 sdq_internal9 sdq_external9 if touse==1
	eststo `exp'_iq: quietly logistic gcse_fiveac `exp' i.ethnicity i.mated i.social_class i.financediff_bin i.housing i.matsmoke_prepreg  i.mat_dep12 i.parentsep i.sexualabuse i.physicalabuse menarche bmi12 sdq_internal9 sdq_external9 iq if touse==1
	estout `exp' `exp'_mh `exp'_iq using "sensitivity_results.xls", eform append cells("b(fmt(4)) ci_l(fmt(4)) ci_u(fmt(4)) p(fmt(4)) se(fmt(4))") keep(*`exp'*) ///
	title (Binary Five A*-C including Maths and English - `exp') note(ODDS RATIOS: crude; adjusted for early SEP, adversity, menarche, bmi, and mental health; adjusted for iq)
}

*Contraception sensitivity analysis structure: 1) adjusted + contraception 2) iq adjusted + contraception 
eststo clear
foreach exp of varlist heavy days_bleed pain {
	eststo `exp'_mh: regress capped_gcse `exp' i.ethnicity i.mated i.social_class i.financediff_bin i.housing i.matsmoke_prepreg  i.mat_dep12 i.parentsep i.sexualabuse i.physicalabuse menarche bmi12 sdq_internal9 sdq_external9 contraception if touse==1
	eststo `exp'_iq: regress capped_gcse `exp' i.ethnicity i.mated i.social_class i.financediff_bin i.housing i.matsmoke_prepreg  i.mat_dep12 i.parentsep i.sexualabuse i.physicalabuse menarche bmi12 sdq_internal9 sdq_external9 iq contraception if touse==1
	estout `exp'_mh `exp'_iq using "sensitivity_results.xls", append cells("b(fmt(4)) ci_l(fmt(4)) ci_u(fmt(4)) p(fmt(4)) se(fmt(4))") keep(*`exp'*) ///
	title (GCSE SCORE - Contraception adjusted) note(adjusted for early SEP, adversity, menarche, bmi, mental health and contraception; adjusted for iq)
}
eststo clear
foreach exp of varlist heavy days_bleed pain {
	eststo `exp'_mh: regress log_percentabsent `exp' i.ethnicity i.mated i.social_class i.financediff_bin i.housing i.matsmoke_prepreg  i.mat_dep12 i.parentsep i.sexualabuse i.physicalabuse menarche bmi12 sdq_internal9 sdq_external9 contraception if touse==1
	eststo `exp'_iq: regress log_percentabsent `exp' i.ethnicity i.mated i.social_class i.financediff_bin i.housing i.matsmoke_prepreg  i.mat_dep12 i.parentsep i.sexualabuse i.physicalabuse menarche bmi12 sdq_internal9 sdq_external9 iq contraception if touse==1
	estout `exp'_mh `exp'_iq using "sensitivity_results.xls", append cells("b(fmt(4)) ci_l(fmt(4)) ci_u(fmt(4)) p(fmt(4)) se(fmt(4))") keep(*`exp'*) eform ///
	title (ABSENCES - Contraception adjusted) note(adjusted for early SEP, adversity, menarche, bmi, mental health and contraception; adjusted for iq)
}

*Contraception stratified analysis - do in greatest sample available 1) crude in all contraception responders 2) crude in not using cont 3) crude in using cont 4) fully adjusted in all contraception responders 5) full adjusted in not using cont 6) fully adjusted in using cont
eststo clear
foreach exp of varlist heavy days_bleed pain {
	eststo `exp'_all_crude: regress capped_gcse `exp' if exp_out_use==1 & contraception!=.
	eststo `exp'_nocont_crude: regress capped_gcse `exp' if exp_out_use==1 & contraception==0
	eststo `exp'_cont_crude: regress capped_gcse `exp' if exp_out_use==1 & contraception==1
	eststo `exp'_all_adj: regress capped_gcse `exp' i.ethnicity i.mated i.social_class i.financediff_bin i.housing i.matsmoke_prepreg  i.mat_dep12 i.parentsep i.sexualabuse i.physicalabuse menarche bmi12 sdq_internal9 sdq_external9 iq if exp_out_use==1 & contraception!=.
	eststo `exp'_nocont_adj: regress capped_gcse `exp' i.ethnicity i.mated i.social_class i.financediff_bin i.housing i.matsmoke_prepreg  i.mat_dep12 i.parentsep i.sexualabuse i.physicalabuse menarche bmi12 sdq_internal9 sdq_external9 iq if exp_out_use==1 & contraception==0
	eststo `exp'_cont_adj: regress capped_gcse `exp' i.ethnicity i.mated i.social_class i.financediff_bin i.housing i.matsmoke_prepreg  i.mat_dep12 i.parentsep i.sexualabuse i.physicalabuse menarche bmi12 sdq_internal9 sdq_external9 iq if exp_out_use==1 & contraception==1
	estout `exp'_all_crude `exp'_nocont_crude `exp'_cont_crude `exp'_all_adj `exp'_nocont_adj `exp'_cont_adj using "sensitivity_results.xls", append cells("b(fmt(4)) ci_l(fmt(4)) ci_u(fmt(4)) p(fmt(4)) se(fmt(4))") keep(*`exp'*) ///
	title (GCSE SCORE - Contraception stratified) note(crude all; crude no contraception; crude contraception; adjusted all; adjusted no contraception; adjusted contraception)
}
eststo clear
foreach exp of varlist heavy days_bleed pain {
	eststo `exp'_all_crude: regress log_percentabsent `exp' if exp_out_use==1 & contraception!=.
	eststo `exp'_nocont_crude: regress log_percentabsent `exp' if exp_out_use==1 & contraception==0
	eststo `exp'_cont_crude: regress log_percentabsent `exp' if exp_out_use==1 & contraception==1
	eststo `exp'_all_adj: regress log_percentabsent `exp' i.ethnicity i.mated i.social_class i.financediff_bin i.housing i.matsmoke_prepreg  i.mat_dep12 i.parentsep i.sexualabuse i.physicalabuse menarche bmi12 sdq_internal9 sdq_external9 iq if exp_out_use==1 & contraception!=.
	eststo `exp'_nocont_adj: regress log_percentabsent `exp' i.ethnicity i.mated i.social_class i.financediff_bin i.housing i.matsmoke_prepreg  i.mat_dep12 i.parentsep i.sexualabuse i.physicalabuse menarche bmi12 sdq_internal9 sdq_external9 iq if exp_out_use==1 & contraception==0
	eststo `exp'_cont_adj: regress log_percentabsent `exp' i.ethnicity i.mated i.social_class i.financediff_bin i.housing i.matsmoke_prepreg  i.mat_dep12 i.parentsep i.sexualabuse i.physicalabuse menarche bmi12 sdq_internal9 sdq_external9 iq if exp_out_use==1 & contraception==1
	estout `exp'_all_crude `exp'_nocont_crude `exp'_cont_crude `exp'_all_adj `exp'_nocont_adj `exp'_cont_adj using "sensitivity_results.xls", append cells("b(fmt(4)) ci_l(fmt(4)) ci_u(fmt(4)) p(fmt(4)) se(fmt(4))") keep(*`exp'*) eform ///
	title (ABSENCES - Contraception stratified) note(crude all; crude no contraception; crude contraception; adjusted all; adjusted no contraception; adjusted contraception)
}
eststo clear

********************************************************************************
**********************CALCULATE NUMBER OF SESSIONS MISSED***********************
********************************************************************************

log using "S:\B4175_howe - B4175_howe\N_sessions_missed", replace

**Median number of available half-day sessions amongst sample
summ lab_totalsessionspossyear11 if touse==1 & totalsessionsposs_outlieryear11==., detail
display r(p50)

***COMPLETE CASE MAIN RESULTS

foreach exp of varlist heavy days_bleed pain {
	quietly regress log_percentabsent `exp' i.ethnicity i.mated i.social_class i.financediff_bin i.housing i.matsmoke_prepreg i.mat_dep12 i.parentsep i.sexualabuse i.physicalabuse menarche bmi12 sdq_internal9 		sdq_external9 iq if touse==1
	display "baseline `exp' sessions missed = " 300*(exp(_b[_cons])-1)/100
	display "symptom `exp' sessions missed = " 300*((exp(_b[_cons])-1)*(exp(_b[`exp'])))/100
}

**Difference in time absent
*Input sessions missed for complete case and imputed in commands below
display "Heavy bleeding is associated with " 19.417218-17.455741 " more sessions absent or " (19.417218-17.455741)/2 " more days absent"
display "Bleeding for 7 days or more is associated with " 18.390013-18.105481 " less sessions absent or " (18.390013-18.105481)/2 " more days absent"
display "Heavy bleeding is associated with " 17.215869-15.502024 " more sessions absent or " (17.215869-15.502024)/2 " more days absent"

***COMPLETE CASE DOCTOR RESULTS

foreach exp of varlist heavy_doctor pain_doctor {
	quietly regress log_percentabsent i.`exp' i.ethnicity i.mated i.social_class i.financediff_bin i.housing i.matsmoke_prepreg  i.mat_dep12 i.parentsep i.sexualabuse i.physicalabuse menarche bmi12 sdq_internal9 sdq_external9 iq if touse==1
	display "baseline `exp' sessions missed = " 300*(exp(_b[_cons])-1)/100
	display "symptom no doctor 1.`exp' sessions missed = " 300*((exp(_b[_cons])-1)*(exp(_b[1.`exp'])))/100
	display "symptom and doctor 2.`exp' sessions missed = " 300*((exp(_b[_cons])-1)*(exp(_b[2.`exp'])))/100
}

**Difference in time absent
*Input sessions missed for complete case and imputed in commands below
display "Heavy bleeding without doctor is associated with " 17.603351-16.618801 " more sessions absent or " (17.603351-16.618801)/2 " more days absent"
display "Heavy bleeding with doctor is associated with " 23.635189-16.618801 " more sessions absent or " (23.635189-16.618801)/2 " more days absent"
display "Pain without doctor is associated with " 14.781659-13.690695 " more sessions absent or " (14.781659-13.690695)/2 " more days absent"
display "Pain with doctor is associated with " 19.615359-13.690695 " more sessions absent or " (19.615359-13.690695)/2 " more days absent"

log close

********************************************************************************************************************************************************************************************************************
*MULTIPLE IMPUTATION

********************************************************************************
****************************MISSING DATA PATTERNS*******************************
********************************************************************************
**Sample sizes
*Complete exposure and outcome = 2698
count if heavy!=. & days_bleed!=. & pain!=. & capped_gcse!=. & percentabsent!=.
*Plus all main confounders = 1361
count if heavy!=. & days_bleed!=. & pain!=. & capped_gcse!=. & percentabsent!=. & ethnicity!=. & mated!=. & social_class!=. & financediff_bin!=. & housing!=. & matsmoke_prepreg!=. & mat_dep12!=. & parentsep!=. & sexualabuse!=. & physicalabuse!=. & menarche!=. & bmi12!=. & sdq_internal9!=. & sdq_external9!=. 
*Plus IQ (competing exposure) = 1274
count if heavy!=. & days_bleed!=. & pain!=. & capped_gcse!=. & percentabsent!=. & ethnicity!=. & mated!=. & social_class!=. & financediff_bin!=. & housing!=. & matsmoke_prepreg!=. & mat_dep12!=. & parentsep!=. & sexualabuse!=. & physicalabuse!=. & menarche!=. & bmi12!=. & sdq_internal9!=. & sdq_external9!=. & iq!=. 
*Plus contraception = 1258 
count if heavy!=. & days_bleed!=. & pain!=. & capped_gcse!=. & percentabsent!=. & ethnicity!=. & mated!=. & social_class!=. & financediff_bin!=. & housing!=. & matsmoke_prepreg!=. & mat_dep12!=. & parentsep!=. & sexualabuse!=. & physicalabuse!=. & menarche!=. & bmi12!=. & sdq_internal9!=. & sdq_external9!=. & iq!=. & contraception!=.


**Need to get the proportion missing data and prevalence/mean of each covariate amongst the sample with complete exposure and outcome data 
*Restrict to sample with complete exposure and outcome data
mark restrict
markout restrict heavy days_bleed pain capped_gcse percentabsent 

log using "S:\B4175_howe - B4175_howe\Missing_Data_Patterns", replace 

**MISSING DATA PATTERNS IN COMPLETE CASE SAMPLE
*Percentage missing
mdesc ethnicity mated social_class financediff_bin housing matsmoke_prepreg mat_dep12 parentsep sexualabuse physicalabuse menarche bmi12 sdq_internal9 sdq_external9 iq contraception if restrict==1
*Prevalence/mean in restricted sample
foreach var of varlist ethnicity mated social_class financediff_bin housing matsmoke_prepreg mat_dep12 parentsep sexualabuse physicalabuse contraception {
	tab `var' if restrict==1
}
foreach var of varlist menarche bmi12 sdq_internal9 sdq_external9 iq {
	summ `var' if restrict==1
}

log close

********************************************************************************
**************************AUXILIARY VARIABLES PREP******************************
********************************************************************************

***Main auxiliary variables

**Maternal anxiety during pregnancy 
*b351 c573 
recode b351 c573 (-7 -1=.)
gen matanx_preg=.
replace matanx_preg=b351 if c573==.
replace matanx_preg=c573 if b351==.
replace matanx_preg=(b351+c573)/2 if b351!=. & c573!=. 
gen matanxbin_preg=1 if inrange(matanx_preg,9,16)
replace matanxbin_preg=0 if inrange(matanx_preg,0,8.5)
label values matanxbin_preg yes_no_lbl

**Maternal depression during pregnancy 
*b370 c600 
recode b370 c600 (-7 -1=.)
gen matdep_preg=.
replace matdep_preg=b370 if c600==.
replace matdep_preg=c600 if b370==.
replace matdep_preg=(b370+c600)/2 if b370!=. & c600!=.
gen matdepbin_preg=1 if inrange(matdep_preg,13,28)
replace matdepbin_preg=0 if inrange(matdep_preg,0,12.5)
label values matdepbin_preg yes_no_lbl

**BMI age 7
gen bmi7=f7ms026a
replace bmi7=. if bmi7<0

**SDQ age 11
**Internalising problems (combine the emotional and peer problems scales - kw6602b kw6604b; continuous from 0 to 10 each)
tab1 kw6602b kw6604b
recode kw6602b kw6604b (-10 -6 -5=.)
gen sdq_internal11=kw6602b + kw6604b
summ sdq_internal11, detail
*mean 2.61 (2.72), range from 0 to 20
**Externalising problems (combine the hyperactivity and conduct problems scales - kw6601b kw6603b; continuous from 0 to 10 each)
tab1 kw6601b kw6603b
recode kw6601b kw6603b (-10 -6 -5=.)
gen sdq_external11= kw6601b + kw6603b
summ sdq_external11, detail
*mean 3.47 (2.88), range from 0 to 20 

***Possible auxiliary variables

**Maternal ethnicity (unsure?)
recode c800 (-1=.), gen(mat_ethnicity)

**Financial problems b594 (major financial problem since pregnancy and affected a lot, fairly, mildly, or no effect)
recode b594 (-7 -1=.) (5=0) (1/4=1), gen (finance_prob)
label values finance_prob finance_lbl
tab finance_prob
*4938 (86.39) no problems, 778 (13.61) any problem

**Maternal smoking
*first 3 months of pregnancy b665
recode b665 (1=0) (2/5=1), gen (matsmoke_3mnth)
label values matsmoke_3mnth smoking 
*last 2 weeks b667
recode b667 (1=0) (2/5=1), gen (matsmoke_2wk)
label values matsmoke_2wk smoking 

**Maternal anxiety postnatally (continuous or binary?)
*h178b (prorated)
recode h178b (-6 -5=.), gen (matanx_postnatal)
gen matanxbin_postnatal=1 if inrange(h178b,9,16)
replace matanxbin_postnatal=0 if inrange(h178b,0,8.5)

**Maternal depression postnatally (continuous or binary?)
*h200b (prorated)
recode h200b (-6 -5=.), gen (matdep_postnatal)
gen matdepbin_postnatal=1 if inrange(h200b,13,30)
replace matdepbin_postnatal=0 if inrange(h200b,0,12.5)
label values matdepbin_postnatal yes_no_lbl

**BMI age 13
gen bmi13=fg3139
replace bmi13=. if bmi13<0

**SDQ age 13
**Internalising problems (emotion and peer - ta7025a ta7025d)
recode ta7025a ta7025d (-10 -1=.)
gen sdq_internal13= ta7025a + ta7025d
**Externalising problems (hyperactivity and conduct - ta7025c ta7025b)
recode ta7025c ta7025b (-10 -1=.)
gen sdq_external13=ta7025c + ta7025b


********************************************************************************
*****************************MULTIPLE IMPUTATION********************************
********************************************************************************

drop if restrict==0
drop if ethnicity>.
*2698 obs

**Missing data patterns
*Below are all  complete
mdesc heavy days_bleed pain capped_gcse log_percentabsent heavy_doctor pain_doctor heavy_prolonged_cat heavy_pain_cat persistent_abs gcse_fiveac
*Model covariates - are all missing
mdesc ethnicity mated social_class financediff_bin housing matsmoke_prepreg  mat_dep12 parentsep sexualabuse physicalabuse menarche bmi12 sdq_internal9 sdq_external9 iq  
*Auxiliary variables - should be missing
mdesc matanx_preg matdep_preg bmi7 sdq_internal11 sdq_external11

**Set up imputation
mi set flong
mi register imputed ethnicity mated social_class financediff_bin housing matsmoke_prepreg mat_dep12 parentsep sexualabuse physicalabuse menarche bmi12 sdq_internal9 sdq_external9 iq matdep_preg bmi7 sdq_internal11 sdq_external11
mi register regular heavy days_bleed pain capped_gcse log_percentabsent heavy_doctor pain_doctor heavy_prolonged_cat heavy_pain_cat persistent_abs gcse_fiveac

*Trace plot version
mi impute chained (ologit, omit(matdep_preg bmi7 sdq_internal11 sdq_external11)) mated ///
	(logit, omit(matdep_preg bmi7 sdq_internal11 sdq_external11 i.heavy_doctor i.pain_doctor i.heavy_prolonged_cat i.heavy_pain_cat i.persistent_abs i.gcse_fiveac)) ethnicity social_class financediff_bin housing matsmoke_prepreg parentsep sexualabuse physicalabuse  ///
	(logit, omit(bmi7 sdq_internal11 sdq_external11)) mat_dep12 ///
	(pmm, knn(5)) matdep_preg ///
	(regress, omit(matdep_preg bmi7 sdq_internal11 sdq_external11)) menarche ///
	(truncreg, ll(40) ul(155) omit(matdep_preg bmi7 sdq_internal11 sdq_external11)) iq ///
	(truncreg, ll(8) ul(42) omit(matdep_preg sdq_internal11 sdq_external11)) bmi12 ///
	(regress) bmi7 ///
	(pmm, knn(5) omit(matdep_preg bmi7)) sdq_internal9 sdq_external9 ///
	(pmm, knn(5) omit(matdep_preg bmi7)) sdq_internal11 sdq_external11 ///
	= i.heavy i.days_bleed i.pain i.heavy_doctor i.pain_doctor i.heavy_prolonged_cat i.heavy_pain_cat capped_gcse log_percentabsent i.persistent_abs i.gcse_fiveac, add(60) burnin(100) rseed(928364) dots chainonly noisily showcommand savetrace("S:\B4175_howe - B4175_howe\Impstats_nomatanx.dta", replace) 
	
/*Read in trace information
use "S:\B4175_howe - B4175_howe\Impstats_nomatanx.dta", clear
describe 
tsset iter

foreach cov in ethnicity mated social_class financediff_bin housing matsmoke_prepreg mat_anx12 mat_dep12 parentsep sexualabuse physicalabuse menarche bmi12 sdq_internal9 sdq_external9 iq contraception {
	tsline `cov'_mean, title(Mean imputed values of `cov') legend(off) 
	graph export `cov'_mean_v2.png, replace
	tsline `cov'_sd, title(Standard deviation imputed values of `cov') legend(off)
	graph export `cov'_sd_v2.png, replace
}
*/

**Run imputation
mi impute chained (ologit, omit(matdep_preg bmi7 sdq_internal11 sdq_external11)) mated ///
	(logit, omit(matdep_preg bmi7 sdq_internal11 sdq_external11 i.heavy_doctor i.pain_doctor i.heavy_prolonged_cat i.heavy_pain_cat i.persistent_abs i.gcse_fiveac)) ethnicity social_class financediff_bin housing matsmoke_prepreg parentsep sexualabuse physicalabuse  ///
	(logit, omit(bmi7 sdq_internal11 sdq_external11)) mat_dep12 ///
	(pmm, knn(5)) matdep_preg ///
	(regress, omit(matdep_preg bmi7 sdq_internal11 sdq_external11)) menarche ///
	(truncreg, ll(37) ul(159) omit(matdep_preg bmi7 sdq_internal11 sdq_external11)) iq ///
	(truncreg, ll(7) ul(42) omit(matdep_preg sdq_internal11 sdq_external11)) bmi12 ///
	(regress) bmi7 ///
	(pmm, knn(5) omit(matdep_preg bmi7)) sdq_internal9 sdq_external9 ///
	(pmm, knn(5) omit(matdep_preg bmi7)) sdq_internal11 sdq_external11 ///
	= i.heavy i.days_bleed i.pain i.heavy_doctor i.pain_doctor i.heavy_prolonged_cat i.heavy_pain_cat capped_gcse log_percentabsent i.persistent_abs i.gcse_fiveac, add(60) burnin(50) rseed(928364) dots
	
*Save 
save "S:\B4175_howe - B4175_howe\Imputed_dataset.dta", replace
*Open
use "S:\B4175_howe - B4175_howe\Imputed_dataset.dta", replace

**Check proportions and means 
label define imputed 0"Observed" 1"Imputed"
gen imputed=0 if _mi_m==0
replace imputed=1 if _mi_m>0
label values imputed imputed 

log using "S:\B4175_howe - B4175_howe\CheckingMI_proportions.smcl", replace 

foreach var of varlist ethnicity mated social_class financediff_bin housing matsmoke_prepreg mat_dep12 parentsep sexualabuse physicalabuse {
	tab `var' imputed, col
}

foreach var of varlist menarche bmi12 sdq_internal9 sdq_external9 iq {
	summ `var' if imputed==0
	summ `var' if imputed==1
}

**Mcerror checks (fully adjusted models)
*1-mcerror of coefficient should be 10% or less of effect SE
*2-mcerror of tstat should be 0.1 or less
*3-mcerror of p should be 0.01 or less if p is 0.05 or 0.02 if p is 0.1

foreach exp of varlist heavy days_bleed pain {
	foreach out of varlist capped_gcse log_percentabsent {
		mi estimate, mcerror: regress `out' i.`exp' i.ethnicity i.mated i.social_class i.financediff_bin i.housing i.matsmoke_prepreg i.mat_dep12 i.parentsep i.sexualabuse i.physicalabuse menarche bmi12 sdq_internal9 sdq_external9 iq
	}
}

log close

*Distributions 
foreach var in menarche bmi12 sdq_internal9 sdq_external9 iq {
	twoway (histogram `var' if imputed==0, color(gray%30)) ///
		(histogram `var' if imputed==1, fcolor(none) lcolor(black)), ///
		legend(order(1 "Observed" 2 "Imputed")) 
		graph export `var'_MIcheck.png, replace 
}


********************************************************************************
**********************************ANALYSIS**************************************
********************************************************************************

use "S:\B4175_howe - B4175_howe\Imputed_dataset.dta", clear

label define imputed 0"Observed" 1"Imputed"
gen imputed=0 if _mi_m==0
replace imputed=1 if _mi_m>0
label values imputed imputed 

***Proportion / means of covariates in imputed sample

log using "S:\B4175_howe - B4175_howe\Missing_Data_Patterns", append 

**IN IMPUTED SAMPLE
foreach var of varlist ethnicity mated social_class financediff_bin housing matsmoke_prepreg mat_dep12 parentsep sexualabuse physicalabuse {
	mi estimate: proportion `var'
}

foreach var of varlist menarche bmi12 sdq_internal9 sdq_external9 iq {
	mi estimate: mean `var' 
}

log close

***Descriptives

log using "S:\B4175_howe - B4175_howe\Imputeddata_descriptives.smcl", replace 

*Main Exposures
foreach exp of varlist heavy days_bleed pain {
	tab `exp' if imputed==0
}

mi passive: gen absence = exp(log_percentabsent)-1

foreach out of varlist capped_gcse absence {
	summ `out' if imputed==0, det
}

*Binary/categorical variables - proportion according to each menstrual symptom
foreach exp of varlist heavy days_bleed pain {
	foreach cov of varlist ethnicity mated social_class financediff_bin housing matsmoke_prepreg mat_dep12 parentsep sexualabuse physicalabuse  {
		mi estimate: proportion `cov', over(`exp')	
	}
}

*Continuous variables - mean according to each menstrual symptom
foreach exp of varlist heavy days_bleed pain {
	foreach cov of varlist menarche bmi12 sdq_internal9 sdq_external9 iq {
		mi estimate: mean `cov', over(`exp')
	}
}

log close 

***Main models
**Results into excel file 
*Main analysis structure: 1) crude, 2) SEP adjusted 3) adversity adjusted 4) menarche adjusted 5) bmi adjusted 6) mental health adjusted 7) iq adjusted 
capture erase "imputed_main_results.xls"
eststo clear
foreach exp of varlist heavy days_bleed pain {
	eststo `exp': mi estimate, post: regress capped_gcse `exp'
	eststo `exp'_sep: mi estimate, post: regress capped_gcse `exp' i.ethnicity i.mated i.social_class i.financediff_bin i.housing i.matsmoke_prepreg 
	eststo `exp'_adv: mi estimate, post: regress capped_gcse `exp' i.ethnicity i.mated i.social_class i.financediff_bin i.housing i.matsmoke_prepreg i.mat_dep12 i.parentsep i.sexualabuse i.physicalabuse 
	eststo `exp'_men: mi estimate, post: regress capped_gcse `exp' i.ethnicity i.mated i.social_class i.financediff_bin i.housing i.matsmoke_prepreg i.mat_dep12 i.parentsep i.sexualabuse i.physicalabuse menarche 
	eststo `exp'_bmi: mi estimate, post: regress capped_gcse `exp' i.ethnicity i.mated i.social_class i.financediff_bin i.housing i.matsmoke_prepreg i.mat_dep12 i.parentsep i.sexualabuse i.physicalabuse menarche bmi12 
	eststo `exp'_mh: mi estimate, post: regress capped_gcse `exp' i.ethnicity i.mated i.social_class i.financediff_bin i.housing i.matsmoke_prepreg i.mat_dep12 i.parentsep i.sexualabuse i.physicalabuse menarche bmi12 sdq_internal9 sdq_external9 
	eststo `exp'_iq: mi estimate, post: regress capped_gcse `exp' i.ethnicity i.mated i.social_class i.financediff_bin i.housing i.matsmoke_prepreg i.mat_dep12 i.parentsep i.sexualabuse i.physicalabuse menarche bmi12 sdq_internal9 sdq_external9 iq 
	estout `exp' `exp'_sep `exp'_adv `exp'_men `exp'_bmi `exp'_mh `exp'_iq using "imputed_main_results.xls", append cells("b(fmt(4)) ci_l(fmt(4)) ci_u(fmt(4)) p(fmt(4)) se(fmt(4))") keep(*`exp'*) ///
	title (GCSE SCORE) note(crude; adjusted for early SEP; adjusted for adversity; adjusted for menarche; adjusted for bmi; adjusted for mental health; adjusted for iq)
}
eststo clear
foreach exp of varlist heavy days_bleed pain {
	eststo `exp': mi estimate, post: regress log_percentabsent `exp' 
	eststo `exp'_sep: mi estimate, post: regress log_percentabsent `exp' i.ethnicity i.mated i.social_class i.financediff_bin i.housing i.matsmoke_prepreg
	eststo `exp'_adv: mi estimate, post: regress log_percentabsent `exp' i.ethnicity i.mated i.social_class i.financediff_bin i.housing i.matsmoke_prepreg i.mat_dep12 i.parentsep i.sexualabuse i.physicalabuse 
	eststo `exp'_men: mi estimate, post: regress log_percentabsent `exp' i.ethnicity i.mated i.social_class i.financediff_bin i.housing i.matsmoke_prepreg i.mat_dep12 i.parentsep i.sexualabuse i.physicalabuse menarche 
	eststo `exp'_bmi: mi estimate, post: regress log_percentabsent `exp' i.ethnicity i.mated i.social_class i.financediff_bin i.housing i.matsmoke_prepreg i.mat_dep12 i.parentsep i.sexualabuse i.physicalabuse menarche bmi12 
	eststo `exp'_mh: mi estimate, post: regress log_percentabsent `exp' i.ethnicity i.mated i.social_class i.financediff_bin i.housing i.matsmoke_prepreg i.mat_dep12 i.parentsep i.sexualabuse i.physicalabuse menarche bmi12 sdq_internal9 sdq_external9 
	eststo `exp'_iq: mi estimate, post: regress log_percentabsent `exp' i.ethnicity i.mated i.social_class i.financediff_bin i.housing i.matsmoke_prepreg i.mat_dep12 i.parentsep i.sexualabuse i.physicalabuse menarche bmi12 sdq_internal9 sdq_external9 iq 
	estout `exp' `exp'_sep `exp'_adv `exp'_men `exp'_bmi `exp'_mh `exp'_iq using "imputed_main_results.xls", append cells("b(fmt(4)) ci_l(fmt(4)) ci_u(fmt(4)) p(fmt(4)) se(fmt(4))") keep(*`exp'*) eform ///
	title (ABSENCES) note(crude; adjusted for early SEP; adjusted for adversity; adjusted for menarche; adjusted for bmi; adjusted for mental health; adjusted for iq)
}

*Doctor sensitivity analysis structure: 1) doctor vars crude 2) doctor vars adjusted 3) doctor vars iq adjusted 
capture erase "imputed_sensitivity_results.xls"
eststo clear
foreach exp of varlist heavy_doctor pain_doctor {
	eststo `exp': mi estimate, post: regress capped_gcse i.`exp' 
	eststo `exp'_mh: mi estimate, post: regress capped_gcse i.`exp' i.ethnicity i.mated i.social_class i.financediff_bin i.housing i.matsmoke_prepreg i.mat_dep12 i.parentsep i.sexualabuse i.physicalabuse menarche bmi12 sdq_internal9 sdq_external9 
	eststo `exp'_iq: mi estimate, post: regress capped_gcse i.`exp' i.ethnicity i.mated i.social_class i.financediff_bin i.housing i.matsmoke_prepreg i.mat_dep12 i.parentsep i.sexualabuse i.physicalabuse menarche bmi12 sdq_internal9 sdq_external9 iq 
	estout `exp' `exp'_mh `exp'_iq using "imputed_sensitivity_results.xls", append cells("b(fmt(4)) ci_l(fmt(4)) ci_u(fmt(4)) p(fmt(4)) se(fmt(4))") keep(*1.`exp'* *2.`exp'*) ///
	title (GCSE SCORE - `exp') note(1=no doctor; 2=doctor: crude; adjusted for early SEP, adversity, menarche, bmi, and mental health; adjusted for iq)
}
eststo clear
foreach exp of varlist heavy_doctor pain_doctor {
	eststo `exp': mi estimate, post: regress log_percentabsent i.`exp' 
	eststo `exp'_mh: mi estimate, post: regress log_percentabsent i.`exp' i.ethnicity i.mated i.social_class i.financediff_bin i.housing i.matsmoke_prepreg i.mat_dep12 i.parentsep i.sexualabuse i.physicalabuse menarche bmi12 sdq_internal9 sdq_external9 
	eststo `exp'_iq: mi estimate, post: regress log_percentabsent i.`exp' i.ethnicity i.mated i.social_class i.financediff_bin i.housing i.matsmoke_prepreg i.mat_dep12 i.parentsep i.sexualabuse i.physicalabuse menarche bmi12 sdq_internal9 sdq_external9 iq 
	estout `exp' `exp'_mh `exp'_iq using "imputed_sensitivity_results.xls", append cells("b(fmt(4)) ci_l(fmt(4)) ci_u(fmt(4)) p(fmt(4)) se(fmt(4))") keep(*1.`exp'* *2.`exp'*) eform ///
	title (ABSENCES- `exp') note(1=no doctor; 2=doctor: crude; adjusted for early SEP, adversity, menarche, bmi, and mental health; adjusted for iq)
}

*4-level heavy variable sensitivity analysis structure: 1) heavy cat crude 2) heavy cat adjusted 3) heavy cat iq adjusted
eststo clear
eststo heavycat: mi estimate, post: regress capped_gcse i.heavy_prolonged_cat 
eststo heavycat_mh: mi estimate, post: regress capped_gcse i.heavy_prolonged_cat i.ethnicity i.mated i.social_class i.financediff_bin i.housing i.matsmoke_prepreg i.mat_dep12 i.parentsep i.sexualabuse i.physicalabuse menarche bmi12 sdq_internal9 sdq_external9 
eststo heavycat_iq: mi estimate, post: regress capped_gcse i.heavy_prolonged_cat i.ethnicity i.mated i.social_class i.financediff_bin i.housing i.matsmoke_prepreg i.mat_dep12 i.parentsep i.sexualabuse i.physicalabuse menarche bmi12 sdq_internal9 sdq_external9 iq 
estout heavycat heavycat_mh heavycat_iq using "imputed_sensitivity_results.xls", append cells("b(fmt(4)) ci_l(fmt(4)) ci_u(fmt(4)) p(fmt(4)) se(fmt(4))") ///
keep(*1.heavy_prolonged_cat* *2.heavy_prolonged_cat* *3.heavy_prolonged_cat*) ///
title (GCSE SCORE - Categorical heavy) note(1=prolonged only; 2=heavy only; 3=both: crude; adjusted for early SEP, adversity, menarche, bmi, and mental health; adjusted for iq)
eststo clear
eststo heavycat: mi estimate, post: regress log_percentabsent i.heavy_prolonged_cat 
eststo heavycat_mh: mi estimate, post: regress log_percentabsent i.heavy_prolonged_cat i.ethnicity i.mated i.social_class i.financediff_bin i.housing i.matsmoke_prepreg i.mat_dep12 i.parentsep i.sexualabuse i.physicalabuse menarche bmi12 sdq_internal9 sdq_external9 
eststo heavycat_iq: mi estimate, post: regress log_percentabsent i.heavy_prolonged_cat i.ethnicity i.mated i.social_class i.financediff_bin i.housing i.matsmoke_prepreg i.mat_dep12 i.parentsep i.sexualabuse i.physicalabuse menarche bmi12 sdq_internal9 sdq_external9 iq 
estout heavycat heavycat_mh heavycat_iq using "imputed_sensitivity_results.xls", append cells("b(fmt(4)) ci_l(fmt(4)) ci_u(fmt(4)) p(fmt(4)) se(fmt(4))") ///
keep(*1.heavy_prolonged_cat* *2.heavy_prolonged_cat* *3.heavy_prolonged_cat*) eform ///
title (ABSENCES - Categorical heavy) note(1=prolonged only; 2=heavy only; 3=both: crude; adjusted for early SEP, adversity, menarche, bmi, and mental health; adjusted for iq)

*Sensitivity combined heavy and pain analysis structure: 1) heavy pain cat crude 2) heavy pain cat adjusted 3) heavy pain cat iq adjusted 
eststo clear
eststo heavy_pain: mi estimate, post: regress capped_gcse i.heavy_pain_cat 
eststo heavy_pain_mh: mi estimate, post: regress capped_gcse i.heavy_pain_cat i.ethnicity i.mated i.social_class i.financediff_bin i.housing i.matsmoke_prepreg i.mat_dep12 i.parentsep i.sexualabuse i.physicalabuse menarche bmi12 sdq_internal9 sdq_external9 
eststo heavy_pain_iq: mi estimate, post: regress capped_gcse i.heavy_pain_cat i.ethnicity i.mated i.social_class i.financediff_bin i.housing i.matsmoke_prepreg i.mat_dep12 i.parentsep i.sexualabuse i.physicalabuse menarche bmi12 sdq_internal9 sdq_external9 iq 
estout heavy_pain heavy_pain_mh heavy_pain_iq using "imputed_sensitivity_results.xls", append cells("b(fmt(4)) ci_l(fmt(4)) ci_u(fmt(4)) p(fmt(4)) se(fmt(4))") ///
keep(*1.heavy_pain_cat* *2.heavy_pain_cat* *3.heavy_pain_cat*) ///
title (GCSE SCORE - Heavy and pain combined) note(1=pain only; 2=heavy only; 3=both: crude; adjusted for early SEP, adversity, menarche, bmi, and mental health; adjusted for iq)
eststo clear
eststo heavy_pain: mi estimate, post: regress log_percentabsent i.heavy_pain_cat 
eststo heavy_pain_mh: mi estimate, post: regress log_percentabsent i.heavy_pain_cat i.ethnicity i.mated i.social_class i.financediff_bin i.housing i.matsmoke_prepreg i.mat_dep12 i.parentsep i.sexualabuse i.physicalabuse menarche bmi12 sdq_internal9 sdq_external9 
eststo heavy_pain_iq: mi estimate, post: regress log_percentabsent i.heavy_pain_cat i.ethnicity i.mated i.social_class i.financediff_bin i.housing i.matsmoke_prepreg i.mat_dep12 i.parentsep i.sexualabuse i.physicalabuse menarche bmi12 sdq_internal9 sdq_external9 iq 
estout heavy_pain heavy_pain_mh heavy_pain_iq using "imputed_sensitivity_results.xls", append cells("b(fmt(4)) ci_l(fmt(4)) ci_u(fmt(4)) p(fmt(4)) se(fmt(4))") ///
keep(*1.heavy_pain_cat* *2.heavy_pain_cat* *3.heavy_pain_cat*) eform ///
title (ABSENCES - Heavy and pain combined) note(1=pain only; 2=heavy only; 3=both: crude; adjusted for early SEP, adversity, menarche, bmi, and mental health; adjusted for iq)

**Persistent absence binary outcome analysis structure: 1) crude 2) adjusted 3) iq adjusted  
eststo clear
foreach exp of varlist heavy days_bleed pain {
	eststo `exp': quietly logistic persistent_abs `exp' 
	eststo `exp'_mh: quietly logistic persistent_abs `exp' i.ethnicity i.mated i.social_class i.financediff_bin i.housing i.matsmoke_prepreg i.mat_dep12 i.parentsep i.sexualabuse i.physicalabuse menarche bmi12 sdq_internal9 sdq_external9 
	eststo `exp'_iq: quietly logistic persistent_abs `exp' i.ethnicity i.mated i.social_class i.financediff_bin i.housing i.matsmoke_prepreg i.mat_dep12 i.parentsep i.sexualabuse i.physicalabuse menarche bmi12 sdq_internal9 sdq_external9 iq 
	estout `exp' `exp'_mh `exp'_iq using "imputed_sensitivity_results.xls", eform append cells("b(fmt(4)) ci_l(fmt(4)) ci_u(fmt(4)) p(fmt(4)) se(fmt(4))") keep(*`exp'*) ///
	title (Binary Persistent Absence - `exp') note(ODDS RATIOS: crude; adjusted for early SEP, adversity, menarche, bmi, and mental health; adjusted for iq)
}

**Five A*-C including maths and english analysis structure: 1) crude 2) adjusted 3) iq adjusted
eststo clear
foreach exp of varlist heavy days_bleed pain {
	eststo `exp': quietly logistic gcse_fiveac `exp' 
	eststo `exp'_mh: quietly logistic gcse_fiveac `exp' i.ethnicity i.mated i.social_class i.financediff_bin i.housing i.matsmoke_prepreg i.mat_dep12 i.parentsep i.sexualabuse i.physicalabuse menarche bmi12 sdq_internal9 sdq_external9 
	eststo `exp'_iq: quietly logistic gcse_fiveac `exp' i.ethnicity i.mated i.social_class i.financediff_bin i.housing i.matsmoke_prepreg i.mat_dep12 i.parentsep i.sexualabuse i.physicalabuse menarche bmi12 sdq_internal9 sdq_external9 iq 
	estout `exp' `exp'_mh `exp'_iq using "imputed_sensitivity_results.xls", eform append cells("b(fmt(4)) ci_l(fmt(4)) ci_u(fmt(4)) p(fmt(4)) se(fmt(4))") keep(*`exp'*) ///
	title (Binary Five A*-C including Maths and English - `exp') note(ODDS RATIOS: crude; adjusted for early SEP, adversity, menarche, bmi, and mental health; adjusted for iq)
}

********************************************************************************
**********************CALCULATE NUMBER OF SESSIONS MISSED***********************
********************************************************************************

log using "S:\B4175_howe - B4175_howe\N_sessions_missed", append

***IMPUTATION RESULTS

foreach exp of varlist heavy days_bleed pain {
	quietly mi estimate, post: regress log_percentabsent `exp' i.ethnicity i.mated i.social_class i.financediff_bin i.housing i.matsmoke_prepreg i.mat_dep12 i.parentsep i.sexualabuse i.physicalabuse menarche bmi12 sdq_internal9 sdq_external9 iq 
	display "baseline `exp' sessions missed = " 300*(exp(_b[_cons])-1)/100
	display "symptom `exp' sessions missed = " 300*((exp(_b[_cons])-1)*(exp(_b[`exp'])))/100
}

**Difference in time absent
*Input sessions missed for complete case and imputed in commands below
display "Heavy bleeding is associated with " 24.067653-20.644246 " more sessions absent or " (24.067653-20.644246)/2 " more days absent"
display "Bleeding for 7 days or more is associated with " 22.322968-22.248752 " more sessions absent or " (22.322968-22.248752)/2 " more days absent"
display "Heavy bleeding is associated with " 21.539126-19.089648 " more sessions absent or " (21.539126-19.089648)/2 " more days absent"

***IMPUTATION DOCTOR RESULTS

foreach exp of varlist heavy_doctor pain_doctor {
	quietly mi estimate, post: regress log_percentabsent i.`exp' i.ethnicity i.mated i.social_class i.financediff_bin i.housing i.matsmoke_prepreg  i.mat_dep12 i.parentsep i.sexualabuse i.physicalabuse menarche bmi12 sdq_internal9 sdq_external9 iq 
	display "baseline `exp' sessions missed = " 300*(exp(_b[_cons])-1)/100
	display "symptom no doctor 1.`exp' sessions missed = " 300*((exp(_b[_cons])-1)*(exp(_b[1.`exp'])))/100
	display "symptom and doctor 2.`exp' sessions missed = " 300*((exp(_b[_cons])-1)*(exp(_b[2.`exp'])))/100
}

**Difference in time absent
*Input sessions missed for complete case and imputed in commands below
display "Heavy bleeding without doctor is associated with " 21.11372-19.033518 " more sessions absent or " (21.11372-19.033518)/2 " more days absent"
display "Heavy bleeding with doctor is associated with " 27.143252-19.033518 " more sessions absent or " (27.143252-19.033518)/2 " more days absent"
display "Pain without doctor is associated with " 18.920702-17.396428 " more sessions absent or " (18.920702-17.396428)/2 " more days absent"
display "Pain with doctor is associated with " 24.755012-17.396428 " more sessions absent or " (24.755012-17.396428)/2 " more days absent"

log close


