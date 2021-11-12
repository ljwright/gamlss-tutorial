***1970 cohort
*C drive
*global data "C:\Users\DAB\OneDrive - University College London\stats_cls\cohorts\"
global output "C:\Users\DAB\OneDrive - University College London\ideas\quantile 2\analysis\output"

*S drive
global data "S:\IOEQSS_Main\Bann\crosscinequality"

use "$data\70c\46y\ukds/bcs_age46_main.dta", clear

cap drop _merge
merge 1:1 BCSID using "$data\70c\derived\bmi_wt_ht0_42.dta" //data on bmi ht wt and closer ses -fathers social class and mat 
cap drop _merge

merge 1:1 BCSID using "$data\70c\birth and 22m subsample\bcs1derived.dta"
drop _merge
merge 1:1 BCSID using "$data\70c\birth and 22m subsample\bcs7072a.dta", keepusing(A0014 A0255)

*10y
cap drop _merge
rename BCSID bcsid   
merge 1:1 bcsid using "$data\70c\10y\sn3723.dta", keepusing (c3_4 e1_1  e1_2 e2_1 e2_2)

*maternal education
cap drop _merge
merge 1:1 bcsid using "$data/derived/closer_education/bcs70_ParentEducationDerived.dta"
rename  bcsid BCSID 

**derived bp medication
cap drop _merge
merge 1:1 BCSID using "$data\70c\bp_med.dta"

cap drop _merge
merge 1:1 BCSID using "$data\70c\29y\bcs6derived.dta"
cap drop _merge

rename BCSID bcsid
merge 1:1 bcsid using "$data\70c\26y\bcs96x.dta"
cap drop _merge

rename  bcsid BCSID
merge 1:1 BCSID using "$data\70c\34y\bcs7derived.dta"
cap drop _merge

rename  BCSID bcsid 
merge 1:1 bcsid using "$data\70c\34y\bcs_2004_followup.dta", keepusing(b7sc)
drop _merge

rename  bcsid BCSID 
merge 1:1 BCSID using "$data\70c\42y\bcs70_2012_flatfile.dta", keepusing(B9CSC B9SMOKIG   B9EXERSE   ) 
drop _merge

*response dataset
merge 1:1 BCSID using "$data\70c\response dataset\bcs_response.dta" 
drop _merge

*10y cog
cap drop _merge
merge m:m BCSID using "$data\70c\10y\bcs3derived.dta"

*wemwebs
cap drop _merge
merge m:m BCSID using "$data\70c\42y\bcs70_2012_derived.dta"

*** remove non-singletons [ie, restrict target population - though likely not needed for this paper]
tab MULTIPNO 
drop if MULTIPNO >0 

*drop NI - not followed-up subsequently in this cohort
drop if BD1CNTRY==4
tab BD1CNTRY

*drop non-core birth sample - 17,196 + 18
drop if OUTCME01==6
tab OUTCME01
*sex [variable sex had lots of missing data - n only 9k not 17k]
cap drop sex
tab SEX 
rename SEX gender
recode gender (1=1 "male") (2=0 "female") (3=.), gen(sex)
tab sex, mi

*replace with other data as 7 have valid BP data yet missing sex
tab B10CMSEX, nolab
recode B10CMSEX (1=1 "male") (2=0 "female"), gen(sex_46y )
replace sex = sex_46y if missing(sex) & !missing(sex_46y) //7 changes

* --------- Childhood SEP
* Father's social class
cap drop fsc*
tab A0014
recode A0014 (1=0 "I professional etc") (2=1 "II intermediate") (3=2 "III NM  skilled(Non-Manual)") ///
(4=3 " IIIM skilled(Manual)") (5=4 "IV partly skilled") (6=5 "V unskilled") (-2 7/max=. ) , gen(fsc)

tab A0014 fsc , mi

*11y
tab c3_4
tab c3_4, nolab

recode c3_4 (1=0 "I professional etc") (2=1 "II intermediate") (3=2 "III NM  skilled(Non-Manual)") ///
(4=3 " IIIM skilled(Manual)") (5=4 "IV partly skilled") (6=5 "V unskilled") (-2 8/max=. ) , gen(fsc11)
tab fsc11 c3_4 , mi

*replace with 11y is missing birth
sum fsc fsc11 
corr fsc fsc11 
sum fsc fsc11 if missing(fsc)

replace  fsc = fsc11 if missing(fsc) & !missing(fsc11) 
tab fsc fsc11, mi

recode fsc (0/2=0 "Non-manual (highest)") (3/5=1 " Manual") , gen(fscb)
tab fsc fscb

*exer pa 42y 
desc B9EXERSE   B9SMOKIG   
sum B9EXERSE   B9SMOKIG   
mvdecode B9EXERSE   B9SMOKIG ,  mv(-9/-1=. )


*replace 7 to 6 as only 2.9% of sample (with bmi etc data) have 6 days only
cap drop patemp
gen patemp = B9EXERSE
replace patemp = 6 if patemp==7
tab patemp

*binary vars 
recode patemp (0=1 "zero pa") (1/7= 0 "1-7 days/week"), gen(pa42b)
tab pa42b B9EXERSE

*all cat version, in sam order as binary version
vreverse patemp, gen(pa42)
tab pa42, nolab
tab pa42 B9EXERSE

***outcomes
*bmi
recode B10MWEIGHT (-9/-1 =.), gen(wt46)
recode B10HEIGHTCM (min/-1 =.), gen(ht46)

gen bmi46 = (wt46/ht46^2)*10000

*drop single individual with bmi=77, 5 poitns above all overs
sum bmi46 
replace bmi46 =. if bmi46 >70

*wemwebs 
desc BD9WEMWB
recode BD9WEMWB (-9/-1 =.), gen(wem46)
sum BD9WEMWB wem46 if BD9WEMWB >0

*label key vars
label var bmi46 "BMI (kg/m{superscript:2}), 46y"
label var wem46 "Mental wellbeing (WEMWEBS), 42y"
label var fscb "Father's social class 0y, 0=non-manual, 1=manual"
label var sex "sex, 0=female, 1=male"
label var pa42b "Exercise 42y, 0= 1-7days/week, 1=none"

*save for R GAMLSS analyses 
keep bmi46 wem46 sex    fscb fsc   pa42b pa42 
save "$output/cleaned_datafor_r.dta", replace