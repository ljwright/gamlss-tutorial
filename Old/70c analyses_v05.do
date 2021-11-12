**To do - restrict N to gamlss N in histograms

global data "C:\Users\DAB\OneDrive - University College London\stats_cls\cohorts\"
global output "C:\Users\DAB\OneDrive - University College London\ideas\quantile 2\analysis\output"

use "$output/cleaned_datafor_r.dta", clear

*Ns
sum bmi46
sum bmi46 if !missing(sex, fsc, pa42b)

sum wem46
sum wem46 if !missing(sex, fsc, pa42b)

*Ns in each subgroup
*table 1
*bmi
tab sex if !missing(sex, fsc, pa42b, bmi46) 
tab fscb if !missing(sex, fsc, pa42b, bmi46)
tab pa42b if !missing(sex, fsc, pa42b, bmi46)

*table 2
tab sex if !missing(sex, fsc, pa42b, wem46)
tab fscb if !missing(sex, fsc, pa42b, wem46)
tab pa42b if !missing(sex, fsc, pa42b, wem46)


corr bmi46 wem46

*table 1 bmi
cap erase "$output/t2_bmi_desc.txt"

estpost tabstat bmi46 if !missing(bmi46, fscb, pa42b), by(sex) stats(mean sd median cv )   
esttab . using "$output/t2_bmi_desc.csv", cells("mean(fmt(%9.3g)) sd p50 cv ") replace noobs title("BMI")

estpost tabstat bmi46 if !missing(bmi46, fscb, pa42b), by(fscb) stats(mean sd median cv )   
esttab . using "$output/t2_bmi_desc.csv", cells("mean(fmt(%9.3g)) sd p50 cv ") append noobs

estpost tabstat bmi46 if !missing(bmi46, fscb, pa42b), by(pa42b) stats(mean sd median cv )  
esttab . using "$output/t2_bmi_desc.csv", cells("mean(fmt(%9.3g)) sd p50 cv ") append noobs

*table 2 wem46
cap erase "$output/t2_bmi_desc.txt"

estpost tabstat wem46 if !missing(wem46, fscb, pa42b), by(sex) stats(mean sd median cv )   
esttab . using "$output/t2_bmi_desc.csv", cells("mean(fmt(%9.3g)) sd p50 cv ") append noobs title("wem")

estpost tabstat wem46 if !missing(wem46, fscb, pa42b), by(fscb) stats(mean sd median cv )   
esttab . using "$output/t2_bmi_desc.csv", cells("mean(fmt(%9.3g)) sd p50 cv ") append noobs

estpost tabstat wem46 if !missing(wem46, fscb, pa42b), by(pa42b) stats(mean sd median cv )  
esttab . using "$output/t2_bmi_desc.csv", cells("mean(fmt(%9.3g)) sd p50 cv ") append noobs

*boc cox power
*export  boxcox estimates
cap erase "$output/boxcox.txt"

foreach outcome of varlist bmi46 wem46 {
foreach exposure of varlist sex fscb pa42b {
 boxcox `outcome' if `exposure'==0 & !missing(sex, fscb, pa42b)
estadd local `outcome'_`exposure'_0 = r(est)

esttab . using "$output/boxcox.txt", stats(`outcome'_`exposure'_0)  append noobs title("") nocons nostar  ///
	plain nolabel nogaps varwidth (15) nolines  compress    nolabel  nocons 

 boxcox `outcome' if `exposure'==1 & !missing(sex, fscb, pa42b)
estadd local `outcome'_`exposure'_1 = r(est)

esttab . using "$output/boxcox.txt", stats(`outcome'_`exposure'_1)  append noobs title("") nocons nostar  ///
	plain nolabel nogaps varwidth (15) nolines  compress    nolabel  nocons 
	
}
}



*try stata colour
color("248 118 109" 
twoway (kdensity bmi46  if sex ==0 & !missing(sex, fscb, pa42b), color("248 118 109") lwidth(thick)) ///
       (kdensity bmi46  if sex==1  & !missing(sex, fscb, pa42b), color("0 191 196")   lwidth(thick) )
	   
	   
hist bmi46, (color p11)

scatter mpg weight , mcolor("255 0 0")

*figure 1

*here
**kdensity bmi by exposure risk factor
foreach outcome of varlist bmi46  wem46 {

*sex
sum `outcome' if sex==0 & !missing(sex, fscb, pa42b)
local m0=round(r(mean),0.01)
local sd0=string(r(sd), "%3.2f") 
local cov0=round(r(sd) /  r(mean) ,0.001)

sum `outcome' if sex==1 & !missing(sex, fscb, pa42b)
local m1=round(r(mean),0.01)
local sd1=string(r(sd), "%3.2f") 
local cov1=round(r(sd) /  r(mean) ,0.001)

twoway (kdensity `outcome' if sex ==0 & !missing(sex, fscb, pa42b), color("248 118 109") lwidth(thick)   ) ///
       (kdensity `outcome' if sex==1  & !missing(sex, fscb, pa42b), color("0 191 196") lwidth(thick)) ///
	      , legend(rows(1) order(1 "Female""Mean=`m0'"  "SD=`sd0'"   "CoV=`cov0'" ///
								 2 "Male"  "Mean=`m1'"  "SD=`sd1'"   "CoV=`cov1'") region(lwidth(none))   position(3) stack)  xtitle("")  ytitle("Density") graphregion(color(white)) 
		  	   graph save `outcome'_sex, replace  


*fsc
sum `outcome' if fscb==0 & !missing(sex, fscb, pa42b)
local m0=round(r(mean),0.01)
local sd0=string(r(sd), "%3.2f") 
local cov0=round(r(sd) /  r(mean) ,0.001)

sum `outcome' if fscb==1 & !missing(sex, fscb, pa42b)
local m1=round(r(mean),0.01)
local sd1=string(r(sd), "%3.2f") 
local cov1=round(r(sd) /  r(mean) ,0.001)

twoway (kdensity `outcome' if fscb ==0  & !missing(sex, fscb, pa42b), color("248 118 109") lwidth(thick)   ) ///
       (kdensity `outcome' if fscb==1 & !missing(sex, fscb, pa42b), color("0 191 196") lwidth(thick)) ///
	      , legend(rows(1) order(1 "Non-manual""Mean=`m0'"  "SD=`sd0'"   "CoV=`cov0'" ///
								 2 "Manual"  "Mean=`m1'"  "SD=`sd1'"   "CoV=`cov1'") region(lwidth(none))   position(3) stack)  xtitle("")  ytitle("Density") graphregion(color(white)) 
		  	   graph save `outcome'_fscb, replace
			   
			   
			   *pa
sum `outcome' if pa42b==0 & !missing(sex, fscb, pa42b)
local m0=round(r(mean),0.01)
local sd0=string(r(sd), "%3.2f") 
local cov0=round(r(sd) /  r(mean) ,0.001)
 
sum `outcome' if pa42b==1 & !missing(sex, fscb, pa42b)
local m1=round(r(mean),0.01)
local sd1=string(r(sd), "%3.2f") 
local cov1=round(r(sd) /  r(mean) ,0.001)

twoway (kdensity `outcome' if pa42b ==0 & !missing(sex, fscb, pa42b), color("248 118 109") lwidth(thick)   ) ///
       (kdensity `outcome' if pa42b==1 & !missing(sex, fscb, pa42b), color("0 191 196") lwidth(thick)) ///
	      , legend(rows(1) order(1 "Active" "Mean=`m0'"  "SD=`sd0'"   "CoV=`cov0'" ///
								 2 "Inactive"  "Mean=`m1'"  "SD=`sd1'"   "CoV=`cov1'") region(lwidth(none))   position(3) stack)  xtitle("`: variable label `outcome''")  ytitle("Density") graphregion(color(white)) 
		  	   graph save `outcome'_pa42b, replace			   
}
 

graph combine bmi46_sex.gph bmi46_fscb.gph bmi46_pa42b.gph  ///
	, ycommon xcommon rows(3) graphregion(color(white) margin(zero))  iscale(.65)
  graph  export "$output/kdensity_bmi46.tif", replace 			     

graph combine wem46_sex.gph wem46_fscb.gph wem46_pa42b.gph  ///
	, ycommon xcommon rows(3) graphregion(color(white) margin(zero))  iscale(.65)
graph  export "$output/kdensity_wem46.tif", replace 	

*table 4 qreg
foreach outcome of varlist bmi46 wem46 {
    cap drop `outcome'log 
	gen `outcome'log = ln(`outcome')
}

*mutually adjusted in table

sqreg bmi46log sex fscb pa42b , q(0.25 0.5 0.75)
outreg2 using "$output/qreg_table.xls", excel  auto(2) nocons  label  nor2 noaster stnum(replace coef=coef*100, replace se=se*100) replace

sqreg wem46log sex fscb pa42b , q(0.25 0.5 0.75)
outreg2 using "$output/qreg_table.xls", excel  auto(2) nocons  label  nor2 noaster stnum(replace coef=coef*100, replace se=se*100) 


*below outputs including baselevels, to aid plotting in R and comparison with GAMLSS
foreach var of varlist sex fscb pa42b {
log using "$output/bmi_`var'_qreg", replace 

*quantile regression. Outcome = BMI, exposure = sex
*bmi is on the absolute kg/m2 scale 
qreg bmi46 i.`var' if !missing(bmi46, sex, pa42b, fscb), q(0.25 ) allbaselevels
qreg bmi46 i.`var' if !missing(bmi46, sex, pa42b, fscb), q(0.5 ) allbaselevels
qreg bmi46 i.`var' if !missing(bmi46, sex, pa42b, fscb), q(0.75 ) allbaselevels

cap log close
log2html "$output/bmi_`var'_qreg", replace //converts to html
}

qreg bmi46log i.pa42b if !missing(bmi46, sex, pa42b, fscb), q(0.75 ) allbaselevels
qreg bmi46 i.pa42b if !missing(bmi46, sex, pa42b, fscb), q(0.75 ) allbaselevels

*manual checks one result which didn't match
qreg bmi46 i.pa42b if !missing(bmi46, sex, pa42b, fscb), q(0.25 ) allbaselevels
qreg bmi46 i.pa42b if !missing(bmi46, sex, pa42b, fscb), q(0.5 ) allbaselevels
qreg bmi46 i.pa42b if !missing(bmi46, sex, pa42b, fscb), q(0.75 ) allbaselevels

qreg bmi46log i.pa42b if !missing(bmi46, sex, pa42b, fscb), q(0.25 ) allbaselevels
qreg bmi46log i.pa42b if !missing(bmi46, sex, pa42b, fscb), q(0.5 ) allbaselevels
qreg bmi46log i.pa42b if !missing(bmi46, sex, pa42b, fscb), q(0.75 ) allbaselevels



*note outputted and placed in R for gamlss plot in logged terms for variables other than sex 
*this does not make material difference to the main comparison drawn - qreg does not operate effectively for this outcome 
foreach var of varlist sex fscb pa42b {
log using "$output/wem46_`var'_qreg", replace 

*quantile regression. Outcome = BMI, exposure = sex
qreg wem46log i.`var' if !missing(wem46, sex, pa42b, fscb), q(0.25 ) allbaselevels
qreg wem46log i.`var' if !missing(wem46, sex, pa42b, fscb), q(0.5 ) allbaselevels
qreg wem46log i.`var' if !missing(wem46, sex, pa42b, fscb), q(0.75 ) allbaselevels

cap log close
log2html "$output/wem46_`var'_qreg", replace //converts to html
}





*suppl table Ns
*bmi
estpost tab  fsc if !missing(bmi46, fscb, pa42b)
esttab . using "$output/suppl_n_bmi46.csv", stats(pct) replace noobs

estpost tab  pa42 if !missing(bmi46, fscb, pa42b)
esttab . using "$output/suppl_n_bmi46.csv", stats(pct) append noobs


*wem
estpost tab  fsc if !missing(wem46, fscb, pa42b)
esttab . using "$output/suppl_n_wem46.csv", stats(pct) replace noobs

estpost tab  pa42 if !missing(wem46, fscb, pa42b)
esttab . using "$output/suppl_n_wem46.csv", stats(pct) append noobs