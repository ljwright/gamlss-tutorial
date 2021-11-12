*checking r outputs
**To do - restrict N to gamlss N in histograms

global data "C:\Users\DAB\OneDrive - University College London\stats_cls\cohorts\"
global output "C:\Users\DAB\OneDrive - University College London\ideas\quantile 2\analysis\output"

use "$output/cleaned_datafor_r.dta", clear

sum 
sum if !missing(bmi46, sex, fsc, pa42)
sum if !missing(wem46, sex, fsc, pa42)

drop wem46 
drop if missing(wem46, sex, fsc, pa42)

foreach outcome of varlist bmi46 {
    cap drop `outcome'log 
	gen `outcome'log = ln(`outcome')
}
sum bmi46log sex fsc pa42
reg bmi46log i.sex if !missing(sex, fsc, pa42)



cap erase "$output/qreg_table.doc"

sqreg bmi46 sex fscb pa42b , q(0.25 0.5 0.75)
outreg2 using "$output/qreg_table", excel  auto(2) nocons  label  nor2 noaster stnum(replace coef=coef*100, replace se=se*100) replace

sqreg wem46 sex fscb pa42b , q(0.25 0.5 0.75)
outreg2 using "$output/qreg_table", excel  auto(2) nocons  label  nor2 noaster stnum(replace coef=coef*100, replace se=se*100) 




*try log

qreg wem46log i.fscb if !missing(wem46, sex, pa42b, fscb), q(0.25 ) allbaselevels
qreg wem46log i.fscb if !missing(wem46, sex, pa42b, fscb), q(0.5 ) allbaselevels
qreg wem46log i.fscb if !missing(wem46, sex, pa42b, fscb), q(0.75 ) allbaselevels


*works
qreg wem46log i.sex if !missing(wem46, sex, pa42b, fscb), q(0.25 ) allbaselevels
qreg wem46log i.sex if !missing(wem46, sex, pa42b, fscb), q(0.5 ) allbaselevels


qreg wem46log i.sex if !missing(wem46, sex, pa42b, fscb), q(0.5 ) allbaselevels
tabstat wem46log , by(sex) stats(mean p25 median p75 n)
tabstat wem46 , by(sex) stats(mean p25 median p75 n)

tab wem46
hist wem46

qreg wem46log i.sex if !missing(wem46, sex, pa42b, fscb), q(0.75 ) allbaselevels

qreg wem46 i.sex if !missing(wem46, sex, pa42b, fscb), q(0.25 ) allbaselevels
qreg wem46 i.sex if !missing(wem46, sex, pa42b, fscb), q(0.5 ) allbaselevels
qreg wem46 i.sex if !missing(wem46, sex, pa42b, fscb), q(0.75 ) allbaselevels



qreg wem46log i.pa42b if !missing(wem46, sex, pa42b, fscb), q(0.25 ) allbaselevels
qreg wem46log i.pa42b if !missing(wem46, sex, pa42b, fscb), q(0.5 ) allbaselevels
qreg wem46log i.pa42b if !missing(wem46, sex, pa42b, fscb), q(0.75 ) allbaselevels

qreg wem46log i.fscb if !missing(wem46, sex, pa42b, fscb), q(0.25 ) allbaselevels
qreg wem46log i.fscb if !missing(wem46, sex, pa42b, fscb), q(0.5 ) allbaselevels
qreg wem46log i.fscb if !missing(wem46, sex, pa42b, fscb), q(0.75 ) allbaselevels

qreg wem46log i.fscb i.sex if !missing(wem46, sex, pa42b, fscb), q(0.75 ) allbaselevels //only works if other vars included!
qreg wem46 i.fscb i.sex if !missing(wem46, sex, pa42b, fscb), q(0.75 ) allbaselevels //only works if other vars included!


qreg bmi46 sex , q(0.50 )
qreg wem46log sex , q(0.50 )
qreg wem46log sex , q(0.75 )
