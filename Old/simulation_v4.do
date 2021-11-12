***stata syntax ***
***a simple simulation exercise to see if one can 'recreate' GAMLSS and qreg findings
***and thus try to better understand why they may arise...
***GAMLSS - risk factors assocaited with mean and variation
***Qreg - risk factors have larger effects on outcome at higher outcome centiles (and SE are wider) 

global output "C:\Users\DAB\OneDrive - University College London\ideas\quantile 2\analysis\output"

cap log close
log using "$output/simulation", replace

*create variables with prespecified variances, Ns, and correlations

clear
set seed 1211
matrix M = 20, 20, 20

matrix V = (9, 5, 2 \ 5, 4, 1 \ 2, 1, 1)

matrix list M
matrix list V

drawnorm exposure b outcome , n(5000) cov(V) means(M)
sum 
corr
corr   exposure b outcome

*drop unecessary additional exposure
drop b

*generate categorical variables to aid subsequent descriptions by subgroup
egen exposureb = xtile(exposure), nq(2)

egen exposureq = xtile(exposure), nq(5)
egen outcomeq = xtile(outcome), nq(5)

*investigate differences in variation (SD) and effect size across outcome centiles (qreg)
tabstat outcome, by(exposureq)  stats(mean sd n)  //no difference in SD
sqreg outcome exposure , q(0.25 0.5 0.75) //same effect size across outcome centiles 
sqreg outcome i.exposureq , q(0.25 0.5 0.75) //same magnitude of association across all quintiles  

*now add random noise to outcome ie, add more variation at upper outcome centiles
*first make random variable 
cap drop ran
generate ran = 1 + runiform()
hist ran
sum ran

*then multiple with outcome to make outcome with more added random variance which is seemingly orthogonal to the exposure
cap drop outcomeran 
gen outcomeran= outcome
replace outcomeran = outcomeran * (ran) if outcome>20
sum outcome outcomeran 

*same for exposure
cap drop exposureran 
gen exposureran= exposure
replace exposureran = exposureran * (ran) if outcome>20
sum exposure exposureran 

*check correlations
corr exposure ran outcome outcomeran

*investigate differences in variation (SD) and effect size across outcome centiles (qreg)
corr exposure outcome outcomeran //weaker corr
corr exposure outcome outcomeran if outcome<20 //weaker corr
corr exposure outcome outcomeran if outcome>20 //weaker corr in higher values only and more variation

tabstat outcome, by(exposureq)  stats(mean sd n) 
tabstat outcomeran, by(exposureq)  stats(mean sd n) //more variation at higher values, where the random noise was added

twoway (hist outcomeran if outcomeran <50, color(red%30)) (hist outcome, color(blue%30) )
graph save "$output/sim_hist.gph", replace //sadly plots do not appear in this log file!

*seperate regression analyses below and above point in which variability was added 
reg outcomeran exposure if outcomeran<20
local beta1: display %5.4f _b[exposure]

reg outcomeran exposure if outcomeran>20
local beta2: display %5.4f _b[exposure]
twoway  (lfitci outcomeran exposure) (scatter outcomeran exposure), note(beta(outcome<20)=`beta1' | beta(outcome>20)=`beta2' ) legend(off)
graph save "$output/sim_scatter.gph", replace //sadly plots do not appear in this log file!


twoway  (lfitci outcome exposure) (scatter outcome exposure), legend(off)



*I have generated the intended effect by adding noise ie, weakened assoc
sqreg outcomeran exposure  , q(0.25 0.5 0.75) //I have generated the intended effect ! 
								 //that is, estimates higher where there is more random noise, which seemingly stengthens pre-existing association

tabstat outcomeran, by(exposureq)  stats(mean sd n)  //more variability in c in later points of a dist, as this is where random variation was added 

*random noise in exposure
egen exposureranq = xtile(exposureran), nq(5)

sqreg outcome exposureran , q(0.25 0.5 0.75) //little evidence for difference
sqreg outcome i.exposureranq , q(0.25 0.5 0.75) //little evidence for difference
tabstat outcome, by(exposureranq)  stats(mean sd n)  //little evidence for difference 

*save file to run GAMLSS
keep exposure exposureq exposureran outcome outcomeran 

save "$output/abc_r2.dta", replace 

log close
*convert to html file
log2html "$output/simulation", replace //converts to html