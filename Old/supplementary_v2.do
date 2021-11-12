 
 
*Supplementary Table 2
//use "$path/cleaned.dta", clear //aase

use "$data/covid/cleaned.dta", clear //david path

tab1 psleep csleep psleepb csleepb ppa cpa ppab cpab pmalc cmalc paclrisk caclrisk pfveg cfveg pfvegb cfvegb

*whole sample
table1_mc [fw=fweight], ///
vars( ///
psleep cat %4.1f \ ///
csleep cat %4.1f \ ///
psleepb bin %4.1f \ ///
csleepb bin %4.1f \ ///
ppa cat %4.1f \ ///
cpa cat %4.1f \ ///
ppab bin %4.1f \ ///
cpab bin %4.1f \ ///
palc cat %4.1f \ ///
calc cat %4.1f \ ///
paclrisk bin %4.1f \ ///
caclrisk bin %4.1f \ ///
pfveg cat %4.1f \ ///
cfveg cat %4.1f \ ///
pfvegb bin %4.1f \ ///
cfvegb bin %4.1f \ ///
) ///
nospace percent  ///
saving("$output/supplementary table 2 whole sample.xlsx", replace)

*cohort
table1_mc [fw=fweight], by(cohort) ///
vars( ///
psleep cat %4.1f \ ///
csleep cat %4.1f \ ///
psleepb bin %4.1f \ ///
csleepb bin %4.1f \ ///
ppa cat %4.1f \ ///
cpa cat %4.1f \ ///
ppab bin %4.1f \ ///
cpab bin %4.1f \ ///
palc cat %4.1f \ ///
calc cat %4.1f \ ///
paclrisk bin %4.1f \ ///
caclrisk bin %4.1f \ ///
pfveg cat %4.1f \ ///
cfveg cat %4.1f \ ///
pfvegb bin %4.1f \ ///
cfvegb bin %4.1f \ ///
) ///
nospace percent  ///
saving("$output/supplementary table 2 cohort.xlsx", replace)


*gender
table1_mc [fw=fweight], by(sex) ///
vars( ///
psleep cat %4.1f \ ///
csleep cat %4.1f \ ///
psleepb bin %4.1f \ ///
csleepb bin %4.1f \ ///
ppa cat %4.1f \ ///
cpa cat %4.1f \ ///
ppab bin %4.1f \ ///
cpab bin %4.1f \ ///
palc cat %4.1f \ ///
calc cat %4.1f \ ///
paclrisk bin %4.1f \ ///
caclrisk bin %4.1f \ ///
pfveg cat %4.1f \ ///
cfveg cat %4.1f \ ///
pfvegb bin %4.1f \ ///
cfvegb bin %4.1f \ ///
) ///
nospace percent  ///
saving("$output/supplementary table 2 gender.xlsx", replace)


*education
table1_mc [fw=fweight], by(smedb) ///
vars( ///
psleep cat %4.1f \ ///
csleep cat %4.1f \ ///
psleepb bin %4.1f \ ///
csleepb bin %4.1f \ ///
ppa cat %4.1f \ ///
cpa cat %4.1f \ ///
ppab bin %4.1f \ ///
cpab bin %4.1f \ ///
palc cat %4.1f \ ///
calc cat %4.1f \ ///
paclrisk bin %4.1f \ ///
caclrisk bin %4.1f \ ///
pfveg cat %4.1f \ ///
cfveg cat %4.1f \ ///
pfvegb bin %4.1f \ ///
cfvegb bin %4.1f \ ///
) ///
nospace percent  ///
saving("$output/supplementary table 2 education.xlsx", replace)


*ethnicity
table1_mc [fw=fweight], by(eth) ///
vars( ///
psleep cat %4.1f \ ///
csleep cat %4.1f \ ///
psleepb bin %4.1f \ ///
csleepb bin %4.1f \ ///
ppa cat %4.1f \ ///
cpa cat %4.1f \ ///
ppab bin %4.1f \ ///
cpab bin %4.1f \ ///
palc cat %4.1f \ ///
calc cat %4.1f \ ///
paclrisk bin %4.1f \ ///
caclrisk bin %4.1f \ ///
pfveg cat %4.1f \ ///
cfveg cat %4.1f \ ///
pfvegb bin %4.1f \ ///
cfvegb bin %4.1f \ ///
) ///
nospace percent  ///
saving("$output/supplementary table 2 ethnicity.xlsx", replace)
