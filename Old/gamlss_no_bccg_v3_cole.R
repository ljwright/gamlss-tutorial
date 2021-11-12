#load the libaries
library(gamlss)
library(sitar)
library(tidyverse)
library(haven)
library(psych) #for the tabstat equivilent describeby to check sub group means, sd

#set wd
setwd("C:/Users/DAB/OneDrive - University College London/ideas/quantile 2/analysis/output")

#load all data
dat <- read_dta('cleaned_datafor_r.dta')

#bmi
bmi46<- subset(dat, select = c("bmi46", "sex", "fscb" , "pa42b"))

#bmi
describeBy(bmi46, group="sex" ,digits=2)
describeBy(bmi46, group="fscb" ,digits=2)
describeBy(bmi46, group="pa42b" ,digits=2)

#output estimates
bmi46sex_no <- gamlss(bmi46 ~ sex  ,
                      sigma.formula = ~sex      ,
                      family = NO (mu.link = log), data = na.omit(bmi46) )


bmi46fscb_no <- gamlss(bmi46 ~ fscb  ,
                       sigma.formula = ~fscb      ,
                       family = NO (mu.link = log), data = na.omit(bmi46) )

bmi46pa42b_no <- gamlss(bmi46 ~ pa42b  ,
                        sigma.formula = ~pa42b      ,
                        family = NO (mu.link = log), data = na.omit(bmi46) )




#mutually adjusted
bmi46adj_no <- gamlss(bmi46 ~ sex + fscb + pa42b  ,
                   sigma.formula = ~sex + fscb + pa42b      ,
                   family = NO (mu.link = log), data = na.omit(bmi46) )

##wemwebs
wem46<- subset(dat, select = c("wem46", "sex", "fscb" , "pa42b"))

wem46sex_no <- gamlss(wem46 ~ sex  ,
                   sigma.formula = ~sex      ,
                   family = NO (mu.link = log), data = na.omit(wem46) )


wem46fscb_no <- gamlss(wem46 ~ fscb  ,
                    sigma.formula = ~fscb      ,
                    family = NO (mu.link = log), data = na.omit(wem46) )

wem46pa42b_no <- gamlss(wem46 ~ pa42b  ,
                     sigma.formula = ~pa42b      ,
                     family = NO (mu.link = log), data = na.omit(wem46) )

#mutually adjusted
wem46adj_no <- gamlss(wem46 ~ sex + fscb + pa42b  ,
                   sigma.formula = ~sex + fscb + pa42b      ,
                   family = NO (mu.link = log), data = na.omit(wem46) )


##other syntax continued  
  
bmi46fscb_bccg <- gamlss(bmi46 ~ fscb  ,
                    sigma.formula = ~fscb      ,
                    nu.formula = ~fscb    ,
                    family = BCCG (mu.link = log), data = na.omit(bmi46) )

bmi46pa42b_bccg <- gamlss(bmi46 ~ pa42b  ,
                     sigma.formula = ~pa42b      ,
                     nu.formula = ~pa42b    ,
                     family = BCCG (mu.link = log), data = na.omit(bmi46) )

#mutually adjusted
bmi46adj_bccg <- gamlss(bmi46 ~ sex + fscb + pa42b  ,
                   sigma.formula = ~sex + fscb + pa42b      ,
                   nu.formula = ~sex   + fscb + pa42b   ,
                   family = BCCG (mu.link = log), data = na.omit(bmi46) )

#wemwebs
wem46<- subset(dat, select = c("wem46", "sex", "fscb" , "pa42b"))

wem46sex_bccg <- gamlss(wem46 ~ sex  ,
                   sigma.formula = ~sex      ,
                   nu.formula = ~sex      ,
                   family = BCCG (mu.link = log), data = na.omit(wem46) )

wem46fscb_bccg <- gamlss(wem46 ~ fscb  ,
                    sigma.formula = ~fscb      ,
                    nu.formula = ~fscb    ,
                    family = BCCG (mu.link = log), data = na.omit(wem46) )

wem46pa42b_bccg <- gamlss(wem46 ~ pa42b  ,
                     sigma.formula = ~pa42b      ,
                     nu.formula = ~pa42b    ,
                     family = BCCG (mu.link = log), data = na.omit(wem46) )

#mutually adjusted
wem46adj_bccg <- gamlss(wem46 ~ sex + fscb + pa42b  ,
                   sigma.formula = ~sex + fscb + pa42b      ,
                   nu.formula = ~sex   + fscb + pa42b   ,
                   family = BCCG (mu.link = log), data = na.omit(wem46) )

#output to file
sink(file = "table1_bmi_no_gamlss.txt")
#bmi
summary(bmi46sex_no)
summary(bmi46sex_bccg)
summary(bmi46fscb_no)
summary(bmi46fscb_bccg)
summary(bmi46pa42b_no)
summary(bmi46pa42b_bccg)
summary(bmi46adj_no)
summary(bmi46adj_bccg)

#stop output to file?
sink(file = NULL)

#output to file
sink(file = "table2_wemwebs_no_gamlss.txt")

#wemwebs
#wem
summary(wem46sex_no)
summary(wem46sex_bccg)
summary(wem46fscb_no)
summary(wem46fscb_bccg)
summary(wem46pa42b_no)
summary(wem46pa42b_bccg)
summary(wem46adj_no)
summary(wem46adj_bccg)

#stop output to file?
sink(file = NULL)

##Supplmentary table 1
##results using all categories of PA and FSC
#mutually adjusted
#set wd
setwd("C:/Users/DAB/OneDrive - University College London/ideas/quantile 2/analysis/output")

#load all data
dat <- read_dta('cleaned_datafor_r.dta')

#bmi
bmi46<- subset(dat, select = c("bmi46", "sex", "fsc" , "pa42"))

bmi46adj_no <- gamlss(bmi46 ~ sex + factor(fsc) + factor(pa42)  ,
                      sigma.formula = ~sex + factor(fsc) + factor(pa42)      ,
                      family = NO (mu.link = log), data = na.omit(bmi46) )

bmi46adj_bccg <- gamlss(bmi46 ~ sex + factor(fsc) + factor(pa42)  ,
                        sigma.formula = ~sex + factor(fsc) + factor(pa42)      ,
                        nu.formula = ~sex   + factor(fsc) + factor(pa42)   ,
                        family = BCCG (mu.link = log), data = na.omit(bmi46) )


#wemwebs
wem46<- subset(dat, select = c("wem46", "sex", "fsc" , "pa42"))

wem46adj_no <- gamlss(wem46 ~ sex + factor(fsc) + factor(pa42)  ,
                      sigma.formula = ~sex + factor(fsc) + factor(pa42)      ,
                      family = NO (mu.link = log), data = na.omit(wem46) )

wem46adj_bccg <- gamlss(wem46 ~ sex + factor(fsc) + factor(pa42)  ,
                        sigma.formula = ~sex + factor(fsc) + factor(pa42)      ,
                        nu.formula = ~sex   + factor(fsc) + factor(pa42)   ,
                        family = BCCG (mu.link = log), data = na.omit(wem46) )


#output to file
sink(file = "supple_all_categories_gamlss.txt")

#wemwebs
#wem
summary(bmi46adj_no)
summary(bmi46adj_bccg)
summary(wem46adj_no)
summary(wem46adj_bccg)

#stop output to file?
sink(file = NULL)

###same for simulation results [not included for now - results largely as expected in stata markdown]

