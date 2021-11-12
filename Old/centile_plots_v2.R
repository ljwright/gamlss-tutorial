#load the libaries
library(gamlss)
library(sitar)
library(tidyverse)
library(haven)
library(ggplot2)
library(ggpubr) 
library(cowplot)
#install.packages("cowplot")

#install.packages("ggpubr")
#set wd
setwd("C:/Users/DAB/OneDrive - University College London/ideas/quantile 2/analysis/output")

#plots 
##BMI and sex

#load all data
dat <- read_dta('cleaned_datafor_r.dta')

#BCCG
#bmi
bmi46<- subset(dat, select = c("bmi46", "sex", "fscb" , "pa42b"))

bmi46sex_bccg <- gamlss(bmi46 ~ sex  ,
                        sigma.formula = ~sex      ,
                        nu.formula = ~sex      ,
                        family = BCCG (mu.link = log), data = na.omit(bmi46) )



dat <- tibble(L = -0.8613 + c(0.4787, 0),
              M = exp(3.29806 + c(0.04119, 0)),
              S = exp(-1.6232 + c(-0.2301, 0)),
              '25' = 23.74061 + c(1.685968, 0),
              '50' = 26.90139 + c(1.286974,  0),
              '75' = 31.5361 + c(-.2513466, 0),
              Sex = c('Male', 'Female'))
#View(dat) #to view the data
#pdLMS = plot frequency distributoins for given L m and S values (ie, power, median, CoV)
with(dat, pdLMS(L = L, M = M, S = S)) 


#this seems to modify the plots 
dat2 <- dat %>% 
  pivot_wider(names_from = Sex, values_from = L:`75`) %>%
  bind_cols(tibble(q = 1:99/100)) %>%
  pivot_longer(-q, names_to = c('.value', 'Sex'), names_sep = '_') %>%
  mutate(BMI = M * (1 + L * S * qnorm(q))^(1/L),
         centile = q * 100)

#View(dat2)

theme_set(theme_bw())

bmi_sex<- ggplot(dat2, aes(centile, BMI, group = Sex, colour = Sex)) +
  geom_path(size = 1) +
  geom_point(aes(cent), size=3, data = dat %>%
               pivot_longer(c('25':'75'), names_to = 'cent', values_to = 'BMI') %>%
               mutate(cent = as.numeric(cent)))
bmi_sex

ggsave('BMI vs centile_sex.tiff', h = 7, w = 8)



###fsc equivilent

#bmi46<- subset(dat, select = c("bmi46", "fscb", "fscb" , "pa42b"))

bmi46fscb_bccg <- gamlss(bmi46 ~ fscb  ,
                         sigma.formula = ~fscb      ,
                         nu.formula = ~fscb      ,
                         family = BCCG (mu.link = log), data = na.omit(bmi46) )

dat <- tibble(L = -0.85072 + c(0.37815, 0),
              M = exp(3.291599 + c(0.043183, 0)),
              S = exp(-1.75254 + c(0.06061, 0)),
              '25' = 23.97711 + c(.8931789, 0),
              '50' = 26.99279 + c(1.00448,  0),
              '75' = 30.36817 + c(1.524023, 0),
              SES = c('Manual', 'Non-manual'))  
#View(dat) #to view the data
#pdLMS = plot frequency distributoins for given L m and S values (ie, power, median, CoV)
with(dat, pdLMS(L = L, M = M, S = S)) 


#this seems to modify the plots 
dat2 <- dat %>% 
  pivot_wider(names_from = SES, values_from = L:`75`) %>%
  bind_cols(tibble(q = 1:99/100)) %>%
  pivot_longer(-q, names_to = c('.value', 'SES'), names_sep = '_') %>%
  mutate(BMI = M * (1 + L * S * qnorm(q))^(1/L),
         centile = q * 100)

#View(dat2)

theme_set(theme_bw())

#change order so risk factor matches across
dat2$SES<- factor(dat2$SES, levels=c("Non-manual", "Manual"), labels=c("Non-manual", "Manual"))



bmi_fsc <-ggplot(dat2, aes(centile, BMI, group = SES, colour = SES)) +
  geom_path(size = 1) +
  geom_point(aes(cent), size=3, data = dat %>%
               pivot_longer(c('25':'75'), names_to = 'cent', values_to = 'BMI') %>%
               mutate(cent = as.numeric(cent)))
ggsave('BMI vs centile_fsc.tiff', h = 7, w = 8)

bmi_fsc

#activity equivilent

bmi46<- subset(dat, select = c("bmi46", "sex", "fscb" , "pa42b"))

bmi46pa42b_bccg <- gamlss(bmi46 ~ pa42b  ,
                          sigma.formula = ~pa42b      ,
                          nu.formula = ~pa42b      ,
                          family = BCCG (mu.link = log), data = na.omit(bmi46) )
#check here [error]

dat <- tibble(L = -0.61064 + c(0.08856, 0),
              M = exp(3.311604 + c(0.028636, 0)),
              S = exp(-1.73918 + c(0.10459, 0)),              
              '25' = 24.45705 + c(.249918, 0),
              '50' = 27.43401 + c(.8290482,  0),
              '75' = 31.01563 + c(1.368374, 0),
              Activity = c('Inactive', 'Active'))
#View(dat) #to view the data
#pdLMS = plot frequency distributoins for given L m and S values (ie, power, median, CoV)
with(dat, pdLMS(L = L, M = M, S = S)) 


#this seems to modify the plots 
dat2 <- dat %>% 
  pivot_wider(names_from = Activity, values_from = L:`75`) %>%
  bind_cols(tibble(q = 1:99/100)) %>%
  pivot_longer(-q, names_to = c('.value', 'Activity'), names_sep = '_') %>%
  mutate(BMI = M * (1 + L * S * qnorm(q))^(1/L),
         centile = q * 100)


#View(dat2)

levels(dat2$Activity)

theme_set(theme_bw())


bmi_pa <- ggplot(dat2, aes(centile, BMI, group = Activity, colour = Activity)) +
  geom_path(size = 1) +
  geom_point(aes(cent), size=3, data = dat %>%
               pivot_longer(c('25':'75'), names_to = 'cent', values_to = 'BMI') %>%
               mutate(cent = as.numeric(cent)))

bmi_pa




#View(dat2)


#wide

bmi_sex_ny <- bmi_sex +  theme(legend.title = element_blank()) + scale_y_continuous(breaks=seq(0,50,5)) + expand_limits(y = 50) + theme(plot.margin=unit(c(1,0,0,0),"cm"))  + theme(axis.line = element_line(colour = "black")) +  theme(panel.border = element_blank())
bmi_fsc_ny <- bmi_fsc + theme(axis.title.y = element_blank()) + theme(legend.title = element_blank()) + scale_y_continuous(breaks=seq(0,50,5)) + expand_limits(y = 50) + theme(plot.margin=unit(c(1,0,0,0),"cm")) + theme(axis.line = element_line(colour = "black")) +  theme(panel.border = element_blank())
bmi_pa_ny <- bmi_pa + theme(axis.title.y = element_blank()) + theme(legend.title = element_blank()) + scale_y_continuous(breaks=seq(0,50,5)) + expand_limits(y = 50) + theme(plot.margin=unit(c(1,0,0,0),"cm"))   + theme(axis.line = element_line(colour = "black")) +  theme(panel.border = element_blank())
bmi_pa_ny

bmi_all  <- ggarrange(bmi_sex_ny, bmi_fsc_ny, bmi_pa_ny,
                      #labels = c("A", "B", "C"),
                      hjust = -.5,  #default 
                      vjust = 1,
                      ncol = 3, nrow = 1, 
                      common.legend = FALSE, legend = "bottom")
bmi_all

ggsave('bmi_centile_all_wide.tiff', h = 5, w = 12)


##wemwebs plots

#set wd
setwd("C:/Users/DAB/OneDrive - University College London/ideas/quantile 2/analysis/output")

#load all data
dat <- read_dta('cleaned_datafor_r.dta')

#sex
wem46<- subset(dat, select = c("wem46", "sex", "fscb" , "pa42b"))

wem46sex_bccg <- gamlss(wem46 ~ sex  ,
                        sigma.formula = ~sex      ,
                        nu.formula = ~sex      ,
                        family = BCCG (mu.link = log), data = na.omit(wem46) )



dat <- tibble(L = 1.68936 + c(0.02724, 0),
              M = exp(3.906786 + c(-0.002833, 0)),
              S = exp(-1.79939 + c(-0.03617, 0)),
              '25' = 44 + c(0, 0),
              '50' = 50 + c(0,  0),
              '75' = 55 + c(0, 0),
              sex = c('Male', 'Female'))
#View(dat) #to view the data
#pdLMS = plot frequency distributoins for given L m and S values (ie, power, median, CoV)
with(dat, pdLMS(L = L, M = M, S = S)) 


#this seems to modify the plots 
dat2 <- dat %>% 
  pivot_wider(names_from = sex, values_from = L:`75`) %>%
  bind_cols(tibble(q = 1:99/100)) %>%
  pivot_longer(-q, names_to = c('.value', 'sex'), names_sep = '_') %>%
  mutate(WEMWEBS = M * (1 + L * S * qnorm(q))^(1/L),
         centile = q * 100)

#View(dat2)

theme_set(theme_bw())

wem_sex <- ggplot(dat2, aes(centile, WEMWEBS, group = sex, colour = sex)) +
  geom_path(size = 1) +
  geom_point(aes(cent), size=3, data = dat %>%
               pivot_longer(c('25':'75'), names_to = 'cent', values_to = 'WEMWEBS') %>%
               mutate(cent = as.numeric(cent)))
ggsave('WEMWEBS vs centile_sex.tiff', h = 7, w = 8)



###fsc equivilent

wem46fscb_bccg <- gamlss(wem46 ~ fscb  ,
                         sigma.formula = ~fscb      ,
                         nu.formula = ~fscb      ,
                         family = BCCG (mu.link = log), data = na.omit(wem46) )

dat <- tibble(L = 1.8221 + c(-0.2034, 0),
              M = exp(3.923739 + c(-0.028689, 0)),
              S = exp(-1.89019 + c(0.10932, 0)),
              '25' = exp(3.80666 + c(-.0454624, 0)),
              '50' = exp( 3.931826 + c(-.0400054,  0)),
              '75' = exp(4.007333 + c(0, 0)),
              SES = c('Manual', 'Non-manual'))
#View(dat) #to view the data
#pdLMS = plot frequency distributoins for given L m and S values (ie, power, median, CoV)
with(dat, pdLMS(L = L, M = M, S = S)) 


#this seems to modify the plots 
dat2 <- dat %>% 
  pivot_wider(names_from = SES, values_from = L:`75`) %>%
  bind_cols(tibble(q = 1:99/100)) %>%
  pivot_longer(-q, names_to = c('.value', 'SES'), names_sep = '_') %>%
  mutate(WEMWEBS = M * (1 + L * S * qnorm(q))^(1/L),
         centile = q * 100)

#View(dat2)

theme_set(theme_bw())

#change order so risk factor matches across
dat2$SES<- factor(dat2$SES, levels=c("Non-manual", "Manual"), labels=c("Non-manual", "Manual"))

wem_fsc <- ggplot(dat2, aes(centile, WEMWEBS, group = SES, colour = SES)) +
  geom_path(size = 1) +
  geom_point(aes(cent), size=3, data = dat %>%
               pivot_longer(c('25':'75'), names_to = 'cent', values_to = 'WEMWEBS') %>%
               mutate(cent = as.numeric(cent)))
ggsave('WEMWEBS vs centile_fsc.tiff', h = 7, w = 8)

#wem and pa 

###activity equivilent

wem46pa42b_bccg <- gamlss(wem46 ~ pa42b  ,
                          sigma.formula = ~pa42b      ,
                          nu.formula = ~pa42b      ,
                          family = BCCG (mu.link = log), data = na.omit(wem46) )

dat <- tibble(L = 1.68732 + c(-0.12302, 0),
              M = exp(3.918680 + c(-0.052216, 0)),
              S = exp(-1.86800 + c(0.16200, 0)),
              '25' = exp(3.806663 + c(-.0689929, 0)),
              '50' = exp(3.931826 + c(-.0606246,  0)),
              '75' = exp(4.007333 + c(-.0183492, 0)),
              Activity = c('Inactive', 'Active'))
#View(dat) #to view the data
#pdLMS = plot frequency distributoins for given L m and S values (ie, power, median, CoV)
with(dat, pdLMS(L = L, M = M, S = S)) 


#this seems to modify the plots 
dat2 <- dat %>% 
  pivot_wider(names_from = Activity, values_from = L:`75`) %>%
  bind_cols(tibble(q = 1:99/100)) %>%
  pivot_longer(-q, names_to = c('.value', 'Activity'), names_sep = '_') %>%
  mutate(WEMWEBS = M * (1 + L * S * qnorm(q))^(1/L),
         centile = q * 100)

#View(dat2)

theme_set(theme_bw())

wem_pa <- ggplot(dat2, aes(centile, WEMWEBS, group = Activity, colour = Activity)) +
  geom_path(size = 1) +
  geom_point(aes(cent), size=3, data = dat %>%
               pivot_longer(c('25':'75'), names_to = 'cent', values_to = 'WEMWEBS') %>%
               mutate(cent = as.numeric(cent)))
ggsave('WEMWEBS vs centile_activity.tiff', h = 7, w = 8)

wem_sex

#combined wemwebs plot
wem_sex_ny <- wem_sex +  theme(legend.title = element_blank()) + scale_y_continuous(breaks=seq(20,70,10)) + expand_limits(y = 20) + theme(plot.margin=unit(c(1,0,0,0),"cm"))  + theme(axis.line = element_line(colour = "black")) +  theme(panel.border = element_blank())
wem_fsc_ny <- wem_fsc + theme(axis.title.y = element_blank()) + theme(legend.title = element_blank()) + scale_y_continuous(breaks=seq(0,70,10)) + expand_limits(y = 20) + theme(plot.margin=unit(c(1,0,0,0),"cm")) + theme(axis.line = element_line(colour = "black")) +  theme(panel.border = element_blank())
wem_pa_ny <- wem_pa + theme(axis.title.y = element_blank()) + theme(legend.title = element_blank()) + scale_y_continuous(breaks=seq(0,70,10)) + expand_limits(y = 20) + theme(plot.margin=unit(c(1,0,0,0),"cm"))   + theme(axis.line = element_line(colour = "black")) +  theme(panel.border = element_blank())


wem_all  <- ggarrange(wem_sex_ny, wem_fsc_ny, wem_pa_ny,
                      #labels = c("A", "B", "C"),
                      hjust = -.5,  #default 
                      vjust = 1,
                      ncol = 3, nrow = 1, 
                      common.legend = FALSE, legend = "bottom")
wem_all

ggsave('wem_centile_all_wide.tiff', h = 5, w = 12)


