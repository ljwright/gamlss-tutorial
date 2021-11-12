library(haven)
library(tidyverse)
library(broom)
library(gamlss)
library(magrittr)

rm(list = ls())

df <- read_dta("D:/Projects/Cognition and Height/Data/2001c_cleaned.dta") %>%
  dplyr::select(id, male, mother_edu_level, bmi_14) %>%
  drop_na() %>%
  as_factor() %>%
  sample_n(1000)

get_gamlss <- function(boot, covar){
  set.seed(boot)
  
  df <- rename(df, covar = all_of(!!covar)) %>%
    sample_frac(replace = TRUE)
  
  capture.output({
    mod <- gamlss(bmi_14 ~ covar,
                   sigma.formula = ~ covar,
                   nu.formula = ~ covar,
                   family = BCCG(mu.link = log), 
                   data = df, print = FALSE)
  })
  
  coefAll(mod) %>%
    map(enframe) %>%
    bind_rows(.id = "parameter")
}

res <- tibble(boot = 1:100) %>%
  mutate(res = map(boot, get_gamlss, "male")) %>%
  unnest(res) %>%
  filter(name == "(Intercept)")

res %>%
  dplyr::select(-name) %>%
  pivot_wider(names_from = parameter, values_from = value) %>%
  uncount(99, .id = "centile") %>%
  mutate(bmi = exp(mu) * (1 + exp(sigma) * nu * qnorm(centile/100))^(1/nu)) %>%
  group_by(centile) %>%
  summarise(beta = quantile(bmi, .5),
            lci = quantile(bmi, .025),
            uci = quantile(bmi, .975)) %>%
  ggplot() +
  aes(x = centile, y = beta, ymin = lci, ymax = uci) +
  geom_ribbon(color = NA, alpha = 0.2) +
  geom_line()


summary(mod)
predict(mod)

dat <- tibble(L = -0.8613 + c(0.4787, 0),
              M = exp(3.29806 + c(0.04119, 0)),
              S = exp(-1.6232 + c(-0.2301, 0)),
              '25' = 23.74061 + c(1.685968, 0),
              '50' = 26.90139 + c(1.286974,  0),
              '75' = 31.5361 + c(-.2513466, 0),
              Sex = c('Male', 'Female'))

pdLMS(dat$L, dat$M, dat$S, plot = FALSE) %>%
  extract(1:2) %>%
  map(as_tibble) %>%
  bind_cols() %>%
  pivot_longer(cols = -value, values_to = "density") %>%
  group_by(name) %>%
  mutate(cum_dens = cumsum(density),
         cum_dens = cum_dens/max(cum_dens)) %>%
  ungroup() %>%
  ggplot() +
  aes(x = cum_dens, y = value, color = name) +
  geom_line()


dat %>% 
  pivot_wider(names_from = Sex, values_from = -Sex) %>%
  bind_cols(tibble(q = 1:99/100)) %>%
  pivot_longer(-q, names_to = c('.value', 'Sex'), names_sep = '_') %>%
  mutate(BMI = M * (1 + L * S * qnorm(q))^(1/L),
         centile = q * 100) %>%
  ggplot() +
  aes(x = centile, y = BMI, color = Sex) +
  geom_


dat %>%
  group_by(name) %>%
  mutate(cum_dens = cumsum(density))
