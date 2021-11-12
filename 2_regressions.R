library(tidyverse)
library(glue)
library(broom)
library(broom.mixed)
library(quantreg)
library(gamlss)

rm(list = ls())

# 1. Load Data ----
load("Data/df_analysis.Rdata")

df <- df %>%
  mutate(bmi_ln = log(bmi_46),
         wemwbs_ln = log(wemwbs_42)) %>%
  filter(miss_covars == FALSE)

mod_covars <- str_subset(names(df), "(sex|father_class|exercise)") %>%
  set_names(., .) %>%
  as.list() %>%
  c(list(all = c("sex", "father_class", "exercise"),
         all_binary = c("sex", "father_class_binary", "exercise_binary")))

mod_specs <- expand_grid(dep_var = c("bmi_46", "wemwbs_42", "bmi_ln", "wemwbs_ln"),
                         mod = names(mod_covars))


# 2. Generalized Linear Regression Models ----
get_glm <- function(dep_var, mod, link){
  mod <- mod_covars[[mod]] %>%
    glue_collapse(" + ") %>%
    paste(dep_var, "~", .) %>%
    as.formula() %>%
    glm(family = gaussian(link = link), df)
  
  tidy(mod, conf.int = TRUE) %>%
    mutate(n = glance(mod)$nobs)
}

res_glm <- mod_specs %>%
  filter(!str_detect(dep_var, "_ln$")) %>%
  expand_grid(link = c("log", "identity")) %>%
  mutate(res = pmap(list(dep_var, mod, link), get_glm)) %>%
  unnest(res)

# 2. Quantile Regression Models ----
get_qreg <- function(dep_var, mod){
  mod_form <- mod_covars[[mod]] %>%
    glue_collapse(" + ") %>%
    paste(dep_var, "~", .) %>%
    as.formula()
  
  map_dfr(1:3/4, 
          ~ rq(mod_form, .x, df) %>%
            tidy())
}

res_qreg <- mod_specs %>%
  filter(!str_detect(mod, "^all"),
         mod %in% c("sex", "father_class_binary", "exercise_binary")) %>%
  mutate(res = map2(dep_var, mod, get_qreg)) %>%
  unnest(res)


# 3. GAMLSS Models BCCG ----
get_bccg <- function(dep_var, mod){
  mod_form <- mod_covars[[mod]] %>%
    glue_collapse(" + ") %>%
    paste("~", .)
  
  df_mod <<- paste(dep_var, mod_form) %>%
    as.formula() %>%
    model.frame(df)
  
  mod <- gamlss(paste(dep_var, mod_form) %>% as.formula(),
                sigma.formula = as.formula(mod_form),
                nu.formula = as.formula(mod_form),
                family = BCCG(mu.link = log), 
                data = df_mod, trace = FALSE)
  
  
  coefs <- coefAll(mod)
  mod_tidy <- tidy(mod, conf.int = TRUE) %>%
    mutate(.rownames = coefs %>% 
             map(names) %>% 
             flatten() %>%
             as.character()) %>%
    rename(term = .rownames)
  
  tibble(tidy = list(mod_tidy),
         coefs = list(coefAll(mod)))
}

res_bccg <- mod_specs %>%
  filter(!str_detect(dep_var, "_ln$")) %>%
  mutate(res = map2(dep_var, mod, get_bccg)) %>%
  unnest(res)


# 4. GAMLSS Models NO ----
get_no <- function(dep_var, mod){
  mod_form <- mod_covars[[mod]] %>%
    glue_collapse(" + ") %>%
    paste("~", .)
  
  df_mod <<- paste(dep_var, mod_form) %>%
    as.formula() %>%
    model.frame(df)
  
  mod <- gamlss(paste(dep_var, mod_form) %>% as.formula(),
                sigma.formula = as.formula(mod_form),
                family = NO(mu.link = log), 
                data = df_mod, trace = FALSE)
  
  coefs <- coefAll(mod)
  mod_tidy <- tidy(mod, conf.int = TRUE) %>%
    mutate(.rownames = coefs %>% 
             map(names) %>% 
             flatten() %>%
             as.character()) %>%
    rename(term = .rownames)
  
  tibble(tidy = list(mod_tidy),
         coefs = list(coefAll(mod)))
}

res_no <- mod_specs %>%
  filter(!str_detect(dep_var, "_ln$")) %>%
  mutate(res = map2(dep_var, mod, get_no)) %>%
  unnest(res)

rm(df_mod)


# 5. Save Objects ----
save(res_glm, res_qreg, res_bccg, res_no,
     file = "Data/reg_results.Rdata")

