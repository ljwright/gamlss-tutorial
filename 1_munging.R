library(tidyverse)
library(haven)
library(glue)
library(magrittr)
library(labelled)

rm(list = ls())

# 1. Load Data ---
raw_dir <- "D:/BCS70"

get_bcs <- function(dir, file, vars){
  glue("{raw_dir}/{dir}/{file}") %>%
    read_dta(col_select = any_of(c("BCSID", "bcsid", vars))) %>%
    rename_with(~ str_replace(.x, "BCSID", "bcsid"))
}

df_raw <- tribble(
  ~dir, ~file, ~vars,
  "0y",	"bcs1derived.dta",	"BD1CNTRY",
  "0y",	"bcs7072a.dta",	"a0014",
  "10y",	"sn3723.dta",	"c3_4",
  "42y",	"bcs70_2012_derived.dta",	"BD9WEMWB",
  "42y",	"bcs70_2012_flatfile.dta",	"B9EXERSE",
  "46y",	"bcs_age46_main.dta",	"B10CMSEX",
  "46y",	"bcs_age46_main.dta",	"B10HEIGHTCM",
  "46y",	"bcs_age46_main.dta",	"B10MWEIGHT",
  "xwave",	"bcs70_response_1970-2016.dta",	"SEX",
  "xwave",	"bcs70_response_1970-2016.dta",	"MULTIPNO",
  "xwave",	"bcs70_response_1970-2016.dta",	"OUTCME01"
) %>%
  chop(vars) %$%
  pmap(list(dir, file, vars), get_bcs) %>%
  reduce(~ full_join(.x, .y, by = "bcsid"))


# 2. Clean Data ----
class_dict <- c("I Professional", "II Intermediate", "III Skilled Non-Manual",
                "III Skilled Manual", "IV Semi-Skilled", "V Unskilled")

df <- df_raw %>%
  filter(  # CHECK CORRECT TO NOT DROP NA
    MULTIPNO == -1, # Singleton births
    BD1CNTRY != 4 | is.na(BD1CNTRY), # Not Northern Ireland
    OUTCME01 != 6 | is.na(OUTCME01) # Not Not Issued
  ) %>%
  mutate(
    sex = case_when(SEX == 1 ~ "Male",
                    SEX == 2 ~ "Female",
                    B10CMSEX == 1 ~ "Male",
                    B10CMSEX == 2 ~ "Female") %>%
      factor(),
    
    father_class = case_when(between(as.double(a0014), 1, 6) ~ a0014,
                             between(as.double(c3_4), 1, 6) ~ c3_4) %>%
      factor(labels = class_dict),
    
    father_class_binary = ifelse(as.integer(father_class) <= 3, "Non-Manual", "Manual") %>%
      fct_rev(),
    
    exercise = case_when(between(as.double(B9EXERSE), 0, 6) ~ B9EXERSE,
                         B9EXERSE == 7 ~ 6) %>%
      factor(labels = c(glue("{0:5} days"), "6-7 days")),
    
    exercise_binary = ifelse(as.integer(exercise) == 1, "0 days", "1+ days") %>%
      fct_rev(),
    
    weight_46 = ifelse(B10MWEIGHT > 0, B10MWEIGHT, NA),
    height_46 = ifelse(B10HEIGHTCM > 0, B10HEIGHTCM, NA),
    bmi_46 = weight_46/(height_46/100)^2,
    bmi_46 = ifelse(bmi_46 <= 70, bmi_46, NA),
    
    wemwbs_42 =  ifelse(BD9WEMWB > 0, BD9WEMWB, NA),
    
    miss_covars = if_any(c(sex, father_class, exercise), is.na)
  ) %>%
  select(bcsid, sex, bmi_46, wemwbs_42,
         father_class, father_class_binary,
         exercise, exercise_binary, miss_covars) %>%
  set_variable_labels(bcsid = "ID",
                      sex = "Sex",
                      bmi_46 = "BMI, 46y",
                      wemwbs_42 = "Mental Wellbeing, 42y",
                      father_class = "Father's Social Class (6-level)",
                      father_class_binary = "Father's Social Class (Manual vs Non-Manual)",
                      exercise = "Exercise Days per Week, 42y",
                      exercise_binary = "Any Exercise, 42y",
                      miss_covars = "Missing Covariates") %>%
  zap_formats()

save(df, file = "Data/df_analysis.Rdata")
write_dta(df, path = "Data/df_analysis.dta")

df_bcs <- df %>%
  sample_frac() %>%
  mutate(id = row_number()) %>%
  select(id, bmi_46, sex) 
save(df_bcs, file = "Tutorial/df_tutorial.Rdata")
