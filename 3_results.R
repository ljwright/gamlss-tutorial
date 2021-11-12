library(tidyverse)
library(glue)
library(summarytools)
library(gallimaufr) # devtools::install_github("ljwright/gallimaufr")
library(flextable)
library(officer)
library(ggpp)

rm(list = ls())

# 1. Load Data ----
load("Data/df_analysis.Rdata")
load("Data/reg_results.Rdata")

pretty_lbls <- c(bmi_46 = "BMI @ Age 46", wemwbs_42 = "WEMWBS @ Age 42",
                 sex = "Sex", father_class_binary = "Father's Class", 
                 father_class = "Father Class", exercise_binary = "Exercise Level",
                 exercise = "Exercise") %>%
  make_lbls(df)

main_ind <- c("sex", "father_class_binary", "exercise_binary")


# 2. Descriptive Statistics ----
df_desc <- df %>%
  filter(miss_covars == FALSE) %>%
  pivot_longer(c(bmi_46, wemwbs_42), 
               names_to = "dep_var", 
               values_to = "dep_value") %>%
  drop_na(dep_value) %>%
  pivot_longer(sex:exercise_binary, 
               names_to = "ind_var", 
               values_to = "ind_value") %>%
  select(-bcsid, -miss_covars)


# Summary Statistics
desc_tbl <- df_desc %>%
  group_by(dep_var, ind_var, ind_value) %>%
  descr(dep_value) %>%
  tb() %>%
  select(dep_var, ind_var, ind_value, 
         mean, sd, median = med, 
         cov = cv, skewness)

desc_tbl <- df_desc %>%
  count(dep_var, ind_var, ind_value) %>%
  add_count(dep_var, ind_var, wt = n, name = "total") %>%
  mutate(prop = round(n*100/total, 1),
         prop = glue("{prop}%")) %>%
  select(-n, -total) %>%
  left_join(desc_tbl, by = c("dep_var", "ind_var", "ind_value"))


# Kernel Density Plots
format_kernel <- function(df){
  df %>%
    filter(ind_var %in% main_ind) %>%
    left_join(pretty_lbls %>%
                select(ind_var = var, ind_clean = var_clean) %>%
                distinct() %>%
                mutate(index = row_number()),
              by = "ind_var") %>%
    mutate(ind_clean = fct_reorder(ind_clean, index)) %>%
    left_join(pretty_lbls %>%
                select(dep_var = var, dep_clean = var_clean) %>%
                distinct(),
              by = "dep_var") %>%
    select(-index)
}

kernel_tbl <- format_kernel(desc_tbl) %>%
  select(dep_clean, ind_clean, ind_value,
         Mean = mean, SD = sd, CoV = cov) %>%
  pivot_longer(Mean:CoV, names_to = "stat") %>%
  mutate(value = round(value, 1)) %>%
  nest(tbl = -c(dep_clean, ind_clean)) %>%
  mutate(tbl = map(tbl,
                   ~ .x %>%
                     pivot_wider(names_from = ind_value,
                                 values_from = value) %>%
                     rename(` ` = stat)))

format_kernel(df_desc) %>%
  ggplot() +
  aes(x = dep_value, color = ind_value, fill = ind_value) +
  facet_grid(ind_clean ~ dep_clean, scales = "free_x", switch = "y") +
  geom_density(alpha = 0.2) +
  geom_table(data = kernel_tbl, aes(x = Inf, y = Inf, label = tbl),
             table.theme = ttheme_gtminimal, hjust = 1.1, vjust = 1.1) +
  scale_color_brewer(palette = "Dark2") +
  scale_fill_brewer(palette = "Dark2") +
  theme_bw() +
  theme(strip.placement = "outside",
        strip.text.y.left = element_text(angle = 0),
        strip.background = element_blank(),
        legend.position = "bottom") +
  labs(x = NULL, y = NULL, color = NULL, fill = NULL)
ggsave("Images/kernel_density.png", 
       width = 29.7, height = 21, units = "cm")


# 3. Regression Figures ----
clean_pred <- function(df_pred){
  df_pred %>%
    mutate(ind_value = map2_chr(mod, level,  ~ levels(df[[.x]])[.y]) %>%
             factor(levels = levels(df_desc$ind_value))) %>%
    rename(ind_var = mod) %>%
    format_kernel() %>%
    mutate(type = type_fac[1])
}

clean_margin <- function(df_pred){
  df_pred %>%
    arrange(dep_var, ind_var, level, centile) %>%
    group_by(dep_var, ind_var, centile) %>%
    mutate(bmi = bmi - lag(bmi)) %>%
    ungroup() %>%
    drop_na() %>%
    mutate(type = type_fac[2]) %>%
    select(dep_var, dep_clean, ind_clean, type, centile, bmi)
}


# Format GAMLSS Results
get_pred <- function(coefs){
  bind_rows(as_tibble(map(coefs, 1)),
            as_tibble(map(coefs, sum)),
            .id = "level") %>%
    uncount(99, .id = "centile") %>%
    mutate(level = as.integer(level),
           tau = centile/100,
           bmi = exp(mu) * (1 + nu * exp(sigma) * qnorm(tau))^(1/nu)) %>%
    select(level, centile, tau, bmi)
}
type_fac <- fct_inorder(c("Predicted BMI", "Marginal Effect"))

gamlss_pred <- res_bccg %>%
  filter(mod %in% main_ind) %>%
  mutate(pred = map(coefs, get_pred)) %>%
  select(dep_var, mod, pred) %>%
  unnest(pred) %>%
  clean_pred()

gamlss_margin <- clean_margin(gamlss_pred)


# Format Quantile Results
qreg_pred <- res_qreg %>%
  filter(mod %in% main_ind,
         dep_var %in% c("bmi_46", "wemwbs_42")) %>%
  group_by(dep_var, mod, tau) %>%
  summarise(level_1 = nth(estimate, 1),
            level_2 = sum(estimate),
            .groups = "drop") %>%
  mutate(centile = tau * 100) %>%
  pivot_longer(cols = c(level_1, level_2), names_to = "level", values_to = "bmi") %>%
  mutate(level = str_sub(level, -1) %>% as.integer()) %>%
  clean_pred()

qreg_margin <- clean_margin(qreg_pred)


# Format GLM Results
glm_margin <- res_glm %>%
  filter(mod %in% main_ind,
         link == "identity",
         term != "(Intercept)") %>%
  rename(ind_var = mod) %>%
  format_kernel() %>%
  mutate(type = type_fac[2])


# Plot Results
plot_gamlss <- function(dep_var, save_plot = FALSE){
  p <- ggplot() +
    aes(x = centile, y = bmi) +
    facet_grid(type ~ ind_clean, scales = "free_y", switch = "y") +
    geom_line(data = filter(gamlss_pred, dep_var == !!dep_var),
              aes(color = ind_value)) +
    geom_line(data = filter(gamlss_margin, dep_var == !!dep_var)) +
    geom_point(data = filter(qreg_pred, dep_var == !!dep_var),
               aes(color = ind_value)) +
    geom_point(data = filter(qreg_margin, dep_var == !!dep_var)) +
    geom_hline(data = tibble(type = type_fac[2]), aes(yintercept = 0),
               linetype = "dashed") +
    geom_hline(data = filter(glm_margin, dep_var == !!dep_var),
               aes(yintercept = estimate), color = "red") +
    scale_color_brewer(palette = "Dark2") +
    theme_bw() +
    theme(strip.placement = "outside",
          strip.text.y.left = element_text(angle = 0),
          strip.background = element_blank(),
          legend.position = "bottom") +
    labs(x = NULL, y = NULL, color = NULL)
  
  if (save_plot){
    glue("Images/gamlss_{dep_var}.png") %>%
      ggsave(plot = p, width = 29.7, height = 21, units = "cm")
  }
  
  return(p)
}

plot_gamlss("bmi_46", TRUE)
plot_gamlss("wemwbs_42", TRUE)


# 4. Regression Tables ----
# GAMLSS and GLM - Main Independent Variables
clean_tidy <- function(df_res){
  df_res %>%
    filter(mod %in% c(main_ind, "all_binary")) %>%
    select(-coefs) %>%
    unnest(tidy) %>%
    filter(term != "(Intercept)") %>%
    mutate(mod = ifelse(mod == "all_binary", 
                        "\tAdjusted difference, % (SE)",
                        "\tUnadjusted difference, % (SE)"),
           across(c(estimate, std.error),
                  ~ ifelse(parameter == "nu", round(.x, 2), round(100 * .x, 1))),
           string = glue("{estimate} ({std.error})"),
           parameter = glue("{parameter}")) %>%
    select(dep_var, ind_value = mod, parameter, term, string) %>%
    pivot_wider(names_from = parameter, values_from = string) %>%
    left_join(pretty_lbls %>%
                select(cat, ind_var = var),
              by = c(term = "cat")) %>%
    select(-term)
}

res_tbl <- left_join(
  clean_tidy(res_bccg) %>%
    rename(median = mu, cov = sigma, skewness = nu),
  clean_tidy(res_no) %>%
    rename(mean = mu, sd = sigma),
  by = c("dep_var", "ind_var", "ind_value")
)

res_tbl <- desc_tbl %>%
  filter(ind_var %in% main_ind) %>%
  mutate(ind_value = as.character(ind_value),
         across(mean:median, round, 1),
         across(cov:skewness, round, 2),
         across(mean:skewness, as.character)) %>%
  bind_rows(res_tbl) %>%
  left_join(pretty_lbls %>% 
              select(var, desc_cat, ind_clean = var_clean, index, level),
            by = c(ind_var = "var", ind_value = "desc_cat")) %>%
  group_by(ind_var) %>%
  mutate(index = min(index, na.rm = TRUE),
         level = case_when(str_detect(ind_value, "Unadjusted") ~ 3L,
                           str_detect(ind_value, "Adjusted") ~ 4L,
                           TRUE ~ level),
         ind_clean = first(ind_clean),
         ind_value = ifelse(level == 1L, glue("{ind_value} (Ref.)"), ind_value)) %>%
  ungroup() %>%
  arrange(dep_var, index, level) %>%
  select(dep_var, ind_clean, ind_value:skewness)

# Make Flextable
tbl_header <- list(ind_clean = "", ind_value = "Risk Factor",
                   prop = "%", mean = "Mean", sd = "SD",
                   median = "Median", cov = "CoV", skewness = "Skewness")

span_header <- list(ind_clean = "", ind_value = "", prop = "",
                    mean = "NO Distribution Family", sd = "NO Distribution Family",
                    median = "BCCG Distribution Family", 
                    cov = "BCCG Distribution Family",
                    skewness = "BCCG Distribution Family")

get_flx <- function(dep_var){
  res_tbl %>%
    filter(dep_var == !!dep_var) %>%
    select(-dep_var) %>%
    make_flx(tbl_header, span_header)
}

flx_bmi <- get_flx("bmi_46")
flx_bmi
save_as_docx(flx_bmi, path = "Tables/res_bmi_46.docx")

flx_wemwbs <- get_flx("wemwbs_42")
flx_wemwbs
save_as_docx(flx_wemwbs, path = "Tables/res_wemwbs_42.docx")

# Non-Binary Variables
clean_cat <- function(df_res){
  df_res %>%
    filter(mod %in% c("father_class", "exercise")) %>%
    select(-coefs) %>%
    unnest(tidy) %>%
    mutate(across(c(estimate, std.error),
                  ~ ifelse(parameter == "nu", 
                           round(.x, 2),
                           round(100 * .x, 1))),
           string = ifelse(term == "(Intercept)",
                           "Ref",
                           glue("{estimate} ({std.error})")),
           term = str_replace(term, mod, ""),
           term = map2_chr(term, mod, ~ ifelse(.x == "(Intercept)", levels(df[[.y]])[1], .x))) %>%
    select(dep_var, mod, parameter, term, string) %>%
    pivot_wider(names_from = parameter, values_from = string)
}

clean_lbls <- pretty_lbls %>%
  select(1:2) %>%
  distinct() %>%
  deframe()

cat_tbl <- left_join(
  clean_cat(res_no) %>%
    rename(mean = mu, sd = sigma),
  clean_cat(res_bccg) %>%
    rename(median = mu, cov = sigma, skewness = nu),
  by = c("dep_var", "mod", "term")
) %>%
  left_join(desc_tbl %>%
              select(dep_var:prop) %>%
              mutate(ind_value = as.character(ind_value)),
            by = c("dep_var", mod = "ind_var", term = "ind_value")) %>%
  mutate(dep_clean = factor(clean_lbls[dep_var], clean_lbls),
         ind_clean = factor(clean_lbls[mod], clean_lbls)) %>%
  rename(ind_value = term) %>%
  select(all_of(names(res_tbl)))

cat_flx <- function(dep_var){
  cat_tbl %>%
    filter(dep_var == !!dep_var) %>%
    select(-dep_var) %>%
    make_flx(tbl_header, span_header)
}

cat_bmi <- cat_flx("bmi_46")
cat_bmi
save_as_docx(cat_bmi, path = "Tables/cat_bmi_46.docx")

cat_wemwbs <- cat_flx("wemwbs_42")
cat_wemwbs
save_as_docx(cat_wemwbs, path = "Tables/cat_wemwbs_42.docx")


# Quantile Regression
qreg_tbl <- res_qreg %>%
  filter(str_detect(dep_var, "_ln"),
         term != "(Intercept)") %>%
  mutate(across(c(estimate, std.error), ~ round(100 * .x, 1)),
         centile = glue("{tau * 100}th centile"),
         string = glue("{estimate} ({std.error})"),
         dep_var = ifelse(dep_var == "bmi_ln", "bmi_46", "wemwbs_42"),
         dep_clean = factor(clean_lbls[dep_var], clean_lbls),
         ind_clean = factor(clean_lbls[mod], clean_lbls)) %>%
  select(Outcome = dep_clean, `Risk Factor` = ind_clean, centile, string) %>%
  pivot_wider(names_from = centile, values_from = string)
  
qreg_flx <- make_flx(qreg_tbl)
qreg_flx
save_as_docx(qreg_flx, path = "Tables/qreg_results.docx")


# 5. Simulated Distributions ----
library(fGarch)
set.seed(1)
p_vals <- runif(1e6)

type_dict <- c(location = "Difference in Mean",
               variance = "Difference in Mean and SD",
               quantile = "Difference in Mean and Skewness")

df_sim <- bind_rows(location = tibble(Control = qnorm(p_vals),
                                      Treatment = qnorm(p_vals, 1.5)),
                    variance = tibble(Control = qnorm(p_vals),
                                      Treatment = qnorm(p_vals, 1.5, 2.3)),
                    quantile = tibble(Control = qnorm(p_vals),
                                      Treatment = rsnorm(1e6, 1.5, xi =  2)),
                    .id = "type") %>%
  pivot_longer(-type, names_to = "x", values_to = "y") %>%
  mutate(type_clean = factor(type_dict[type], type_dict))

ggplot(df_sim) +
  aes(x = y, color = x, fill = x) +
  facet_grid(type_clean ~ ., switch = "y", space = "free_y") +
  geom_density(alpha = 0.4) +
  geom_vline(xintercept = c(0, 1.5), linetype = "dashed") +
  scale_color_brewer(palette = "Dark2") +
  scale_fill_brewer(palette = "Dark2") +
  theme_minimal() +
  theme(legend.position = "bottom",
        strip.placement = "outside",
        strip.text.y.left = element_text(angle = 0)) +
  labs(x = NULL, y = NULL, color = NULL, fill = NULL) +
  coord_cartesian(c(-6, 6))

ggsave("Images/simulations.png",
       width = 21, height = 16, units = "cm", dpi = 300)
