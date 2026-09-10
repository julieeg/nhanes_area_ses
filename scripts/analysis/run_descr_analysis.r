# Run descriptive analyses

################################################################################
## Set Up; Load Required Packages & Data Files
################################################################################

## set local directory
setwd('~/Documents/GitHub/nhanes_area_ses/run')

## load required base pacakges
list_of_packages <- c(
  "tidyverse", "data.table", "nhanesA", "progress", "sociome", "jsonlite", "haven", "forcats"
) ; invisible(lapply(list_of_packages, function(pkg) {
  if(!requireNamespace(pkg, quietly = TRUE)) { 
    install.packages(pkg) } ; library(pkg, character.only = TRUE)
}))


## load pre-built pantry functions (for data wrangling)
get_github_scripts <-function(user, repo, path) {
  api <- sprintf("https://api.github.com/repos/%s/%s/contents/%s", user, repo, path)
  scripts <- grep("*.R", jsonlite::fromJSON(api)$name, value = T)
  URLs <- lapply(scripts, function(f) sprintf("https://raw.githubusercontent.com/%s/%s/main/%s/%s", user, repo, path, f))
  invisible(lapply(URLs, source))
} ; get_github_scripts("julieeg", "pantry", "functions")

## Load nhanes_area_ses scripts
get_github_scripts("julieeg", "nhanes_area_ses", "scripts/analysis/build_nhanes_survdesign.r")

## Load nhanes data & prevent variables
nhanes_dat <- readRDS("../data/processed/nhanes_postprocessed_linked_ndi_prvnt_sdi.rds")


################################################################################
## Descriptive (Table 1) summaries by Age, Sex and Race/Ethnicity 
################################################################################

strata_vars <- c("gender", "age_4lvl", "racethn_combn")

# ==============================================
## Descriptive characteristics 
# ==============================================

vars_to_descr <- c(age="Age, years", bmi="BMI, kg/m2", smoke_status = "Smoking status",
                   alch_freq_wk = "Alcohol frequency", pa_level_mets = "Physical activity, MET/wk", 
                   educ_level = "Education level", inc_to_pov_level = "PIR Level", 
                   employ_status = "Employment Status", diabetes = "Diabetes", 
                   sbp_mean="SBP (average), mmHg", dbp_mean="DBP (average), mmHg", 
                   tc="Total cholesterol, mg/dL", hdl="HDL-c, mg/dL", ldl="LDL-c, mg/dL",
                   tg="Triglyeride, mg/dL", hba1c="HbA1c (%)", uacr="UACR", egfr="eGFR")


descr_sumtab_bystrata.l <- lapply(c("Years", strata_vars), function(stratavar) {
  build_nhanes_summarytable.fun(vars_to_descr, strata=stratavar) 
}) ; names(descr_sumtab_bystrata.l) <- c("cycle", "sex", "age", "race")

descr_sumtab_bystrata.l %>% 
  saveRDS("../data/output/nhanes_sumtab_descr_bystrata.rds")


# ==============================================
## PREVENT input variables
# ==============================================

## INPUT variables -------------
prevent_input_vars <- c(prevent_age = "Age, 30-80 years", prevent_sex = "Female/Male",
                        prevent_sbp = "SBP, 90-180 mmHg", prevent_bprx = "Use BP medications",
                        prevent_tc = "Total cholesterol, 130-320 mmHg", prevent_hdl = "HDL-c, 20-100, mg/dL",
                        prevent_statin = "Use Statins", prevent_diab = "T2D diagnosis",
                        prevent_smoking = "Current smoker", prevent_egfr = "eGFR, 15-40",
                        prevent_bmi = "BMI, 18.5-39.9 kg/m2", prevent_hba1c = "hbA1c, 4.5-15 %",
                        prevent_uacr = "UACR, 0.1-25000"
)

prvnt_sumtab_bystrata.l <- lapply(c("Years", strata_vars), function(stratavar) {
  
  if(strata=="age_4lvl") { dat_use <- nhanes_dat %>% filter(age_4lvl != "under30y") 
  } else { dat_use <- nhanes_dat } ; build_nhanes_summarytable.fun(
    prevent_input_vars, strata=stratavar, data=dat_use)
}) ; names(prvnt_sumtab_bystrata.l) <- c("cycle", "sex", "age", "race")

prvnt_sumtab_bystrata.l %>% 
  saveRDS("../data/output/nhanes_sumtab_prevent_input_bystrata.rds")



## EOF


