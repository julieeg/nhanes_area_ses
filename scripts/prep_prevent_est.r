## Rscript to prepare PREVENT predictions

################################################################################
## Set Up & Load Required Packages 
################################################################################

## set local directory
#setwd('C:/Users/sarac/OneDrive/Documents/Research')
setwd('~/Documents/GitHub/nhanes_area_ses/run')

## load required base pacakges
list_of_packages <- c(
  "tidyverse", "data.table", "nhanesA", "progress", "sociome", "jsonlite", "haven", "forcats"
) ; invisible(lapply(list_of_packages, function(pkg) {
  if(!requireNamespace(pkg, quietly = TRUE)) { 
    install.packages(pkg) } ; library(pkg, character.only = TRUE)
}))


## load PREVENTR
library(preventr)

## Load processed NHANES data
nhanes_processed <- readRDS("../data/processed/nhanes_processed.rda")


################################################################################
## Build prevent outcomes 
################################################################################

## Add prevent-specific age and sbp values based on max/min
nhanes_processed <- nhanes_processed %>% 
  mutate(
    prevent_age = ifelse(age>30 & age <80, age, NA),
    prevent_sex = ifelse(female == 1, "female", "male"),
    prevent_sbp = ifelse(sbp_mean>90 & sbp_mean<180, sbp_mean, NA),
    prevent_bprx = ifelse(rx_use_bpmed=="Yes",1,0),
    prevent_tc = ifelse(tc>130 & tc<320, tc, NA),
    prevent_hdl = ifelse(hdl>20 & hdl<100, hdl, NA),
    prevent_statin = ifelse(rx_use_statin=="Yes",1,0),
    prevent_diab = diabetes,
    prevent_smoking = ifelse(smoke_current == "Current smoker", 1, 0),
    prevent_egfr = ifelse(egfr >15 & egfr <140, egfr, NA),
    prevent_egfr_race = ifelse(egfr_race >15 & egfr_race <140, egfr_race, NA),
    prevent_bmi = ifelse(bmi>=18.5 & bmi<=39.9, bmi, NA),
    prevent_hba1c = ifelse(hba1c>=4.5 & hba1c <=15, hba1c, NA),
    prevent_uacr = ifelse(uacr >= 0.1 & uacr <= 25000, uacr, NA)) %>%
  mutate(prevent_complete_base = ifelse(
    !is.na(prevent_age) & !is.na(prevent_sex) & !is.na(prevent_sbp) & 
      !is.na(prevent_bprx) & !is.na(prevent_tc) & !is.na(prevent_hdl) & 
      !is.na(prevent_statin) & !is.na(prevent_diab) & !is.na(prevent_smoking) & 
      !is.na(prevent_egfr) & !is.na(prevent_bmi), 1, 0)) %>%
  mutate(prevent_complete_baseA1c = ifelse(prevent_complete_base == 1 & !is.na(prevent_hba1c), 1, 0),
         prevent_complete_baseUACR = ifelse(prevent_complete_base == 1 & !is.na(prevent_uacr), 1, 0))


## Write wrapper function to calculate PREVENT risk estimates, for EACH model
prevent_models <- c(base="Base", hba1c="Base+HbA1c", uacr="Base+UACR")

calculate_prevent_riskest.fun <- function(prevent_model, 
                                          egfr_input = "prevent_egfr", # Specify eGFR, calculated with/without Race
                                          data = nhanes_processed) {
  
  # Create subset of complete data, based on PREVENT model -------------
  if(prevent_model == "base") { 
    data_complete <- data %>% filter(prevent_complete_base==1) }
  if (prevent_model == "hba1c") {
    data_complete <- data %>% filter(prevent_complete_baseA1c == 1) }
  if(prevent_model == "uacr") {
    data_complete <- data %>% filter(prevent_complete_baseUACR == 1)
  }
  
  # List of prevent outcomes -------------
  prevent_outcomes <- c("total_cvd", "ascvd", "heart_failure", "chd", "stroke")
  
  # Calculate each risk estimate, at 10 & 30 years, in each model ---------- 
  prevent.dat <- estimate_risk(
    use_dat = data_complete %>% 
      select(SEQN, Years, starts_with("prevent"), racethn_addNHA, 
             prevent_egfr_use = egfr_input),
    age=prevent_age, sex=prevent_sex, sbp=prevent_sbp, bp_tx = prevent_bprx,
    total_c = prevent_tc, hdl_c = prevent_hdl, statin = prevent_statin, 
    dm = prevent_diab, smoking = prevent_smoking, bmi = prevent_bmi, 
    egfr = prevent_egfr_use, hba1c = prevent_hba1c, uacr = prevent_uacr, 
    #zip = input_zip, 
    time = "both", model = prevent_model
    )
  
  # Compile & reshape results into wide-format, for nhanes_proc merging
  prevent.res <- prevent.dat %>%
    select(SEQN, prevent_outcomes, over_years) %>%
    pivot_wider(values_from = prevent_outcomes, names_from=over_years) %>%
    rename_at(paste0(rep(prevent_outcomes, each=2), "_", c(10,30)), 
              ~paste0("prevent_", ., "yr_", prevent_model)) %>%
    rename_with(., ~gsub("total_cvd", "cvd", gsub("heart_failure", "hf", .)))
  
  return(list(results = prevent.res, 
              input_errors = prevent.dat %>% 
                select(SEQN, model, over_years, input_problems) %>% 
                filter(!is.na(input_problems)) )
         )
}

prevent_results.l <- lapply(names(prevent_models), function(mod) {
  message("Calculating PREVENT risk estimates in the ", prevent_models[[mod]], " Model")
  
  calculate_prevent_riskest.fun(
    data = nhanes_processed,
    prevent_model = mod, egfr_input = "prevent_egfr")
  }) ; names(prevent_results.l) <- names(prevent_models)

prevent_results_only.l <- lapply(prevent_results.l, function(est) est$results)
prevent_results_seqn <- reduce(prevent_results_only.l, full_join, by = "SEQN")

## Save prevent estimates
fwrite(prevent_results_seqn, file="../data/processed/nhanes_prevent_estimates.csv")
prevent_results_seqn <- fread("../data/processed/nhanes_prevent_estimates.csv")


## =====================================================
## Create analytical dataframes 
## =====================================================

base_vars <- names(nhanes_processed %>% select(
  "Years", "SEQN", "RIDSTATR", starts_with("SD"), starts_with("WT"), -"wt")
)

# PREVENT variables ------------------------
nhanes_processed_prevent <- prevent_results_seqn %>% 
  left_join(
    nhanes_processed %>% select(
      base_vars, age,  age_gt65, female, racethn, racethn_addNHA, 
      diabetes, ldl, sbp_mean, dbp_mean, 
      cvd, ascvd, ckd, copd, htn, diabetes_undx, starts_with("prevent_")),
    by="SEQN") %>%
  ## Participant exclusions: Age >30 and <79 years
  filter(!is.na(prevent_age)) %>% 
  ## Participant sub-sets based on outcomes: 
  # For Dyslipidemia: a) 30-39 & no ASCVD | 40-79 no ASCVD or diabetes
  # b) no statin use; c) LDLc between 70-189 mg/dL
  mutate(
    prevent_include_dyslip = case_when(
      c(age<39 & ascvd == 0 | age>=39 & ascvd==0 & diabetes == 0) & 
        prevent_statin == 0 & ldl >70 & ldl <189 ~ 1,
      TRUE ~ 0),
    prevent_include_bp = case_when(
      ascvd==0 & diabetes == 0 & diabetes_undx == 0 & ckd == 0 &
        prevent_bprx != 1 & sbp_mean <139 & dbp_mean <89 ~ 1,
      TRUE ~ 0)
  )

# Save PREVENT df with relevant variables and binary variables for subsetting
nhanes_processed_prevent %>% saveRDS("../data/processed/nhanes_processed_prevent.rda")

## EOF
## Last Updated: 08-17-2026




