## Rscript to generate descriptive statitics & regression outputs in NHANES 
## using custom nhanes_survdesign functions 


################################################################################
## Set Up; Load Required Packages & Data Files
################################################################################

## set local directory
setwd('~/Documents/GitHub/nhanes_area_ses/run')

## Load nhanes data & prevent variables
nhanes_dat <- readRDS("../data/processed/nhanes_postprocessed_linked_ndi_prvnt.rds")

## Run dependent build_nhanes_survdesign.r script 
source("../scripts/build_nhanes_survdesign.r")


################################################################################
## Descriptive (Table 1) summaries by Age, Sex and Race/Ethnicity 
################################################################################

# ==============================================
## Descriptive participant characteristics 
# ==============================================

vars_to_descr <- c(age="Age, years", bmi="BMI, kg/m2", smoke_status = "Smoking status",
                   alch_freq_wk = "Alcohol frequency", pa_level_mets = "Physical activity, MET/wk", 
                   educ_level = "Education level", inc_to_pov_level = "PIR Level", 
                   employ_status = "Employment Status", diabetes = "Diabetes", 
                   sbp_mean="SBP (average), mmHg", dbp_mean="DBP (average), mmHg", 
                   tc="Total cholesterol, mg/dL", hdl="HDL-c, mg/dL", ldl="LDL-c, mg/dL",
                   tg="Triglyeride, mg/dL", hba1c="HbA1c (%)", uacr="UACR", egfr="eGFR")

descr_sumtab_bycycle <- build_nhanes_summarytable.fun(vars_to_descr, strata="Years")
descr_sumtab_bysex <- build_nhanes_summarytable.fun(vars_to_descr, strata="gender")
descr_sumtab_byage <- build_nhanes_summarytable.fun(vars_to_descr, strata="age_4lvl")
descr_sumtab_byracethn <- build_nhanes_summarytable.fun(vars_to_descr, strata="racethn_combn")

nhanes_sumtab_strata.l <- list(
  cycle = descr_sumtab_bycycle,
  age = descr_sumtab_byage, 
  sex = descr_sumtab_bysex, 
  race = descr_sumtab_byracethn
) %>% saveRDS("../data/output/nhanes_sumtab_descr_bystrata.rda")



# ==============================================
## PREVENT variables
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

preventin_sumtab_bycycle <- build_nhanes_summarytable.fun(prevent_input_vars, strata="Years", data=nhanes_dat)
preventin_sumtab_bysex <- build_nhanes_summarytable.fun(prevent_input_vars, strata="gender")
preventin_sumtab_byage <- build_nhanes_summarytable.fun(prevent_input_vars, strata="age_4lvl", 
                                                        data = nhanes_dat %>% filter(age_4lvl != "under30y"))
preventin_sumtab_byracethn <- build_nhanes_summarytable.fun(prevent_input_vars, strata="racethn_combn")

preventin_sumtab_strata.l <- list(
  cycle = preventin_sumtab_bycycle,
  sex = preventin_sumtab_bysex, 
  age = preventin_sumtab_byage, 
  race = preventin_sumtab_byracethn
) ; saveRDS(preventin_sumtab_strata.l, "../data/output/nhanes_sumtab_prevent_input_bystrata.rda")


## Risk Estimates -------------
prevent_est_vars <- c(
  nhanes_dat %>% select(starts_with("prevent") & contains("yr_")) %>% names()
)  

preventest_sumtab_bycycle <- build_nhanes_summarytable.fun(prevent_est_vars, strata="Years", data=nhanes_dat)
preventest_sumtab_bysex <- build_nhanes_summarytable.fun(prevent_est_vars, strata="gender")
preventest_sumtab_byage <- build_nhanes_summarytable.fun(prevent_est_vars, strata="age_4lvl", 
                                                        data = nhanes_dat %>% filter(age_4lvl != "under30y"))
preventest_sumtab_byracethn <- build_nhanes_summarytable.fun(prevent_est_vars, strata="racethn_combn")

preventest_sumtab_strata.l <- list(
  cycle = preventest_sumtab_bycycle,
  sex = preventest_sumtab_bysex, 
  age = preventest_sumtab_byage, 
  race = preventest_sumtab_byracethn
) ; saveRDS(preventest_sumtab_strata.l, "../data/output/nhanes_sumtab_prevent_riskest_bystrata.rda")


################################################################################
## Run GLMs over lists of SES exposures x composite list of outcomes 
################################################################################

exposures <- c("educ_level", "inc_to_pov", "inc_to_pov_level") # , "employ_status"
outcomes_set1_tastediet <- c(
  # Exam values ------------
  "bmi", "waist", "whr", # "wt", "sbp_mean", "dbp_mean", 
  # Taste/Smell outcomes ------------
  "taste_mouth_quinine_glms", "taste_mouth_nacl_1M_glms", "taste_mouth_nacl_320mM_glms",
  "smell_pst_total", "smell_dysfun",
  # Diet quality ------------
  "hei2015_total", "ahei_total", "dii_total", "restaur_freq_wk",
  # Disease outcomes
  "diabetes", "cvd", "mdd", "obese", "obesity_abd"
  )
  
  #paste0("q_tastechange_",c("sweet", "salt", "sour", "bitter", "flavor"))
  ## Behavioral traits ------------
  #"smoke_status", "alch_freq_wk", "pa_level_mets", #"genhealth"
  ## Lab values
  #"alb", "creatinine", "uacr", "egfr", "u_albumin", "u_creatinine",
  #"alt", "ast", "bicarbonate", "bilirubin", "bun", "glu", "hdl",
  #"tc", "hba1c", "potassium", "chloride", "sodium",
  #"hb", "eopct", "lypct", "hepct", "mchc", "mcvsi", "plt",
  ## Fasting lab values
  #"2hg", "ldl", "tg", "fg",
  ## Disease outcomes
  #disease_vars, "phq9_total",
  ## Medication use 
  #"rx_use_bp", "rx_use_statin", "rx_use_diab",
  ## Health care system interactions 
  #"insur_any", "insur_private", "uninsur_lastyr")


glms_set1_tastediet <- lapply(exposures, function(exp) {
  out_glms.l <- lapply(outcomes_set1_tastediet, function(out) {
    run_survdesign_glm.fun(exposure=exp, outcome=out, covariates=c("age","gender"))
  }) ; names(out_glms.l) <- outcomes_set1_tastediet ; return(out_glms.l)
}) ; names(all_glms.l) <- exposures


lapply(exposures, function(exp) {
  lapply(outcomes, function(out) all_glms.l[[exp]][[out]]$modelsum) %>% 
    do.call(rbind.data.frame, .)
}) %>% do.call(rbind.data.frame, .)

