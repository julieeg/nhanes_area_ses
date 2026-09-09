## Rscript to generate descriptive statitics & regression outputs in NHANES 
## using custom nhanes_survdesign functions 


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
#get_github_scripts("julieeg", "nhanes_area_ses", "functions")


## Load nhanes data & prevent variables
nhanes_dat <- readRDS("../data/processed/nhanes_postprocessed_linked_ndi_prvnt_sdi.rds")

## Run dependent build_nhanes_survdesign.r script 
source("../scripts/build_nhanes_survdesign.r")


################################################################################
## Descriptive (Table 1) summaries by Age, Sex and Race/Ethnicity 
################################################################################

strata_vars <- c("gender", "age_4lvl", "racethn_combn")

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


################################################################################
## Run GLMs over lists of SES exposures x composite list of outcomes 
################################################################################

# Individual-levels -----------
exposures <- c("educ_level", "inc_to_pov", "inc_to_pov_level", "employ_status") 

# Area-level 
# urban/rural

outcomes_list <- list(
  
  # Annthrop/BP ------------
  anthropbp = c(
    "bmi", "waist", "whr", "wt", "sbp_mean", "dbp_mean"),
  
  ## Clinical labs/risk factors
  clinical = c(
    "fg", "fi", "tg", "tc", "hdl", "ldl_friedewald", "glu", "tgglu",
    "ogtt_2hg", "hba1c", "uacr", "egfr", "crp", "creatinine"),
    
  ## Blood biomarkers, heavy metals & toxins --------
  bm_metals_toxins = c(
    "vitd", "b_carot","g_tocoph", "t_lycop", "manganese", "selenium",
    "mercury", "lead", "cadmium",
    "hcb", "hcb_adj", "hepox", "pcb180", "pcbdiox", "u_bpa"),
  
  ## Hormones ------------
  hormones = c("testosterone", "estradiol", "hpv_oral"),
  
  ## Behavioral traits ------------
  healthbehav = c(
    "smoke_current", "smoke_ever", "alch_drink_curr", "alch_drink_daily", "alch_drink_weekly", 
    "pa_total_mets_wk", "pa_meets_guidelines", 
    "genhealth_low_vs_other", "restaur_freq_gt2",
    "hc_drvisit", "hc_hospadmit", "insur_any", "uninsur_lastyr",
    "govtmeal_any", "foodinsecure"),
  
  ## Disease outcomes
  disease = c(
    "diabetes", "diabetes_undx", "htn", "htn_undx", "htn_stg1", 
    "obese", "obese_abd", "cvd", "ascvd", "chf", "mdd",
    "ckd", "ckd_gfr_gte3", "ckd_any", 
    "copd", "copd_pft", "pft_lt07", 
    "fib4", "fib4_mod_vs_low", "fib4_high_vs_low", 
    "nfs", "nfs_mod_vs_low", "nfs_high_vs_low",
    "liver_cap_any", "liver_cap_mod_sev"),
  
  # Dietary intake ------------
  diet = c("hei2015_total", "ahei_total", "dii_total", "dietsuppl_any", "dietsuppl_num",
           "addsalt_prep_often", "addsalt_table_often"),
  
  # Taste/Smell outcomes ------------
  chemos = c(
    "taste_mouth_quinine_glms", "taste_mouth_nacl_1M_glms", "taste_mouth_nacl_320mM_glms",
    "smell_pst_total", "smell_dysfun_any", "smell_dysfun_severe",
    paste0("tastechange_", c("sweet", "salt", "bitter", "sour"), "_worse"))
)

outcomes <- unlist(outcomes_list, use.names = FALSE)

# First: double check varnames in nhanes_dat
lapply(names(outcomes_list), function(yset) {
  yvars<-outcomes_list[[yset]]
  return(yvars[!yvars %in% names(nhanes_dat)])
})

## Organize run_survdesign_glm to run all models in FULL & STRATA samples

strata_vars <- c("full", "gender", "age_4lvl", "racethn_combn")
exposures <- c("educ_level", "inc_to_pov", "inc_to_pov_level", "employ_status")

wrap_survdesign_glm.fun <- function(exposure_vars, outcome_vars, strata_vars) { 
  
  # For each STRATA --------------------
  glm_strata <- lapply(strata_vars, function(z) {
    
    if(z =="full") { strata = NULL } else { strata = z }
    
    # Define covariates (age+gender; unless otherwise specified)
    covars = c("age", "gender") ; if(z %in% c("gender", "female", "sex")) { 
      covars = "age" } else if (z == "age_4lvl") { covars = "gender" }
    
    # For each EXPOSURE ---------------------
    glm_exposures <- lapply(exposure_vars, function(x) {
      
      # Run for each OUTCOME --------------
      glm_outcomes <- lapply(outcome_vars, function(y) {
        run_survdesign_glm.fun(data = nhanes_dat, exposure = x, outcome = y, 
                               covariates = covars, strata = strata) 
        
      }) ; names(glm_outcomes) <- outcome_vars ; return(glm_outcomes)
    }) ; names(glm_exposures) <- exposure_vars ; return(glm_exposures)
  }) ; names(glm_strata) <- strata_vars ; return(glm_strata)
  
}

## Run over each set of outcomes
glms_indv_x_healthbehav <- wrap_survdesign_glm.fun(
  exposure_vars = exposures, outcome_vars = outcomes_list$healthbehav,
  strata_vars = strata_vars[1:2])

glms_indv_x_bm_metals_toxins <- wrap_survdesign_glm.fun(
  exposure_vars = exposures, outcome_vars = outcomes_list$bm_metals_toxins,
  strata_vars = strata_vars[1:2])


lapply(glms_indv_x_bm_metals_toxins, function() 
  
# Compile model summaries into dataframe

glms_set1_modelsum <- lapply(sampleset, function(set) {
  lapply(exposures, function(exp) {
    lapply(outcomes_set1[4:5], function(out) {
      glms_set1.l[[set]][[exp]][[out]]$modelsum }) %>% 
      do.call(rbind.data.frame, .) 
    }) %>% do.call(rbind.data.frame, .)
  }) #%>% do.call(rbind.data.frame, .)

glms_set1_modelsum

  
  
  