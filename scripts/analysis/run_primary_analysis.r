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
get_github_scripts <-function(user, repo, path, file=NULL) {
  api <- sprintf("https://api.github.com/repos/%s/%s/contents/%s", user, repo, path)
  scripts <- grep("*.R", jsonlite::fromJSON(api)$name, value = T, ignore.case = T)
  if(is.null(file)) {
    URLs <- lapply(scripts, function(f) sprintf("https://raw.githubusercontent.com/%s/%s/main/%s/%s", user, repo, path, f))
  } else {
    URLs <- sprintf("https://raw.githubusercontent.com/%s/%s/main/%s/%s", user, repo, path, file)
  } ; invisible(lapply(URLs, source))
} ; get_github_scripts("julieeg", "pantry", "functions")

## Load nhanes_area_ses scripts
get_github_scripts("julieeg", "nhanes_area_ses", "scripts/analysis", "build_nhanes_survdesign.r")

## Load nhanes data & prevent variables
nhanes_dat <- readRDS("../data/processed/nhanes_postprocessed_linked_ndi_prvnt_sdi.rds")


################################################################################
## Define lists of exposures, outcomes, and strata
################################################################################

# ===================
## EXPOSURES 
# ===================

# Individual-level
indiv_exposures <- c("educ_level", "inc_to_pov", "inc_to_pov_level", "employ_status") 

# Area-level *******
# urban/rural


# ===================
## OUTCOMES 
# ===================

all_outcomes.l <- list(

  anthropbp = c( # Annthrop/BP -------------------------
    "bmi", "waist", "whr", "wt", "sbp_mean", "dbp_mean"),
  
  clinical = c( ## Clinical labs/risk factors ----------------------
    "fg", "fi", "tg", "tc", "hdl", "ldl_friedewald", "glu", "tgglu",
    "ogtt_2hg", "hba1c", "uacr", "egfr", "crp", "creatinine"),
    
  bm_metals_toxins = c( ## Blood biomarkers, metals & toxins --------
    "vitd", "b_carot","g_tocoph", "t_lycop", "manganese", "selenium",
    "mercury", "lead", "cadmium",
    "hcb", "hcb_adj", "hepox", "pcb180", "pcbdiox", "u_bpa"),
  
  hormones = c( ## Hormones ---------------
    "testosterone", "estradiol", "hpv_oral"),
  
  healthbehav = c( ## Behavioral traits --------------------------------
    "smoke_current", "smoke_ever", "alch_drink_curr", "alch_drink_daily",
    "alch_drink_weekly", "pa_total_mets_wk", "pa_meets_guidelines", 
    "genhealth_low_vs_other", "restaur_freq_gt2",
    "hc_drvisit", "hc_hospadmit", "insur_any", "uninsur_lastyr",
    "govtmeal_any", "foodinsecure"),
  
  disease = c( ## Disease outcomes ---------------------------
    "diabetes", "diabetes_undx", "htn", "htn_undx", "htn_stg1", 
    "obese", "obese_abd", "cvd", "ascvd", "chf", "mdd",
    "ckd", "ckd_gfr_gte3", "ckd_any", 
    "copd", "copd_pft", "pft_lt07", 
    "fib4", "fib4_mod_vs_low", "fib4_high_vs_low", 
    "nfs", "nfs_mod_vs_low", "nfs_high_vs_low",
    "liver_cap_any", "liver_cap_mod_sev"),
  
  diet = c( # Diet quality & intake habits ---------------------
    "hei2015_total", "ahei_total", "dii_total", "dietsuppl_any",
    "dietsuppl_num", "addsalt_prep_often", "addsalt_table_often"),
  
  chemos = c( # Taste/Smell outcomes ----------------------
    "taste_mouth_quinine_glms", "taste_mouth_nacl_1M_glms", 
    "taste_mouth_nacl_320mM_glms", "smell_pst_total", "smell_dysfun_any",
    "smell_dysfun_severe", 
    paste0("tastechange_", c("sweet", "salt", "bitter", "sour"), "_worse"))
)

all_outcomes <- unlist(all_outcomes.l, use.names = FALSE)

# First: double check varnames in nhanes_dat
#sapply(names(outcomes_list), function(yset) {
#  yvars<-outcomes_list[[yset]]
#  return(yvars[!yvars %in% names(nhanes_dat)])
#})

# ===================
## STRATA 
# ===================

strata_vars <- c("full", "gender", "age_4lvl", "racethn_combn")


################################################################################
## Run GLMs over lists of SES exposures x composite list of outcomes 
################################################################################

# ============================================================
## Define wrapper functions for running & compiling glms
# ============================================================

## Build function to wrap run_glm_survdesign over multiple exp/out
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


## Build function to compile wrap_glm_survdesign output
collapse_glm_survdesign.fun <- function(wrapped_glm_survdesign) {
  # Get strata, exposures & outcomes
  glm_strata <- names(wrapped_glm_survdesign)
  glm_exposures <- names(wrapped_glm_survdesign[[1]])
  glm_outcomes <- names(wrapped_glm_survdesign[[1]][[1]])
  
  glm_collapsed <- lapply(glm_strata, function(strat) {
    lapply(glm_exposures, function(exp) {
      lapply(glm_outcomes, function(out) {
        wrapped_glm_survdesign[[strat]][[exp]][[out]]$modelsum
      }) %>% do.call(rbind.data.frame, .)
    }) %>% do.call(rbind.data.frame, .)
  }) %>% do.call(rbind.data.frame, .)
  
  return(glm_collapsed)
}
        

# ============================================================
## Run all GLMs over indiv/area SES exposures & outcomes
# ============================================================

library(parallel)

## Full sample -----------------
nhanes_glms_indiv_full <- mclapply(seq_along(all_outcomes.l), function(y) {
  
  outcome_set <- names(all_outcomes.l)[y]
  
  # Run GLMs
  glms_wrapped.l <- wrap_survdesign_glm.fun(
    exposure_vars = indiv_exposures, outcome_vars = all_outcomes.l[[y]],
    strata_vars = "full")
  
  # Collapse modelsum output
  return(collapse_glm_survdesign.fun(glms_wrapped.l))
  
}) %>% do.call(rbind.data.frame, .)

nhanes_glms_indiv_full %>% fwrite(., file = "../data/output/nhanes_glms_indiv_full.csv")


# Sex-stratified -----------------
nhanes_glms_indiv_sex <- lapply(seq_along(all_outcomes.l), function(y) {
  
  outcome_set <- names(all_outcomes.l)[y]
  
  # Run GLMs
  glms_wrapped.l <- wrap_survdesign_glm.fun(
    exposure_vars = indiv_exposures, outcome_vars = all_outcomes.l[[y]],
    strata_vars = "gender")
  
  # Collapse modelsum output
  return(collapse_glm_survdesign.fun(glms_wrapped.l))
  
}) %>% do.call(rbind.data.frame, .)

nhanes_glms_indiv_sex %>% fwrite(., file = "../data/output/nhanes_glms_indiv_sex.csv")

# Age (4-lvl)-stratified ---------------
nhanes_glms_indiv_age <- lapply(seq_along(all_outcomes.l), function(y) {
  
  outcome_set <- names(all_outcomes.l)[y]
  
  # Run GLMs
  glms_wrapped.l <- wrap_survdesign_glm.fun(
    exposure_vars = indiv_exposures, outcome_vars = all_outcomes.l[[y]],
    strata_vars = "age_4lvl")
  
  # Collapse modelsum output
  return(collapse_glm_survdesign.fun(glms_wrapped.l))
  
}) %>% do.call(rbind.data.frame, .)

nhanes_glms_indiv_age %>% fwrite(., file = "../data/output/nhanes_glms_indiv_age.csv")


# Race/Ethnicity-stratified ---------------

nhanes_glms_indiv_racethn <- lapply(seq_along(all_outcomes.l), function(y) {
  
  outcome_set <- names(all_outcomes.l)[y]
  
  # Run GLMs
  glms_wrapped.l <- wrap_survdesign_glm.fun(
    exposure_vars = indiv_exposures, outcome_vars = all_outcomes.l[[y]],
    strata_vars = "racethn_combn")
  
  # Collapse modelsum output
  return(collapse_glm_survdesign.fun(glms_wrapped.l))
  
}) %>% do.call(rbind.data.frame, .)

nhanes_glms_indiv_racethn %>% fwrite(., file = "../data/output/nhanes_glms_indiv_racethn.csv")


## EOF


