# Recipe syntax for NHANES Area x SES analyses


################################################################################
## Set Up; Load Required Packages & Data Files
################################################################################

## set local directory ------------------------
setwd('~/Documents/GitHub/nhanes_area_ses/run') 


## load required base pacakges ------------------------
list_of_packages <- c(
  "tidyverse", "data.table", "nhanesA", "progress", "sociome", "jsonlite", "haven", 
  "forcats", "parallel", "survey") ; invisible(lapply(list_of_packages, function(pkg) {
    if(!requireNamespace(pkg, quietly = TRUE)) { 
      install.packages(pkg) } ; library(pkg, character.only = TRUE)
  }))


## Create sub-folders to organize outputs -----------------
dir.create("../data/output/descr")


## Load nhanes_area_ses scripts -------------------
get_github_scripts("julieeg", "nhanes_area_ses", "scripts")
source("../scripts/functions/nhanes_survdesign_functions.r")


## Load nhanes data & prevent variables ------------------
nhanes_dat <- readRDS("../data/processed/nhanes_postprocessed_linked_ndi_prvnt_sdi.rds")

path_to_output <- "../data/output/"


################################################################################
## Define lists of exposures, outcomes, and strata
################################################################################

# ===================
## SES Exposures
# ===================

ses_indiv_exposures <- c(educ="educ_level", incpov="inc_to_pov", incpovlvl="inc_to_pov_level", 
                         employ="employ_status") ## ADD FOOD INSECURITRY!

ses_area_exposures <- c(urbrur = "acs_urbrur.bin", urbrurcat = "acs_urbrur.cat",
                        pctlths="acs_educ_lths_pct", pctltcoll="acs_educ_ltcoll_pct",
                        pctunemp = "acs_unemp_pct", mhi="acs_incpov_mhi", 
                        pctfpl200="acs_incpov_fpl200_pct", "sdi", "svi", "adi")

ses_exposures <- c(ses_indiv_exposures) #, ses_area_exposures)

# List of continuous exposures, only


# ==========================
## Stratifying variables
# ==========================

strata_vars <- c(sex="gender", agecat="age_4lvl", racethn="racethn_combn")


# ===================
## All outcomes 
# ===================

all_outcomes.l <- list(
  
  behav = c( ## Behavioral traits --------------------------------
             "smoke_current", "smoke_ever", "alch_drink_curr", "alch_drink_daily",
             "alch_drink_weekly", "pa_total_mets_wk", "pa_meets_guidelines", 
             "hei2015_total", "ahei_total", "dii_total", "dietsuppl_any",
             "dietsuppl_num", "addsalt_prep_often", "addsalt_table_often",
             "restaur_freq_gt2", "govtmeal_any", "foodinsecure", 
             "genhealth_low_vs_other", "hc_drvisit", "hc_hospadmit", 
             "insur_any", "uninsur_lastyr"),
  
  biomark = c( ## Annthrop/BP, clinical labs, metals/toxins, ----------------------
               "bmi", "waist", "whr", "wt", "sbp_mean", "dbp_mean",
               "fg", "fi", "tg", "tc", "hdl", "ldl_friedewald", "glu", "tgglu",
               "ogtt_2hg", "hba1c", "uacr", "egfr", "crp", "creatinine",
               "vitd", "b_carot","g_tocoph", "t_lycop", "manganese", "selenium",
               "mercury", "lead", "cadmium", "hcb", "hcb_adj", "hepox", "pcb180", 
               "pcbdiox", "u_bpa", "testosterone", "estradiol", "hpv_oral"),
  
  disease = c( ## Disease outcomes ---------------------------
               "diabetes", "diabetes_undx", "htn", "htn_undx", "htn_stg1", 
               "obese", "obese_abd", "cvd", "ascvd", "chf", "mdd",
               "ckd", "ckd_gfr_gte3", "ckd_any", "copd", "copd_pft", "pft_lt07", 
               "nfs", "nfs_mod_vs_low", "nfs_high_vs_low",
               "liver_cap_any", "liver_cap_mod_sev"),
  
  riskpred = c( ## Disease risk predictions: FIB4 & PREVENT est  ----------
                "fib4", "fib4_mod_vs_low", "fib4_high_vs_low", 
                paste0(rep("prevent_",40), 
                       rep(c("cvd", "ascvd", "hf", "chd", "stroke"), each=8), 
                       rep(c("_10yr", "_30yr"), each=4), 
                       c("_base", "_hba1c", "_uacr", "_full"))),
  
  chemosen = c( # Taste/Smell outcomes ----------------------
                "taste_mouth_quinine_glms", "taste_mouth_nacl_1M_glms", 
                "taste_mouth_nacl_320mM_glms", "smell_pst_total", "smell_dysfun_any",
                "smell_dysfun_severe", 
                paste0("tastechange_", c("sweet", "salt", "bitter", "sour"), "_worse"))
)

# Make separate list of NDI outcomes
ndi_outcomes <- list(
  ndi = c(
    "ndi_mortstat", "ndi_mortstat_hd", "ndi_mortstat_cbvd", "ndi_mortstat_cvd", 
    "ndi_mortstat_diab", "ndi_mortstat_diab_any" 
  )
)

