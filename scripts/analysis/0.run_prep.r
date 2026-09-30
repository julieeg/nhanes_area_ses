# Recipe syntax for NHANES Area x SES analyses


################################################################################
## Set Up
################################################################################

# ================================================
## Set up output dir & load required packages
# ================================================

## set local directory ------------------------
setwd('~/Documents/GitHub/nhanes_area_ses/run') 

## Set path to data output ------------
path_to_output <- "../data/output/"

# Create sub-folders to organize output
dir.create(paste0(path_to_output, "/descr"))
dir.create(paste0(path_to_output, "/glms"))
dir.create(paste0(path_to_output, "/coxph"))
dir.create(paste0(path_to_output, "/pred"))


## load required base pacakges ------------------------
list_of_packages <- c(
  "tidyverse", "data.table", "nhanesA", "progress", "sociome", "jsonlite", "haven", 
  "forcats", "parallel", "survey") ; invisible(lapply(list_of_packages, function(pkg) {
    if(!requireNamespace(pkg, quietly = TRUE)) { 
      install.packages(pkg) } ; library(pkg, character.only = TRUE)
  }))


# =====================================================================
## Load scripts & functions from GitHub repos
# =====================================================================

# Custom function to load GH files ===========
get_github_files <-function(user, repo, path, load_fn = source, file_ext="r", file_pf=NULL) {
  api <- sprintf("https://api.github.com/repos/%s/%s/contents/%s", user, repo, path)
  files <- grep(paste0("\\.",file_ext,"$"), fromJSON(api)$name, value = T, ignore.case = T)
  if(!is.null(file_pf)) { 
    files <- grep(file_pf, files, value=T) 
  } ; print(sprintf("LOADING | %s/%s/%s ... FILES | %s", user, repo, path, paste0(files, collapse=", ")))
  
  URLs <- lapply(files, function(f) sprintf("https://raw.githubusercontent.com/%s/%s/main/%s/%s", user, repo, path, f))
  if(file_ext != "r") {
    gh_files.l <- invisible(lapply(URLs, load_fn)) ; 
    names(gh_files.l) <- gsub(paste0("[.]", file_ext), "", files) 
    return(gh_files.l) 
  } else { invisible(lapply(URLs, load_fn)) }
}


## Load nhanes_area_ses scripts -------------------
get_github_files("julieeg", "nhanes_area_ses", path="scripts/functions", 
                 load_fn = source, file_ext="r")


# ====================================
## Load pre-built NHANES datasets 
# ====================================

nhanes_dat <- readRDS("../data/processed/nhanes_postprocessed_linked_ndi_prvnt_sdi.rds")
nhanes_acs_ses <- fread("../data/processed/acs_ses_to_merge.csv") %>% 
  mutate_at("acs_urbrur", ~factor(., levels = c("urban", "rural"))) %>% 
  mutate_at("acs_urbrur_cat", ~ factor(., levels = c(
    "metropolitan", "micropolitan", "town", "rural")))



################################################################################
## Define lists of exposures, outcomes, and strata
################################################################################

# ===================
## SES exposures
# ===================

# Individual-level ----------
ses_exposures.indiv <- c(educ="educ_level", incpov="inc_to_pov", incpovlvl="inc_to_pov_level", 
                         employ="employ_status", foodinsec = "foodinsecure")

# Area-leve -------------
ses_exposures.area <- c(urbrur = "acs_urbrur", urbrurcat = "acs_urbrur_cat",
                        pctlths="acs_educ_lths_pct", pctltcoll="acs_educ_ltcoll_pct",
                        pctunemp = "acs_unemp_pct", mhi="acs_incpov_mhi", 
                        pctfpl200="acs_incpov_fpl200_pct", sdi="sdi", svi="svi", adi="adi")

# Combined list ---------
ses_exposures <- c(ses_exposures.indiv, ses_exposures.area)


# ==========================
## Stratifying variables
# ==========================

# Individual-level ------
strata_vars.indiv <- c(sex="gender", agecat="age_4lvl", racethn="racethn_combn",
                       educ="educ_level", incpovlvl="inc_to_pov_level", 
                       employ="employ_status", foodinsec = "foodinsecure")
descr_strata_vars.indiv <- c(cycle="Years", strata_vars.indiv)

# Area-level ------
strata_vars.area <- c(urbrur = "acs_urbrur", urbrur_type = "acs_urbdud_type")
descr_strata_vars.indiv <- c(cycle="Years", strata_vars.area)


# Combined list -----
strata_vars <- c(strata_vars.indiv, strata_vars.area)
descr_strata_vars <- c(cycle="Years", strata_vars)


# ==========================
## Descriptive variables 
# ==========================

addn_table1_vars <- c("smoke_status", "alch_freq_wk", "pa_level_mets", "genhealth",
                      "income_hh", "income_fam", "foodsecure_level",
                      "uacr_level", "ckd_gfr_level", "fib4_cat", "nfs_cat")

# Intersection variables
intx_vars.indiv <- c(raceXeduc="intx_raceXeduc", raceXincpov="intx_raceXincpov",
                    sexXeduc="intx_sexXeduc", sexXincpov="intx_sexXincpov",
                    agecatXeduc="intx_agecatXeduc", agecatXincpov="intx_agecatXincpov")

intx_vars.area <- c(urbruXeduc="intx_urburXeduc", urbruXincpov="intx_urbruXincpov")


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
                paste0("tastechange_", c("sweet", "salt", "bitter", "sour"), "_worse")),
  
  riskpred_sdi = c( ## Disease risk predictions: PREVENT+SDI  ----------
                    paste0(rep("prevent_",24), 
                           rep(c("cvd", "ascvd", "hf", "chd", "stroke"), each=4), 
                           rep(c("_10yr", "_30yr"), each=2), c("_sdi", "_full.sdi")))
)

# Make separate list of NDI outcomes
ndi_outcomes <- list(
  ndi = c(
    "ndi_mortstat", "ndi_mortstat_hd", "ndi_mortstat_cbvd", "ndi_mortstat_cvd", 
    "ndi_mortstat_diab", "ndi_mortstat_diab_any" 
  )
)


## EOF



