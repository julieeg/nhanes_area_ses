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
get_github_scripts("julieeg", "nhanes_area_ses", "../scripts/functions/", file="nhanes_survdesign_functions.r")
source("../scripts/functions/nhanes_survdesign_functions.r")

## Load nhanes data & prevent variables
nhanes_dat <- readRDS("../data/processed/nhanes_postprocessed_linked_ndi_prvnt_sdi.rds")


################################################################################
## Define lists of exposures, outcomes, and strata
################################################################################

# ===================
## SES Exposures
# ===================

ses_indiv_exposures <- c(educ="educ_level", incpov="inc_to_pov", incpovlvl="inc_to_pov_level", 
                         employ="employ_status", foodinsecure = "foodinsecure")

ses_area_exposures <- c(urbrur = "acs_urbrur.bin", urbrurcat = "acs_urbrur.cat",
                        pctlths="acs_educ_lths_pct", pctltcoll="acs_educ_ltcoll_pct",
                        pctunemp = "acs_unemp_pct", mhi="acs_incpov_mhi", 
                        pctfpl200="acs_incpov_fpl200_pct", sdi="sdi", svi="svi", adi="adi")

ses_exposures <- c(ses_indiv_exposures, ses_area_exposures)


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

all_outcomes <- unlist(all_outcomes.l, use.names = FALSE)

# Make separate list of NDI outcomes
ndi_outcomes <- list(
  ndi = c(
    "ndi_mortstat", "ndi_mortstat_hd", "ndi_mortstat_cbvd", "ndi_mortstat_cvd", 
    "ndi_mortstat_diab", "ndi_mortstat_diab_any" 
  )
)

# First: double check varnames in nhanes_dat
#sapply(names(outcomes_list), function(yset) {
#  yvars<-outcomes_list[[yset]]
#  return(yvars[!yvars %in% names(nhanes_dat)])
#})


################################################################################
## Run GLMs over lists of SES exposures x composite list of outcomes 
################################################################################

library(parallel)

dir.create("../data/output/glms")

# ============================================================
## Run GLMs over ALL indiv/area SES exposures & outcomes
# ============================================================

## Full sample -----------------
mclapply(seq_along(all_outcomes.l), function(yset) {
  outcome_set <- names(all_outcomes.l)[yset]
  
  # Run GLMs on each SES exposure
  lapply(seq_along(ses_indiv_exposures), function(x) {
    collapse_survdesign_regress.fun(
      wrap_survdesign_regress.fun(
        regress_fun = run_survdesign_glm.fun,
        exposure_vars = ses_indiv_exposures[[x]], outcome_vars = all_outcomes.l[[yset]],
        strata_vars = "full")) %>%
      # save glm output for each exp-outcome pair
      fwrite(paste0("../data/output/glms/glms_total_", 
                    names(ses_indiv_exposures[x]),"_", outcome_set, ".csv"))
  })
}) 


## By Strata -----------------
mclapply(seq_along(strata_vars), function(z) {
  stratavar <- strata_vars[[z]]
  
  lapply(seq_along(all_outcomes.l), function(yset) {
    outcome_set <- names(all_outcomes.l)[yset]
    
    # Run GLMs on each SES exposure
    lapply(seq_along(ses_indiv_exposures), function(x) {
      collapse_survdesign_regress.fun(
        wrap_survdesign_regress.fun(
          regress_fun = run_survdesign_glm.fun,
          exposure_vars = ses_indiv_exposures[[x]], outcome_vars = all_outcomes.l[[yset]],
          strata_vars = stratavar)) %>%
        # save glm output for each exp-outcome pair
        fwrite(paste0("../data/output/glms/glms_by", names(strata_vars[z]), "_", 
                      names(ses_indiv_exposures[x]), "_",  outcome_set, ".csv"))
    })
  })
})


# ============================================================
## Run COXPH over ALL indiv/area SES exposures & NDI data
# ============================================================

dir.create("../data/output/coxph")

## Full sample -----------------
mclapply(seq_along(ses_indiv_exposures), function(x) {
  collapse_survdesign_regress.fun(
    wrap_survdesign_regress.fun(
      regress_fun = run_survdesign_coxph.fun,
      exposure_vars = ses_indiv_exposures[[x]], outcome_vars = ndi_outcomes$ndi,
      strata_vars = "full")) %>%
    # save glm output for each exp-outcome pair
    fwrite(paste0("../data/output/coxph/coxph_total_", 
                  names(ses_indiv_exposures[x]),"_ndi.csv"))
})

## By Strata -----------------
mclapply(seq_along(strata_vars), function(z) {
  stratavar <- strata_vars[[z]] 
  
  lapply(seq_along(ses_indiv_exposures), function(x) {
    collapse_survdesign_regress.fun(
      wrap_survdesign_regress.fun(
        regress_fun = run_survdesign_coxph.fun,
        exposure_vars = ses_indiv_exposures[[x]], outcome_vars = ndi_outcomes$ndi,
        strata_vars = stratavar)) %>%
      # save glm output for each exp-outcome pair
      fwrite(paste0("../data/output/coxph/coxph_by", names(strata_vars[z]), "_",
                    names(ses_indiv_exposures[x]),"_ndi.csv"))
  })
})


# ==============================================================================
## Calculate R2 and AUC for prevent predictions on disease/mortality outcomes
# ==============================================================================

dir.create("../data/output/pred")

pred_strata_vars <- c("total", strat_vars, "educ_level", "inc_to_pov_level", "employ_status")
names(pred_strata_vars) <- c("total", "sex", "agecat", "racethn", 
                             "educ", "incpov", "employ")

# FIB4/Liver cap (continuous) ===================
fib4_exposures <- c("fib4", "fib4_mod_vs_low", "fib4_high_vs_low")
liver_outcomes <- "liver_cap"

lapply(pred_strata, function(z) {
  lapply(liver_outcomes, function(y) {
    lapply(fib4_exposures, function(x) {
      if(z == "gender") {covariates = "age"} ; if(z=="age") {covariates = "gender"}
      tryCatch({
        calc_nhanes_riskpred.fun(
          exp=x, out=y, stat="r2", cov=covariates, strata = z, data = nhanes_dat)
      }, error = function(e) {return(NULL) 
      })
    }) %>% bind_rows()
  }) %>%  bind_rows()
}) %>%  bind_rows() %>% 
  fwrite("../data/output/pred/pred_fib4liver_allstrata.csv")


# FIB4/Liver cap (binary) ===================
fib4_exposures <- c("fib4", "fib4_mod_vs_low", "fib4_high_vs_low")
liver_bin_outcomes <- c("liver_cap_any", "liver_cap_mod_sev")

lapply(pred_strata, function(z) {
  lapply(liver_bin_outcomes, function(y) {
    lapply(fib4_exposures, function(x) {
      if(z == "gender") {covariates = "age"} ; if(z=="age") {covariates = "gender"}
      tryCatch({
        calc_nhanes_riskpred.fun(
          exp=x, out=y, stat="auc", cov=covariates, strata = z, data = nhanes_dat)
      }, error = function(e) {return(NULL) })
    }) %>% bind_rows()
  }) %>%  bind_rows()
}) %>%  bind_rows() %>% 
  fwrite("../data/output/pred/pred_fib4liver_bin_allstrata.csv")


# PREVENT Equations on mortality outcomes ===================
prev_exposures <- all_outcomes.l$riskpred[-c(1:3)]
mort_outcomes <- ndi_outcomes$ndi

pred_prev_test <- lapply(pred_strata, function(z) {
  lapply(mort_outcomes, function(y) {
    lapply(prev_exposures, function(x) {
      if(z == "gender") {covariates = "age"} ; if(z=="age") {covariates = "gender"}
      tryCatch({
        calc_nhanes_riskpred.fun(
          exp=x, out=y, stat="cstat", cov=covariates, strata = z, data = nhanes_dat)
      }, error = function(e) {return(NULL) })
    }) %>% bind_rows()
  }) %>% bind_rows()
}) %>% bind_rows()  
fwrite("../data/output/pred/pred_prevent_mortstat_allstrata.csv")

pred_prevent_mortstat_allstrata <- fread("../data/output/pred/pred_prevent_mortstat_allstrata.csv")


## EOF


