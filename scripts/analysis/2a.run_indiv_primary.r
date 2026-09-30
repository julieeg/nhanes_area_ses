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
## Run GLMs over lists of SES exposures x composite list of outcomes 
################################################################################

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

pred_strata_vars <- c("total", strata_vars, "educ_level", "inc_to_pov_level",
                      "employ_status", "foodinsecure")
names(pred_strata_vars) <- c("total", "sex", "agecat", "racethn", 
                             "educ", "incpov", "employ", "foodinsec")

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


# PREVENT Equations on disease occurences (binary) ===================
prev_exposures <- all_outcomes.l$riskpred[-c(1:3)]
prevent_disease_outcomes <- c("cvd", "ascvd", "chf", "htn", "diabetes")

lapply(pred_strata_vars, function(z) {
  lapply(prevent_disease_outcomes, function(y) {
    lapply(prev_exposures, function(x) {
      if(z == "gender") {covariates = "age"} ; if(z=="age") {covariates = "gender"}
      tryCatch({
        calc_nhanes_riskpred.fun(
          exp=x, out=y, stat="auc", cov=covariates, strata = z, data = nhanes_dat)
      }, error = function(e) {return(NULL) })
    }) %>% bind_rows()
  }) %>%  bind_rows()
  }) %>%  bind_rows() %>% 
  fwrite("../data/output/pred/pred_prevent_disease_allstrata.csv")


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


