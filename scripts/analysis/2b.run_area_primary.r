## Rscript to generate descriptive statitics & regression outputs in NHANES 
## using custom nhanes_survdesign functions 


################################################################################
## Run GLMs over lists of SES exposures x composite list of outcomes 
################################################################################

path_to_output <- "../data/output/RDCtest/"

# ============================================================
## Run GLMs over ALL indiv/area SES exposures & outcomes
# ============================================================

# ---------------
## Total cohort 
# ----------------
output_file <- paste0(path_to_output, "/glms/table5_glms_alloutcomes_bytotal.csv")
lapply(seq_along(all_outcomes.l), function(yset) {
  
  # 1. List outcomes, exposures & models
  Outcomes <- names(all_outcomes.l)[yset] ; outcomes <- all_outcomes.l[[yset]]
  Exposures <- names(ses_exposures) ; exposures <- ses_exposures
  Models <- names(models) ; Strata = "Total"
  
  cat(paste0(" --> WRAPPER | REGRESS [strata] | ", toupper(Outcomes), 
             " outcomes ~ ALL exposures | \n"))
  
  # Use WRAPPER to regress each outcome SET on each SES exp, in each model 
  wrap_nhanes_regress.fun(
    regress_fun = run_nhanes_glm.fun, exposure_vars = exposures, 
    outcome_vars = outcomes, strata_var = "none", models = models,
    data = nhanes_merged, model_fit_stats = "glm")
    
}) %>% bind_rows() %>% 
  # Combine across all outcomes, and save
  fwrite(output_file)


# -----------------
## By Strata
# ----------------

lapply(seq_along(strata_vars), function(z) {
  
  Strata <- names(strata_vars)[z]
  strata <- unname(strata_vars)[z]
  output_file <- paste0(path_to_output, "/glms/table5_glms_alloutcomes_by", Strata, ".csv")
  
  lapply(seq_along(all_outcomes.l), function(yset) {
  
  # 1. List outcomes, exposures & models
  Outcomes <- names(all_outcomes.l)[yset] ; outcomes <- all_outcomes.l[[yset]]
  Exposures <- names(ses_exposures) ; exposures <- ses_exposures
  Models <- names(models) 
  
  cat(paste0(" --> WRAPPER | REGRESS [strata] | ", toupper(Outcomes), 
             " outcomes ~ ALL exposures | \n"))
  
  # Use WRAPPER to regress each outcome SET on each SES exp, in each model 
  wrap_nhanes_regress.fun(
    regress_fun = run_nhanes_glm.fun, exposure_vars = exposures, 
    outcome_vars = outcomes, strata_var = strata, models = models,
    data = nhanes_merged, model_fit_stats = "glm")
  
  # For each STRATA, combine across all outcomes & save
  }) %>% bind_rows() %>% 
  fwrite(output_file)

}) 



################################################################################
## Run COXPH over ALL indiv/area SES exposures on NDI mortality outcomes
################################################################################

ndi_outcomes.l$ndi <- ndi_out

# ---------------
## Total cohort 
# ----------------
output_file <- paste0(path_to_output, "/coxph/table6_coxph_ndimort_bytotal.csv")
lapply(seq_along(ndi_outcomes.l), function(yset) {
  
  # 1. List outcomes, exposures & models
  Outcomes <- names(ndi_outcomes.l)[yset] ; outcomes <- ndi_outcomes.l[[yset]]
  Exposures <- names(ses_exposures) ; exposures <- ses_exposures
  Models <- names(models) ; Strata = "Total"
  
  cat(paste0(" --> WRAPPER | REGRESS [strata] | ", toupper(Outcomes), 
             " outcomes ~ ALL exposures | \n"))
  
  # Use WRAPPER to regress each outcome SET on each SES exp, in each model 
  wrap_nhanes_regress.fun(
    regress_fun = run_nhanes_coxph.fun, exposure_vars = exposures, 
    outcome_vars = outcomes, strata_vars = "none", models = models,
    data = nhanes_merged, model_fit_stats = "coxph")
  
}) %>% bind_rows() #%>% 
  # Combine across all outcomes, and save
  #fwrite(output_file)


# -----------------
## By Strata
# ----------------

lapply(seq_along(strata_vars), function(z) {
  
  Strata <- names(strata_vars)[z]
  strata <- unname(strata_vars)[z]
  output_file <- paste0(path_to_output, "/coxph/table6_coxph_ndimort_by", Strata, ".csv")
  
  lapply(seq_along(ndi_outcomes.l), function(yset) {
    
    # 1. List outcomes, exposures & models
    Outcomes <- names(ndi_outcomes.l)[yset] ; outcomes <- ndi_outcomes.l[[yset]]
    Exps <- names(ses_exposures) ; exp_vars <- ses_exposures
    Models <- names(models) 
    
    cat(paste0(" --> WRAPPER | REGRESS [STRATA] | OUTCOME ~ EXPOSURE | MODEL | CYCLES | FILTER | \n"))
    
    # Use WRAPPER to regress each outcome SET on each SES exp, in each model 
    wrap_nhanes_regress.fun(
      regress_fun = run_nhanes_coxph.fun, exposure_vars = exposures, 
      outcome_vars = outcomes, strata_var = strata, models = models,
      data = nhanes_merged, model_fit_stats = "coxph")
     
    # For each STRATA, combine across all outcomes & save
  }) %>% bind_rows() %>% 
    fwrite(output_file)
  
}) 


################################################################################
## Calculate R2/AUC & C-stat for prevent predictions on disease/mortality outcomes
################################################################################

dir.create("../data/output/pred")

pred_strata_vars <- c("total"="total", strata_vars)

# FIB4/Liver cap metrics ===================
fib4_exposures <- c("fib4", "fib4_mod_vs_low", "fib4_high_vs_low")
liver_outcomes <-  c("liver_cap", "liver_cap_any", "liver_cap_mod_sev")

tab_pred_fib4liver <- lapply(seq_along(pred_strata_vars), function(z) {
  
  Strata <- names(pred_strata_vars)[z]
  strata <- unname(pred_strata_vars)[z]
  
  if(Strata %in% c("gender", "sex")) {covariates = "age"}
  if(Strata == "agecat") {covariates = "gender"}
  
  lapply(seq_along(liver_outcomes), function(y) {
    outcome <- unname(liver_outcomes)[y]
    
    lapply(seq_along(fib4_exposures), function(x) {
      
      exposure <- fib4_exposures[x]
      
      ## Select stat based on output type
      outcome_levels <- (data %>% select(outcome) %>% filter(complete.cases(.)) 
                       %>% unique() %>% pull(outcome))
      stat <- if(is.numeric(data[[outcome]]) & length(outcome_levels) > 2) "r2" else "auc"
      
      tryCatch({
        calc_nhanes_riskpred.fun(
          exp=exposure, out=outcome, stat=stat, cov=covariates, 
          strata = strata, data = nhanes_merged) %>% 
          rename_with(., ~gsub("auc", "modfit", gsub("modR2", "modfit", 
                                                     gsub("rmse", "modfit_se", .)))) %>%
          mutate(fitstat = stat, .before=modfit)
      }, error = function(e) {return(NULL) 
      })
    }) %>% bind_rows()
  }) %>%  bind_rows()
}) %>%  bind_rows()

tab_pred_fib4liver %>% fwrite(paste0(path_to_output, "/pred/table9_pred_fib4liver_bystrata.csv"))
  

# PREVENT Equations on mortality outcomes ===================
prev_exposures <- all_outcomes.l$riskpred[-c(1:3)]
ndi_outcomes <- ndi_outcomes.l$ndi

lapply(seq_along(pred_strata_vars), function(z) {
  
  Strata <- names(pred_strata_vars)[z]
  strata <- unname(pred_strata_vars)[z]
  
  if(Strata %in% c("gender", "sex")) {covariates = "age"}
  if(Strata == "agecat") {covariates = "gender"}
  
  lapply(seq_along(ndi_outcomes), function(y) {
    outcome <- unname(ndi_outcomes)[y]
    
    lapply(seq_along(prev_exposures), function(x) {
      
      exposure <- prev_exposures[x]
      
      tryCatch({
        calc_nhanes_riskpred.fun(
          exp=exposure, out=outcome, stat="cstat", cov=covariates, 
          strata = strata, data = nhanes_merged) %>% 
          rename_with(., ~gsub("cstat", "modfit", .)) %>%
          mutate(fitstat = "cstat", .before=modfit)
      }, error = function(e) {return(NULL) 
      })
    }) %>% bind_rows()
  }) %>%  bind_rows()
}) %>%  bind_rows() %>% 
  fwrite(paste0(path_to_output, "/pred/table9_pred_preventndi_bystrata.csv"))


## EOF


