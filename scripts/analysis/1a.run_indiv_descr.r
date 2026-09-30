# Run descriptive analyses

################################################################################
## Descriptive statistics of all var by all INDIV-LVL exposures & strata 
################################################################################

## NOTE: Removed PREVENT+SDI variables from all_outcomes
all_outcomes.l$riskpred_sdi <- NULL

## List of all (indiv-level) vars to describe
descr_tab1_vars.l <- c(
  years="Years", 
  list(strata = unname(strata_vars.indiv)), 
  list(ses = unname(ses_exposures.indiv)), 
  list(descr = addn_table1_vars),
  c(all_outcomes.l, ndi_outcomes)
  )

lapply(seq_along(strata_vars.indiv), function(z) {
  lapply(seq_along(descr_tab1_vars.l), function(y) {
    
    cat(sprintf("Describing vars, %s | By strata = %s \n", 
                toupper(names(descr_tab1_vars.l)[y]), toupper(names(strata_vars.indiv)[z])))
      
    build_nhanes_summarytable.fun(descr_tab1_vars.l[[y]], 
                                  strata = strata_vars.indiv[[z]], 
                                  data = nhanes_dat, d=3) }) %>% 
    do.call(bind_rows, .) %>% 
    fwrite(paste0("../data/output/descr/tab_descr_allvars_by", 
                  names(strata_vars.indiv[z]), ".csv"))  
  
  ## Finished: printing output
  cat(sprintf("Finished. | Saving table to %s \n", paste0(
    path_to_output, "descr/tab_descr_allvars_by", names(strata_vars.indiv[z]), ".csv"))
  )
  
}) 


################################################################################
## Extract distribution (counts or quantiles) of all SES exposures by 
## other INDIV-LVL exposures, strata, or *expXstrat intersections*
################################################################################

strata_intx_vars.indiv <- c(strata_vars.indiv, intx_vars.indiv)

## Check INTERSECTIONS of SES measures across Strata by Education or Income
lapply(seq_along(strata_intx_vars.indiv), function(z) {
  intx_res <- lapply(seq_along(ses_exposures.indiv), function(x) {
    xVar=toupper(names(ses_exposures.indiv)[x])
    zVar=toupper(names(strata_intx_vars.indiv)[z])
    cat(sprintf("Summarizing %s | strata ~ %s \n", xVar, zVar))
    res <- tryCatch({ # skip if exposure is part of the intersection
      if(is.numeric(nhanes_dat %>% pull(ses_exposures.indiv[[x]]))) {
        get_nhanes_quantiles.fun(
          ses_exposures.indiv[[x]], strata = strata_intx_vars.indiv[[z]], 
          probs=c(0.05,0.25,0.50,0.75,0.95), data = nhanes_dat)
      } else { 
        get_nhanes_distrib.fun(
          ses_exposures.indiv[[x]], strata = strata_intx_vars.indiv[[z]], 
          data = nhanes_dat) }
    }, error = function(e) {
        # If anything fails, print 'SKIP' an error and return NULL
        cat(sprintf("SKIPPING | %s x %s \n", xVar, zVar))
        message(sprintf("    Error details: %s", conditionMessage(e)))
        return(NULL)
      }) 
    }) %>% do.call(rbind.data.frame, .)
  # Reformat intersection output
  intx_res %>% 
    select(-contains(c("teststat")), -"p_value") %>% 
    pivot_longer(cols = -c(Variable, Level, contains("total")),
                 names_sep="_x_", names_to=c("Var1_level", "Var2_level")) %>%
    pivot_wider(names_from=Var1_level) %>%
    mutate(Intersect = names(strata_intx_vars.indiv)[z], .before=1) %>% 
    mutate(teststat = intx_res$teststat_df[1], teststat_val = intx_res$teststat_val[1],
           p_value = intx_res$p_value[1]) %>% 
    select(Intersect, Variable, Level, Var2_level, ends_with("total"), 
           starts_with(c("mean", "se", "teststat", "p_val"))) %>% 
    fwrite(paste0("../data/output/descr/tab_descr_distrib_indivses_by", names(strata_intx_vars.indiv[z]), ".csv"))
})



################################################################################
## Pearson correlations of SES measures, overall by STRATA
################################################################################

## Add integer versions of categorical SES exposures
nhanes_dat <- nhanes_dat %>% 
  mutate(
    educ_level.int = as.integer(educ_level),
    inc_to_pov_level.int = as.integer(inc_to_pov_level),
    employ_status.int = as.integer(employ_status),
    foodsecure_level.int = as.integer(foodsecure_level))

corr_ses_exposures.indiv <- c(
  "educ_level.int", "inc_to_pov_level.int", "inc_to_pov", 
  "employ_status.int", "foodsecure_level.int")


## Correlations in total sample ---------------
cor_total.l <- get_nhanes_cormat.fun(vars = corr_ses_exposures.indiv)
full_join(cor_total.l$corr, cor_total.l$pvals, by = c("strata", "variable")) %>% 
  fwrite("../data/output/descr/tab_descr_corr_indivses_bytotal.csv")


## Correlations, by strata ---------------
mclapply(seq_along(strata_vars.indiv), function(z) {
  corres <- get_nhanes_cormat.fun(
    vars = corr_ses_exposures.indiv, strata=strata_vars.indiv[[z]]
    ) ; full_join(
      corres$corr, corres$pvals, by = c("strata", "level", "variable")
      ) %>% 
    fwrite(paste0(path_to_output, "descr/tab_descr_corr_indivses_by", names(strata_vars.indiv)[z], ".csv"))
  })


################################################################################
## Describe PREVENT predictions, before/after accounting for SDI, by strata
################################################################################

nhanes_dat %>% select(SEQN, unname(ses_indiv_exposures), unname(strata_vars),
                      starts_with("prevent_") & contains(c("10yr", "30yr")))

prevent_cvd_10yr_est <- nhanes_dat %>% select(starts_with("prevent_cvd_10yr")) %>% names()
tab_prevent_cvd_10yr_est <- build_nhanes_summarytable.fun(
  vars_to_summarise = prevent_cvd_10yr_est, strata = "racethn_combn", data = nhanes_dat, digits = 3)



## EOF



## FINAL STEPS;

## Water fall plot for prevent equation outcomes; each IND is an up or down on waterfall, based on how their prediction changes hen you add SDI
# and color code by race

# HOLD ON THIS: but think about how to consolidate the risk estimates when adding SDI information
# calcualte delta in prevent equation, base to sdi; base to full; did they meet a threshod in each strata
# % pppl in each stratum that met a criterium

# multi-stratify by age, sex, and race/ethn
# JUST THE PREDICTION; how does the prediction change when SDI is added (IRRESPECTIVE of how well it connects to the outcome)
# Just crious about the change in prediction, 

## BUILD-IN CODE TO ADD THE AREA-LEVEL VARIABLES
# AND THE SDI & ChOOSE category**

# take SDI --> assign SDI cat --> assign true predict 


