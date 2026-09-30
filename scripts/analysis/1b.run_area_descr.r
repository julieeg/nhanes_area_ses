# Run descriptive analyses


################################################################################
## Additional RDC prep to merge and construct final data sets
################################################################################

## Load pre-prepared acs_ses csv file
acs_ses <- fread("../data/processed/acs_ses_to_merge.csv") %>% 
  # Clean categorical variables; and set REF to highest SES level
  mutate_at("acs_urbrur", ~factor(., levels = c("urban", "rural"))) %>% 
  mutate_at("acs_urbrur_cat", ~ factor(., levels = c(
    "metropolitan", "micropolitan", "town", "rural")))


## Merge nhanes_dat WITH acs_ses_merged 
nhanes_acs <- full_join(nhanes_dat, acs_ses, by = "SEQN")


## Select correct PREVENT sdi prediction, based on
table(acs_ses$sdizip_preventcat)

# Add intersection variables for 
nhanes_acs <- nhanes_acs %>% 
  mutate(
    intx_urbrXeduc = case_when(
      !is.na(acs_urbrur) & !is.na(educ_level) ~ paste0(acs_urbrur, "_x_", educ_level),
      TRUE ~ NA),
    intx_urbruXincpov = case_when(
      !is.na(acs_urbrur) & !is.na(inc_to_pov_level) ~ paste0(acs_urbrur, "_x_", inc_to_pov_level),
      TRUE ~ NA)
  ) %>% 
  mutate_at("acs_urbrur", ~factor(., levels = c("urban", "rural"))) %>%
  mutate_at("acs_urbrur_cat", ~factor(., levels =c("metropolitan", "micropolitan"))
  )

# In case of coding artifacts
nhanes_dat <- nhanes_acs

################################################################################
## Descriptive summaries by Age, Sex and Race/Ethnicity 
################################################################################

# =====================================================================
## Descriptive Table 1s, by AREA-LEVEL exposures & strata for ALL variables
# =====================================================================

descr_tab1_vars.l <- c(years="Years", 
                       list(strata = unname(strata_vars.indiv)), 
                       list(ses = unname(ses_exposures.area)),
                       # Add in area-level STRATA & SES variables
                       list(strata_area = unname(strata_vars.area)), 
                       list(ses_area = unname(ses_exposures.area)), 
                       list(descr = addn_table1_vars),
                       c(all_outcomes.l, ndi_outcomes)
)


lapply(seq_along(descr_strata_vars.area), function(z) {
  lapply(seq_along(descr_tab1_vars.l), function(y) {
    
    cat(sprintf("Describing vars, %s | By strata = %s \n", 
                toupper(names(descr_tab1_vars.l)[y]), toupper(names(descr_strata_vars.area)[z])))
    
    build_nhanes_summarytable.fun(descr_tab1_vars.l[[y]], 
                                  strata = descr_strata_vars.area[[z]], 
                                  data = nhanes_dat, d=3) }) %>% 
    do.call(bind_rows, .) %>% 
    fwrite(paste0("../data/output/descr/tab_descr_allvars_by", 
                  names(descr_strata_vars.area[z]), ".csv"))  
  
  ## Finished: printing output
  cat(sprintf("Finished. | Saving table to %s \n", paste0(
    path_to_output, "descr/tab_descr_allvars_by", names(descr_strata_vars.area[z]), ".csv"))
  )
  
}) 


################################################################################
## Distributions of AREA-level SES measures by STRATA
################################################################################

strata_intx_vars <- c(strata_vars, intx_vars)

## Check INTERSECTIONS of SES measures across Strata by Education or Income
## Add additional check to SKIP if all vars are INDIV to save RDC time

lapply(seq_along(strata_intx_vars), function(z) {
  intx_res <- lapply(seq_along(ses_exposures), function(x) {
    
    xvar=names(ses_exposures)[x] ; zvar=names(strata_intx_vars)[z]
    cat(sprintf("Summarizing %s | strata ~ %s ... \n", toupper(xvar), toupper(zvar)))
    
    res <- tryCatch({ # skip if exposure is part of the intersection
      if(xvar %in% names(ses_exposures.indiv) & zvar %in% names(strata_intx_vars.indiv)) {
        cat(sprintf("   SKIPPED | Comparisons of all individual-level data were run locally. \n"))
        return(NULL)
      } else {
        if(is.numeric(nhanes_dat %>% pull(ses_exposures[[x]]))) {
          get_nhanes_quantiles.fun(
            ses_exposures.indiv[[x]], strata = strata_intx_vars.indiv[[z]], 
            probs=c(0.05,0.25,0.50,0.75,0.95), data = nhanes_dat)
        } else { 
          get_nhanes_distrib.fun(
            ses_exposures.indiv[[x]], strata = strata_intx_vars.indiv[[z]], 
            data = nhanes_dat) }
      }
    }, error = function(e) {
      # If anything fails, print 'SKIP' an error and return NULL
      message(sprintf("   FAILED | Error details: %s", conditionMessage(e)))
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
           pvalue = intx_res$p_value[1]) %>% 
    select(Intersect, Variable, Level, Var2_level, ends_with("total"), 
           starts_with(c("mean", "se", "teststat")), pvalue) #%>% 
    #fwrite(paste0("../data/output/descr/tab_descr_distrib_indivses_by", names(strata_intx_vars.indiv[z]), ".csv"))
})



################################################################################
## Pearson correlations of indiv and area-level SES measures, overall by STRATA
################################################################################

## Add integer versions of categorical SES exposures
nhanes_dat <- nhanes_dat %>% 
  mutate(
    educ_level.int = as.integer(educ_level),
    inc_to_pov_level.int = as.integer(inc_to_pov_level),
    employ_status.int = as.integer(employ_status))

ses_exposures.corr <- c("educ_level.int", "inc_to_pov_level.int", "inc_to_pov", 
                        "employ_status.int")

## Correlations in total sample ---------------
cor_total.l <- get_nhanes_cormat.df(vars = ses_exposures.corr)
full_join(cor_total.l$corr, cor_total.l$pvals, by = c("strata", "variable")) %>% 
  fwrite("../data/output/descr/tab_descr_corr_ses_bytotal.csv")


## Correlations, by strata ---------------
mclapply(seq_along(strata_descr), function(z) {
  corres <- get_nhanes_cormat.dßf(vars = ses_exposures.corr, strata=strata_descr[[z]]) 
  full_join(corres$corr, corres$pvals, by = c("strata", "level", "variable")) %>% 
    fwrite(paste0("../data/output/descr/tab_descr_corr_ses_by", names(strata_descr)[z], ".csv"))
})



## EOF



## FINAL STEPS;

# * list of pa ckages requied
# * update the saving directory (SC to access scripts on github?)
# * separate the scripts for ind vs area level models
#

## Water fall plot for prevent equation outcomes; each IND is an up or down on waterfal, based on how their prediction changes hen you add SDI
#and color code by race

# HOLD ON THIS: but think about how to consolidate the risk estimates when adding SDI information
# calcualte delta in prevent equation, base to sdi; base to full; did they meet a threshod in each strata
# % pppl in each stratum that met a criterium

# multi-stratify by age, sex, and race/ethn
# JUST THE PREDICTION; how does the prediction change when SDI is added (IRRESPECTIVE of how well it connects to the outcome)
# Just crious about the change in prediction, 

## BUILD-IN CODE TO ADD THE AREA-LEVEL VARIABLES
# AND THE SDI & ChOOSE category**

# take SDI --> assign SDI cat --> assign true predict 


