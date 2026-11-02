# Rscript to run in the RDC to prepare & tabulate descriptive statistics of 
# area-level SES measures in NHANES

path_to_output = "../data/output/RDCtest/"
source("../scripts/analysis/0.run_prep.r")
path_to_output = "../data/output/RDCtest/"


################################################################################
## Additional RDC prep to merge and construct final data sets
################################################################################

#/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/

## TEMPORARY: Separate out SUBSET for PRACTICE runs
set.seed(314159)
nhanes_subset <- sample(x = nhanes_dat$SEQN, size = 10000, replace=F)
nhanes_dat <- nhanes_dat %>% filter(SEQN %in% nhanes_subset)
dim(nhanes_dat) ; table(nhanes_dat$Years)

## Use Income-Level as proxy for SDI decile (LowInc=3, MiddleInc=2, HighInc=1) 
nhanes_dat <- nhanes_dat %>% mutate(
  sdi_decile = case_when(
    inc_to_pov_level == "HighInc" ~ 1,
    inc_to_pov_level == "MiddleInc" ~ 5,
    inc_to_pov_level == "LowInc" ~ 10,
    ))

## TEMPORARY: Create indicator for seqn in acs_ses to merge
acs_ses <- fread("../data/processed/acs_ses_to_merge.csv") %>% 
  filter(complete.cases(.))

nhanes_seqn_tomerge <- nhanes_dat %>% 
  filter(Years %in% c("2009-2010", "2013-2014", "2017-2020")) %>% 
  pull(SEQN)

set.seed(314159)
nhanes_shuffled <- tibble(
  SEQN = sample(nhanes_seqn_tomerge, replace = FALSE),
  row_id = seq_along(SEQN)
)

acs_ses <- acs_ses %>% 
  mutate(row_id = row_number()) %>% 
  left_join(nhanes_shuffled, by = "row_id") %>% 
  select(-row_id) %>% 
  filter(!is.na(SEQN))

# Merge back into acs_ses
head(acs_ses) ; dim(acs_ses)
nhanes_merged <- nhanes_dat %>% full_join(acs_ses, by = "SEQN")
dim(nhanes_merged)
#\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\


## START WITH: nhanes_merged

# ==============================================================================
## Derive PREVENT SDI estimates, based on newly added SDI variable from RDC
# ==============================================================================

# 1. Grab names of all prevent prediction columns
prevent_sdi.vars <- unique(sub("sdi[123]", "sdi", nhanes_merged %>% select(
  starts_with("prevent_") & contains(c("_sdi", "_full.sdi")) & 
    contains("yr"), -contains("zip")) %>% names()))

# 2. Use lapply to derive final prevent_sdi_etc vars
prevent_sdi.dat <- lapply(prevent_sdi.vars, function(var) {
  nhanes_merged %>% 
    select(SEQN, sdi_decile, matches(paste0(gsub("sdi", "sdi[1-3]", var),"$"))) %>%
    rename_with(~ sub("^.*(sdi[1-3]+).*$", "\\1", .x), -SEQN) %>% 
    mutate(
      x = case_when(
        sdi_decile == 1 ~ sdi1,
        sdi_decile == 5 ~ sdi2,
        sdi_decile == 10 ~ sdi3,
        TRUE ~ NA)) %>% 
    select(SEQN, x) %>%
    rename_with(., ~gsub("x", var, .)) 
}) %>% reduce(., full_join, by="SEQN")

# 3. Merge into working dataframe & delete sdi[1-3] vars
nhanes_merged <- nhanes_merged %>% 
  select(-c(contains("prevent") & contains("yr") & contains("sdi"))) %>% 
  left_join(prevent_sdi.dat, by="SEQN")


# ==========================================================
## Re-code AREA-LEVEL SES EXPOSURES as Ref = highest SES
# ==========================================================

urbrur.lvls <- c("urban", "rural")
urbrurcat.lvls <- c("metropolitan", "micropolitan", "town", "rural")

nhanes_merged <- nhanes_merged %>% 
  mutate_at("acs_urbrur", ~factor(., levels=urbrur.lvls)) %>% 
  mutate_at("acs_urbrur_cat", ~factor(., levels=urbrurcat.lvls)) 

# -----------------------------------------------------------------------------
## Add integer-level coding for area-level SES exposures, for correlations
# -----------------------------------------------------------------------------

nhanes_merged <- nhanes_merged %>% 
  mutate(
    acs_urbrur.int = as.integer(acs_urbrur),
    acs_urbrur_cat.int = as.integer(acs_urbrur_cat)
    )

# Add new int vars to list
ses_exposures.corr <- c(ses_exposures.corr, "acs_urbrur.int", "acs_urbrur_cat.int",
                        unname(ses_exposures.area)[-c(1:2)])


# ---------------------------------------------------------
## Add intersection variables for area-level SES exposures 
# ---------------------------------------------------------

nhanes_merged <- nhanes_merged %>% 
  mutate(
    intx_urbruXeduc = case_when(
      !is.na(acs_urbrur) & !is.na(educ_level) ~ paste0(acs_urbrur, "_x_", educ_level),
      TRUE ~ NA),
    intx_urbrucatXeduc = case_when(
      !is.na(acs_urbrur_cat) & !is.na(educ_level) ~ paste0(acs_urbrur_cat, "_x_", educ_level),
      TRUE ~ NA),
    intx_urbruXincpov = case_when(
      !is.na(acs_urbrur) & !is.na(inc_to_pov_level) ~ paste0(acs_urbrur, "_x_", inc_to_pov_level),
      TRUE ~ NA),
    intx_urbrucatXincpov = case_when(
      !is.na(acs_urbrur_cat) & !is.na(inc_to_pov_level) ~ paste0(acs_urbrur_cat, "_x_", inc_to_pov_level),
      TRUE ~ NA),
    intx_urbruXrace = case_when(
      !is.na(acs_urbrur) & !is.na(racethn_combn) ~ paste0(acs_urbrur, "_x_", racethn_combn),
      TRUE ~ NA),
    intx_urbrucatXrace = case_when(
      !is.na(acs_urbrur_cat) & !is.na(racethn_combn) ~ paste0(acs_urbrur_cat, "_x_", racethn_combn),
      TRUE ~ NA),
    intx_urbruXraceXeduc = case_when(
      !is.na(acs_urbrur) & !is.na(racethn_combn) & !is.na(educ_level) ~ paste0(
        acs_urbrur, "_x_", racethn_combn, "_x_", educ_level),
      TRUE ~ NA),
    intx_urbruXraceXincpov = case_when(
      !is.na(acs_urbrur) & !is.na(racethn_combn) & !is.na(inc_to_pov_level) ~ 
        paste0(acs_urbrur, "_x_",racethn_combn, "_x_", inc_to_pov_level), 
      TRUE ~ NA),
    intx_urbruXsexXrace = case_when(
      !is.na(acs_urbrur) & !is.na(gender) & !is.na(racethn_combn) ~ 
        paste0(acs_urbrur, "_x_", gender, "_x_", racethn_combn), 
      TRUE ~ NA),
    intx_urbruXagecatXrace = case_when(
      !is.na(acs_urbrur) & !is.na(age_4lvl) & !is.na(racethn_combn) ~ 
        paste0(acs_urbrur, "_x_", age_4lvl, "_x_", racethn_combn), 
      TRUE ~ NA),
    intx_urbruXsexXagecatXrace = case_when(
      !is.na(acs_urbrur) & !is.na(gender) & !is.na(age_4lvl) & !is.na(racethn_combn) ~ 
        paste0(acs_urbrur, "_x_", gender, "_x_", age_4lvl, "_x_", racethn_combn), 
      TRUE ~ NA)
    ) 


################################################################################
## Descriptive summaries by Age, Sex and Race/Ethnicity 
################################################################################

# ==============================================================================
## TABLE 1s. Descriptive table by AREA-LEVEL exposures/strata for ALL variables
# ==============================================================================

descr_tab1_vars.l <- c(years="Years", 
                       list(strata = c("gender", "age_4lvl", "racethn_combn")), 
                       list(ses = unname(ses_exposures.indiv)),
                       list(ses_area = unname(ses_exposures.area)), 
                       list(descr = addn_table1_vars),
                       c(all_outcomes.l, ndi_outcomes.l)
)

## Summarize all variables by categorical area-level SES exposures
lapply(seq_along(strata_vars), function(z) {
  lapply(seq_along(descr_tab1_vars.l), function(x) {
    
    xvar=names(descr_tab1_vars.l)[x] ; zvar=names(strata_vars)[z]
    zvar.lab = str_to_sentence(names(strata_vars)[z])
    cat(sprintf("DESCRIBE | Var ~ Strata | %s ~ %s \n", xvar, zvar))
    
    tryCatch({
      build_nhanes_summarytable.fun(
          vars_to_summarise = descr_tab1_vars.l[[x]],
          strata = strata_vars[[z]], subsets = alist(age >= 18),
          flag_low_n = 10, digits=3, data = nhanes_merged) %>% 
        rename_with(., ~gsub("Total", zvar.lab, .))
    }, error = function(e) {
      message(sprintf("FAILED:  %s", conditionMessage(e))) ; return(NULL)
    })
    
  }) %>% do.call(bind_rows, .) %>% 
    fwrite(paste0(path_to_output, "descr/table1_descr_allvars_by", 
                  names(strata_vars[z]), ".csv"))  
  
  ## Finished: printing output
  cat(sprintf("DONE | Saving table to %s \n", paste0(
    path_to_output, "/descr/table1_descr_allvars_by", 
    names(strata_vars[z]), ".csv")))
  
}) 


################################################################################
## TABLEs 2-3. Distributions of indiv & area-level exposures by STRATA & INTX
################################################################################

# ======================================================================
## Table 2. Summarize distributions of all SES variables by all STRATA
# ======================================================================
lapply(seq_along(strata_vars), function(z) { # For each strata ... 
  
  output_file <- paste0(path_to_output, "/descr/table2_distr_ses_by", 
                        names(strata_vars[z]), ".csv")
  
  lapply(seq_along(ses_exposures), function(x) { # For each SES exposure ... 
    
    xvar=names(ses_exposures)[x] ; zvar=names(strata_vars)[z]
    cat(sprintf("DESCRIBE | ses ~ strata | %s ~ %s ... \n", xvar, zvar))
    
    # SKIP if exposure & strata are BOTH individual-level
    #if(xvar %in% names(ses_exposures.indiv) & zvar %in% names(strata_vars.indiv)) {
    #  cat(sprintf("   SKIP %s ~ %s | Run indiv-level vars, locally. \n", xvar,zvar))
    #} else {
    
    tryCatch({  
      if(is.numeric(nhanes_merged %>% pull(ses_exposures[[x]]))) {
        get_nhanes_quantiles.fun(var_to_summarise = ses_exposures[[x]], 
                                  strata = strata_vars[[z]], 
          subsets = alist(age >= 18),
          probs=c(0.05,0.25,0.50,0.75,0.95), data = nhanes_merged)
      } else { 
        get_nhanes_distrib.fun(
          ses_exposures[[x]], strata = strata_vars[[z]], 
          subsets = alist(age>=18), data = nhanes_merged) 
      }
    }, error = function(e) {
      message(sprintf("   SKIPPED | %s", conditionMessage(e))) 
      return(NULL) 
    })
  #}
  }) %>% bind_rows() %>% 
    fwrite(output_file)
  
  ## Finished: printing output
  cat(sprintf("    FINISHED | Saving table to %s \n", output_file))
  
})


# ==============================================================================
## Summarize distributions of all SES vars by INTERSECTIONS of strata x Educ/Inc
# ==============================================================================

lapply(seq_along(intx_vars), function(z) { # For each intersection ... 
  
  # Output file name
  output_file <- paste0(path_to_output, "/descr/table3_distr_ses_by", 
                        names(intx_vars[z]), ".csv")
  
  intx_res <- lapply(seq_along(ses_exposures), function(x) { # For each SES exposure ... 
    
    xvar=names(ses_exposures)[x] ; zvar=names(intx_vars)[z]
    cat(sprintf("DESCRIBE | ses ~ strataXses | %s ~ %s ... \n", xvar, zvar))
    
    tryCatch({ 
      
      # skip if exposure is part of the intersection
      #if(xvar %in% names(ses_exposures.indiv) & zvar %in% names(intx_vars.indiv)) {
      #  cat(sprintf("   SKIP | Run indiv-level comparisons, locally. \n"))
      #  return(NULL)
      #} else {
      
        if(is.numeric(nhanes_merged %>% pull(ses_exposures[[x]]))) {
          get_nhanes_quantiles.fun(
            ses_exposures[[x]], strata = intx_vars[[z]], 
            subsets = alist(age >= 18),
            probs=c(0.05,0.25,0.50,0.75,0.95), data = nhanes_merged)
        } else { 
          get_nhanes_distrib.fun(
            ses_exposures[[x]], strata = intx_vars[[z]], 
            subsets = alist(age>=18), data = nhanes_merged) 
          }
      #}
    }, error = function(e) {
      message(sprintf("   FAILED | %s", conditionMessage(e)))
      return(NULL) 
    }) 
  }) %>% bind_rows()
  
  # Check for ≥3-lvl intersections
  intx_lvls <- (length(gregexpr("X", names(intx_vars[z]))[[1]]))+1
  
  # Reformat intersection output
  intx_res %>% 
    pivot_longer(cols = -c(Variable, Level, p_value, p_method, contains(c("teststat", "total"))),
                 names_sep="_x_", names_to=c(
                   "msr_Level", paste0("Intx", 2:intx_lvls, "_Level"))) %>%
    separate("msr_Level", into = c("msr", "Intx1_Level")) %>%
    unite("msr_Level", c(msr, paste0("Intx", intx_lvls, "_Level")), sep="_") %>% 
    rename(Variable_Level = Level) %>% 
    pivot_wider(names_from=msr_Level) %>%
    mutate(Intersect = names(intx_vars)[z], .before=1) %>% 
    select(
      Intersect, Variable, Variable_Level, ends_with("Level"), ends_with("total"), 
      starts_with(c("mean", "se", "teststat", "p_"))) %>% 
    ## Save output
    fwrite(output_file)
  
  ## Finished: printing output
  cat(sprintf("   Finished | Saving table to %s \n", output_file))
  
})


################################################################################
## TABLE 3. Pearson correlations of area-level SES measures, overall by STRATA
################################################################################

## Correlations in total sample ---------------
cor_total.l <- get_nhanes_cormat.fun(vars=ses_exposures.corr, data=nhanes_merged,
                                     method = "pearson")

full_join(cor_total.l$corr, cor_total.l$pvals, by = c("level", "variable")) %>% 
  fwrite(paste0(path_to_output, "/descr/table4_cormat_allses_bytotal.csv"))


## Correlations, by strata ---------------
mclapply(seq_along(strata_vars), function(z) {
  corres <- get_nhanes_cormat.fun(vars = ses_exposures.corr, strata=strata_vars[[z]],
                                  method = "pearson", data = nhanes_merged)
  full_join(corres$corr, corres$pvals, by = c("strata", "level", "variable")) %>% 
    fwrite(paste0(
      path_to_output, "/descr/table4_corr_allses_by", 
      names(strata_vars)[z],".csv")
      )
})


################################################################################
## TABLE [8]. Summary of PREVENT estimates, before/after adding SDI, by strata
################################################################################

# ==============================================================================
## I) Summarize prevent estimates, when -/+ SDI, by strata & intx,in FULL cohort
# ==============================================================================

prevent_outcomes <- list(
  "10yr_CVD", "30yr_CVD", "10yr_ASCVD", "30yr_ASCVD", 
  "10yr_HF", "30yr_HF") ; prevent_vars.l <- lapply(
    prevent_outcomes, function(y) {
      nhanes_merged %>% select(contains(y)) %>% select(
        ends_with(y), matches("diff|class")) %>% names()
      }) ; names(prevent_vars.l) <- prevent_outcomes 

prevent_vars.l

# ---------------------------------------------------------
## Build summary table by all strata & intx in full cohort
# ---------------------------------------------------------

# For each STRATA 
lapply(seq_along(strata_intx_vars), function(z) {
  
  # 1. Set strata & output file names
  strata <- unname(strata_intx_vars)[z]
  Strata <- names(strata_intx_vars[z])
  output_file <- paste0(path_to_output, "/descr/table8_descr_prevest_by", Strata, ".csv")
  
  # 2. Describe EACH prevent VAR
  lapply(seq_along(prevent_vars.l), function(x) {
    
    descrvars = prevent_vars.l[[x]]
    Descrvars = names(prevent_vars.l)[x] 
    cat(sprintf("DESCRIBE | prevent_est ~ strata | %s ~ %s \n", Descrvars, Strata))
    
    # a. Build summary table 
    build_nhanes_summarytable.fun( 
      vars_to_summarise = descrvars, strata = strata, d=3, 
      subsets = alist(age >= 18), data = nhanes_merged)
    
  }) %>% 
    
  # 3. For each STRATA, combine & save table
  do.call(bind_rows, .) %>% 
  fwrite(output_file)  
  
  ## Finished: printing output_file
  cat(sprintf("   Finished | Saving table to %s \n", output_file))
  
})

# -----------------------------------------------------------
## Summarize risk differences by strata & intersections
# -----------------------------------------------------------

prevent_riskdiff_vars <- list(
  "10yr_CVD", "30yr_CVD", "10yr_ASCVD", "30yr_ASCVD", 
  "10yr_HF", "30yr_HF") ; prevent_riskdiff.l <- lapply(
    prevent_outcomes, function(y) {
      nhanes_merged %>% select(contains(y)) %>% select(
        matches("diff")) %>% names()
    }) ; names(prevent_riskdiff.l) <- prevent_riskdiff_vars 

prevent_riskdiff.l


## For each STRATA/INTx
lapply(seq_along(strata_intx_vars), function(z) { # For each strata ... 
  
  # 1. Set strata & output file names
  strata <- unname(strata_intx_vars)[z]
  Strata <- names(strata_intx_vars[z])
  output_file <- paste0(path_to_output, "descr/table8_distr_prevest_diffs_by", Strata, ".csv")
  
  # 2. Describe EACH SET of prevent VARs
  distrib <- lapply(seq_along(prevent_riskdiff.l), function(x) {
    
    descrvars = prevent_riskdiff.l[[x]]
    Descrvars = names(prevent_riskdiff.l)[x] 
    cat(sprintf("DISTRIBUTIONS | prevent_est ~ strata | %s ~ %s \n", Descrvars, Strata))
    
    # a. Get quantiles for each VAR
    lapply(descrvars, function(descrvar) {
      tryCatch({
        get_nhanes_quantiles.fun(descrvar, strata = strata, 
                                 probs=c(0.05,0.25,0.50,0.75,0.95), d=3,
                                 subsets = alist(age >= 18), data = nhanes_merged)
        }, error = function(e) {
          message(sprintf("   SKIPPED | %s", conditionMessage(e))) 
          return(NULL) 
        })
      
    # Combine output for each set of VARs & each STRATA
    }) %>% bind_rows() 
  }) %>% bind_rows() 
  
  # 3. Format output table
  ## a. Check if intersections has >2 levels,
  check_intx <- gregexpr("X", strata, ignore.case = FALSE)[[1]]
  n_intx <- if (check_intx[1] == -1) 0 else length(check_intx)+1
  
  ## b. If intx >2 levels, 
  if(n_intx == 0) distrib else {
    
    # If table could not be created, due to to small Ns,
    if(nrow(distrib) == 0 & ncol(distrib) == 0) {
      cat(sprintf("    FINISHED with ERROR | Insufficient data to save table %s \n", output_file))
      
    } else {
      
      # Reformat output, so 1 intersection spans the columns; others span rows 
      distrib %>% 
        pivot_longer(cols = -c(Variable, Level, p_value, p_method, 
                               contains(c("test", "total"))),
                     names_sep="_x_", names_to=c(
                       "msr_Level", paste0("Intx", 2:n_intx, "_Level"))) %>%
        separate("msr_Level", into = c("msr", "Intx1_Level")) %>%
        unite("msr_Level", c(msr, paste0("Intx", n_intx, "_Level")), sep="_") %>% 
        rename(Variable_Level = Level) %>% 
        pivot_wider(names_from=msr_Level) %>%
        mutate(Intersect = Strata, .before=1) %>% 
        select(
          Intersect, Variable, Variable_Level, ends_with("Level"), ends_with("total"), 
          starts_with(c("mean", "se", "teststat", "p_"))) %>% 
        
        ## 4. Save output table, for each STRATA
        fwrite(output_file)
    }
  }

  ## Finished: printing output
  cat(sprintf("    FINISHED | Saving table to %s \n", output_file))
  
})


# ==============================================================================
## II)  Re-run PREVENT descriptive analysis, in SUBSET using htn/dyslip guidelines
# ==============================================================================

prevent_subsets.l <- list(
  use_htn=alist(age >= 18 & prevent_use_htn ==1),
  use_dyslip=alist(age >= 18 & prevent_use_dyslip == 1)
)

# ---------------------------------------------------------
## Build summary table by all strata & intx in full cohort
# ---------------------------------------------------------

# For each STRATA 
lapply(seq_along(strata_intx_vars), function(z) {
  
  # 1. Set strata & output file names
  strata <- unname(strata_intx_vars)[z]
  Strata <- names(strata_intx_vars[z])
  output_file <- paste0(path_to_output, "/descr/table8_descr_prevdescr_by", 
                        Strata, "_subsetsGL.csv")
  
  # 2. For each prevent SUBSET
  lapply(seq_along(prevent_subsets.l), function(j) {
    
    Subset = names(prevent_subsets.l)[[j]] 
    subset = unname(prevent_subsets.l)[[j]]
    
    # 3. For each set of VARs
    lapply(seq_along(prevent_vars.l), function(x) {
    
      descrvars = prevent_vars.l[[x]]
      Descrvars = names(prevent_vars.l)[x] 
      cat(sprintf("DESCRIBE | prevent_est [subset] ~ strata | %s [%s] ~ %s \n", 
                  Descrvars, Subset, Strata))
      
      # Build summary table --------------------------
      build_nhanes_summarytable.fun(descrvars, strata, subsets = subset, d=3,
                                   data = nhanes_merged) %>% 
        mutate(Subset = paste(prevent_subsets.l[j]), .before=1)
     
    # Combine output, across VAR sets 
    }) %>% do.call(bind_rows, .)
  
  # Combine output, across both subsets 
  }) %>% do.call(bind_rows, .) %>%
    
  # 4. For each STRATA, combine & save output
  fwrite(output_file)  

  ## Finished: printing output_file
  cat(sprintf("   Finished | Saving table to %s \n", output_file))
  
})


# -----------------------------------------------------------
## Summarize risk differences by strata & intersections
# -----------------------------------------------------------

## For each STRATA/INTx
lapply(seq_along(strata_intx_vars), function(z) { # For each strata ... 
  
  # 1. Set strata & output file names
  strata <- unname(strata_intx_vars)[z]
  Strata <- names(strata_intx_vars[z])
  output_file <- paste0(path_to_output, "descr/table8_distr_prevest_diffs_by", 
                        Strata, "_subsetsGL.csv")
  
  # 2. For each prevent SUBSET
  lapply(seq_along(prevent_subsets.l), function(j) {
    
    Subset = names(prevent_subsets.l)[[j]] 
    subset = unname(prevent_subsets.l)[[j]]
    
    # 3. Describe EACH SET of prevent VARs
    distrib <- lapply(seq_along(prevent_riskdiff.l), function(x) {
      
      descrvars = prevent_riskdiff.l[[x]]
      Descrvars = names(prevent_riskdiff.l)[x] 
      cat(sprintf("DISTRIBUTIONS | prevent_est [subset] ~ strata | %s [%s] ~ %s \n", 
                  Descrvars, Subset, Strata))
      
      # 4. Summarize quantiles for EACH var
      lapply(descrvars[1:2], function(descrvar) {
        tryCatch({
          get_nhanes_quantiles.fun(descrvar, strata, subsets = subset,
                                   probs=c(0.05,0.25,0.50,0.75,0.95), d=3,
                                   data = nhanes_merged)
          }, error = function(e) {
            message(sprintf("   SKIPPED | %s", conditionMessage(e))) 
            return(NULL) 
          })
        
      # Combine output for each set of VARs & each STRATA
      }) %>% bind_rows()
    }) %>% bind_rows() 
    
    # 3. Format output table
    ## a. Check if intersections has >2 levels,
    check_intx <- gregexpr("X", strata, ignore.case = FALSE)[[1]]
    n_intx <- if (check_intx[1] == -1) 0 else length(check_intx)+1
    
    ## b. If intx >2 levels, 
    if(n_intx == 0) distrib else {
      
      # If table could not be created, due to to small Ns,
      if(nrow(distrib) == 0 & ncol(distrib) == 0) {
        cat(sprintf("    FINISHED with ERROR | Insufficient data to save table %s \n", output_file))
        
      } else {
        
        # Reformat output, so 1 intersection spans the columns; others span rows 
        distrib %>% 
          pivot_longer(cols = -c(Variable, Level, p_value, p_method, 
                                 contains(c("test", "total"))),
                       names_sep="_x_", names_to=c(
                         "msr_Level", paste0("Intx", 2:n_intx, "_Level"))) %>%
          separate("msr_Level", into = c("msr", "Intx1_Level")) %>%
          unite("msr_Level", c(msr, paste0("Intx", n_intx, "_Level")), sep="_") %>% 
          rename(Variable_Level = Level) %>% 
          pivot_wider(names_from=msr_Level) %>%
          mutate(Intersect = Strata, .before=1) %>% 
          select(
            Intersect, Variable, Variable_Level, ends_with("Level"), ends_with("total"), 
            starts_with(c("mean", "se", "teststat", "p_")))
      }
    }
  }) %>% bind_rows() %>%
    
  # For each STRATA, save output file
  fwrite(output_file)
    
  ## Finished: printing output
  cat(sprintf("    FINISHED | Saving table to %s \n", output_file))
  
})


# -----------------------------------------------------------
## Prepare boxplots of PREVENT changes, by strata
# -----------------------------------------------------------

# Reshape for plotting
prevent_sdidiff_dist_toplot <- prevent_sdidiff_all_dist.df %>% 
  mutate(Quantile = paste0("p", as.numeric(quantile) * 100),
         Outcome = gsub(".*yr_", "", gsub("_diff.*", "", Variable)),
         Time = gsub(".*_", "", gsub("yr_.*", "yr", Variable)),
         Model = str_to_sentence(gsub("_.*", "", gsub("prevent_", "", Variable)))) %>% 
  # note: Select mean or mean_total depending on which variable you want to plot
  select(Model, Outcome, Time, Strata, Level = stratalvl, Quantile, mean) %>% 
  pivot_wider(names_from = Quantile, values_from = mean) %>% 
  mutate_at("Strata", ~factor(., levels = c(
    "sex", "agecat", "racethn", "educ", "incpovlvl", "employ", "foodinsec", "sexXagecatXrace"))) %>% 
  mutate(intxlvl = ifelse(Strata == "sexXagecatXrace", Level, NA)) %>% 
  mutate_at("Level", ~factor(., levels = c(
    "Female", "Male", 
    "30to39y", "40to64y", "over65y", ## Missing under30y ??
    "NHW", "NHB",  "MexAm", "OthHis", "Oth/Mult/NHA", "NHA", "Oth/Mult",
    "HighInc", "MiddleInc", "LowInc",
    "LessThan9th", "SomeHS", "HSgrad", "SomeColl", "Collgrad",
    "Full-time employed", "Part-time employed", "Student", "Unemployed",
    "0", "1")
  ))


## Generate TEST Plots
prevent_sdidiff_dist_toplot %>% 
  filter(Model == "Base" & Time == "10yr" & Strata != "sexXagecatXrace") %>%
  distinct(Outcome, Time, Strata, Level, .keep_all = TRUE) %>%
  ggplot(aes(x = Level, fill = Level)) +
  facet_grid(Outcome ~ Strata, space="free_x", scale="free_x") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") + 
  geom_boxplot(aes(ymin = p5, lower = p25, middle = p50, upper = p75, ymax = p95,
                   group = interaction(Level, Strata)), 
               stat = "identity", 
               position = position_dodge(0.4), width=0.55) +
  scale_x_discrete(expand = expansion(add = c(1.25, 1.25))) +
  scale_fill_manual(values = c(
    palettes$NatExt$Oranges[c(4,6)], palettes$NatExt$Purples[c(5,4,3)],
    palettes$NatExt$Blues[4:2], palettes$NatExt$Teals[5:2],
    palettes$NatExt$Greens[c(3,4,5)], palettes$NatExt$Reds[2:6],
    palettes$NatExt$Yellows[2:5], palettes$NatExt$Olives[c(4,6)])) +
  ggplot_theme +
  guides(fill = "none") + # <-- Removes the legend for fill / Level
  theme(axis.text.x = element_text(angle = 45, color = "black", 
                                   hjust=1, vjust = 1),
        panel.border = element_blank(),
        panel.spacing.x = unit(0, "mm"), 
        panel.grid = element_blank()) +
  labs(
    title = "Boxplot from Pre-calculated Summary Statistics",
    x = "Variable",
    y = "Change in Estimate (Base - Base+SDI)"
  )



## SECOND test plot, of intersection: sexXageXrace
prevent_sdidiff_dist_toplot %>% 
  filter(Model == "Base" & Time == "10yr" & Strata == "sexXagecatXrace") %>%
  distinct(Outcome, Time, Strata, intxlvl, .keep_all = TRUE) %>%
  mutate(Sex=gsub("x.*","",intxlvl),
         Race=gsub(".*yx", "", intxlvl)) %>%
  ggplot(aes(x = intxlvl, fill = intxlvl)) +
  facet_grid(Outcome ~ Race, space="free_x", scale="free_x") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") + 
  geom_boxplot(aes(ymin = p5, lower = p25, middle = p50, upper = p75, ymax = p95,
                   group = interaction(intxlvl)), 
               stat = "identity", 
               position = position_dodge(0.4), width=0.55) +
  scale_x_discrete(expand = expansion(add = c(1.25, 1.25))) +
  #scale_fill_manual(values = c(
  #  palettes$NatExt$Oranges[c(4,6)], palettes$NatExt$Purples[c(5,4,3)],
  #  palettes$NatExt$Blues[4:2], palettes$NatExt$Teals[5:2],
  #  palettes$NatExt$Greens[c(3,4,5)], palettes$NatExt$Reds[2:6],
  #  palettes$NatExt$Yellows[2:5], palettes$NatExt$Olives[c(4,6)])) +
  ggplot_theme +
  guides(fill = "none") + # <-- Removes the legend for fill / Level
  theme(axis.text.x = element_text(angle = 45, color = "black", 
                                   hjust=1, vjust = 1),
        panel.border = element_blank(),
        panel.spacing.x = unit(0, "mm"), 
        panel.grid = element_blank()) +
  labs(
    title = "Boxplot from Pre-calculated Summary Statistics",
    x = "Variable",
    y = "Change in Estimate (Base - Base+SDI)"
  )


## EOF



## FINAL STEPS;

## Water fall plot for prevent equation outcomes; each IND is an up or down on waterfal, based on how their prediction changes hen you add SDI
#and color code by race

# HOLD ON THIS: but think about how to consolidate the risk estimates when adding SDI information
# calcualte delta in prevent equation, base to sdi; base to full; did they meet a threshod in each strata
# % pppl in each stratum that met a criterium

# multi-stratify by age, sex, and race/ethn
# JUST THE PREDICTION; how does the prediction change when SDI is added (IRRESPECTIVE of how well it connects to the outcome)
# Just crious about the change in prediction, 

## 
