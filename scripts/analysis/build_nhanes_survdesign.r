# Rscript for applying survey weights and running regressions in NHANES

 
################################################################################
## Set Up; Load Required Packages & Data Files
################################################################################

## set local directory
#setwd('C:/Users/sarac/OneDrive/Documents/Research')
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


## Load nhanes data & prevent variables
nhanes_dat <- readRDS("../data/processed/nhanes_postprocessed_linked_ndi_prvnt_sdi.rds")

## Load required library
library(survey)


################################################################################ 
## Build function to create NHANES summary tables
################################################################################ 

## ===================================================
## Write function to select correct variable weight
## ===================================================

find_nhanes_weight.fun <- function(variables, data = nhanes_dat) {
  weight_order <- paste0(c("WTSEPH", "WTSPO", "WTSAF", "WTDR", "WTMEC", "WTINT"), ".COMBN")
  weights_compl <- data %>% 
    select(starts_with("WT"), -wt, -WTYRS, all_of(variables)) %>% 
    drop_na(all_of(variables)) %>% # filter(complete.cases(VAR)) %>% 
    select(where(~ !any(is.na(.))) & starts_with("WT")) %>% 
    names() ; intersect(weight_order, weights_compl)[1] 
}


## b. Calculate Total Years represented
# * 1999-2002 = 1 set of 4yr weights = 4 years
# * 2003-2016 = 7 standard 2-year cycles = 14 years
# * 2017-March 2020 = 1 pre-pandemic cycle = 3.2 years
# * [2021-2023 = 1 post-pandemic 2 year cycle = 2 years]
# TOTAL YEARS = 23.2 years; i.e. To calculate var weights:
# weight function: WTX.COMBN * (3.2 / total_years)
# --> else(), WTX_COMBN * (2 / total_years)

# Note: WTMEC2YR is the 2-year MEC weight. If combining multiple cycles, 
# adjust weights by dividing WTMEC2YR by the number of cycles (e.g., / 3 for 3 cycles).

build_nhanes_summarytable.fun <- function(vars_to_summarise, strata, 
                                          data = nhanes_dat, digits=1) {
  
  # Generate survey deisgn & summary stats separately, by variable
  sumtab.l <- lapply(1:length(vars_to_summarise), function(x) {
    
    # Set variable & check if numeric or otherwise
    var_input <- vars_to_summarise[x]
    if(!is.null(names(var_input))) {
      var_name <- var_input[[1]] ; var <- names(var_input)[1]
    } else {
      var_name <- var_input ; var <- var_input
    }
    
    n_values <- length(na.omit(unique(data[[var]])))
    var_type <- if(is.numeric(data[[var]]) & n_values != 2) "numeric" else if (
      n_values == 2) "binary" else "categorical"
    
    # 1. Get variable weight
    var_weight <- find_nhanes_weight.fun(var) 
    var_complete <- data %>% select(SEQN, YEARS=Years, nYRS, VAR=var, AGE=age, STRATA=strata, 
                                    SDMVPSU, SDMVSTRA, WEIGHT=var_weight, WTYRS) %>%
      mutate(TOTAL="Total") %>% filter(complete.cases(.)) 
      
    if(var_type == "binary" & is.numeric(var_complete$VAR)) {
      var_complete <- var_complete %>% mutate(
        VAR = factor(VAR))
    }
  
    # 2. Count number of represented years
    cycle_yrs <- var_complete %>% select(YEARS, nYRS) %>% unique() 
    cycles <- cycle_yrs$YEARS ; n_cycles <- length(unique(cycles))
    n_yrs <- sum(cycle_yrs$nYRS)
    yr_range <- as.numeric(c(gsub("-.*","", min(cycles)), gsub(".*-","", max(cycles))))
  
    # 3. Adjust weight, based on n_years
    var_complete <- var_complete %>% 
      mutate(WEIGHT_adj = WEIGHT*(WTYRS/n_yrs))
    
    # 4. Build survey object
    var_survdesign <- svydesign(id = ~SDMVPSU, strata = ~SDMVSTRA, 
      weights = ~WEIGHT_adj, nest = TRUE, data = var_complete)
     
      # Subset survey object to age >18
      var_survdesign_gt18 <- subset(var_survdesign, AGE >= 18)
      
    # 5. Summarize mean (se) and % (se) by strata
    strata_levels <- unique(var_complete$STRATA)
    var_sum <- rbind.data.frame(
      svyby(~VAR, by = ~TOTAL, design  = var_survdesign_gt18, FUN = svymean, na.rm = TRUE) %>%
        rename(STRATA=TOTAL),
      svyby(~VAR, by = ~STRATA, design  = var_survdesign_gt18, FUN = svymean, na.rm = TRUE)
      )
    
    # ----------------------------------------------
    ## For numeric/continuous variables 
    # ----------------------------------------------
    
    if(var_type == "numeric") {
      # Tabulate Raw and Weighted Ns, per STRATA
      n_sum <- var_survdesign_gt18$variables %>% 
        group_by(STRATA) %>% 
        summarise(n_raw = n(), n_weight = sum(WEIGHT_adj[WEIGHT_adj > 0])) %>% 
        add_row(STRATA="Total", n_raw = sum(.$n_raw),
                n_weight=sum(.$n_weight), .before=1)
      var_summary <- var_sum %>% rename(mean=VAR) %>% 
        mutate(Variable = var_name, .after=STRATA) %>%
        mutate(sum = sprintf("%s \u00B1 %s", round(mean, digits), round(se,digits))) %>% 
        select(STRATA, Variable, sum, starts_with("n_")) %>% 
        # merge in n_sum -----------------------------
        left_join(n_sum, by = c("STRATA")) %>% 
        pivot_wider(names_from="STRATA", values_from=-c(STRATA, Variable)) %>% 
        rename_with(., ~gsub("sum_","",.))
    
    } else {
      
      # ----------------------------------------------
      ## For factor/categorical variables 
      # ----------------------------------------------
      
      # Tabulate Raw and Weighted Ns, per STRATA and VAR category
      n_sum <- bind_rows(
        var_survdesign_gt18$variables %>% 
          group_by(VAR) %>% 
          summarise(n_raw = n(), n_weight = sum(WEIGHT_adj[WEIGHT_adj > 0]),
                    .groups="drop") %>% 
          mutate(STRATA="Total", .before=1), 
        var_survdesign_gt18$variables %>% 
          group_by(STRATA, VAR) %>% 
          summarise(n_raw = n(), n_weight = sum(WEIGHT_adj[WEIGHT_adj > 0]),
                    .groups="drop")) %>%
        rename(Variable=VAR)
      
      # Combine & format output table, including n_sum
      var_summary <- var_sum %>% 
        rename_with(., ~gsub("VAR","", .)) %>% 
        rename_at(vars(-STRATA, -starts_with("se")), ~ paste0("mean.", .x)) %>%
        pivot_longer(cols = -STRATA, names_sep="[.]", names_to=c("msr","Variable")) %>%
        pivot_wider(names_from=msr) %>% 
        mutate(across(c(mean, se), ~.*100)) %>%
        mutate(sum = sprintf("%s (%s)", round(mean,digits), round(se,digits))) %>% 
        select(STRATA, Variable, sum) %>% 
        # merge in n_sum -----------------------------
        left_join(n_sum, by = c("STRATA", "Variable")) %>% 
        pivot_wider(names_from="STRATA", values_from=-c(STRATA, Variable)) %>%
        mutate_at("Variable", ~paste0(" ", .)) %>%
        add_row(Variable = var_name, .before = 1) %>%
        mutate(across(-Variable, ~ifelse(is.na(.), "", .))) %>% 
        rename_with(., ~gsub("sum_","",.))

    return(var_summary)
    }
    
  })
  
  sumtab_df <- do.call(rbind.data.frame, sumtab.l) %>% as.data.frame() %>%
    mutate(across(starts_with("n_"), ~ifelse(
      . == "", "", round(as.numeric(.), 0))) )
  return(sumtab_df)
  
}



################################################################################ 
## Build function to generate survey object (for glms)
################################################################################ 

build_nhanes_survdesign.fun <- function(vars_to_include, data = nhanes_dat, strata=NULL) {
  
  # 1. Get variable weight
  varweight <- find_nhanes_weight.fun(c(vars_to_include, strata))
  vars_complete <- data %>% select(
    SEQN, Years, nYRS, WTYRS, SDMVPSU, SDMVSTRA, STRATA=strata, age,
    WEIGHT=all_of(varweight), all_of(vars_to_include)) %>%
    mutate(AGE=age) %>% 
    filter(complete.cases(.))
  
  # 2. Count number of represented years
  cycle_yrs <- vars_complete %>% select(Years, nYRS) %>% unique()
  cycles <- cycle_yrs$Years ; n_yrs <- sum(cycle_yrs$nYRS); n_cycles <- length(cycles)
  yr_range <- as.numeric(c(gsub("-.*","", min(cycles)), gsub(".*-","", max(cycles))))
  survdesign_cycles <- cbind.data.frame(cycles=paste0(yr_range, collapse="-"), n_yrs, n_cycles)
  
  # 3. Adjust weight, based on n_years
  vars_complete <- vars_complete %>% 
    mutate(WEIGHT_adj = WEIGHT * (WTYRS / n_yrs))
  
  # 4. Build survey object
  ## If no strata: 
  vars_survdesign <- svydesign(
    id = ~SDMVPSU, strata = ~SDMVSTRA, 
    weights = ~WEIGHT_adj, nest = TRUE, data = vars_complete)
  
  # Subset survey object to age >18
  vars_survdesign_gt18 <- subset(vars_survdesign, AGE >= 18)
  vars_survdesign_gt18_dat <- vars_survdesign_gt18$variables %>% 
    filter(weights(vars_survdesign_gt18) > 0) # Same as $variables$WEIGHT_adj
  
  # Extract raw and weighted sample sizes & model degrees of freedom
  n_raw <- nrow(vars_survdesign_gt18$cluster)
  n_weighted <- sum(vars_survdesign_gt18$variables$WEIGHT_adj)
  survdf <- degf(vars_survdesign_gt18)
  survdesign_sample <- cbind.data.frame("n_raw"=n_raw, "n_weighted"=n_weighted, df=survdf)
  
  return(list(
    survdesign = vars_survdesign_gt18, 
    survdesign_data = vars_survdesign_gt18_dat,
    survdesign_weight = varweight, 
    survdesign_sample = survdesign_sample,
    survdesign_cycles = survdesign_cycles)
  )
  
}
 
   
###########################################################
## Build function to run and summarize survey glms
###########################################################

run_survdesign_glm.fun <- function(exposure, outcome, covariates, strata=NULL, data = nhanes_dat) { 
  
  # List model variables
  vars_to_include <- unique(c(exposure, outcome, covariates, strata))
  run_by_strata <- ifelse(!is.null(strata), T, F) 
  
  ## Construct NHANES survey design, using an embedded function to select custom WEIGHT
  nhanes_survdesign <- build_nhanes_survdesign.fun(vars_to_include, data, strata = strata)
  survdesign <- nhanes_survdesign$survdesign
  survweight <- nhanes_survdesign$survdesign_weight 
  survcycles <- nhanes_survdesign$survdesign_cycles
  survsample <- nhanes_survdesign$survdesign_sample
  
  ## Grab strata levels, if provided (otherwise, default to )
  if(run_by_strata) {
    strata_levels <- na.omit(unique(survdesign$variables$STRATA))
  } else {
    strata_levels <- "none"
  }
  
  # Initiate lists for storing glm summaries and models 
  survglm_summary.l <- list()
  survglm_model.l <- list()
  
  for (lvl in strata_levels) {
    
    # If provided, subset survdesign by strata level; if not, use full sample
    if(run_by_strata) {
      survdesign_use <- subset(survdesign, survdesign$variables$STRATA == lvl)
      cat(sprintf("REGRESS [%s = %s] | %s ~ %s | %s | %s ...", strata, lvl, outcome, 
                  exposure, paste0(covariates, collapse = "+"), survcycles$cycles))
    } else {
      survdesign_use <- survdesign
      cat(sprintf("REGRESS [Full sample] | %s ~ %s | %s | %s ...", outcome, 
                  exposure, paste0(covariates, collapse = "+"), survcycles$cycles))
    }
    
    # Initiate TryCatch for any modeling errors
    result_lvl <- tryCatch({
    
    ## Build model formula
    model_formula <- formula(paste0(outcome,"~", exposure,"+", paste0(covariates, collapse = "+")))
    
    # Determine model family type 
    outcome_vals <- na.omit(data[[outcome]])
    is_binary <- length(unique(outcome_vals)) == 2 || is.factor(outcome_vals) || is.logical(outcome_vals)
  
    # Ensure binary outcomes are coded as 0/1 (not no/yes)
    if(is_binary & sum(unique(outcome_vals) %in% c(0,1)) != 2) {
      survdesign_use$variables[[outcome]] <- as.numeric(
          as.factor(survdesign_use$variables[[outcome]])) - 1
      }
  
    fam <- if (is_binary) quasibinomial() else gaussian()
    
    ## Fit survey glm & perform F-test for categorical exposures 
    survglm_fit <- suppressWarnings(
      svyglm(model_formula, design = survdesign_use, family = fam)
    ) ; f_test <- list(Ftest=NA, p=NA)
    
    # Extract counts & degrees of freedom
    exposure_vals <- survdesign_use$variables[[exposure]]
    is_categorical <- is.factor(exposure_vals) || is.character(exposure_vals)
    
    f_test <- list(Ftest = NA, p = NA)
    
    if (is_categorical) {
      f_test <- regTermTest(survglm_fit, test.terms = exposure, method = "Wald")
    
      # Tabulate raw and unweighted sample size overall, and by exposure category
      survglm_n <- survdesign_use$variables %>% 
        group_by(across(exposure)) %>% 
        summarise(n_exp_raw = n(), n_exp_weighted = sum(WEIGHT_adj),
                  .groups = "drop") %>% 
        rename("exp_level" = exposure)
    
      n_levels <- length(unique(exposure_vals))
      survglm_summary <- summary(survglm_fit)$coef[1:n_levels,c(1:2,4)] %>% 
        as.data.frame() %>% 
        mutate(fstat = f_test$Ftest, f_pval = f_test$p) %>% 
        rownames_to_column(var="exp_level") %>% 
        mutate_at("exp_level", ~gsub(
          "[(]Intercept[)]", levels(exposure_vals)[1], gsub(exposure, "", .)))
    
    } else { 
      survglm_n <- survdesign_use$variables %>% 
      summarise(
        n_exp_raw = n(),
        n_exp_weighted = sum(WEIGHT_adj)) %>% 
      mutate("exp_level"=exposure)
    
      # Compile regression output
      survglm_summary <- rbind(summary(survglm_fit)$coef[2,c(1:2,4)]) %>% 
        as.data.frame() %>% 
        mutate(fstat = f_test$Ftest, f_pval = f_test$p) %>% 
        mutate(exp_level = exposure, .before=1)
    }
    
    pval_col <- colnames(summary(survglm_fit)$coef)[4]
  
    ## Assembly regression summary table
    survglm_summary <- survglm_summary %>% 
      mutate(
        strata_var       = ifelse(run_by_strata==F, "None", strata),
        strata_level     = as.character(lvl),
        cycle_yrs        = survcycles$cycles, 
        n_yrs.           = survcycles$n_yrs, 
        survweight.      = survweight, 
        model_covars     = paste0(covariates, collapse="+"), 
        exposure         = exposure, 
        outcome          = outcome,
        n_total_raw      = nrow(survdesign_use$variables), 
        n_total_weighted = sum(survdesign_use$variables$WEIGHT_adj), 
        model_df         = degf(survdesign_use),
        model_type       = sprintf("%s (%s)", fam[[1]], fam[[2]]),
        .before = 1) %>% 
      left_join(survglm_n, by = "exp_level") %>%
      rename("beta"=Estimate, "se"=`Std. Error`, "pval"=pval_col) %>% 
      # Add lower/upper 95% CI
      mutate(
        low95CI=beta-1.96*se, up95CI=beta+1.96*se,
        .after="pval"
      )
    
    # Relabel reference category as level (Intercept) for categorical exposures
    if(is_categorical) survglm_summary <- survglm_summary %>% 
      mutate_at("exp_level", ~ifelse(
        . == levels(exposure_vals)[1], paste(., "(Intercept)"), .)
      )
    
    # Return a list containing both the model and summary on success
    list(summary = survglm_summary, model = survglm_fit)
    
    }, error = function(e) {
      # If anything fails in this strata level, print an error and return NULL
      cat(" ... FAILED.\n")
      message(sprintf("    Error details: %s", conditionMessage(e)))
      return(NULL)
    })
    
    # If no errors produced, save the outputs to your lists
    if (!is.null(result_lvl)) {
      survglm_summary.l[[as.character(lvl)]] <- result_lvl$summary
      survglm_model.l[[as.character(lvl)]] <- result_lvl$model
      cat(" ... ... ... DONE. \n")
    }
  }
  
  # Combine summary tables across all strata levels safely 
  # (in case all iterations failed and the list is empty)
  if (length(survglm_summary.l) > 0) {
    survglm_fullsummary <- bind_rows(survglm_summary.l)
  } else {
    survglm_fullsummary <- NULL
  }
  
  return(list(model = survglm_model.l, modelsum = survglm_fullsummary))
}
    
 
## EOF
# Last Updated: 08-31-2026
  
  
  
