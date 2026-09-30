# Repo: nhanes_are_ses
# Path: scripts/functions/nhanes_survdesign_functions
# Description: Custom functions for survey-weighted data prep & regressions in NHANES

################################################################################
## Set Up; Load Required Packages & Data Files
################################################################################

## set local directory
#setwd('~/Documents/GitHub/nhanes_area_ses/run')

## load required base pacakges
list_of_packages <- c(
  "tidyverse", "data.table", "nhanesA", "progress", "sociome", "jsonlite", "haven", 
  "forcats", "parallel", "survey") ; invisible(lapply(list_of_packages, function(pkg) {
    if(!requireNamespace(pkg, quietly = TRUE)) { 
      install.packages(pkg) } ; library(pkg, character.only = TRUE)
  }))


# Set survey package option: 
options(survey.lonely.psu = "adjust")


################################################################################ 
## Build function to summarize descriptive statistics in NHANES
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


## =======================================================
## Write function to build summary tables by STRATA
## =======================================================

## To calculate Total Years represented
# * 1999-2002 = 1 set of 4yr weights = 4 years
# * 2003-2016 = 7 standard 2-year cycles = 14 years
# * 2017-March 2020 = 1 pre-pandemic cycle = 3.2 years
# * [2021-2023 = 1 post-pandemic 2 year cycle = 2 years]
# TOTAL YEARS = 23.2 years; i.e. To calculate var weights:
# weight function: WTX.COMBN * (3.2 / total_years)
# --> else(), WTX_COMBN * (2 / total_years)

# Note: WTMEC2YR is the 2-year MEC weight. If combining multiple cycles, 
# adjust weights by dividing WTMEC2YR by the number of cycles (e.g., / 3 for 3 cycles).

build_nhanes_summarytable.fun <- function(vars_to_summarise, strata = NULL, 
                                          data = nhanes_dat, digits=1) {
  
  # Generate survey deisgn & summary stats separately, by variable
  sumtab.l <- lapply(1:length(vars_to_summarise), function(x) {
    
    # Select variable to summarize; Check if numeric or otherwise
    var_input <- vars_to_summarise[x]
    if(!is.null(names(var_input))) {
      var_name <- var_input[[1]] ; var <- names(var_input)[1]
    } else {
      var_name <- var_input ; var <- var_input
    } 
    
    n_values <- length(na.omit(unique(data[[var]])))
    if(is.numeric(data[[var]]) & n_values != 2) {
      var_type <- "numeric" 
      } else if (n_values == 2) {
        var_type <- "binary" 
      } else {
          var_type <- "categorical" 
      }
    
    if(is.null(strata)) { data <- data %>% mutate(strata="1") }
    
    # 1. Get variable weight
    var_weight <- find_nhanes_weight.fun(var, data = data) 
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
      
    }
    
    # print var_summary
    var_summary <- var_summary %>% 
      mutate(across(starts_with("n_"), as.numeric))
    var_summary
    
  })
  
  # Compile & clean summary table ------------------
  sumtab_df <- do.call(bind_rows, sumtab.l) %>% 
    as.data.frame() %>%
    mutate(across(starts_with("n_"), ~ifelse(
      . == "", "", round(as.numeric(.), 0))) )
  
  if(is.null(strata)) { sumtab_df <- sumtab_df %>% select(-"1", -ends_with("_1"))}
  
  return(sumtab_df)
  
}


################################################################################ 
## Write function to build NHANES survey objects
################################################################################ 

build_nhanes_survdesign.fun <- function(vars_to_include, data = nhanes_dat, strata=NULL) {
  
  # 1. Get variable weight
  varweight <- find_nhanes_weight.fun(c(vars_to_include, strata))
  vars_complete <- data %>% select(
    SEQN, Years, nYRS, WTYRS, SDMVPSU, SDMVSTRA, STRATA=strata, age,
    WEIGHT=all_of(varweight), all_of(vars_to_include)) %>%
    mutate(AGE=age) %>% 
    filter(complete.cases(.))
  
  # If no strata is supplied
  if(is.null(strata)) {vars_complete <- vars_complete %>% mutate(STRATA = "Total")}
  
  # 2. Count number of represented years
  cycle_yrs <- vars_complete %>% select(Years, nYRS) %>% unique()
  cycles <- cycle_yrs$Years ; n_yrs <- sum(cycle_yrs$nYRS); n_cycles <- length(cycles)
  yr_range <- as.numeric(c(gsub("-.*","", min(cycles)), gsub(".*-","", max(cycles))))
  survdesign_cycles <- cbind.data.frame(cycles=paste0(yr_range, collapse="-"), n_yrs, n_cycles)
  
  # 3. Adjust weight, based on n_years
  vars_complete <- vars_complete %>% 
    mutate(WEIGHT_adj = WEIGHT * (WTYRS / n_yrs))
  
  # 4. Build survey object
  ## If no strata, strata = NULL
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
  survdesign_sample <- vars_survdesign_gt18_dat %>% 
    group_by(STRATA) %>% 
    summarise(
      n_raw = n(),
      n_weighted = sum(WEIGHT_adj),
      .groups = "drop") %>% 
    mutate(df = survdf) 
  
  # Add "Total" row if a strata was supplied
  if(!is.null(strata)) {
    survdesign_sample <- survdesign_sample %>% bind_rows(
      vars_survdesign_gt18_dat %>% 
      summarise(
        STRATA = "Total",
        n_raw = n(), n_weighted = sum(WEIGHT_adj)) %>% 
      mutate(df = survdf))
  }
  
  return(list(
    survdesign = vars_survdesign_gt18, 
    survdesign_data = vars_survdesign_gt18_dat,
    survdesign_weight = varweight, 
    survdesign_sample = survdesign_sample,
    survdesign_cycles = survdesign_cycles)
  )
  
}
 

################################################################################ 
## Write descriptive functions to summarize NHANES variables by STRATA
################################################################################ 

# ===============================================
## Summarize QUANTILES by categorical strata 
# ===============================================
get_nhanes_quantiles.fun <- function(var_to_summarise, strata, print_se = T, 
                                     get_pvalue = F,
                                     probs = c(0, 0.05, 0.25, 0.50, 0.75, 0.95, 1),
                                     data = nhanes_dat, digits=3) {
  
  # List model variables
  vars_to_include <- unique(c(var_to_summarise, strata))
  
  ## Construct NHANES survey design, using an embedded function to select custom WEIGHT
  nhanes_survdesign <- build_nhanes_survdesign.fun(vars_to_include, data, strata = strata)
  survdesign <- nhanes_survdesign$survdesign
  survweight <- nhanes_survdesign$survdesign_weight 
  survcycles <- nhanes_survdesign$survdesign_cycles
  survsample <- nhanes_survdesign$survdesign_sample
  
  # 1. Summarize quantiles by strata
  var_formula <- formula(paste0("~", var_to_summarise))
  
  q_total <- (svyquantile(var_formula, design = survdesign, 
                          quantiles = probs, ci =T, na.rm = TRUE))[[1]] %>% 
    as.data.frame() %>% 
    rownames_to_column("Level") %>% 
    select(Level, mean_total=quantile, se_total=se)
  
  var_summary <- svyby(var_formula, by = ~STRATA, design = survdesign, 
                       FUN = svyquantile, quantiles = probs, 
                       keep.var = T, na.rm = TRUE, vartype = "se") %>%
    as.data.frame() %>%
    rename_with(~ gsub("\\.", "", .)) %>% 
    rename_with(., ~gsub(var_to_summarise,"", .)) %>% #gsub("^\\.", "", 
    rename_at(vars(-STRATA, -starts_with("se")), ~ paste0("mean_", .x)) %>%
    rename_at(vars(-STRATA, starts_with("se")), ~ gsub("se", "se_", .x)) %>%
    pivot_longer(cols = -STRATA, names_sep="_", names_to=c("msr","Level")) %>%
    pivot_wider(names_from=msr) %>% 
    pivot_wider(id_cols=Level, names_from=STRATA, values_from=c(mean, se)) %>% 
    mutate(Level = as.character(probs)) %>%
    left_join(q_total, by = "Level") %>% 
    mutate(Variable=var_to_summarise, .before=1) %>%
    select(Variable, Level, mean_total, starts_with("mean_"), se_total, starts_with("se_"))
  
  # Calculate ANOVA p-value
  svy_anova <- svyglm(formula(paste0(var_to_summarise, "~STRATA")), design = survdesign)
  res_anova <- regTermTest(svy_anova, test.terms = ~STRATA)
  
  var_summary <- var_summary %>% mutate(
    teststat_df = sprintf("F(%s, %s)", res_anova$df, res_anova$ddf),
    teststat_val = res_anova$Ftest,
    p_value = res_anova$p
  )
  
    return(var_summary)
}


# =================================================
## Summarize DISTRIBUTIONS by categorical strata 
# =================================================
get_nhanes_distrib.fun <- function(var_to_summarise, strata, data = nhanes_dat) {
  
  # List model variables
  vars_to_include <- unique(c(var_to_summarise, strata))
  
  ## Construct NHANES survey design, using an embedded function to select custom WEIGHT
  nhanes_survdesign <- build_nhanes_survdesign.fun(vars_to_include, data, strata = strata)
  survdesign <- nhanes_survdesign$survdesign
  survweight <- nhanes_survdesign$survdesign_weight 
  survcycles <- nhanes_survdesign$survdesign_cycles
  survsample <- nhanes_survdesign$survdesign_sample
  
  var_formula <- formula(paste0("~", var_to_summarise))
  
  # Summarize total proportions
  p_total <- svymean(var_formula, design = survdesign, na.rm = TRUE) %>% 
    as.data.frame() %>% 
    rownames_to_column("Level") %>% 
    mutate_at("Level", ~gsub(var_to_summarise, "", .)) %>%
    rename(mean_total = mean, se_total=SE)
  
  # Summarize proportions by strata
  var_summary <- svyby(var_formula, by = ~STRATA, design = survdesign, 
                           FUN = svymean, na.rm = TRUE) %>% 
    rename_with(., ~gsub(var_to_summarise,"", .)) %>% 
    rename_at(vars(-STRATA, -starts_with("se")), ~ paste0("mean.", .x)) %>%
    pivot_longer(cols = -STRATA, names_sep="[.]", names_to=c("msr","Level")) %>%
    pivot_wider(names_from=msr) %>% 
    pivot_wider(id_cols=Level, names_from=STRATA, values_from=c(mean, se)) %>% 
    left_join(p_total, by = "Level") %>% 
    mutate(across(starts_with(c("mean", "se")), ~.*100)) %>%
    mutate(Variable=var_to_summarise, .before=1) %>%
    select(Variable, Level, mean_total, starts_with("mean_"), se_total, starts_with("se_"))
  
  # Chi Square p-value
  svy_chisqr <- svychisq(formula(paste0("~", var_to_summarise, "+STRATA")), design = survdesign)
  
  var_summary <- var_summary %>% 
    mutate(teststat_df = sprintf("F(%s, %s)", round(svy_chisqr$parameter["ndf"],2), round(svy_chisqr$statistic,2)),
           teststat_val = svy_chisqr$statistic,
           p_value = svy_chisqr$p.value)
  
  return(var_summary)
  
}

# ======================================================================
## Function to calculate pearson correlations of cont/int variables
# ======================================================================
get_nhanes_cormat.df <- function(vars, strata = NULL, data = nhanes_dat) {
  
  n_vars <- length(vars)
  
  if(is.null(strata)) { strata_lvls <- "Total" } else {
    strata_lvls <- as.character(na.omit(unique(data[[strata]])))
  }
  
  corr_list <- list() ; pval_list <- list()
  for (lvl in strata_lvls) { 
    
    # Create empty matrices for r and p
    r_mat <- matrix(NA, n_vars, n_vars, dimnames = list(vars, vars))
    p_mat <- matrix(NA, n_vars, n_vars, dimnames = list(vars, paste0("p_", vars)))
    
    for(i in 1:n_vars) {
      for(j in 1:n_vars) {
        if(i == j) {
          r_mat[i, j] <- 1 ; p_mat[i, j] <- 0
        } else if (i < j) { 
          v1 <- vars[i] ; v2 <- vars[j]
          survdesign <- build_nhanes_survdesign.fun(c(v1,v2), strata = strata, data=data)$survdesign
          
          if (lvl != "Total") {
            sub_design <- subset(survdesign, survdesign$variables[[strata]] == lvl)
          } else {
            sub_design <- survdesign
          }
          
          # 1. Calculate correlation (r)
          f_cov <- as.formula(paste0("~", v1, "+", v2))
          cov_obj <- svyvar(f_cov, survdesign, na.rm = TRUE)
          r_val <- cov2cor(coef(cov_obj))[1, 2]
          
          # 2. Get exact p-value
          f_glm <- as.formula(paste0(v1, " ~ ", v2))
          mod <- survey::svyglm(f_glm, design = survdesign)
          p_val <- summary(mod)$coefficients[v2, "Pr(>|t|)"]
          
          # Fill both sides of the symmetric matrices
          r_mat[i, j] <- r_mat[j, i] <- r_val
          p_mat[i, j] <- p_mat[j, i] <- p_val
        }
      }
    }
    
    # Format as lists
    corr_list[[lvl]] <- as.data.frame(r_mat) %>%
      rownames_to_column("variable") %>%
      mutate(strata = strata, level = lvl, .before = 1)
    
    pval_list[[lvl]] <- as.data.frame(p_mat) %>%
      rownames_to_column("variable") %>%
      mutate(strata = strata, level = lvl, .before = 1)
    
  } ; return(list(corr = bind_rows(corr_list), pvals = bind_rows(pval_list)))
  
}


###########################################################
## Build function to run and summarize survey glms
###########################################################

run_nhanes_glm.fun <- function(exposure, outcome, covariates, strata=NULL, data = nhanes_dat) { 
  
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
    


################################################################################
## Build function to run and summarize Cox PH models for NDI Mortality data
################################################################################

run_nhanes_coxph.fun <- function(exposure, outcome = "ndi_mortstat", 
                                     time_var_prefix="ndi_peryear",
                                     covariates = c("age", "gender"), 
                                     strata = NULL, data = nhanes_dat) { 
  
  # Rename outcome as event_var for survival data
  event_var <- outcome
  
  # Apply suffix to timevar, based on nhanes sub-sample WEIGHT
  model_weight <- find_nhanes_weight.fun(c(exposure, covariates), data = data)
  time_var <- paste0(time_var_prefix, "_", ifelse(grepl("INT", model_weight), "int", "exm"))
  
  # List model variables
  vars_to_include <- unique(c(exposure, time_var, event_var, covariates, strata))
  run_by_strata <- ifelse(!is.null(strata), T, F) 
  
  # Initiate lists for storing summaries and models 
  ## Construct NHANES survey design
  nhanes_survdesign <- build_nhanes_survdesign.fun(vars_to_include, data, strata = strata)
  survdesign <- nhanes_survdesign$survdesign
  survweight <- nhanes_survdesign$survdesign_weight 
  survcycles <- nhanes_survdesign$survdesign_cycles
  survsample <- nhanes_survdesign$survdesign_sample
  
  ## Grab strata levels
  if(run_by_strata) {
    strata_levels <- na.omit(unique(survdesign$variables$STRATA))
  } else {
    strata_levels <- "none"
  }
  
  survcox_summary.l <- list()
  survcox_model.l <- list()
  
  for (lvl in strata_levels) {
    
    if(run_by_strata) {
      survdesign_use <- subset(survdesign, survdesign$variables$STRATA == lvl)
      cat(sprintf("COX PH [%s = %s] | Surv(%s, %s) ~ %s | %s | %s ... \n", strata, lvl, time_var, 
                  event_var, exposure, paste0(covariates, collapse = "+"), survcycles$cycles))
    } else {
      survdesign_use <- survdesign
      cat(sprintf("COX PH [Full sample] | Surv(%s, %s) ~ %s | %s | %s ... \n", time_var, event_var, 
                  exposure, paste0(covariates, collapse = "+"), survcycles$cycles))
    }
    
    result_lvl <- tryCatch({
      
      ## Build Cox model formula
      model_formula <- formula(paste0("Surv(", time_var, ", ", event_var, ") ~ ", 
                                      exposure, " + ", paste0(covariates, collapse = "+")))
      
      ## Fit survey Cox model
      survcox_fit <- suppressWarnings(
        svycoxph(model_formula, design = survdesign_use)
        )
      
      # Extract exposure data to check if categorical
      exposure_vals <- survdesign_use$variables[[exposure]]
      is_categorical <- is.factor(exposure_vals) || is.character(exposure_vals)
      
      f_test <- list(Ftest = NA, p = NA)
      
      # Extract coefficients matrix
      coefs_sum <- summary(survcox_fit)$coefficients[,c(1:2,4,6)]
      coefs_sum <- rbind(coefs_sum[grep(paste0("^",exposure), rownames(coefs_sum)),])
      
      if(is_categorical) {
        
        # Gather exposure levels
        exposure_lvls <- levels(exposure_vals)
        coefs_sum <- rbind.data.frame(c(0,1,NA,NA), coefs_sum)
        rownames(coefs_sum) <- exposure_lvls
        
        # Perform Wald test for overall significance of the categorical variable
        f_test <- regTermTest(survcox_fit, test.terms = exposure, method = "Wald")
        
        # Tabulate sample sizes, events, and person-time per category
        survcox_n <- survdesign_use$variables %>% 
          group_by(across(all_of(exposure))) %>% 
          summarise(
            # 1. Sample Size
            n_exp_raw = n(), n_exp_weighted = sum(WEIGHT_adj),
            
            # 2. Event Counts
            n_event_raw = sum(.data[[event_var]] == 1, na.rm = TRUE),
            n_event_weighted = sum(WEIGHT_adj[.data[[event_var]] == 1], na.rm = TRUE),
            
            # 3. Person-Time
            n_person_time_raw = sum(.data[[time_var]], na.rm = TRUE),
            n_person_time_weighted = sum(.data[[time_var]] * WEIGHT_adj, na.rm = TRUE),
            .groups = "drop") %>% 
          
          rename("exp_level" = exposure)
        
        # Extract exposure rows (Cox models have no intercept, so levels start immediately)
        survcox_summary <- rbind(coefs_sum) %>% 
          as.data.frame() %>% 
          mutate(fstat = f_test$Ftest[1], f_pval = f_test$p[1]) %>% 
          rownames_to_column(var="exp_level") %>% 
          mutate_at("exp_level", ~gsub(exposure, "", .))
        
        n_events_per_var <- 10*c(length(covariates)+length(exposure_lvls)-1)
        
      } else { 
        
        # Continuous exposure logic (Calculates totals for the whole model sample)
        survcox_n <- survdesign_use$variables %>% 
          summarise(
            # 1. Sample Size
            n_exp_raw = n(), n_exp_weighted = sum(WEIGHT_adj),
            
            # 2. Event Counts
            n_event_raw = sum(.data[[event_var]] == 1, na.rm = TRUE),
            n_event_weighted = sum(WEIGHT_adj[.data[[event_var]] == 1], na.rm = TRUE),
            
            # 3. Person-Time
            n_person_time_raw = sum(.data[[time_var]], na.rm = TRUE),
            n_person_time_weighted = sum(.data[[time_var]] * WEIGHT_adj, na.rm = TRUE)
          ) %>% 
          mutate("exp_level" = exposure, .before=1)
        
        survcox_summary <- rbind(coefs_sum) %>%
          as.data.frame() %>% 
          mutate(fstat = f_test$Ftest, f_pval = f_test$p) %>% 
          mutate(exp_level = exposure, .before=1)
        
        n_events_per_var <- 10*c(length(covariates)+1)
      }
      
      ## Assemble regression summary table & calculate Hazard Ratios
      survcox_summary <- survcox_summary %>% 
        mutate(
          strata_var       = ifelse(run_by_strata==F, "None", strata),
          strata_level     = as.character(lvl),
          cycle_yrs        = survcycles$cycles, 
          n_yrs            = survcycles$n_yrs, 
          survweight       = survweight, 
          model_covars     = paste0(covariates, collapse="+"), 
          exposure         = exposure, 
          time_var         = time_var,
          event_var        = event_var,
          n_total_raw      = nrow(survdesign_use$variables), 
          n_total_weighted = sum(survdesign_use$variables$WEIGHT_adj), 
          model_df         = degf(survdesign_use),
          model_type       = "Cox Proportional Hazards",
          .before = 1) %>% 
        left_join(survcox_n, by = "exp_level") %>%
        rename(beta="coef", se_robust="robust se", HR="exp(coef)", pval="Pr(>|z|)") %>% 
        
        # Calculate Hazard Ratios and 95% CI ------------------------
        mutate(
          low95CI_HR = exp(beta - 1.96 * se_robust), 
          up95CI_HR = exp(beta + 1.96 * se_robust),
          .after = "pval") %>% 
        
        ## DATA QUALITY FLAGS
        # Total Model Power (Rule of thumb: 10 events per exposure variable/level)
        mutate(n_events_req = n_events_per_var) %>% 
        rowwise() %>% 
        mutate(
          flag_n_total = ifelse(n_events_req < n_event_raw, "Pass", "FLAG: Low Total Events"), 
          flag_n_level = ifelse(n_event_raw > 5, "Pass", "FLAG: Low Category Events (<5)"))
      
      # Relabel reference category as level (reference) for categorical exposures
      if(is_categorical) survcox_summary <- survcox_summary %>% 
        mutate_at("exp_level", ~ifelse(
          . == levels(exposure_vals)[1], paste(., "(Reference)"), .))
      
      list(summary = survcox_summary, model = survcox_fit)
      
    }, error = function(e) {
      cat("FAILED.\n")
      message(sprintf("    Error details: %s", conditionMessage(e)))
      return(NULL)
    })
    
    if (!is.null(result_lvl)) {
      survcox_summary.l[[as.character(lvl)]] <- result_lvl$summary
      survcox_model.l[[as.character(lvl)]] <- result_lvl$model
      cat("DONE. \n")
    }
  }
  
  # Return NULL if no models could be run ---------------
  if (length(survcox_summary.l) > 0) {
    survcox_fullsummary <- bind_rows(survcox_summary.l)
  } else {
    survcox_fullsummary <- NULL
  }
  
  return(list(model = survcox_model.l, modelsum = survcox_fullsummary))
  
}


################################################################################
## Define wrapper functions for running & compiling regression summaries
################################################################################

# ======================================================================
## Wrap nhanes survdesign modelsum outputs, over lists of exp, out, strata
# ======================================================================

wrap_nhanes_regress.fun <- function(regress_fun = run_nhanes_glm.fun,
    exposure_vars, outcome_vars, strata_vars, models = "base") { 
  
  # For each STRATA --------------------
  regress_strata <- lapply(strata_vars, function(strata_var) {
    
    if(strata_var =="full") { strata = NULL } else { strata = strata_var }
    
    # Define model covariates (base = age+gender; unless otherwise specified)
    if(models == "base") {covars = c("age", "gender")}
    if(strata_var %in% c("gender", "female", "sex")) { 
      covars = "age" } else if (strata_var == "age_4lvl") { covars = "gender" }
    
    # For each EXPOSURE ---------------------
    regress_exposures <- lapply(exposure_vars, function(x) {
      
      # Run for each OUTCOME --------------
      regress_outcomes <- lapply(outcome_vars, function(y) {
        
        regress_fun(exposure = x, outcome = y, data = nhanes_dat,
                    covariates = covars, strata = strata) 
      }) ; names(regress_outcomes) <- outcome_vars ; return(regress_outcomes)
    }) ; names(regress_exposures) <- exposure_vars ; return(regress_exposures)
  }) ; names(regress_strata) <- strata_vars ; return(regress_strata)
  
}

# ========================================================================
## Summary function to compile wrapped regression modseum outputs 
# ========================================================================

collapse_survdesign_regress.fun <- function(wrapped_glm_survdesign) {
  
  # Get strata, exposures & outcomes
  regress_strata <- names(wrapped_glm_survdesign)
  regress_exposures <- names(wrapped_glm_survdesign[[1]])
  regress_outcomes <- names(wrapped_glm_survdesign[[1]][[1]])
  
  regress_collapsed <- lapply(regress_strata, function(strat) {
    lapply(regress_exposures, function(exp) {
      lapply(regress_outcomes, function(out) {
        wrapped_glm_survdesign[[strat]][[exp]][[out]]$modelsum
      }) %>% do.call(rbind.data.frame, .)
    }) %>% do.call(rbind.data.frame, .)
  }) %>% do.call(rbind.data.frame, .)
  
  return(regress_collapsed)
}



################################################################################
## Prediction performance function (e.g., AUC, R2 and C-stat)
################################################################################

# ===================================================
## Calculate model AUC for GLM on binary outcomes 
# ===================================================
get_svymodel_auc.fun <- function(exposure, outcome, covariates=c("age","gender"), 
                                 strata=NULL, print_mod = T, data=nhanes_dat) {
  
  mod <- run_nhanes_glm.fun(exp=exposure, out=outcome, covar=covariates, 
                                strata=strata, data=data)
  
  tab_summary <- lapply(1:length(mod$model), function(z) {
    modlvl <- mod$model[[z]]
    wts <- modlvl$prior.weights
    roc_obj <- pROC::roc(
      response = modlvl$y, 
      predictor = modlvl$fitted.values, 
      weights = wts, quiet = TRUE)
    
    auc_val <- as.numeric(auc(roc_obj))
    auc_se <- tryCatch(sqrt(as.numeric(var(roc_obj))), error = function(e) NA)
    
    return(data.frame(strata_level = names(mod$model)[z], 
                      auc = round(auc_val, 4), 
                      auc_se = round(auc_se, 4)))
    
    }) %>% do.call(rbind.data.frame, .)
  
  return(list(modperf = full_join(mod$modelsum, tab_summary, by=c("strata_level")), 
              model=mod$model))
}

# ======================================================
## Calculate model R2 for GLM on continuous outcomes 
# ======================================================
get_svymodel_modR2.fun <- function(exposure, outcome, covariates=c("age","gender"),
                                   strata=NULL, data=nhanes_dat) {
  
  mod <- run_nhanes_glm.fun(exp=exposure, out=outcome, covar=covariates, 
                                  strata=strata, data=data)
  
  tab_summary <- lapply(1:length(mod$model), function(z) {
    modlvl <- mod$model[[z]]
    
    r2_val <- 1 - (modlvl$deviance / modlvl$null.deviance)
    
    # Weighted Root Mean Square Error (RMSE)
    wts <- modlvl$prior.weights
    rmse_val <- sqrt(sum(wts * (modlvl$y - modlvl$fitted.values)^2) / sum(wts))
    return(data.frame(
      strata_level = names(mod$model)[z], 
      modR2 = round(r2_val,4),
      rmse = round(rmse_val, 4)))
    }) %>% do.call(rbind.data.frame, .)
  
  ## Merge in regression data
  return(list(modperf = full_join(mod$modelsum, tab_summary, by=c("strata_level")), 
              model=mod$model))
}

# ===============================================================
## Calculate model C-Statistic for CoxPH on survival outcomes 
# ===============================================================
get_svymodel_cstat.fun <- function(exposure, outcome, covariates=c("age","gender"),
                                   strata=NULL, data=nhanes_dat) {
  
  mod <- run_nhanes_coxph.fun(exp=exposure, out=outcome, covar=covariates, 
                                  strata=strata, data=data)
  
  tab_summary <- lapply(1:length(mod$model), function(z) {
    modlvl <- mod$model[[z]]
    c_stat <- survival::concordance(modlvl)
    return(data.frame(strata_level = names(mod$model)[z], 
                      cstat = round(c_stat$concordance, 4),
                      cstat_se = round(c_stat$var, 4)))
    }) %>% do.call(rbind.data.frame, .)
  return(list(modperf = full_join(mod$modelsum, tab_summary, by="strata_level"), 
              model = mod$model))
}


# =======================================================================
## Wrapped function to calcualte model prediction performance stat
# =======================================================================
calc_nhanes_riskpred.fun <- function(exposure, outcome, covariates = c("age", "gender"), 
                                    stat = c("cstat", "auc", "r2"), strata="total", 
                                    data = nhanes_dat) {
  
  if(stat == "r2") { modperf_fun = get_svymodel_modR2.fun }
  if(stat == "auc") { modperf_fun = get_svymodel_auc.fun }
  if(stat == "cstat") { modperf_fun = get_svymodel_cstat.fun }
  if(strata == "total") {strata = NULL} 
  
  return(modperf_fun(exp=exposure, out=outcome, cov=covariates, 
                     strat=strata, dat=data)$modperf)
}


###########################################################################
## HOLD: Functions to plot box/barplots from distribution function
############################################################################

ggplot_theme <- theme_bw() + 
  theme(axis.text = element_text(color="black"),
        panel.grid.minor = element_blank(),
        axis.title = element_text(face="bold"),
        strip.background = element_blank(),
        strip.text.x.top = element_text(face="bold"))


plot_nhanes_boxplot.fun <- function(var_to_plot="inc_to_pov", strata = "racethn_combn",
                                    probs_to_plot=c(0.05,0.25,0.5,0.75,0.95),
                                    summary_data=tab_distrib) {
  params <- list(
    xvar = var_to_plot, 
    zlevels = gsub("mean_","",names(summary_data %>% select(starts_with("mean_"))))
  )
  
  summary_data %>% 
    filter(Vartype == "Quant") %>%
    filter(Variable == var_to_plot) %>% 
    select(-starts_with("se_")) %>% 
    pivot_longer(cols = -c("Variable", "Vartype", "Level"), names_to="Strata") %>% 
    mutate_at("Level", ~paste0("Q",.)) %>% 
    mutate_at("Strata", ~factor(gsub("mean_","",.), levels=params$zlevels)) %>%
    pivot_wider(names_from=Level) %>%
    ggplot(aes(x = Strata, fill=Variable)) +
    geom_boxplot(
      aes(
        ymin = Q0.05,   # Bottom whisker (5th percentile)
        lower = Q0.25,  # Bottom of the box (25th percentile)
        middle = Q0.5,  # Median line (50th percentile)
        upper = Q0.75,  # Top of the box (75th percentile)
        ymax = Q0.95    # Top whisker (95th percentile)
      ),
      stat = "identity",
      alpha = 0.7, width = 0.5
    ) + 
    ylab("Distribution (n)") +
    scale_fill_manual(values=palettes$NatExt$Greens[3]) +
    ggplot_theme 
}


plot_nhanes_barplot.fun <- function(var_to_plot="educ_level", strata = "age_4lvl",
                                    summary_data=tab_distrib_test) {
  params <- list(
    xvar = var_to_plot, 
    xlevels = tab_nhanes_distrib %>% filter(Variable == var_to_plot) %>% pull(Level),
    zlevels = gsub("mean_","",names(tab_nhanes_distrib %>% select(starts_with("mean_"))))
  )
  
  summary_data %>% 
    filter(Vartype == "Prop") %>%
    filter(Variable == var_to_plot) %>% 
    pivot_longer(cols = -c("Variable", "Vartype", "Level"), names_to="Strata") %>% 
    mutate(msr=ifelse(startsWith(Strata, "mean"), "mean", "se")) %>% 
    mutate_at("Strata", ~gsub("se_", "", gsub("mean_","",.))) %>%
    pivot_wider(names_from="msr") %>% 
    mutate(SES_Level = factor(rep(1:length(params$xlevels), each=length(params$zlevels)))) %>%
    mutate_at("Strata", ~factor(., levels=params$zlevels)) %>% 
    ggplot(aes(x = Strata, y = mean, group = SES_Level, fill=SES_Level)) +
    facet_wrap(~Variable, scale="free") + 
    geom_bar(stat = "identity", position = position_dodge(0.9)) +
    scale_fill_manual(values=rev(palettes$NatExt$Greens[2:(length(params$xlevels)+1)]),
                      name="Level", labels=params$xlevels) + 
    ylab("Proportion (%)") +
    ggplot_theme
  
}

## EOF
# Last Updated: 09-14-2026
  

