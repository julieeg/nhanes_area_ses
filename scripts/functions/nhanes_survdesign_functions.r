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

## Function to recode values in NHANES as missing
recode_nhanes_na.fun <- function(x) {
  if(is.numeric(x)) {
    x[x %in% c(777, 7777, 77777, 999, 9999, 99999)] <- NA_real_
    return(x)
  } else if(is.character(x)) {
    x[x %in% c("Don't know", "Refused", "", 
               "777", "7777", "77777", "999", "9999", "99999")] <- NA_character_
    return(x)
  } else {return(x)}
}

## Add descriptive labels (option to set factor order)
add_descr_labels <- function(data, base_var, labs_vals, ordered = T) {
  base <- data %>% select(all_of(base_var)) 
  x <- rep(NA, length(base))
  for(i in 1:length(labs_vals)) {
    x[base == labs_vals[i] ] <- names(labs_vals)[i]
  } ; if(ordered == T) {
    x <- factor(x, levels=names(labs_vals)) 
  } ; return(x)
}


## ===================================================
## Write function to select correct variable weight
## ===================================================

find_nhanes_weight.fun <- function(variables, data) {
  weight_order <- paste0(c("WTSPO", "WTSEPH", "WTSOG", "WTSAF", "WTDR", "WTMEC", "WTINT"), ".COMBN")
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
    
    # Select variable to summarize; 
    var_input <- vars_to_summarise[x]
    vars_to_include <- unique(c(var_input, strata))
    
    # Standardize variable display name vs actual column name
    if (!is.null(names(var_input)) && names(var_input)[1] != "") { 
      var_name <- names(var_input)[1]
      var <- var_input[[1]] 
    } else {
      var_name <- unname(var_input)
      var <- unname(var_input)
    }
    
    # 1. Prepare data BEFORE passing to survey design
    # Create explicit temporary dummy columns if needed
    data_use <- data %>%
      mutate(
        VAR = .data[[var]],
        TOTAL = "Total"
      )
    
    # If binary/numeric recoding is needed before design setup
    n_values <- length(na.omit(unique(data_use$VAR)))
    var_type <- if (is.numeric(data_use$VAR) && n_values != 2) {
      "numeric" } else if (n_values == 2) { 
        "binary" } else { "categorical" }
    
    if (var_type == "binary" && is.numeric(data_use$VAR)) {
      data_use$VAR <- as.factor(data_use$VAR)
    }
    
    ## Construct NHANES survey design, using an embedded function to select custom WEIGHT
    nhanes_survdesign <- build_nhanes_survdesign.fun(
      vars_to_include = c("VAR", "TOTAL", vars_to_include), 
      data = data_use, strata = strata)
    
    survdesign <- nhanes_survdesign$survdesign
    survdesign_data <- survdesign$variables
    survweight <- nhanes_survdesign$survdesign_weight 
    survcycles <- nhanes_survdesign$survdesign_cycles
    survsample <- nhanes_survdesign$survdesign_sample
    
    # 3. Compute Weighted Means/Proportions
    strata_levels <- unique(survdesign$variables$STRATA)
    var_sum <- rbind.data.frame(
      svyby(~VAR, by = ~TOTAL, design  = survdesign, FUN = svymean, na.rm = TRUE) %>%
        rename(STRATA=TOTAL),
      svyby(~VAR, by = ~STRATA, design = survdesign, FUN = svymean, na.rm = TRUE))
    # If var_type is not numeric, remove VAR from name
    if(var_type != "numeric") {
      var_sum <- var_sum %>% rename_with(. ,~gsub("VAR", "", .))
    }
    
    # ----------------------------------------------
    ## For numeric/continuous variables 
    # ----------------------------------------------
    
    if(var_type == "numeric") {
      # Tabulate Raw and Weighted Ns, per STRATA
      n_sum <- survdesign$variables %>% 
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
        survdesign$variables %>% 
          group_by(VAR) %>% 
          summarise(n_raw = n(), n_weight = sum(WEIGHT_adj[WEIGHT_adj > 0]),
                    .groups="drop") %>% 
          mutate(STRATA="Total", .before=1), 
        survdesign$variables %>% 
          group_by(STRATA, VAR) %>% 
          summarise(n_raw = n(), n_weight = sum(WEIGHT_adj[WEIGHT_adj > 0]),
                    .groups="drop")) %>%
        rename(Variable=VAR)
      
      # Combine & format output table, including n_sum
      var_summary <- var_sum %>% 
        rename_with(., ~gsub("VAR","", .)) %>% 
        rename_at(vars(-STRATA, -starts_with("se")), ~ paste0("mean.", .x)) %>%
        pivot_longer(cols = -STRATA, names_to=c("msr","Variable"), 
                     names_pattern = "^(mean|se)\\.(.*)$") %>%
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

build_nhanes_survdesign.fun <- function(vars_to_include, data = nhanes_dat, strata = NULL) {
  
  # 1. Identify weight variable and subset/complete cases for columns needed
  varweight <- find_nhanes_weight.fun(c(vars_to_include, strata), data = data)
  
  vars_complete <- data %>% 
    select(SEQN, Years, nYRS, WTYRS, SDMVPSU, SDMVSTRA, STRATA = all_of(strata), 
           AGE = age, WEIGHT = all_of(varweight), all_of(vars_to_include)) %>% 
    filter(complete.cases(.))
  
  # Default to "Total" if no strata is supplied
  if (is.null(strata)) {
    vars_complete <- vars_complete %>% mutate(STRATA = "Total")
  }
  
  # 2. Count number of represented years
  cycle_yrs <- vars_complete %>% select(Years, nYRS) %>% distinct()
  cycles <- cycle_yrs$Years 
  n_yrs <- sum(cycle_yrs$nYRS)
  n_cycles <- length(cycles)
  yr_range <- as.numeric(c(gsub("-.*", "", min(cycles)), gsub(".*-", "", max(cycles))))
  survdesign_cycles <- data.frame(cycles = paste0(yr_range, collapse = "-"), n_yrs, n_cycles)
  
  # 3. Adjust weights based on n_years
  vars_complete <- vars_complete %>% 
    mutate(WEIGHT_adj = WEIGHT * (WTYRS / n_yrs))
  
  # 4. Build survey object with all complete cases first
  vars_survdesign <- svydesign(
    id = ~SDMVPSU, 
    strata = ~SDMVSTRA, 
    weights = ~WEIGHT_adj, 
    nest = TRUE, 
    data = vars_complete
  )
  
  # 5. Correctly subset the ENTIRE survey object for adults (AGE >= 18) and positive weights
  vars_survdesign_gt18 <- subset(vars_survdesign, AGE >= 18 & WEIGHT_adj > 0)
  
  # Extract clean design data frame after subsetting
  svydesign_data <- vars_survdesign_gt18$variables
  
  # 6. Extract raw and weighted sample sizes & model degrees of freedom
  survdf <- degf(vars_survdesign_gt18)
  
  survdesign_sample <- svydesign_data %>% 
    group_by(STRATA) %>% 
    summarise(
      n_raw = n(), n_weighted = sum(WEIGHT_adj, na.rm = TRUE), .groups = "drop"
      ) %>% 
    mutate(df = survdf) 
  
  # Add "Total" row if a strata was supplied
  if (!is.null(strata)) {
    total_row <- svydesign_data %>% 
      summarise(
        STRATA = "Total",
        n_raw = n(), 
        n_weighted = sum(WEIGHT_adj, na.rm = TRUE)
      ) %>% 
      mutate(df = survdf)
    
    survdesign_sample <- bind_rows(survdesign_sample, total_row)
  }
  
  return(list(
    survdesign  = vars_survdesign_gt18, 
    survdesign_data = svydesign_data,
    survdesign_weight = varweight, 
    survdesign_sample = survdesign_sample,
    survdesign_cycles = survdesign_cycles
  ))
  
}


################################################################################ 
## Write descriptive functions to summarize NHANES variables by STRATA
################################################################################ 


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
    
    # Select variable to summarize; 
    var_input <- vars_to_summarise[x]
    vars_to_include <- unique(c(var_input, strata))
    
    # Standardize variable display name vs actual column name
    if (!is.null(names(var_input)) && names(var_input)[1] != "") { 
      var_name <- names(var_input)[1]
      var <- var_input[[1]] 
    } else {
      var_name <- unname(var_input)
      var <- unname(var_input)
    }
    
    # 1. Prepare data BEFORE passing to survey design
    # Create explicit temporary dummy columns if needed
    data_use <- data %>%
      mutate(
        VAR = .data[[var]],
        TOTAL = "Total"
      )
    
    # If binary/numeric recoding is needed before design setup
    n_values <- length(na.omit(unique(data_use$VAR)))
    var_type <- if (is.numeric(data_use$VAR) && n_values != 2) {
      "numeric" } else if (n_values == 2) { 
        "binary" } else { "categorical" }
    
    if (var_type == "binary" && is.numeric(data_use$VAR)) {
      data_use$VAR <- as.factor(data_use$VAR)
    }
    
    ## Construct NHANES survey design, using an embedded function to select custom WEIGHT
    nhanes_survdesign <- build_nhanes_survdesign.fun(
      vars_to_include = c("VAR", "TOTAL", vars_to_include), 
      data = data_use, strata = strata)
    
    survdesign <- nhanes_survdesign$survdesign
    survdesign_data <- survdesign$variables
    survweight <- nhanes_survdesign$survdesign_weight 
    survcycles <- nhanes_survdesign$survdesign_cycles
    survsample <- nhanes_survdesign$survdesign_sample
    
    # 3. Compute Weighted Means/Proportions
    strata_levels <- unique(survdesign$variables$STRATA)
    var_sum <- rbind.data.frame(
      svyby(~VAR, by = ~TOTAL, design  = survdesign, FUN = svymean, na.rm = TRUE) %>%
        rename(STRATA=TOTAL),
      svyby(~VAR, by = ~STRATA, design = survdesign, FUN = svymean, na.rm = TRUE))
    # If var_type is not numeric, remove VAR from name
    if(var_type != "numeric") {
      var_sum <- var_sum %>% rename_with(. ,~gsub("VAR", "", .))
    }
      
    # ----------------------------------------------
    ## For numeric/continuous variables 
    # ----------------------------------------------
    
    if(var_type == "numeric") {
      # Tabulate Raw and Weighted Ns, per STRATA
      n_sum <- survdesign$variables %>% 
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
        survdesign$variables %>% 
          group_by(VAR) %>% 
          summarise(n_raw = n(), n_weight = sum(WEIGHT_adj[WEIGHT_adj > 0]),
                    .groups="drop") %>% 
          mutate(STRATA="Total", .before=1), 
        survdesign$variables %>% 
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
get_nhanes_cormat.fun <- function(vars, strata = NULL, data = nhanes_dat,
                                  method = c("pearson", "spearman")) {
  
  method <- match.arg(method) # Ensures 'method' is exactly "pearson" or "spearman"
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
          
          # Build design
          survdesign <- build_nhanes_survdesign.fun(
            vars = c(v1,v2), strata = strata, data=data)$survdesign
          
          # Subset design
          if (lvl != "Total") {
            sub_design <- subset(survdesign, survdesign$variables$STRATA == lvl,
                                 data = data)
          } else {
            sub_design <- survdesign
          }
          
          # Make placeholders, in case of function failure
          r_val <- NA ; p_val <- NA
          
          tryCatch({
            
            # If Spearman, overwrite variables with rank inside survey object
            if (method == "spearman") {
              sub_design$variables[[v1]] <- rank(sub_design$variables[[v1]], na.last = "keep")
              sub_design$variables[[v2]] <- rank(sub_design$variables[[v2]], na.last = "keep")
            }
            
            # 1. Calculate correlation (r / rho)
            f_cov <- as.formula(paste0("~", v1, "+", v2))
            cov_obj <- svyvar(f_cov, sub_design, na.rm = TRUE)
            r_val <- cov2cor(coef(cov_obj))[1, 2]
            
            # 2. Get exact p-value
            f_glm <- as.formula(paste0(v1, " ~ ", v2))
            mod <- survey::svyglm(f_glm, design = sub_design)
            p_val <- summary(mod)$coefficients[v2, "Pr(>|t|)"]
            
          }, error = function(e) {
            message(sprintf("FAILED | skipping corr for %s-%s | ~%s==%s: %s", 
                            v1, v2, strata, lvl, e$message))
          })
          
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
    
  } 
  
  return(list(corr = bind_rows(corr_list), pvals = bind_rows(pval_list)))
}



###########################################################
## Build function to run and summarize survey glms
###########################################################

run_nhanes_glm.fun <- function(exposure, outcome, covariates, strata=NULL, data) { 
  
  # List model variables
  if(grepl("[+]", covariates[1])) {covariates <- unlist(str_split(covariates, "[+]"))}
  vars_to_include <- unique(c(exposure, outcome, covariates, strata))
  run_by_strata <- ifelse(!is.null(strata), T, F) 
  
  ## Construct NHANES survey design, using an embedded function to select custom WEIGHT
  nhanes_survdesign <- build_nhanes_survdesign.fun(vars_to_include, data = data, strata = strata)
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

run_nhanes_coxph.fun <- function(exposure, outcome, time_var_prefix="ndi_peryear",
                                 covariates = NULL, strata = NULL, 
                                 data = nhanes_dat) { 
  
  # Rename outcome as event_var for survival data
  event_var <- outcome
  
  # Reformat model/covariates, if provided as model syntax
  if(grepl("[+]", covariates[1])) {
    covariates <- unlist(str_split(covariates, "[+]")) }
  
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
      
      ## Fit survey Cox model with tryCatch
      survcox_fit <- tryCatch({
        suppressWarnings(svycoxph(model_formula, design = survdesign_use))
      }, error = function(e) {
        message(sprintf("Model failed for level %s: ", lvl), e$message) 
        return(NULL) 
      })
      
      # 2. ONLY extract and mutate if the model actually created an object
      if (!is.null(survcox_fit)) {
        
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
        
      } else {
        # Added explicit NULL return so the tryCatch knows what to pass if the model failed
        NULL 
      }
      
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
  } # End of for loop
  
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

run_nhanes_regress_wrapper.fun <- function(regress_fun,
    exposure_vars, outcome_vars, strata_vars = "none", 
    models, data = nhanes_dat, collapse = T) { 
  
  # For each STRATA --------------------
  regress_strata <- lapply(strata_vars, function(strata_var) {
    
    if(strata_var =="none") { strata = NULL } else { strata = strata_var }
    
    # For each MODEL -----------------------
    regress_models <- lapply(seq_along(models), function(m) {
      
      # Define covariates & remove overlap with strata
      covariates <- unlist(str_split(models[[m]], "[+]"))
      
      if(strata_var %in% c("gender", "sex")) {
        covariates <- covariates[-c(which(covariates %in% c("gender", "sex")))] 
        } else if (strata_var == "age_4lvl") { 
          covariates <- covariates[-c(which(covariates %in% c("age")))] 
        }
      
      # For each EXPOSURE ---------------------
      regress_exposures <- lapply(exposure_vars, function(x) {
        
        # Run for each OUTCOME --------------
        regress_outcomes <- lapply(outcome_vars, function(y) {
          
          res <- regress_fun(exposure = x, outcome = y, data = data,
                             covariates = covariates, strata = strata)
          
          # If regression ran, add model 
          if(!is.null(res$modelsum)) {
            res$modelsum <- res$modelsum %>% mutate(
              model = names(models)[m], .before = "model_covars") 
            } ; return(res)
          
        }) ; names(regress_outcomes) <- outcome_vars ; return(regress_outcomes)
      }) ; names(regress_exposures) <- exposure_vars ; return(regress_exposures)
    }) ; names(regress_models) <- names(models) ; return(regress_models) 
  }) ; names(regress_strata) <- strata_vars 
  
  # If collapse == T
  if(collapse == F) {
    regress_strata <- regress_strata[[1]] ; return(regress_strata) 
  } else {
    collapsed <- lapply(names(regress_strata), function(strat) {
      lapply(names(regress_strata[[strat]]) , function(mod) {
        lapply(names(regress_strata[[strat]][[mod]]), function(exp) {
          lapply(names(regress_strata[[strat]][[mod]][[exp]]), function(out) {
            
            regress_strata[[strat]][[mod]][[exp]][[out]]$modelsum
            
            }) %>% do.call(bind_rows, .)
          }) %>% do.call(bind_rows, .)
        }) %>% do.call(bind_rows, .)
    }) %>% do.call(bind_rows, .)
    
    return(collapsed)
  }
}
    

################################################################################
## Prediction performance function (e.g., AUC, R2 and C-stat)
################################################################################

# ===================================================
## Calculate model AUC for GLM on binary outcomes 
# ===================================================
get_svymodel_auc.fun <- function(exposure, outcome, covariates, strata=NULL, 
                                 print_mod = T, data=nhanes_dat) {
  
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
  

