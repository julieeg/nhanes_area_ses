# Rscript for applying survey weights and running regressions in NHANES

 
################################################################################
## Set Up & Load Required Packages 
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
nhanes_processed <- readRDS("../data/processed/nhanes_processed.rda")
#nhanes_processed_prevent <- readRDS("../data/processed/nhanes_processed_prevent.rda")


################################################################################
## Build basic function for survey weighting
################################################################################

## Load required library
library(survey)

nhanes_var_weights <- c(int2yr="WTINT2YR", intprp="WINTPRP", mec2yr="WTMEC2YR", 
                        mecprp="WTMECPRP", fast2yr="WTSAF2YR", fastprp="WTSAFPRP",
                        diet="WTDR2D")

nhanes_vars_by_weight.l <- list(
  int = c(nhanes_demo_processed %>% names(), nhanes_quest_processed %>% names(),
                "cvd", "ascvd", "copd", "chf"),
  mec = c(nhanes_exam_processed %>% names(), 
           nhanes_lab_processed %>% select(-c("fg","tg","ldl")) %>% names(),
          "bmi_calc", "obese", "obesity_abd", "mdd", "htn", "htn_undx", 
          "htn_stg1", "copd_pft", "ckd", "ckd_gfr_level", "ckd_malb_level", "ckd_any",
          "fib4", "fib4_cat", "nfs", "nfs_cat", "liver_valid_elastography",
          "liver_cap_steatosis", "liver_cap_level", "liver_cap_masld", 
          "liver_cap_moderate_severe"),
  fast = c("fg", "tg", "ldl", "diabetes", "diabetes_undx"),
  diet = c(nhanes_diet_processed %>% names())
  )

## a. Harmonize Interview and Exam weights across 2-yr cycles from 1999-2000 and 2021-2023 
## with the pre/post pandemic (3.2) year cycle from 2017-March, 2020
nhanes_processed <- nhanes_processed %>% 
  mutate(WTINT2YR_COMBN = ifelse(Years == "2017-2020", WTINTPRP, WTINT2YR),
         WTMEC2YR_COMBN = ifelse(Years == "2017-2020", WTMECPRP, WTMEC2YR),
         WTSAF_COMBN = ifelse(Years == "2017-2020", WTSAFPRP, WTSAF2YR)
  )


get_var_weight <- function(vars, data=nhanes_processed) {
  
  if(any(vars %in% nhanes_vars_by_weight.l$fast)) {
    weightvar="WTSAF_COMBN" } else if (
      any(vars %in% nhanes_vars_by_weight.l$diet)) { 
      weightvar="WTDR2D" } else if (
        any(vars %in% nhanes_vars_by_weight.l$mec)) {
        weightvar="WTMEC2YR_COMBN" } else if (
          any(vars %in% nhanes_vars_by_weight.l$int)) {
            weightvar="WTINT2YR_COMBN" }
          return(weightvar)
}

  
## b. Calculate Total Years represented
# * 1999-2016 = 9 standard 2-year cycles = 18 years
# * 2017-March 2020 = 1 pre-pandemic cycle = 3.2 years
# * 2021-2023 = 1 post-pandemic 2 year cycle = 2 years
# * TOTAL YEARS = 23.2 years; i.e. To calculate var weights:
# --> if(Years == 2017-2020), WTX_COMBN * (3.2 / total_years)
# --> else(), WTX_COMBN * (2 / total_years)

vars_to_descr <- c(age="Age, years", bmi="BMI, kg/m2", smoke_current = "Smoking status",
                   alch_freq_wk = "Alcohol frequency", pa_level_mets = "Physical activity, MET/wk", 
                   educ_level = "Education level", inc_to_pov_level = "PIR Level", 
                   diabetes = "Diabetes", sbp_mean="SBP (average), mmHg", 
                   dbp_mean="DBP (average), mmHg", tc="Total cholesterol, mg/dL", 
                   hdl="HDL-c, mg/dL", ldl="LDL-c, mg/dL", tg="Triglyeride, mg/dL",
                   hba1c="HbA1c (%)", uacr="UACR", egfr="eGFR")


## ================================================
## Build function to create summary tables
## ================================================

# Note: WTMEC2YR is the 2-year MEC weight. If combining multiple cycles, 
# adjust weights by dividing WTMEC2YR by the number of cycles (e.g., / 3 for 3 cycles).

build_nhanes_summarytable.fun <- function(var_list, strata, data = nhanes_processed) {
  
  # Subset to complete observations for var & varweight
  lapply(1:length(vars_to_descr), function(x) {
    
    var <- names(vars_to_descr)[x]
    
    # 1. Get variable weight
    var_weight <- get_var_weight(var) 
    var_complete <- data %>% select(SEQN, Years, VAR=var, AGE=age, STRATA=strata, 
                                    SDMVPSU, SDMVSTRA, WEIGHT=var_weight) %>%
      mutate(TOTAL="Total") %>% filter(complete.cases(.))
  
    # 2. Count number of represented years
    cycles <- var_complete %>% pull(Years) %>% unique() ; n_cycles <- length(cycles)
    years <- as.numeric(c(gsub("-.*","", min(cycles)), gsub(".*-","", max(cycles))))
    n_years <- years[2]-years[1]
    
    # 3. Adjust weight, based on n_years
    var_complete <- var_complete %>% mutate(
      WEIGHT_adj = ifelse(Years == "2017-2020", WEIGHT*(3.2/n_years), WEIGHT*(2/n_years)))
    
    # 5. Build survey object
    var_survdesign <- svydesign(id = ~SDMVPSU, strata = ~SDMVSTRA, 
      weights = ~WEIGHT_adj, nest = TRUE, data = var_complete)
     
      # Subset survey object to age >18
      var_survdesign <- subset(var_survdesign, AGE >= 18)
    
      # 6. Summrize mean (se) and % (se) by strata
      strata_levels <- unique(var_complete$STRATA)
      var_sum <- rbind.data.frame(
        svyby(~VAR, by = ~TOTAL, design  = var_survdesign, FUN = svymean, na.rm = TRUE) %>%
          rename(STRATA=TOTAL),
        svyby(~VAR, by = ~STRATA, design  = var_survdesign, FUN = svymean, na.rm = TRUE))
      
    ## For numeric/continuous variables -----------
    if(is.numeric(var_complete %>% pull(VAR))) {
      var_summary <- var_sum %>% rename(mean=VAR) %>% 
        mutate(Variable = vars_to_descr[[var]], .after=STRATA) %>%
        mutate(sum = sprintf("%s \u00B1 %s",round(mean,digits), round(se,digits))) %>% 
        select(STRATA, Variable, sum) %>% 
        pivot_wider(names_from="STRATA", values_from=sum) # Var= #lowCI = mean-1.96*se, upCI=mean+1.96*se)
    } else {
      var_summary <- var_sum %>% 
        rename_with(., ~gsub("VAR","", .)) %>% 
        rename_at(vars(-STRATA, -starts_with("se")), ~ paste0("mean.", .x)) %>%
        pivot_longer(cols = -STRATA,names_sep="[.]", names_to=c("msr","Variable")) %>%
        pivot_wider(names_from=msr) %>% 
        mutate(across(c(mean, se), ~.*100)) %>%
        mutate(sum = sprintf("%s (%s)", round(mean,digits), round(se,digits))) %>% 
        select(STRATA, Variable, sum) %>% 
        pivot_wider(names_from="STRATA", values_from=sum) %>%
        mutate_at("Variable", ~paste0(" ", .)) %>%
        add_row(Variable = vars_to_descr[[var]], .before = 1) %>%
        mutate(across(-Variable, ~ replace_na(.x, "")))
    }
      return(var_summary)
  }) %>% do.call(rbind.data.frame, .)
}

## Summarize Table 1 by Age, Sex and Race/Ethnicity ------------------------

nhanes_sumtab_bysex <- build_nhanes_summarytable.fun(var_list = vars_to_descr, strata="gender")
nhanes_sumtab_byage <- build_nhanes_summarytable.fun(var_list = vars_to_descr, strata="age_3lvl")
nhanes_sumtab_byracethn <- build_nhanes_summarytable.fun(var_list = vars_to_descr, strata="racethn_combn")
  
nhanes_sumtab_strata.l <- list(
  sex = nhanes_sumtab_bysex, 
  age = nhanes_sumtab_byage, 
  race = nhanes_sumtab_byracethn
  ) %>% saveRDS("../data/output/nhanes_sumtab_bystrata.rda")
      

## ====================================================
## Build function to tabulate linear regressions
## ====================================================

build_nhanes_survdesign.fun <- function(vars_to_include, data = nhanes_processed) {
  
  # 1. Get variable weight
  vars_weight <- get_var_weight(vars_to_include) 
  vars_complete <- data %>% select(SEQN, Years, SDMVPSU, SDMVSTRA, WEIGHT=var_weight,
                                   all_of(vars_to_include)) %>%
    mutate(AGE=age) %>% 
    filter(complete.cases(.))
  
  # 2. Count number of represented years
  cycles <- vars_complete %>% pull(Years) %>% unique() ; n_cycles <- length(cycles)
  years <- as.numeric(c(gsub("-.*","", min(cycles)), gsub(".*-","", max(cycles))))
  n_years <- n_cycles*2 #(years[2]-years[1])
  
  # 3. Adjust weight, based on n_years
  vars_complete <- vars_complete %>% mutate(
    WEIGHT_adj = ifelse(Years == "2017-2020", WEIGHT*(3.2/n_years),
                        WEIGHT*(2/n_years)))
  
  # 4. Build survey object
  vars_survdesign <- svydesign(
    id = ~SDMVPSU, strata = ~SDMVSTRA, 
    weights = ~WEIGHT_adj, nest = TRUE, data = vars_complete)
  
  # Subset survey object to age >18
  vars_survdesign <- subset(vars_survdesign, AGE >= 18)
  
  return(list(survdesign=vars_survdesign, survweight=vars_weight, 
              survcycles=cbind.data.frame(cycles, n_years)))
  
}


#exposure="educ_level" ; outcome = "taste_mouth_quinine_glms" ; covariates = c("age","gender")

run_nhanes_lm.fun <- function(exposure, outcome, covariates, data = nhanes_processed) { 
  
  # List model variables
  vars_to_include <- c(exposure, outcome, covariates)
  
  # Construct NHANES survey design, using an embedded function to select custom WEIGHT
  nhanes_survdesign <- build_nhanes_survdesign.fun(vars_to_include, data = nhanes_processed)
  survdesign <- nhanes_survdesign$survdesign
  survweight <- nhanes_survdesign$survweight 
  survcycles <- nhanes_survdesign$survcycles
  
  # Build model formula
  model_formula <- formula(paste0(outcome,"~", exposure,"+", paste0(covariates, collapse = "+")))
  
  # Checks if outcome is binary (0/1 or 2 unique non-NA values) or continuous
  outcome_vals <- na.omit(data[[outcome]])
  is_binary <- length(unique(outcome_vals)) == 2 || is.factor(outcome_vals) || is.logical(outcome_vals)
  fam <- if (is_binary) quasibinomial() else gaussian()
  
  # 4. Fit the survey GLM & perform F-test for categorical exposures 
  fit <- svyglm(model_formula, design = survdesign, family = fam)
  
  exp_val <- nhanes_survdesign$variables[[exposure]]
  f_test <- NULL ; if (is.factor(exp_val) || is.character(exp_val)) {
    f_test <- regTermTest(fit, test.terms = exposure, method = "Wald")
  }
  
  ## Build regression summary table
  exp_levels <- length(fit$xlevels[[exposure]])
  out<-matrix(NA, exp_levels, 11, dimnames = list(
    NULL, c("weight", "cycles", "model", "exposure", "outcome", "n", "beta", "se", "p", "f", "f_p")))
  out[,1:3] <- rep(c(varweight, survcycles$cycles, outcome), each=nrow(out)) ; out[,2] <- rep(lab, nrow(out)) ; out[,3] <- mod$xlevels[[exposure]]
  out[2:nrow(out),c(5:7)] <- summary(mod)$coef[2:nX, c(1:2,4)] ; out[1,8:9] <-c(anova(mod)[1,4], anova(mod)[1,5])
  out[,4] <- as.vector(table(mod$model[[exposure]]))
  
  # 6. Return fitted model along with F-test results
  return(list(
    model = fit,
    f_test = f_test
    )
}
  
  

  
  
  
  
  
