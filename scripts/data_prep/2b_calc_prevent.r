## Rscript to prepare PREVENT predictions

## PREVENT inputs are coded based on AHA primary definitions.
## Values outside of the pre-specified lower/upper bounds for lab measures will 
## be recoded as those respective bounds (primary analyses). 
## Planned sensitivity analyses will also be conducted to recode any values 
## outside the pre-specified ranges as missing. 

## Make a 'dummy SDI variable of 1 to generate base+SDI model (note: betas for 
## ALL other estiamtes change when SDI is included)

################################################################################
## Set Up & Load Required Packages 
################################################################################

## set local directory
#setwd('C:/Users/sarac/OneDrive/Documents/Research')
setwd('~/Documents/GitHub/nhanes_area_ses/run')
dir.create("../data/processed/prevent")

## load required base pacakges
list_of_packages <- c(
  "tidyverse", "data.table", "nhanesA", "progress", "sociome", "jsonlite", "haven", "forcats"
) ; invisible(lapply(list_of_packages, function(pkg) {
  if(!requireNamespace(pkg, quietly = TRUE)) { 
    install.packages(pkg) } ; library(pkg, character.only = TRUE)
}))


## load PREVENTR
library(preventr)

## Load processed NHANES data
nhanes_processed <- readRDS("../data/processed/nhanes_processed.rds")


################################################################################
## Build prevent outcomes 
################################################################################

## Add prevent-specific inputs
nhanes_processed <- nhanes_processed %>% 
  # Filter to 
  mutate(
    prevent_age = ifelse(age > 30 & age < 80, age, NA),
    prevent_sex = ifelse(female == 1, "female", "male"),
    prevent_sbp = case_when(
      sbp_mean < 90 ~ 90, 
      sbp_mean > 180 ~ 180, 
      TRUE~sbp_mean),
    prevent_bprx = ifelse(rx_use_bp == "Yes", 1, 0),
    prevent_tc = case_when(
      tc < 130 ~ 130, 
      tc > 320 ~ 320, 
      TRUE ~ tc),
    prevent_hdl = case_when(
      hdl < 20 ~ 20, 
      hdl > 100 ~ 100, 
      TRUE ~ hdl),
    prevent_statin = ifelse(rx_use_statin == "Yes", 1, 0),
    prevent_diab = case_when(
      diabetes == "0"~ 0, 
      diabetes == "1" ~ 1, 
      TRUE ~ NA_integer_),
    prevent_smoking = ifelse(smoke_status == "Current smoker", 1, 0),
    prevent_egfr = case_when(
      egfr < 15 ~ 15, 
      egfr > 40 ~ 40, 
      TRUE ~ egfr),
    prevent_bmi = case_when(
      bmi < 18.5 ~ 18.5, 
      bmi > 39.9 ~ 39.9, 
      TRUE ~ bmi),
    prevent_hba1c = case_when(
      hba1c < 4.5 ~ 4.5, 
      hba1c > 15 ~ 15, 
      TRUE ~ hba1c),
    prevent_uacr = case_when(
      uacr < 0.1 ~ 0.1, 
      uacr > 25000 ~ 25000, 
      TRUE ~ uacr)) %>%
  mutate(
    prevent_zip = "",
    prevent_zip_sdi1 = "90210", #Beverly Hills, CA (Affluent/Low SDI)
    prevent_zip_sdi2 = "01085", # Westfield, MA (Intermediate/Moderate SDI)
    prevent_zip_sdi3 = "01040" # Holyoke, MA (Underserved/High SDI)
    ) %>%
  mutate(
    prevent_complete_base = ifelse(
      !is.na(prevent_age) & !is.na(prevent_sex) & !is.na(prevent_sbp) & 
        !is.na(prevent_bprx) & !is.na(prevent_tc) & !is.na(prevent_hdl) & 
        !is.na(prevent_statin) & !is.na(prevent_diab) & !is.na(prevent_smoking) & 
        !is.na(prevent_egfr) & !is.na(prevent_bmi), 1, 0)
    ) %>%
  mutate(
    prevent_complete_baseA1c = ifelse(prevent_complete_base == 1 & !is.na(prevent_hba1c), 1, 0),
    prevent_complete_baseUACR = ifelse(prevent_complete_base == 1 & !is.na(prevent_uacr), 1, 0),
    prevent_complete_full = ifelse(prevent_complete_baseA1c == 1 & prevent_complete_baseUACR == 1, 1, 0)) %>% 
  mutate(across(starts_with("prevent_complete_"), ~as.factor(.)))


## Write wrapper function to calculate PREVENT risk estimates, for EACH model
prevent_models <- c(base="Base", hba1c="Base+HbA1c", uacr="Base+UACR", full="Base+HbA1c+UACR")

calculate_prevent_riskest.fun <- function(prevent_model, data = nhanes_processed, sdi=NULL) {
  
  # Create subset of complete data, based on PREVENT model -------------
  if(prevent_model == "base" | prevent_model == "sdi") { 
    data_complete <- data %>% filter(prevent_complete_base==1) }
  if (prevent_model == "hba1c") {
    data_complete <- data %>% filter(prevent_complete_baseA1c == 1) }
  if(prevent_model == "uacr") {
    data_complete <- data %>% filter(prevent_complete_baseUACR == 1) } 
  if(prevent_model == "full") {
    data_complete <- data %>% filter(prevent_complete_full == 1) 
  }
  
  # List of prevent outcomes -------------
  prevent_outcomes <- c("total_cvd", "ascvd", "heart_failure", "chd", "stroke")
  
  # set SDI level 
  if(!is.null(sdi)) {
    prevent_sdi <- sym(paste0("prevent_zip_sdi", sdi)) 
    } else { prevent_sdi = sym("prevent_zip")}
  
  # Calculate each risk estimate, at 10 & 30 years, in each model ---------- 
  prevent.dat <- estimate_risk(
    use_dat = data_complete %>% 
      select(SEQN, Years, starts_with("prevent"), racethn_combn, 
             prevent_zip = prevent_sdi),
    age=prevent_age, sex=prevent_sex, sbp=prevent_sbp, bp_tx = prevent_bprx,
    total_c = prevent_tc, hdl_c = prevent_hdl, statin = prevent_statin, 
    dm = prevent_diab, smoking = prevent_smoking, bmi = prevent_bmi, 
    egfr = prevent_egfr, hba1c = prevent_hba1c, uacr = prevent_uacr, 
    zip = prevent_zip, time = "both", model = prevent_model
    )
  
  # Compile & reshape results into wide-format, for nhanes_proc merging
  prevent.res <- prevent.dat %>%
    select(SEQN, prevent_outcomes, over_years) %>% 
    pivot_wider(values_from = prevent_outcomes, names_from=over_years) %>%
    rename_at(paste0(rep(prevent_outcomes, each=2), "_", c(10,30)), 
              ~paste0("prevent_", ., "yr_", prevent_model)) %>%
    rename_with(., ~gsub("total_cvd", "cvd", gsub("heart_failure", "hf", .)))
  
  return(list(results = prevent.res, 
              input_errors = prevent.dat %>% 
                select(SEQN, model, over_years, input_problems) %>% 
                filter(!is.na(input_problems)) )
         )
}


## ================================================================
## Run function for all outcomes, at 10 and 30 yr, in each model
## ================================================================

## PREVENT estimates for base, hba1c, uacr and full models -----------
prevent_results.l <- lapply(names(prevent_models), function(mod) {
  message("Calculating PREVENT risk estimates in the ",
          prevent_models[[mod]], " Model")
  calculate_prevent_riskest.fun(
    data = nhanes_processed, prevent_model = mod)
  }) ; names(prevent_results.l) <- names(prevent_models)


# Extract results, only --------------------
prevent_results_merge <- reduce(
  lapply(prevent_results.l, function(est) est$results),
  full_join, by = "SEQN")

## Save prevent estimates --------------------
prevent_results.l %>% saveRDS("../data/processed/prevent/nhanes_prevent_estimates_output.rds")

## Mege in prevent_inputs and save as .csv
prevent_results_merge %>% 
  full_join(nhanes_processed %>% select(SEQN, starts_with("prevent_"))) %>% 
  fwrite(., file="../data/processed/prevent/nhanes_prevent_complete.csv")


# -----------------------------------
## Run with different SDI levels
# -----------------------------------

prevent_sdi_models <- c(sdi="Base+SDI", full="Base+HbA1c+UACR+SDI")

prevent_sdi_results.l <- lapply(c(1:3), function(i){
  prevent_results.l <- lapply(names(prevent_sdi_models), function(mod) {
    message("Calculating PREVENT risk estimates in the ",
            prevent_sdi_models[[mod]], " Model | Using SDI Level ", i)
    calculate_prevent_riskest.fun(
      data = nhanes_processed, prevent_model = mod, sdi = i)
  }) ; return(prevent_results.l)
}) ; names(prevent_sdi_results.l) <- names(prevent_sdi_models)


# add list labels -------------
names(prevent_sdi_results.l) <- paste0("sdi", 1:3)
names(prevent_sdi_results.l$sdi1) <- names(prevent_sdi_models)
names(prevent_sdi_results.l$sdi2) <- names(prevent_sdi_models)
names(prevent_sdi_results.l$sdi3) <- names(prevent_sdi_models)

## Save prevent+SDI estimates --------------------
prevent_sdi_results.l %>% saveRDS("../data/processed/prevent/nhanes_prevent_sdi_estimates_output.rds")

## Reduce prevent estimates to dataframe 
prevent_sdi_results_merge <- lapply(
  names(prevent_sdi_results.l), function(sdi) {
    lapply(names(prevent_sdi_models), function(mod) {
      prevent_sdi_results.l[[sdi]][[mod]]$results %>% 
        rename_with(., ~gsub("sdi", sdi, .)) %>% 
        rename_with(., ~gsub("full", paste0("full.", sdi), .))
      }) %>% reduce(., full_join, by = "SEQN")
    }) %>% reduce(., full_join, by = "SEQN")


# -----------------------------------------------  
## Merge in prevent_inputs and save as .csv
# -----------------------------------------------

prevent_results_merge %>% 
  full_join(prevent_sdi_results_merge, by = "SEQN") %>% 
  fwrite(., file="../data/processed/prevent/nhanes_prevent_sdi_complete.csv")

## EOF
## Last Updated: 09-04-2026




