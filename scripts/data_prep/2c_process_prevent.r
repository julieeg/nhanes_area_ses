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

## load PREVENTR
library(AHAprevent)

## Load processed NHANES data
nhanes_processed <- readRDS("../data/processed/nhanes_processed.rds")


################################################################################
## Build prevent outcomes 
################################################################################

## Add prevent-specific inputs
nhanes_processed <- nhanes_processed %>% 
  # Filter to 
  mutate(
    prevent_age = as.numeric(ifelse(age >= 30 & age <= 79, age, NA)),
    prevent_sex = ifelse(female == 1, 1, 0),
    prevent_sbp = case_when(
      sbp_mean < 90 ~ 90, sbp_mean >= 200 ~ 200, 
      TRUE~sbp_mean),
    prevent_bprx = ifelse(rx_use_bp == "Yes", 1, 0),
    prevent_tc = case_when(
      tc < 130 ~ 130, tc > 320 ~ 320, 
      TRUE ~ tc),
    prevent_hdl = case_when(
      hdl < 20 ~ 20, hdl > 100 ~ 100, 
      TRUE ~ hdl),
    prevent_statin = ifelse(rx_use_statin == "Yes", 1, 0),
    prevent_diab = case_when(
      diabetes == "0"~ 0, diabetes == "1" ~ 1, 
      TRUE ~ NA_integer_),
    prevent_smoking = ifelse(smoke_status == "Current smoker", 1, 0),
    prevent_egfr = case_when(
      egfr < 15 ~ 15, egfr > 150 ~ 150, 
      TRUE ~ egfr),
    prevent_bmi = case_when(
      bmi < 18.5 ~ 18.5, bmi > 40 ~ 40, 
      TRUE ~ bmi),
    prevent_hba1c = case_when(
      hba1c < 3 ~ 3, hba1c > 15 ~ 15, 
      TRUE ~ hba1c),
    prevent_uacr = case_when(
      uacr < 0.1 ~ 0.1, uacr > 25000 ~ 25000, 
      TRUE ~ uacr)) %>%
  mutate(
    prevent_sdi1 = 1, # Deciles 1-3
    prevent_sdi2 = 5, # Deciles 4-6
    prevent_sdi3 = 10 # Deciles 7-10
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
prevent_models <- c(base="Base", hba1c="Base+HbA1c", uacr="Base+UACR", full="Full")

calculate_prevent_riskest.fun <- function(prevent_model, data = nhanes_processed, sdi=NULL) {
  
  message("Calculating PREVENT risk estimates in the ", toupper(prevent_model), sdi, " Model")
  
  # Create subset of complete data, based on PREVENT model -------------
  if(prevent_model %in% c("base", "sdi")) { 
    use_dat <- data %>% filter(prevent_complete_base==1) }
  if (prevent_model == "hba1c") {
    use_dat <- data %>% filter(prevent_complete_baseA1c == 1) }
  if(prevent_model == "uacr") {
    use_dat <- data %>% filter(prevent_complete_baseUACR == 1) } 
  if(prevent_model %in% c("full", "full.sdi")) {
    use_dat <- data %>% filter(prevent_complete_full == 1) 
  }
  
  # List of prevent outcomes -------------
  prevent_outcomes <- c("CVD", "ASCVD", "HF")
  
  # set SDI level 
  sdi.dat <- if (!is.null(sdi)) {
    sdiX <- sym(paste0("prevent_sdi", sdi)) ; use_dat[[sdiX]] 
    } else NA 
  
  # Calculate each risk estimate, at 10 & 30 years, in each model ---------- 
  if(prevent_model == "base") {
    
    riskest <- AHAprevent::prevent_base(
      sex=use_dat$prevent_sex, age=use_dat$prevent_age, tc=use_dat$prevent_tc, 
      hdl=use_dat$prevent_hdl, sbp=use_dat$prevent_sbp, dm=use_dat$prevent_diab, 
      smoking=use_dat$prevent_smoking, bmi=use_dat$prevent_bmi, 
      egfr=use_dat$prevent_egfr, bptreat=use_dat$prevent_bprx, 
      statin=use_dat$prevent_statin)
    
  } else if(prevent_model == "uacr") {
    
    riskest <- AHAprevent::prevent_uacr(
      sex=use_dat$prevent_sex, age=use_dat$prevent_age, tc=use_dat$prevent_tc, 
      hdl=use_dat$prevent_hdl, sbp=use_dat$prevent_sbp, dm=use_dat$prevent_diab, 
      smoking=use_dat$prevent_smoking, bmi=use_dat$prevent_bmi, 
      egfr=use_dat$prevent_egfr, bptreat=use_dat$prevent_bprx, statin=use_dat$prevent_statin,
      uacr=use_dat$prevent_uacr)
    
  } else if(prevent_model == "hba1c") {
    
    riskest <- AHAprevent::prevent_hba1c(
      sex=use_dat$prevent_sex, age=use_dat$prevent_age, tc=use_dat$prevent_tc, 
      hdl=use_dat$prevent_hdl, sbp=use_dat$prevent_sbp, dm=use_dat$prevent_diab, 
      smoking=use_dat$prevent_smoking, bmi=use_dat$prevent_bmi, 
      egfr=use_dat$prevent_egfr, bptreat=use_dat$prevent_bprx, statin=use_dat$prevent_statin,
      hba1c=use_dat$prevent_hba1c)
    
  }else if(prevent_model == "sdi") {
      
      riskest <- AHAprevent::prevent_sdi(
        sex=use_dat$prevent_sex, age=use_dat$prevent_age, tc=use_dat$prevent_tc, 
        hdl=use_dat$prevent_hdl, sbp=use_dat$prevent_sbp, dm=use_dat$prevent_diab, 
        smoking=use_dat$prevent_smoking, bmi=use_dat$prevent_bmi, 
        egfr=use_dat$prevent_egfr, bptreat=use_dat$prevent_bprx, statin=use_dat$prevent_statin,
        sdi=sdi.dat) %>% 
        rename_with(., ~gsub("sdi", paste0("sdi",sdi), .))
      
    } else if(prevent_model == "full") {
      
      riskest <- AHAprevent::prevent_full(
        sex=use_dat$prevent_sex, age=use_dat$prevent_age, tc=use_dat$prevent_tc, 
        hdl=use_dat$prevent_hdl, sbp=use_dat$prevent_sbp, dm=use_dat$prevent_diab, 
        smoking=use_dat$prevent_smoking, bmi=use_dat$prevent_bmi, 
        egfr=use_dat$prevent_egfr, bptreat=use_dat$prevent_bprx, statin=use_dat$prevent_statin,
        uacr=use_dat$prevent_uacr, hba1c=use_dat$prevent_hba1c, sdi=NA)
      
    } else if(prevent_model == "full.sdi") {
      
      riskest <- AHAprevent::prevent_full(
        sex=use_dat$prevent_sex, age=use_dat$prevent_age, tc=use_dat$prevent_tc, 
        hdl=use_dat$prevent_hdl, sbp=use_dat$prevent_sbp, dm=use_dat$prevent_diab, 
        smoking=use_dat$prevent_smoking, bmi=use_dat$prevent_bmi, 
        egfr=use_dat$prevent_egfr, bptreat=use_dat$prevent_bprx, statin=use_dat$prevent_statin,
        uacr=use_dat$prevent_uacr, hba1c=use_dat$prevent_hba1c, sdi=sdi.dat) %>% 
        rename_with(., ~gsub("full", paste0("full.sdi",sdi), .))
    }
  
  ## Unnest the lists
  riskest <- riskest %>% mutate(across(where(is.list), unlist))
  
  ## Compile risk estimates & reshape to long
  return(cbind(use_dat %>% select(SEQN), riskest))
  
}


## ================================================================
## Run function for all outcomes, at 10 and 30 yr, in each model
## ================================================================

## PREVENT estimates for base, hba1c, uacr and full models -----------
prevent_results <- lapply(names(prevent_models), function(mod) {
  calculate_prevent_riskest.fun(prevent_model = mod, data = nhanes_processed)
  }) %>% reduce(., full_join, by = "SEQN")

str(prevent_results)


# -----------------------------------
## Run with different SDI levels
# -----------------------------------

prevent_sdi_models <- c(sdi="Base+SDI", full.sdi="Full+SDI")

prevent_sdi_results <- lapply(c(1:3), function(i){
  prevent_results.l <- lapply(names(prevent_sdi_models), function(mod) {
    calculate_prevent_riskest.fun(prevent_model = mod, data = nhanes_processed, sdi = i)
  }) ; return(reduce(prevent_results.l, full_join, by = "SEQN"))
  }) %>% reduce(., full_join, by = "SEQN") 


# ================================================================
## Merge in prevent_inputs; and Add CLASSIFICATION variable
# ================================================================

## Reduce prevent estimates to dataframe 
prevent_results_merge <- prevent_results %>% 
  full_join(., prevent_sdi_results, by = "SEQN") %>% 
  left_join(nhanes_processed %>% select(SEQN, starts_with("prevent_")))

prevent_vars <- prevent_results_merge %>% 
  select(contains(c("CVD", "ASCVD", "HF"))) %>% 
  names()

## Derived risk classifications for all PREVENT estimates
prevent_riskclass <- lapply(prevent_vars, function(var) {
  prevent_results_merge %>% 
    select(SEQN, var=var) %>% 
    mutate(
      x = ifelse(!is.na(var), case_when(
        var < 5 ~ "Below 5%", #lt_5pct
        var >= 5 & var < 7.5 ~ "5-7.5%", #bw_5to7.5pct
        var >= 7.5 & var < 10 ~ "7.5-10%", #bw_7.5to10pct
        var >= 10 & var <20 ~ "10-20%", #bw_10to20pct
        var >= 20 ~ "Gte 20%", #gte_20pct
        TRUE ~ NA_character_), NA_character_)) %>% 
    mutate_at("x", ~factor(., levels=c(
      "Below 5%", "5-7.5%", "7.5-10%", "10-20%", "Gte 20%"))) %>% 
    select(SEQN, x) %>% 
    rename_with(., ~gsub("x", paste0(var, "_riskclass"), .))
  })%>% reduce(., full_join, by="SEQN")

prevent_results_merge <- prevent_results_merge %>% 
  left_join(prevent_riskclass, by = "SEQN")

# ================================================
## Calculate change in risk estimate with +SDI
# ================================================
## Add change in prevnt risk estimates, with each SDI value:

prevent_results_merge <- prevent_results_merge %>% 
  mutate(
    # % crossing up/down across 10yrCVD >=7.5% for the hypertension guidelines
    # base/base+sdi
    prevent_sdi1_10yr_CVD_reclass = case_when(
      prevent_base_10yr_CVD < 7.5 & prevent_sdi1_10yr_CVD >= 7.5 ~ "Increase above 7.5%",
      prevent_base_10yr_CVD >= 7.5 & prevent_sdi1_10yr_CVD < 7.5 ~ "Decrease below 7.5%",
      TRUE ~ "No change"), 
    prevent_sdi2_10yr_CVD_reclass = case_when(
      prevent_base_10yr_CVD < 7.5 & prevent_sdi2_10yr_CVD >= 7.5 ~ "Increase above 7.5%",
      prevent_base_10yr_CVD >= 7.5 & prevent_sdi2_10yr_CVD < 7.5 ~ "Decrease below 7.5%",
      TRUE ~ "No change"), 
    prevent_sdi3_10yr_CVD_reclass = case_when(
      prevent_base_10yr_CVD < 7.5 & prevent_sdi3_10yr_CVD >= 7.5 ~ "Increase above 7.5%",
      prevent_base_10yr_CVD >= 7.5 & prevent_sdi3_10yr_CVD < 7.5 ~ "Decrease below 7.5%",
      TRUE ~ "No change"), 
    # full/full+sdi
    prevent_full.sdi1_10yr_CVD_reclass = case_when(
      prevent_full_10yr_CVD < 7.5 & prevent_full.sdi1_10yr_CVD >= 7.5 ~ "Increase above 7.5%",
      prevent_full_10yr_CVD >= 7.5 & prevent_full.sdi1_10yr_CVD < 7.5 ~ "Decrease below 7.5%",
      TRUE ~ "No change"), 
    prevent_full.sdi2_10yr_CVD_reclass = case_when(
      prevent_full_10yr_CVD < 7.5 & prevent_full.sdi2_10yr_CVD >= 7.5 ~ "Increase above 7.5%",
      prevent_full_10yr_CVD >= 7.5 & prevent_full.sdi2_10yr_CVD < 7.5 ~ "Decrease below 7.5%",
      TRUE ~ "No change"), 
    prevent_full.sdi3_10yr_CVD_reclass = case_when(
      prevent_full_10yr_CVD < 7.5 & prevent_full.sdi3_10yr_CVD >= 7.5 ~ "Increase above 7.5%",
      prevent_full_10yr_CVD >= 7.5 & prevent_full.sdi3_10yr_CVD < 7.5 ~ "Decrease below 7.5%",
      TRUE ~ "No change")
    ) %>% 
  mutate(
    # % crossing up/down across 10yrASCVD>=5% and >=7.5%
    # base/base+sdi
    prevent_sdi1_10yr_ASCVD_reclass = case_when(
      prevent_base_10yr_ASCVD < 5 & prevent_sdi1_10yr_ASCVD >= 5 ~ "Increase above 5%",
      prevent_base_10yr_ASCVD >= 5 & prevent_sdi1_10yr_ASCVD < 5 ~ "Decrease below 5%",
      prevent_base_10yr_ASCVD < 7.5 & prevent_sdi1_10yr_ASCVD >= 7.5 ~ "Increase above 7.5%",
      prevent_base_10yr_ASCVD >= 7.5 & prevent_sdi1_10yr_ASCVD < 7.5 ~ "Decrease below 7.5%",
      TRUE ~ "No change"), 
    prevent_sdi2_10yr_ASCVD_reclass = case_when(
      prevent_base_10yr_ASCVD < 5 & prevent_sdi2_10yr_ASCVD >= 5 ~ "Increase above 5%",
      prevent_base_10yr_ASCVD >= 5 & prevent_sdi2_10yr_ASCVD < 5 ~ "Decrease below 5%",
      prevent_base_10yr_ASCVD < 7.5 & prevent_sdi2_10yr_ASCVD >= 7.5 ~ "Increase above 7.5%",
      prevent_base_10yr_ASCVD >= 7.5 & prevent_sdi2_10yr_ASCVD < 7.5 ~ "Decrease below 7.5%",
      TRUE ~ "No change"), 
    prevent_sdi3_10yr_ASCVD_reclass = case_when(
      prevent_base_10yr_ASCVD < 5 & prevent_sdi3_10yr_ASCVD >= 5 ~ "Increase above 5%",
      prevent_base_10yr_ASCVD >= 5 & prevent_sdi3_10yr_ASCVD < 5 ~ "Decrease below 5%",
      prevent_base_10yr_ASCVD < 7.5 & prevent_sdi3_10yr_ASCVD >= 7.5 ~ "Increase above 7.5%",
      prevent_base_10yr_ASCVD >= 7.5 & prevent_sdi3_10yr_ASCVD < 7.5 ~ "Decrease below 7.5%",
      TRUE ~ "No change"), 
    # full/full+sdi
    prevent_full.sdi1_10yr_ASCVD_reclass = case_when(
      prevent_full_10yr_ASCVD < 5 & prevent_full.sdi1_10yr_ASCVD >= 5 ~ "Increase above 5%",
      prevent_full_10yr_ASCVD >= 5 & prevent_full.sdi1_10yr_ASCVD < 5 ~ "Decrease below 5%",
      prevent_full_10yr_ASCVD < 7.5 & prevent_full.sdi1_10yr_ASCVD >= 7.5 ~ "Increase above 7.5%",
      prevent_full_10yr_ASCVD >= 7.5 & prevent_full.sdi1_10yr_ASCVD < 7.5 ~ "Decrease below 7.5%",
      TRUE ~ "No change"), 
    prevent_full.sdi2_10yr_ASCVD_reclass = case_when(
      prevent_full_10yr_ASCVD < 5 & prevent_full.sdi2_10yr_ASCVD >= 5 ~ "Increase above 5%",
      prevent_full_10yr_ASCVD >= 5 & prevent_full.sdi2_10yr_ASCVD < 5 ~ "Decrease below 5%",
      prevent_full_10yr_ASCVD < 7.5 & prevent_full.sdi2_10yr_ASCVD >= 7.5 ~ "Increase above 7.5%",
      prevent_full_10yr_ASCVD >= 7.5 & prevent_full.sdi2_10yr_ASCVD < 7.5 ~ "Decrease below 7.5%",
      TRUE ~ "No change"), 
    prevent_full.sdi3_10yr_ASCVD_reclass = case_when(
      prevent_full_10yr_ASCVD < 5 & prevent_full.sdi3_10yr_ASCVD >= 5 ~ "Increase above 5%",
      prevent_full_10yr_ASCVD >= 5 & prevent_full.sdi3_10yr_ASCVD < 5 ~ "Decrease below 5%",
      prevent_full_10yr_ASCVD < 7.5 & prevent_full.sdi3_10yr_ASCVD >= 7.5 ~ "Increase above 7.5%",
      prevent_full_10yr_ASCVD >= 7.5 & prevent_full.sdi3_10yr_ASCVD < 7.5 ~ "Decrease below 7.5%",
      TRUE ~ "No change")) %>% 
  # % crossing up/down across 30yrASCVD >= 20%
  mutate(
    # base/base+sdi
    prevent_sdi1_30yr_ASCVD_reclass = case_when(
      prevent_base_30yr_ASCVD < 20 & prevent_sdi1_30yr_ASCVD >= 20 ~ "Increase above 20%",
      prevent_base_30yr_ASCVD >= 20 & prevent_sdi1_30yr_ASCVD < 20 ~ "Decrease below 20%",
      TRUE ~ "No change"), 
    prevent_sdi2_30yr_ASCVD_reclass = case_when(
      prevent_base_30yr_ASCVD < 20 & prevent_sdi2_30yr_ASCVD >= 20 ~ "Increase above 20%",
      prevent_base_30yr_ASCVD >= 20 & prevent_sdi2_30yr_ASCVD < 20 ~ "Decrease below 20%",
      TRUE ~ "No change"), 
    prevent_sdi3_30yr_ASCVD_reclass = case_when(
      prevent_base_30yr_ASCVD < 20 & prevent_sdi3_30yr_ASCVD >= 20 ~ "Increase above 20%",
      prevent_base_30yr_ASCVD >= 20 & prevent_sdi3_30yr_ASCVD < 20 ~ "Decrease below 20%",
      TRUE ~ "No change"), 
    # full/full+sdi
    prevent_full.sdi1_30yr_ASCVD_reclass = case_when(
      prevent_full_30yr_ASCVD < 20 & prevent_full.sdi1_30yr_ASCVD >= 20 ~ "Increase above 20%",
      prevent_full_30yr_ASCVD >= 20 & prevent_full.sdi1_30yr_ASCVD < 20 ~ "Decrease below 20%",
      TRUE ~ "No change"), 
    prevent_full.sdi2_30yr_ASCVD_reclass = case_when(
      prevent_full_30yr_ASCVD < 20 & prevent_full.sdi2_30yr_ASCVD >= 20 ~ "Increase above 20%",
      prevent_full_30yr_ASCVD >= 20 & prevent_full.sdi2_30yr_ASCVD < 20 ~ "Decrease below 20%",
      TRUE ~ "No change"), 
    prevent_full.sdi3_30yr_ASCVD_reclass = case_when(
      prevent_full_30yr_ASCVD < 20 & prevent_full.sdi3_30yr_ASCVD >= 20 ~ "Increase above 20%",
      prevent_full_30yr_ASCVD >= 20 & prevent_full.sdi3_30yr_ASCVD < 20 ~ "Decrease below 20%",
      TRUE ~ "No change")
  )
  

# ================================================
## Calculate change in risk estimate with +SDI
# ================================================
prevent_root.vars <- names(prevent_results_merge %>% select(
  !contains("riskclass") & contains("yr") & contains(c("base_", "full_")))
)

prevent_results_merge <- prevent_results_merge %>% full_join(
  lapply(prevent_root.vars, function(x) {
    lapply(1:3, function(i){
      sdi_var <- gsub("base", paste0("sdi",i), gsub("full", paste0("full.sdi",i), x))
      prevent_results_merge %>% 
        select(SEQN, ref=x, sdi=sdi_var) %>%
        mutate(diff = sdi - ref,
               pctdiff = ((sdi - ref)/ref)*100) %>% 
        select(SEQN, diff, pctdiff) %>%
        rename_with(., ~paste0(paste0(x, "_", gsub("diff", paste0("diff_sdi",i), .))), -SEQN) 
    }) %>% reduce(., full_join, by="SEQN") 
    }) %>% reduce(., full_join, by = "SEQN"),
  by="SEQN")

prevent_results_merge %>%
  fwrite(., file="../data/processed/prevent/nhanes_prevent_sdi_complete_v2.csv")

## EOF
## Last Updated: 10-15-2026




