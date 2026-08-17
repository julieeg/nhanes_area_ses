## Rscript to prepare PREVENT predictions

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


## load PREVENTR
library(preventr)

## Load processed NHANES data
nhanes_processed <- readRDS("../data/processed/nhanes_processed.rda")

#set.seed(314159)
#nhanes_dat <- nhanes_processed[sample(1:nrow(nhanes_processed), size = 1000, replace = F),]


################################################################################
## Build prevent outcomes 
################################################################################

## create prevent-specific age and sbp values based on max/min
prevent_nhanes_processsed <- nhanes_processed %>% mutate(
  prevent_age = ifelse(age>30 & age <80, age, NA),
  prevent_sex = ifelse(female == 1, "female", "male"),
  prevent_sbp = ifelse(sbp_avg>90 & sbp_avg<180, sbp_avg, NA),
  prevent_bprx = ifelse(rx_use_bpmed=="Yes",1,0),
  prevent_tc = ifelse(tc>130 & tc<320, tc, NA),
  prevent_hdl = ifelse(hdl>20 & hdl<100, hdl, NA),
  prevent_statin = ifelse(rx_use_statin=="Yes",1,0),
  prevent_diab = diabetes,
  prevent_smoking = smoke_current, 
  prevent_egfr = ifelse(egfr_ckdepi >15 & egfr_ckdepi <140, egfr_ckdepi, NA),
  prevent_egfr_race = ifelse(egfr_ckdepi_race >15 & egfr_ckdepi_race <140, egfr_ckdepi_race, NA),
  prevent_bmi = ifelse(bmi>=18.5 & bmi<=39.9, bmi, NA),
  prevent_hba1c = ifelse(hba1c>=4.5 & hba1c <=15, hba1c, NA),
  prevent_uacr = ifelse(uacr >= 0.1 & uacr <= 25000, uacr, NA)
)

prevent_nhanes_complete <- prevent_nhanes_processsed %>% 
  select(SEQN, Years, racecat, starts_with("prevent_")) %>%
  filter(complete.cases(.))


## Write wrapper function to calculate PREVENT risk estimtes, for list of model parameters
calculate_prevent_riskest.fun <- function(model_input = "base", egfr_input = "prevent_egfr",
                                          data=prevent_nhanes_complete) {
  riskest <- estimate_risk(
    use_dat = data[1:1000,] %>% select(SEQN, Years, starts_with("prevent"), racecat, 
                                       prevent_egfr_use = egfr_input),
    age=prevent_age, sex=prevent_sex, sbp=prevent_sbp, bp_tx = prevent_bprx,
    total_c = prevent_tc, hdl_c = prevent_hdl, statin = prevent_statin, 
    dm = prevent_diab, smoking = prevent_smoking, bmi = prevent_bmi, 
    egfr = prevent_egfr_use, hba1c = prevent_hba1c, uacr = prevent_uacr, 
    #zip = input_zip, 
    time = "both", model = model_input) 
  
  riskest_wide <- riskest %>%
    pivot_wider(values_from = c("total_cvd", "ascvd", "heart_failure", "chd", "stroke"),
                names_from=over_years)
  return(riskest_wide)
}

print_summary_table(data = prevent_nhanes_processsed %>% mutate(sex=prevent_sex, age=prevent_age),
                    vars_to_summarize = prevent_vars_to_select, p_adjust = "none")

prevent_vars_to_select <- names(prevent_nhanes_processsed %>% select(starts_with("prevent_")))



