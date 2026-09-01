## Postprocess NHANES datafile to create analytical datasets

################################################################################
## Apply post-processing to build final NHANES dataset
################################################################################

## =========================================================
## Load in processed datasets 
## =========================================================

## nhanes phenotypes, from 1999-2020 -------------
nhanes_processed <- readRDS("../data/processed/nhanes_processed.rds")

## ndi nhanes data, from 1999 to 2018 ---------------
nhanes_ndi_raw <- fread("../data/raw/ndi_nhanes/nhanes_ndi_raw.csv") 

## prevent estimated in nhanes, from 1999 to 2020 -----------------
nhanes_prevent_raw <- fread("../data/processed/prevent/nhanes_prevent_complete.csv")


## =====================================================
## Create analytical dataframes 
## =====================================================

## Prepare NDI data -------
nhanes_ndi_processed <- nhanes_ndi_raw %>% 
  # Convert follow-up months to person years; NOTE: var used depends on WEIGHT
  mutate(
    ndi_peryear_int = ndi_permth_int / 12,
    ndi_peryear_exm = ndi_permth_exm / 12
  ) %>% 
  # Filter to eligible participants, only
  filter(ndi_eligstat == 1)


## Prepare PREVENT data ------------

base_vars <- names(nhanes_processed %>% select(
  "Years", "SEQN", "RIDSTATR", starts_with("SD"), starts_with("WT"), -"wt"))

nhanes_prevent_processed <- nhanes_prevent_raw %>% 
  left_join(
    nhanes_processed %>% select(
      SEQN, age, ascvd, ckd, diabetes, diabetes_undx, ldl, sbp_mean, dbp_mean), 
    by="SEQN") %>%
  ## Participant sub-sets based on outcomes: 
  mutate(
    # For Dyslipidemia: a) 30-39 & no ASCVD | 40-79 no ASCVD or diabetes
    prevent_include_dyslip = case_when(
      c(age < 39 & ascvd == 0 | age >= 39 & ascvd == 0 & diabetes == 0) & 
        prevent_statin == 0 & ldl > 70 & ldl < 189 ~ 1,
      TRUE ~ 0),
    # b) no statin use; c) LDLc between 70-189 mg/dL
    prevent_include_bp = case_when(
      ascvd == 0 & diabetes == 0 & diabetes_undx == 0 & ckd == 0 &
        prevent_bprx != 1 & sbp_mean < 139 & dbp_mean < 89 ~ 1,
      TRUE ~ 0)
  ) %>% select(-c(
    age, ascvd, ckd, diabetes, diabetes_undx, ldl, sbp_mean, dbp_mean), 
  )


## Combine all nhanes_processed datasets & Save --------------
nhanes_postprocessed <- full_join(
  nhanes_processed, nhanes_ndi_processed, by = c("SEQN", "Years")) %>%
  full_join(nhanes_prevent_processed, by = c("SEQN"))
  
nhanes_postprocessed %>% saveRDS("../data/processed/nhanes_postprocessed_linked_ndi_prvnt.rds")
nhanes_postprocessed %>% fwrite("../data/processed/nhanes_postprocessed_linked_ndi_prvnt.csv")


################################################################################
## Build NHANES data-dictionary, including all variables
################################################################################

# A complete data-dictionary is required, containing the final variable names 
# (in nhanes_postprocessed) and the raw variable inputs.

## EOF
# Last Updated: 08-28-2026
