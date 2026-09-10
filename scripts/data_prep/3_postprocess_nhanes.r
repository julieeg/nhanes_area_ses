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
#nhanes_prevent_raw <- fread("../data/processed/prevent/nhanes_prevent_complete.csv")
nhanes_prevent_raw <- fread("../data/processed/prevent/nhanes_prevent_sdi_complete.csv")


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
  filter(ndi_eligstat == 1) %>% 
  mutate(across(c("ndi_eligstat", "ndi_mortstat", "ndi_diabetes", "ndi_htn"), ~as.factor(.)))


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
nhanes_postprocessed <- left_join(
  nhanes_processed, nhanes_ndi_processed, by = c("SEQN", "Years")) %>%
  full_join(nhanes_prevent_processed, by = c("SEQN"))


################################################################################
## Assign raw & derived NHANES variables to WEIGHT categories
################################################################################

## a. Harmonize Interview and Exam weights across 2-yr cycles from 1999-2000 and 2021-2023 
## with the pre/post pandemic (3.2) year cycle from 2017-March, 2020

nhanes_postprocessed <- nhanes_postprocessed %>% 
  mutate(
    WTINT.COMBN = case_when(
      Years %in% c("1999-2000", "2001-2002") ~ WTINT4YR,
      Years == "2017-2020" ~ WTINTPRP, 
      TRUE ~ WTINT2YR),
    WTMEC.COMBN = case_when(
      Years %in% c("1999-2000", "2001-2002") ~ WTMEC4YR,
      Years == "2017-2020" ~ WTMECPRP, 
      TRUE ~ WTMEC2YR),
    WTSAF.COMBN = case_when(
      Years %in% c("1999-2000", "2001-2002") ~ WTSAF4YR,
      Years == "2017-2020" ~ WTSAFPRP, 
      TRUE ~ WTSAF2YR), 
    WTDR.COMBN = case_when(
      Years == "2017-2020" ~ WTDR2DPP, 
      TRUE ~ WTDR2D), 
    # Combined weights for toxins/PCBs 
    WTSPO.COMBN = case_when(
      Years == "2003-2004" ~ WTSC2YR,
      Years %in% c("1999-2000", "2001-2002") ~ WTSPO4YR,
      TRUE ~ NA),
    # Combined weights for enviro phenols (BPA)
    WTSEPH.COMBN = case_when(
      Years == "2003-2004" ~ WTSC2YR,
      Years %in% c("2005-2006", "2007-2008", "2009-2010", "2013-2014", "2015-2016") ~ WTSB2YR,
      Years == "2011-2012" ~ WTSA2YR,
      TRUE ~ NA),
    WTYRS = case_when(
      Years %in% c("1999-2000", "2001-2002") ~ 4,
      Years == "2017-2020" ~ 3.2, 
      TRUE ~ 2),
    nYRS = ifelse(Years == "2017-2020", 3.2, 2)
  )


## Save nhanes_postprocessed data, weight combined weight variables -----------
nhanes_postprocessed %>% saveRDS("../data/processed/nhanes_postprocessed_linked_ndi_prvnt_sdi.rds")
nhanes_postprocessed %>% fwrite("../data/processed/nhanes_postprocessed_linked_ndi_prvnt_sdi.csv")


################################################################################
## Build NHANES data-dictionary, including all variables
################################################################################

# A complete data-dictionary is required, containing the final variable names 
# (in nhanes_postprocessed) and the raw variable inputs.

nhanes_var_datadict <- as.data.frame(
  matrix(names(nhanes_postprocessed), dimnames = list(NULL, "Variable.Name"))) %>% 
  left_join(., 
    nhanes_vars_datadict %>% select(Variable.Name = New.Variable.Name, Raw.Variable.Names) %>% 
      filter(Variable.Name %in% names(nhanes_postprocessed)) %>% 
      distinct() %>% 
      group_by(Variable.Name) %>% 
      reframe(Raw.Variables = paste0(unique(Raw.Variable.Names), collapse=",")),
    by = "Variable.Name") %>% 
    rowwise() %>% 
    mutate(Assigned.Weight = find_nhanes_weight.fun(Variable.Name, data = nhanes_postprocessed)) 

## Add columns for variable type; category levels and value range
var_descr.fun <- function(variable, return) {
  var_dat <- nhanes_postprocessed %>% pull(sym(variable))
  var_type <- ifelse(is.numeric(var_dat), "Numeric", "Categorical")
  if(return == "var_type") { var_type } else if(return == "var_summary") {
    if(var_type == "Categorical") { ifelse(
      is.factor(var_dat), paste0("Levels: ", paste0(levels(var_dat), collapse = "; ")),
      paste0("Values: ", paste0(unique(var_dat), collapse = "; "))) 
    } else { sprintf("Mean: %s; Median: %s; Range: [%s, %s]", 
                     round(mean(var_dat, na.rm=T),1), round(median(var_dat, na.rm=T),1), 
                     round(min(var_dat, na.rm=T),1), round(max(var_dat, na.rm=T),1)) }
  }
}

nhanes_var_datadict <- nhanes_var_datadict %>% 
  mutate(var_type = var_descr.fun(Variable.Name, return="var_type")) %>% 
  mutate(var_summary = var_descr.fun(Variable.Name, return="var_summary")) 
  
nhanes_var_datadict %>% fwrite(., "./nhanes_variable_datadict_09102026.csv")
View(nhanes_var_datadict)


## EOF
# Last Updated: 09-08-2026

