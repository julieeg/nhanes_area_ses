# Rscript for cleaning nhanes data 


################################################################################
## Set Up & Load Required Packages 
################################################################################

## set local directory
#setwd('C:/Users/sarac/OneDrive/Documents/Research')
setwd('~/Documents/GitHub/nhanes_area_ses/run')

## load required base pacakges
list_of_packages <- c(
  "tidyverse", "data.table", "nhanesA", "progress", "sociome", "jsonlite", 
  "haven", "forcats" , "stringdist"
) ; invisible(lapply(list_of_packages, function(pkg) {
  if(!requireNamespace(pkg, quietly = TRUE)) { 
    install.packages(pkg) } ; library(pkg, character.only = TRUE)
}))

## List data categories with labels
data_categories <- c("DEMO", "LABORATORY", "EXAM", "QUESTIONNAIRE", "DIET")
data_categories.labs <- c("Demographics"="DEMO", "Laboratory"="LABORATORY", 
                          "Examination"="EXAM", "Questionnaire"="QUESTIONNAIRE", 
                          "Dietary"="DIET")


###############################################################
## Load pre-built NHANES index tables & variable summaries
###############################################################

## Description of all NHANES tables ---------------
nhanes_tables_all <- fread("../data/raw/nhanes_tablelist_all.csv")

## List of all NHANES variables --------------------
nhanes_vars_all <- fread("../data/raw/nhanes_varlist_all.csv") 

## Load ALL nhanes variables
nhanes_data_raw <- fread("../data/raw/nhanes_data_raw.csv")

## List NHANES years ---------
nhanes_yrs <- names(table(nhanes_tables_all$Years))
nhanes_yrs <- nhanes_yrs[-c(2,7,14)] # remove 1999-2004, 2007-2012 and 2021-2023

# Make array of years/labels for selecting tables
names(nhanes_yrs) <- c("", paste0("_", LETTERS[2:10]),"P_")

## Load project data dictionary to grab years, tables & variables of interest
project_datadict <- readxl::read_xlsx("./nhanes_pullrequest_IndVsArea_09012026.xlsx")

## Load all nhanes tables & var descriptions
table_cats <- c("demo", "lab", "exam", "quest", "diet")
nhanes_tables_raw.l <- lapply(table_cats, function(cat) {
  readRDS(sprintf("../data/raw/nhanes_%s_tables.rds", cat)) 
}) ; names(nhanes_tables_raw.l) <- table_cats

# Load datadictionary for selection nhanes vars
nhanes_vars_datadict <- fread("../data/raw/nhanes_vars_datadict.csv")


################################################################################
## NHANES Variable Cleaning and Data Set Preparation 
################################################################################

base_vars <- names(nhanes_data_raw %>% select(
  "Years", "SEQN", "RIDSTATR", starts_with("SD"), starts_with("WT"), -"wt")
  )

## Count n participants that completed BOTH interview + exams
nhanes_data_raw <- nhanes_data_raw %>% 
  mutate_at("RIDSTATR", ~gsub("Both Int", "Both int", gsub("Only", "only", .))) 

nhanes_data_raw %>%
  filter(age >= 18) %>%
  group_by(RIDSTATR) %>% reframe(n=n())
  # RIDSTATR                               n
  # Both interviewed and MEC examined   59799 (71669) **
  # Interviewed Only                     3242 (5381)


# Write generic function to recode missingness as NA
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


## ==================================
## Prepare DEMOGRAPHICS variables 
## ==================================

#racethn.labs <- c(
#  "NHW" = "Non-Hispanic White", "NHB" = "Non-Hispanic Black", 
#  "NHAsian" = "Non-Hispanic Asian", "Mexican-American" = "Mexican American", 
#  "Other Hispanic" = "Other Hispanic", 
#  "Other/Multi-Racial" = "Other Race - Including Multi-Racial")

racethn_abbrev.labs <- c(
  "NHW" = "Non-Hispanic White", "NHB" = "Non-Hispanic Black", 
  "NHA" = "Non-Hispanic Asian", "MexAm" = "Mexican American", 
  "OthHis" = "Other Hispanic", 
  "Oth/Mult" = "Other Race - Including Multi-Racial")


#educ_level.labs <- c(
#  "Less than 9th grade"="Less than 9th grade", 
#  "9-11th grade (includes 12th grade with no diploma)"="9-11th grade", 
#  "High school graduate/ged or equivalent"="HS graduate or GED", 
#  "High school grad/ged or equivalent"="HS graduate or GED",
#  "Some college or aa degree"="Some college or AA degree",
#  "College graduate or above"="College graduate or above")

educ_level_abbrev.labs <- c(
  "Less than 9th grade"="LessThan9th", 
  "9-11th grade (includes 12th grade with no diploma)"="SomeHS_9to11", 
  "High school graduate/ged or equivalent"="HSgrad_GED", 
  "High school grad/ged or equivalent"="HSgrad_GED",
  "Some college or aa degree"="SomeColl_AA",
  "College graduate or above"="Collgrad_Above")

# income levels
inc.vals <- c(
  "$0-$4,999", "$5,000-$9,999", "$10,000-$14,999", "$15,000-$19,999",
  "Under $20,000", "Over $20,000", "$20,000-$24,999", "$25,000-$34,999", 
  "$35,000-$44,999", "$45,000-$54,999", "$55,000-$64,999", "$65,000-$74,999",
  "Over $75,000", "$75,000-$99,999", "Over $100,000")


## Build nhanes_demo_processed --------------------------
nhanes_demo_processed <- nhanes_data_raw %>% 
  
  # Age & sex --------------------
  mutate(
    age_4lvl = factor(case_when(
      age < 30 ~ "under30y", 
      age >= 30 & age < 40 ~ "30to39y",
      age >=40 & age < 65 ~ "40to64y",
      age >= 65 ~ "over65y",
      TRUE ~ NA), levels=c("under30y", "30to39y", "40to64y", "over65y")),
    female = as.factor(ifelse(gender == "Female", 1, 0))) %>%
  
  # Race/ethnicity --------------------
  mutate_at("racethn_addNHA", ~ifelse(.=="", racethn1, .)) %>%
  mutate(
    racethn = add_descr_labels(., "racethn1", racethn_abbrev.labs, ordered = T),       
    racethn_addNHA = add_descr_labels(., "racethn_addNHA", racethn_abbrev.labs, ordered = T)) %>%
  mutate(
    racethn_combn = case_when(
      racethn_addNHA == "Oth/Mult" & 
        Years %in% c(nhanes_yrs[1:6]) ~ "Oth/Mult/NHA", # Definitions changed Pre-/Post-2011
      TRUE ~ racethn_addNHA)) %>%
  mutate(
    race_white = case_when(
      racethn == "NHW"~1, 
      is.na(racethn) ~ NA, 
      TRUE ~ 0)) %>%
  
  # Education levels --------------------
  mutate(across(
    c("educ_under20", "educ_20plus"), ~recode_nhanes_na.fun(str_to_sentence(.)))) %>%
  mutate(
    educ_level = case_when(
      age < 18 ~ NA, 
      age >= 18 & age < 20 ~ str_to_sentence(educ_under20), 
      age >= 20 ~ str_to_sentence(educ_20plus),
      TRUE ~ NA)) %>% 
  mutate_at("educ_level", ~case_when(
    . %in% c(paste(c("9th", "10th", "11th"), "grade"), "12th grade, no diploma",
             "9-11th grade (includes 12th grade with no diploma)") ~ "SomeHS_9to11",
    . %in% c("Ged or equivalent", "High school graduate") ~ "HSgrad_GED",
    . == "More than high school" ~ "SomeColl_AA",
    TRUE ~ unname(educ_level_abbrev.labs[.]))) %>% 
  mutate_at("educ_level", ~ factor(
    ., levels=unique(rev(unname(educ_level_abbrev.labs))))) %>% 
    
  # Income level --------------------
  mutate_at(c("income_hh", "income_fam"), ~factor(gsub("er", "er ", gsub(" ", "", gsub(" to ", "-", .))))) %>% 
  mutate_at(c("income_hh", "income_fam"), ~factor(case_when(
    . == "$75,000andOver " ~ "Over $75,000",
    . == "$100,000andOver " ~ "Over $100,000",
    . == "Don'tknow" ~ "Don't know", 
    TRUE ~ .), levels=inc.vals)) %>%
  mutate_at(c("income_hh", "income_fam"), ~recode_nhanes_na.fun(.)) %>%
  
  # Family Income-Poverty Ratio (f-PIR) ---------------
  mutate(
    inc_to_pov_level = case_when(
      inc_to_pov < 1 ~"LowInc",
      inc_to_pov >= 1 & inc_to_pov < 4 ~ "MiddleInc",
      inc_to_pov >= 4 ~ "HighInc",
      TRUE ~ NA)) %>%
  mutate_at("inc_to_pov_level", ~ factor(., levels=c(
    "HighInc", "MiddleInc", "LowInc"))) %>%
  
  select(base_vars, age, age_4lvl, gender, female, 
         racethn, racethn_addNHA, racethn_combn, educ_level, 
         income_hh, income_fam, inc_to_pov, inc_to_pov_level)

demo_vars <- nhanes_demo_processed %>% select(-base_vars) %>% names()


## ==================================
## Prepare EXAM variables 
## ==================================

exam_cat_vars <- nhanes_vars_datadict %>% 
  filter(Category == "exam") %>%
  filter(!grepl("taste_", New.Variable.Name) & 
           !grepl("smell_", New.Variable.Name)) %>%
  pull(New.Variable.Name) %>% unique()


## Chemosensory Exam -----------------------
# Write function to create "id_correct" vars for taste/smell tests
recode_chemos_correct.fun <- function(data, test_var, correct_value) {
  var_correct <- paste0(test_var, "_correct")
  data %>% mutate(
    !!var_correct := case_when(
      .data[[test_var]] == correct_value ~ 1,
      .data[[test_var]] == "" ~ NA, TRUE ~ 0
      ))
  }

nhanes_csexam_processed <- nhanes_data_raw %>%
  select(SEQN, Years, starts_with("taste_"), starts_with("smell_")) %>%
  mutate_at("taste_status", ~case_when(
    . %in% c("", "Not done") ~ NA, 
    TRUE ~ .)) %>%
  # Code in tongue-to-mouth ratio measures for taste perception
  mutate(
    taste_tip2mouth_quinine = case_when(
      !is.na(taste_tongue_quinine_glms) & !is.na(taste_mouth_quinine_glms) & 
        taste_mouth_quinine_glms >0 ~ 
        taste_tongue_quinine_glms / taste_mouth_quinine_glms,
      TRUE ~ NA),
    taste_tip2mouth_nacl_1M = case_when(
      !is.na(taste_tongue_nacl_glms) & !is.na(taste_mouth_nacl_1M_glms) & 
        taste_mouth_nacl_1M_glms > 0 ~ 
        taste_tongue_nacl_glms / taste_mouth_nacl_1M_glms,
      TRUE ~ NA)) %>% 
  # Binary correct=1/incorrect=0/missing variables for smell ID tests ----------
  recode_chemos_correct.fun(test_var = "smell_chocolate", correct_value = "Chocolate") %>%
  recode_chemos_correct.fun(test_var = "smell_strawberry", correct_value = "Strawberry") %>%
  recode_chemos_correct.fun(test_var = "smell_smoke", correct_value = "Smoke") %>%
  recode_chemos_correct.fun(test_var = "smell_leather", correct_value = "Leather") %>%
  recode_chemos_correct.fun(test_var = "smell_soap", correct_value = "Soap") %>%
  recode_chemos_correct.fun(test_var = "smell_grape", correct_value = "Grape") %>%
  recode_chemos_correct.fun(test_var = "smell_onion", correct_value = "Onion") %>%
  recode_chemos_correct.fun(test_var = "smell_gas", correct_value = "Gas") %>% 
  # Calculate total Pocket Smell Test (pst) score
  mutate(
    smell_pst_total = rowSums(select(
      ., ends_with("_correct") & starts_with("smell_")), na.rm = FALSE)) %>% 
  mutate(
    smell_dysfun_any = factor(case_when(
      smell_pst_total > 0 & smell_pst_total <= 5 ~ 1,
      !is.na(smell_pst_total) & smell_pst_total >5 ~ 0,
      TRUE ~ NA)) ) %>% 
  mutate(
    smell_dysfun_severe = factor(case_when(
      smell_pst_total %in% c(0,1,2,3) ~ 1, #"Anosmia/Severe hyposmia",
      smell_pst_total %in% c(4,5) ~ 0, #"Hyposmia",
      !is.na(smell_pst_total) & smell_pst_total > 5 ~ 0, #"Normal"
      TRUE ~ NA))) %>% 
  
  select(SEQN, Years, taste_mouth_quinine_glms, taste_mouth_nacl_1M_glms, 
         taste_mouth_nacl_320mM_glms, contains("tip2mouth"), 
         smell_pst_total, smell_dysfun_any, smell_dysfun_severe)

# merge into basic exam vars
nhanes_exam_processed <- nhanes_data_raw %>% 
  
  # Calculate average sbp/dbp, by hand
  mutate(
    sbp_mean = rowMeans(pick(sbp1, sbp2, sbp3), na.rm = T),
    dbp_mean = rowMeans(pick(dbp1, dbp2, dbp3), na.rm = T)) %>% 
  mutate_at(c("sbp_mean", "dbp_mean"), ~ifelse(is.na(.), NA, .)) %>%
  
  # Abdominal obesity ---------
  mutate(whr = waist / hip) %>%

  select(SEQN, Years, bmi, sbp_mean, dbp_mean, fev1_pre, fvc_pre,
         waist, hip, whr, wt, ht, starts_with("liver_"), 
         -starts_with(c("taste_","smell_"))) %>% 
  full_join(nhanes_csexam_processed, by = c("SEQN", "Years"))

exam_vars <- nhanes_exam_processed %>% select(-c("SEQN", "Years")) %>% names()

#rm(nhanes_csexam_processed)

## =============================================================
## Prepare LABORATORY variables (& vars for PREVENT equation) 
## =============================================================

lab_cat_vars <- nhanes_vars_datadict %>% 
  filter(Category=="lab" & !(New.Variable.Name %in% base_vars)) %>% 
  filter(!startsWith(New.Variable.Name, "WT")) %>%
  # remove outdated variable names
  filter(!New.Variable.Name %in% c("hcbenzene", "hepepox", "pcbpdiox", "u_bisphena")) %>% 
  pull(New.Variable.Name) %>% unique()


## Equation to calculate EGFR using CKD-EPI 2025 calculator --------------
# Ref: https://github.com/hayden-farquhar/NHANES-EXWAS/blob/main/00_functions.R
calc_egfr_ckdepi.fun <- function(creatinine, age, female) {
  # creatinine in mg/dL, age in years, female = 1/0
  kappa <- ifelse(female==1, 0.7, 0.9)
  alpha <- ifelse(female==1, -0.241, -0.302)
  female_mult <- ifelse(female==1, 1.012, 1.0)
  
  scr <- creatinine / kappa
  eGFR <- 142 *
    pmin(scr, 1)^alpha * 
    pmax(scr, 1)^(-1.200) *
    0.9938^age * 
    female_mult
  
  return(eGFR)
}

calc_egfr_ckdepi_race.fun <- function(creatinine, age, female, black) {
  # creatinine in mg/dL, age in years, female = 1/0
  kappa <- ifelse(female==1, 0.7, 0.9)
  alpha <- ifelse(female==1, -0.329, -0.411)
  female_mult <- ifelse(female==1, 1.018, 1)
  race_mult  <- ifelse(black==1, 1.159, 1)
  
  scr <- creatinine / kappa
  egfr_race <- 141 *
    (pmin(scr/kappa, 1) ^ alpha) *
    (pmax(scr/kappa, 1) ^ -1.209) *
    (0.993^age) * female_mult * race_mult
  
  return(egfr_race)
}

# ====================================================================
## Apply basic lab corrections to account for instrument changes 
# ====================================================================

nhanes_lab_processed <- nhanes_data_raw %>%
  # Add binary var for black race (for egfr calculation)
  mutate(racethn_black = case_when(
    racethn1 == "Non-Hispanic Black" ~ 1,
    racethn1 == "" ~ NA, TRUE ~ 0)) %>%
  select(SEQN, Years, gender, age, racethn_black, all_of(lab_cat_vars)) %>%
  mutate(female = ifelse(gender == "Female",1,0)) %>%
  
  # Serum Creatinine, from 1999-2000 ---------
  mutate_at("creatinine", ~case_when(
    Years %in% c("1999-2000", "2001-2002") ~ (1.013 * .) + 0.147,
    Years == "2005-2006" ~ (0.978 * .) - 0.016,
    TRUE ~ .)) %>% 
  
  # U_creatinine: instrument change in 2007 -----------------------
  # (https://wwwn.cdc.gov/Nchs/Data/Nhanes/Public/2007/DataFiles/ALB_CR_E.htm)
  mutate_at("u_creatinine", ~ case_when(
    Years %in% c(unname(nhanes_yrs)[1:4]) ~ case_when(
      . < 75 ~ (1.02 * sqrt(.) - 0.36)^2,
      . >= 75 & . < 250 ~ (1.05 * sqrt(.) - 0.74)^2,
      . >= 250 ~ (1.01 * sqrt(.) - 0.10)^2,
      TRUE ~ .), 
    TRUE ~ . )) %>%
  
  ## Urinary albumin-to-creatinine ratio (mg/g) for PREVENT Equation -------
  # UACR (mg/g) = l_u_albumin (ug/mL) / l_u_creatinine (mg/dL)
  mutate(uacr = (u_albumin / u_creatinine) *100) %>%
  mutate(uacr_level = factor(case_when(
    uacr <30 ~ "uacr_lt30", 
    uacr >= 30 & uacr < 100 ~ "uacr_30to100",
    uacr >= 100 & uacr < 300 ~ "uacr_100to300",
    uacr >= 300 ~ "uacr_gte300",
    TRUE ~ NA),
    levels = c("uacr_lt30", "uacr_30to100", "uacr_100to300", "uacr_gte300"))) %>%
  mutate(uacr_level.lab = factor(case_when(
    uacr_level == "uacr_lt30" ~ "Normal",
    uacr_level == "uacr_30to100" ~ "Low",
    uacr_level == "uacr_100to300" ~ "Moderate",
    uacr_level == "uacr_gte300" ~ "High"),
    levels = c("Normal", "Low", "Moderate", "High"))) %>% 
  
  ## eGFR, baed on CKD-EPI 2021 equation ----------
  mutate(egfr = calc_egfr_ckdepi.fun(creatinine = creatinine, age=age, female=female)) %>%
  mutate(egfr_race = calc_egfr_ckdepi_race.fun(
    creatinine = creatinine, age=age, female=female, black=racethn_black)) %>%
  select(-c(age, female, gender, racethn_black)) %>% 
  
  ## Fasting glucose & insulin --------------------
  mutate_at("fg", ~case_when(
    Years %in% c(unname(nhanes_yrs)[1:3]) ~ (0.9815*. + 3.5707) +1.148,
    Years == "2005-2006" ~ . + 1.148,
    TRUE ~ .)) %>% 
  mutate_at("fi", ~case_when(
    Years %in% c("1999-2000", "2001-2002") ~ 0.9501 * (1.0027*. - 2.2934) + 1.4890,
    Years == "2003-2004" ~ 0.9501 * . + 1.4890,
    TRUE ~ .)) %>% 
  
  ## Calculate ldl using Friedwald equation, to use prepared hdl var
  mutate(ldl_friedewald = ifelse(tg < 400, tc - hdl - (tg/5), NA)) %>% 
  
  ## Add TG:Glucose ratio
  mutate(tgglu = case_when(
    !is.na(tg) & !is.na(glu) ~ tg/glu,
    TRUE ~ NA)) %>% 
  
  ## Rename OGTT variable
  mutate(ogtt_2hg = `2hg`) %>%
  
  ## Vitamin levels (vitD) ---------------
  mutate_at("vitd", ~ case_when(
    Years == "2001-2002" ~ 0.95212 * . + 6.43435,
    Years == " 2003-2004" ~ 0.98284 * . + 1.72786,
    Years == "2005-2006" ~ 0.97012 * . + 8.36753,
    TRUE ~ .))  %>% 
  
  ## Hormones, & CRP levels ----------
  mutate_at("testosterone", ~case_when(
    Years %in% unname(nhanes_yrs[1:3]) ~ . * 100,
    TRUE ~ .)) %>% 
  mutate_at("crp", ~case_when(
    Years != "2015-2016" ~ 10 * .,
    TRUE ~ .)) %>% 
  
  ## Environmental toxins, phenols & heavy metals ----------
  mutate_at("cotinine", ~ case_when(
    # Cotnine (tobacco exposure): Drop 99-01, given less precise LOD (0.05) vs 
    # later (0.035). If kept, all years must be corrected to less precise LOD
    Years == "1999-2001" ~ NA, 
    TRUE ~ .)) %>%
  # NOTE: Dioxins (pcbdiox), Polychlorinated biphenyls (PCBs) & organochloride  
  # pesticides (hcb and hpeox) will be NA from 05/06 on, since it shifted to POOLED 

  ## Oral HPV -------------
  mutate_at("hpv_oral", ~ factor(case_when(
    . == "Positive" ~ 1,
    . == "Negative" ~ 0,
    . == "Not evaluated " ~ NA,
    TRUE ~ NA)))

lab_vars <- nhanes_lab_processed %>% select(-c("SEQN", "Years")) %>% names()


## ===================================================
## Prepare QUESTIONNAIRE variables 
## ====================================================

# ----------------------------------------------
## Medication use (Statins, BP, Diabetes) 
# -----------------------------------------------

rxq_url <- "https://wwwn.cdc.gov/Nchs/Data/Nhanes/Public/1988/DataFiles/RXQ_DRUG.xpt"
rxq_drug <- nhanesA::nhanesFromURL(rxq_url)


## 1) Create list of drug codes for each medication type ----------------

# STATIN medications
rx_statin <- rxq_drug %>% 
  filter(endsWith(RXDDRUG, "STATIN") & 
           RXDDCN1C == "HMG-COA REDUCTASE INHIBITORS (STATINS)") %>%
  select(RXDDRGID, RXDDRUG) %>% 
  mutate(RXUSE="statin")

# ANTI-HYPERTENSIVE (BP) medications
rx_bpx <- c("\\b.*pril\\b", "\\b.*sartan\\b", "\\b.*dipine\\b", "\\b.*thiazide\\b", 
            "\\b.*lol\\b", "\\b.*osin\\b", "Hydralazine", "Isosorbide", 
            "Spironolactone", "Triamterene", "Eplerenone", "Amiloride", 
            "Verapamil", "Diltiazem", "Chlorthalidone", "Clonidine", "Indapamide")

rx_bp <- rxq_drug %>%
  filter(grepl(paste0(rx_bpx, collapse = "|"), RXDDRUG, ignore.case = TRUE) | 
           # OR any word in RXDDRUG is within 1 character of KEYWORD (in case of typos)
           sapply(RXDDRUG, function(x) {
             words <- unlist(strsplit(x, "[; /,-]+"))
             min_dist <- min(stringdistmatrix(tolower(words), tolower(rx_bpx), method = "lv"))
             min_dist > 0 & min_dist <= 1
           })) %>%
  # Remove non-BP medications (& explicitly TIMOLOL, more common as a topical agent)
  filter(!grepl("ANTIARRHYTHMICS|NRTIS|OPHTHALMIC GLAUCOMA AGENTS|TOPICAL ANESTHETICS", 
                RXDDCN1C, ignore.case = T)) %>%
  filter(!grepl("TIMOLOL",RXDDRUG, ignore.case = T)) %>%
  select(RXDDRGID, RXDDRUG) %>% 
  mutate(RXUSE = "bp")


# GLUCOSE LOWERING/DIABETES medications
rx_diabx <- c("insulin", "metformin", "glipizide", "glyburide", "glimepiride", 
              "gliclazide", "tolbutamide", "chlorpropamide", "tolazamide", 
              "acetohexamide", "glinide", "glitazone", "gliptin") 

rx_diab <- rxq_drug %>%
  filter(
    grepl(paste(paste0(rx_diabx, collapse = "|"), collapse = "|"), RXDDRUG, ignore.case = TRUE) |
      # OR any word in RXDDRUG is within 1-2 character edits of your keywords
      sapply(RXDDRUG, function(x) {
        words <- unlist(strsplit(x, "[; /,-]+"))
        min_dist <- min(stringdistmatrix(tolower(words), tolower(rx_diabx), method = "lv"))
        min_dist > 0 & min_dist <= 2
      })) %>% 
  select(RXDDRGID, RXDDRUG) %>% 
  mutate(RXUSE = "diab")


## Add use of GLP-1 RECEPTOR AGONISTS (and dual GIP/GLP-1s)
rx_glp1x <- c("exenatide", "liraglutide", "dulaglutide", 
              "albiglutide", "lixisenatide", "semaglutide", "tirzepatide") 

rx_glp1 <- rxq_drug %>%
  filter(
    grepl(paste(rx_glp1x, collapse = "|"), RXDDRUG, ignore.case = TRUE) |
      # Match common GLP-1 suffixes (catches variations/combinations)
      grepl("glutide|senatide", RXDDRUG, ignore.case = TRUE)) %>% 
  select(RXDDRGID, RXDDRUG) %>% 
  mutate(RXUSE = "glp1")


# Merge all rxtypes into df
rx_alltypes <- rbind.data.frame(rx_statin, rx_bp, rx_diab, rx_glp1) %>% distinct()


## 2) Match participants against target medications -------------------

rx_alluse <- nhanes_data_raw %>% 
  select(SEQN, starts_with("q_rx_drugid_")) %>% 
  pivot_longer(
    cols=starts_with("q_rx_drugid_"), 
    names_to="drug_id", values_to="RXDDRGID",
    values_drop_na = TRUE) %>%
  inner_join(rx_alltypes, by = "RXDDRGID", relationship = "many-to-many") %>% 
  select(SEQN, RXDDRGID, RXDDRUG, RXUSE) %>%
  distinct() %>% 
  # Convert to wide-format to merge with nhanes_quest_processed
  group_by(SEQN, RXUSE) %>% 
  summarise(
    rx_use = "Yes",
    rx_id = paste0(unique(RXDDRGID), collapse="; "),
    rx_drug = paste0(unique(RXDDRUG), collapse="; "),
    .groups = "drop") %>%
  pivot_wider(
    id_cols = "SEQN", names_from="RXUSE", 
    values_from = c(rx_use, rx_id, rx_drug),
    names_glue="{.value}_{RXUSE}")


## 3) Merge back to main dataset & resolve No/NA values ----------------

nhanes_quest_rx_processed <- nhanes_data_raw %>% 
  select(SEQN, Years, rx_use_any = q_rx_use_1, starts_with("q_med_")) %>%
  left_join(rx_alluse, by = "SEQN") %>%
  # First clean raw survey NA values
  mutate(across(starts_with(c("q_med", "rx_")), ~recode_nhanes_na.fun(.))) %>%
  # Explicitly assign "No" for unobserved target medication usage
  mutate(
    across(starts_with("rx_use_"), ~ case_when(
      . == "Yes" ~ "Yes",
      rx_use_any == "No" ~ "No",
      is.na(.) ~ "No",
      TRUE ~ .))
    )

rm(rx_bp)
rm(rx_alluse)
rm(rx_alltypes)

# --------------------------------------
## Physical activity data (METs) 
# --------------------------------------

## To calculate METs (Metabolic Equivalents) per week = 
## MET-min/wk = MET Intensity x Frequency (times/week) x Duration (min/session)
## NOTE: MET estimates must be calcualted separately for 1999-2006 / 2007 onward
## to account for a change in PA survey 

## 1) Calculate METs in 1999-2006 cycles;
nhanes_pa_pre07 <- nhanes_data_raw %>% 
  
  # Gather & merge in required var over 1999-2006 cycles ----------
  select(SEQN, Years, starts_with("q_mvpa")) %>%
  filter(Years %in% nhanes_yrs[1:4]) %>%
  
  # Reshape 'Any PA' (anympa/anyvpa) variables  -----
  pivot_longer(
    cols=c(starts_with("q_mvpa_"),-starts_with("q_mvpa_any")),
    names_to=c(".value", "slot"),
    names_pattern = "q_mvpa_(type|freq|dur|mets)_(\\d+)"
    ) %>%
  
  # Calculate METs for each PA 'slot' 
  mutate(across(c("freq", "dur", "mets"), ~as.numeric(.))) %>% 
  mutate(
    slot_mets_wk = case_when(
      !is.na(freq) ~ mets * (freq/4.33) * dur,
    TRUE ~ 0)) %>% 
  
  # Summarize by SEQN and Years
  mutate(across(starts_with("q_mvpa_any"), ~ifelse(is.na(.), "NA", .))) %>%
  group_by(SEQN, Years, q_mvpa_anyvpa, q_mvpa_anympa) %>%
  summarize(
    sum_slot_mets_wk = sum(slot_mets_wk, na.rm = TRUE), 
    .groups = "drop") %>% 
  
  # Recode missing values, based on Screener Gate Logic
  mutate(
    pa_total_mets_wk = case_when(
      q_mvpa_anyvpa %in% c("No", "Unable to do activity") & 
        q_mvpa_anympa %in% c("No", "Unable to do activity") ~ 0,
      q_mvpa_anyvpa == "Yes" | q_mvpa_anympa == "Yes" ~ sum_slot_mets_wk,
      q_mvpa_anyvpa %in% c("Don't know", "Refused", "NA") & 
        q_mvpa_anympa %in% c("Don't know", "Refused", "NA") ~ NA)
    ) %>% 
  
  # Binary Flag: Meeting CDC Guidelines (≥500 MET-min/week)
  mutate(pa_meets_guidelines = factor(ifelse(pa_total_mets_wk >= 500, 1, 0))) %>% 
  
  # Standard CDC sensitivity check: Cap extreme values at 16,800 MET-min/wk 
  # (Equivalent to ~5 hrs/day of vigorous exercise 7 days/wk)
  mutate(pa_total_mets_wk_cap = if_else(pa_total_mets_wk > 16800, 16800, pa_total_mets_wk)) %>% 
  select(SEQN, Years, pa_total_mets_wk, pa_total_mets_wk_cap, pa_meets_guidelines)


# 2) Calculate METs in 2007-2023 cycles; as Days/wk * Mins/day * METs
# Units are already coded as: freq in days/week; dur in min/day
nhanes_pa_post07 <- nhanes_data_raw %>%
  select(SEQN, Years, starts_with("q_mpa"), starts_with("q_vpa"), starts_with("q_comm")) %>%
  filter(Years %in% nhanes_yrs[5:13]) %>%
  mutate(across(ends_with("_yn"), ~ifelse(. %in% c("Don't know", "Refused"), NA, .))) %>%
  # Recode 99|999 as "Don't know" and 77|7777 as "Refused" for _freq|_dur values
  mutate(across(ends_with("_freq"), ~case_when(.==99 | .==77 ~ NA, TRUE ~ .))) %>%
  mutate(across(c(ends_with("_dur"),ends_with("_minday")), ~case_when(.==9999 | .==7777 ~ NA, TRUE ~ .))) %>%
  
  mutate(
    # I. Work Vigorous PA (METs = 8)
    vpa_work_mets = case_when(
      q_vpa_work_yn %in% c("","No") ~ 0,
      q_vpa_work_yn == "Yes" ~ (8 * q_vpa_work_freq * q_vpa_work_dur),
      TRUE ~ NA),
    
    # II. Moderate Work PA (METs = 4)
    mpa_work_mets = case_when(
      q_mpa_work_yn %in% c("","No") ~ 0,
      q_mpa_work_yn == "Yes" ~ (4 * q_mpa_work_freq * q_mpa_work_dur),
      TRUE ~ NA),
    
    # III. Transport/Travel PA; e.g., walking/biking (METs = 4)
    commute_mets = case_when(
      q_commute_bike_yn %in% c("", "No") ~ 0,
      q_commute_bike_yn == "Yes" ~ (4 * q_commute_bike_freq * q_commmute_bike_minday),
        TRUE ~ NA),
    
    # IV. Recreational Vigorous PA (METs = 8)
    vpa_recr_mets = case_when(
      q_vpa_recr_yn %in% c("", "No") ~ 0,
      q_vpa_recr_yn == "Yes" ~ (8 * q_vpa_recr_freq * q_vpa_recr_minday),
      TRUE ~ NA),
    
    # V. Recreation Moderate PA (4 METs)
    mpa_recr_mets = case_when(
      q_mpa_recr_yn %in% c("","No") ~ 0,
      q_mpa_recr_yn == "Yes" ~ (4 * q_mpa_recr_freq * q_mpa_recr_minday),
      TRUE ~ NA)
  ) %>% 
    
  # Aggregate Total MET-min per week
  mutate(
    pa_work_mets_wk = vpa_work_mets + mpa_work_mets,
    pa_recr_mets_wk = vpa_recr_mets + mpa_recr_mets,
    pa_total_mets_wk = vpa_work_mets + mpa_work_mets + commute_mets + 
      vpa_recr_mets + mpa_recr_mets) %>%
  
  # Binary Flag: Meeting CDC Guidelines (≥500 MET-min/week)
  mutate(pa_meets_guidelines = if_else(pa_total_mets_wk >= 500, 1, 0)) %>% 
  
  # Standard CDC sensitivity check: Cap extreme values at 16,800 MET-min/wk 
  # (Equivalent to ~5 hrs/day of vigorous exercise 7 days/wk)
  mutate(pa_total_mets_wk_cap = if_else(pa_total_mets_wk > 16800, 16800, pa_total_mets_wk)) %>% 
  select(SEQN, Years, pa_total_mets_wk, pa_total_mets_wk_cap, pa_meets_guidelines)


## Bind cycle-stratified PA datasets 
nhanes_quest_pa_processed <- rbind.data.frame(
  nhanes_pa_pre07, nhanes_pa_post07) %>% 
  mutate(
    pa_level_mets = case_when(
      pa_total_mets_wk < 500 ~ "Low",
      pa_total_mets_wk >= 500 & pa_total_mets_wk < 1500 ~ "Moderate",
      pa_total_mets_wk >= 1500 ~ "High"
    )
  )

rm(nhanes_pa_pre07)
rm(nhanes_pa_post07)

# -----------------------------------------------------------------------
## Behavioral Variables: Smoking, Alcohol, Depression (PHQ), Insurance
# -----------------------------------------------------------------------

# Alcohol intake frequency categories ----------
alch_freq.labs <- c("Never drinker" = "Never drinker",
  "Never in the last year" = "Non-drinker", 
  "1 to 2 times in the last year" = "Less than 1 per month",
  "3 to 6 times in the last year" = "Less than 1 per month",
  "7 to 11 times in the last year" = "Less than 1 per month",
  "Once a month" = "Less than 1 per week" , 
  "2 to 3 times a month" = "Less than 1 per week" , 
  "Once a week" = "1-2 per week", "2 times a week" = "1-2 per week",
  "3 to 4 times a week" = "3-6 per week", "Nearly every day" = "3-6 per week",
  "Every day" = "1 or more per day")

## Employment categories ------------------
employ.labs <- c("Looking for work, or" = "Looking for work", 
                 "Not working at a job or business?" = "Not working at a job/business", 
                 "With a job or business but not at work," = "With a job/business but not working",
                 "Working at a job or business," = "Working at a job/business")

## Household food security ---------
foodsecure.labs <- c(
  "HH full food security: 0" = "Food secure",
  "HH marginal food security: 1-2" = "Food secure",
  "HH low food security: 3-5 (HH w/o child) / 3-7 (HH w/ child)" = "Low food security",
  "HH low food security: 3-5 / 3-7 (HH with child)" = "Low food security",
  "HH very low food security: 6-10 (HH w/o child) / 8-18 (HH w/ child)" = "Very low food security",
  "HH very low food security: 6-10 / 8-18 (HH with child)" = "Very low food security"
)

## Function to recode PHQ9 for Depression -------------
recode_phq.fun <- function(x) {
  case_when(
    x == "Not at all" ~ 0,
    x == "Several days" ~ 1,
    x == "More than half the days" ~ 2,
    x == "Nearly every day" ~ 3,
    x %in% c("Refused", "Don't know", "Missing", "") ~ NA,
    TRUE ~ NA  # Catches any other unexpected values
  )
} ; phq_vars <- nhanes_data_raw %>% select(
  starts_with("q_phq_"), -"q_phq_work") %>% names()


## Function to recode tastechange variables -----------
recode_tastechange.fun <- function(x, recode_as) {
  if(recode_as == "any") {
    factor(case_when(
      x %in% c("Better", "Worse") ~ 1,
      x == "No Change" ~ 0, TRUE ~ NA))
    } else {
      factor(case_when(
        x == recode_as ~ 1,
        is.na(x) ~ NA, TRUE ~ 0))
    }
}

nhanes_quest_behav_processed <- nhanes_data_raw %>% 
  
  ## Recode missing values for all questionnaire variables  -------------
  mutate(across(c(
    "q_genhealth_rating", "q_had_dialysis", "q_govtmeal", "q_hh_foodsec",
    "q_uninsur_pastyr", "q_nowork_reason", starts_with(c(
      paste0("q_", c("alc_", "hc_", "insur_", "phq_", "rest", "smoke_", "told_",
                     "tastechange", "tasteq", "work"))) )), 
    recode_nhanes_na.fun)) %>% 
  
  ## Smoking: current smoker, yes/no ---------------
  # a. Clean 'smoking cigarettes now' (SMQ020)
  # b. Define from smoke_history (smoked ≥100 cigarettes in your life?);
  mutate_at("q_smoke_current", ~gsub(",.*", "", gsub("[?]", "", .))) %>%
  mutate(
    smoke_status = case_when(
      q_smoke_ever == "No" ~ "Never smoker", # Never-smoker (<100 cigarettes/lifetime)
      q_smoke_ever == "Yes" & q_smoke_current == "Not at all" ~ "Former smoker", # Former-smoker (>100 cigarettes/life, but "Not at all" now)
      q_smoke_ever == "Yes" & q_smoke_current %in% c("Some days", "Every day") ~ "Current smoker",
      TRUE ~ NA)) %>%
  mutate(
    smoke_current = as.factor(case_when(
      smoke_status == "Current smoker" ~ 1, 
      smoke_status %in% c("Fomer smoker", "Never smoker") ~ 0,
      TRUE ~ NA)),
    smoke_ever = as.factor(case_when(
      smoke_status %in% c("Current smoker", "Former smoker") ~ 1,
      smoke_status == "Never smoker" ~ 0,
      TRUE ~ NA))) %>% 
  rename(smoke_everstop=q_smoke_everstop) %>%
  
  ## Alcohol frequency, times per week  ---------------
  mutate(alch_everdrinker = factor(case_when(
    Years == "2017-2020" ~ q_alc_any_life,
    TRUE ~ case_when(
      q_alc_any_yr == "Yes" | q_alc_any_life == "Yes" ~ "1",
      q_alc_any_life == "No" ~ "0",
      is.na(q_alc_any_yr) & is.na(q_alc_any_life) ~ NA,
      TRUE ~ "0")))) %>% 
  # Code frequency per week for pre/post-2017 --
  mutate(
    # Drinking occasions, per week/month/yr (q_alc_freq_unit)
    alch_freq_wk_pre2017.num = case_when(
      Years == "2017-2020" ~ NA,
      TRUE ~ case_when(
        q_alc_freq_units == "Week" ~ as.numeric(q_alc_freq),
        q_alc_freq_units == "Month" ~ as.numeric(q_alc_freq)/4.33,
        q_alc_freq_units == "Year" ~ as.numeric(q_alc_freq)/52.14,
        TRUE ~ NA)),
    alch_freq_wk_post2017.lab = case_when(
      Years == "2017-2020" ~ unname(alch_freq.labs[q_alc_freq]),
      TRUE ~ NA)) %>% 
  mutate(
    alch_freq_wk_pre2017.lab = case_when(
      alch_freq_wk_pre2017.num == 0 ~ "Non-drinker",
      alch_freq_wk_pre2017.num > 0 & alch_freq_wk_pre2017.num < 1/4 ~ "Less than 1 per month",
      alch_freq_wk_pre2017.num >= 1/4 & alch_freq_wk_pre2017.num < 1 ~ "Less than 1 per week",
      alch_freq_wk_pre2017.num >= 1 & alch_freq_wk_pre2017.num < 2 ~ "1-2 per week",
      alch_freq_wk_pre2017.num >= 2 & alch_freq_wk_pre2017.num < 6 ~ "3-6 per week",
      alch_freq_wk_pre2017.num >= 6 ~ "1 or more per day",
      TRUE ~ NA)) %>% 
  # Combine frequency/wk across exam cycles
  mutate(
    alch_freq_wk = factor(case_when(
      alch_everdrinker == 0 ~ "Never drinker",
      Years =="2017-2020" ~ alch_freq_wk_post2017.lab,
      TRUE ~ alch_freq_wk_pre2017.lab),
      levels = unique(unname(alch_freq.labs)))) %>% 
  mutate(
    alch_drink_curr = as.factor(case_when(
      alch_freq_wk %in% c("Never drinker", "Non-drinker") ~ 0,
      is.na(alch_freq_wk) ~ NA,
      TRUE ~ 1)),
    alch_drink_daily = as.factor(case_when(
      alch_freq_wk %in% c("1 or more per day") ~ 1,
      is.na(alch_freq_wk) ~ NA,
      TRUE ~ 0)),
    alch_drink_weekly = as.factor(case_when(
      alch_freq_wk %in% c("1 or more per day", "3-6 per week", "1-2 per week") ~ 1,
      is.na(alch_freq_wk) ~ NA,
      TRUE ~ 0))
    ) %>%
  
  ## Health insurance coverage -----------------
  rename(insur_any = q_insur_any, uninsur_lastyr = q_uninsur_pastyr) %>% 
  mutate(across(c("insur_any", "uninsur_lastyr"), ~factor(case_when(
    . == "Yes" ~ 1, . == "No" ~ 0, TRUE ~ NA)))) %>% 
  
  ## Health care utilization -------------
  mutate(
    # Have a usual place to go for health care?
    hc_usualplace = as.factor(case_when(
      q_hc_place == "There is no place" ~ 0,
      q_hc_place %in% c("Yes", "There is more than one place") ~ 1,
      TRUE ~ NA)),
    # Any doctor visits or hospital admissions last year?
    hc_drvisit = as.factor(case_when(
      q_hc_drvisit == "None" ~ 0, is.na(q_hc_drvisit) ~ NA, TRUE ~ 1)),
    hc_hospadmit = as.factor(case_when(
      q_hc_admit == "No" ~ 0, q_hc_admit == "Yes" ~ 1, TRUE ~ NA))
    ) %>%
  
  ## Employment Status (if age ≥ 16 yrs) ------------------
  mutate(
    work_lastwk = unname(employ.labs[q_work_lastwk])) %>% 
  mutate(
    employ_status = case_when(
      age<16 ~ "Age <16 y", 
      q_nowork_reason == "Going to school" ~ "Student",
      work_lastwk %in% c("Working at a job/business", "With a job/business but not working") & 
        (q_work_hrs >= 35 | q_work_gt35hr == "Yes") ~ "Full-time employed",
      work_lastwk %in% c("Working at a job/business", "With a job/business but not working") &
        (q_work_hrs < 35 | is.na(q_work_hrs) & 
           q_work_gt35hr == "No" | is.na(q_work_gt35hr)) ~ "Part-time employed",
      work_lastwk %in% c("Not working at a job/business", "Looking for work") ~ "Unemployed",
      TRUE ~ NA)) %>%
  mutate_at("employ_status", ~factor(., levels=c(
    "Full-time employed", "Part-time employed", "Student", "Unemployed"))
    ) %>%
    
  # Anxiety/Depression (self-report & PHQ score) ------------------
  # NOTE: PHQ9 was added in 05/06 and replaced q_depress
  mutate(across(phq_vars, recode_phq.fun)) %>% 
  mutate(phq9_total_post05 = rowSums(across(phq_vars))) %>% 
  mutate(diagn_depress_pre05 = case_when(
    q_depress == "Positive Diagnosis" ~ 1,
    q_depress == "Negative Diagnosis" ~ 0,
    q_depress == "" ~ NA)) %>% 
  
  ## General health rating -------------
  mutate(
    genhealth = factor(
      gsub(",.*", "", gsub("[?]", "", q_genhealth_rating)),
      levels=c("Poor", "Fair", "Good", "Very good", "Excellent"))) %>%
  mutate(
    genhealth_low_vs_other = as.factor(case_when(
      genhealth %in% c("Poor", "Fair") ~ 1,
      is.na(genhealth) ~ NA,
      TRUE ~ 0))) %>% 
  
  ## n Restaurant meals per week ---------------
  mutate(
    restaur_freq_wk = case_when(
      q_restaur_week == 6666 ~ 0.5, # Less than weekly
      q_restaur_week == 5555 | q_restaur_week == 26 ~ 21, # More than 21 times per week; re-coded to 21
      TRUE ~ q_restaur_week)) %>% 
  mutate(restaur_freq_gt2 = as.factor(case_when(
    restaur_freq_wk <= 2 ~ 0,
    restaur_freq_wk > 2 ~ 1,
    TRUE ~ NA)) ) %>%
  
  ## Food security/Govt assistance
  mutate(
    govtmeal_any = factor(case_when(
      q_govtmeal == "Yes" ~ 1, 
      q_govtmeal == "No" ~ 0,
      TRUE ~ NA)),
    foodsecure_level = factor(
      unname(foodsecure.labs[q_hh_foodsec]),
      levels=unique(unname(foodsecure.labs)))) %>% 
  mutate(
    foodinsecure = factor(case_when(
      foodsecure_level == "Food secure" ~ 0,
      is.na(foodsecure_level) ~ NA,
      TRUE ~ 1))) %>% 
  
  # Recode tasetchange variables as No Change (Reference)/Worse/Better
  mutate(
    tastechange_sweet_any = recode_tastechange.fun(q_tastechange_sweet, "any"),
    tastechange_sweet_worse = recode_tastechange.fun(q_tastechange_sweet, "Worse"),
    tastechange_bitter_any = recode_tastechange.fun(q_tastechange_bitter, "any"),
    tastechange_bitter_worse = recode_tastechange.fun(q_tastechange_bitter, "Worse"),
    tastechange_salt_any = recode_tastechange.fun(q_tastechange_salt, "any"),
    tastechange_salt_worse = recode_tastechange.fun(q_tastechange_salt, "Worse"),
    tastechange_sour_any = recode_tastechange.fun(q_tastechange_sour, "any"),
    tastechange_sour_worse = recode_tastechange.fun(q_tastechange_sour, "Worse")) %>%
  rename(tastechange_flavor_any = q_tastechange_flavor) %>%
  mutate_at("q_tastechange_time", ~factor(case_when(
    . == "Less than 3 Months Ago" ~ "Less than 3m",
    . == "3 to 12 Months (1 Yesr) Ago" ~ "3m to 1yr",
    . == "1 to 4 years ago" ~ "1 to 4yr",
    . == "5 to 9 years ago" ~ "5 to 9yr",
    . == "Ten or more years ago" ~ "More than 10yr",
    TRUE ~ NA), levels=c("Less than 3m", "3m to 1yr", "1 to 4yr", "5 to 9yr", "More than 10yr")
    )) %>%
  
  ## Select cleaned questionnaire variables 
  select(SEQN, Years, 
         starts_with(c("smoke_", "alch_drink_", "hc_", "genh", "restaur", "tastech")), 
         employ_status, work_lastwk, insur_any, uninsur_lastyr, restaur_freq_wk,
         govtmeal_any, foodsecure_level, foodinsecure, alch_freq_wk,
         phq9_total_post05, diagn_depress_pre05, q_wtloss_dietrx, q_had_dialysis,
         starts_with("q_told_"))


## Combine all quest_processed dataframes
nhanes_quest_processed <- full_join(
  nhanes_quest_rx_processed, nhanes_quest_behav_processed, 
  by = c("SEQN", "Years")) %>% 
  full_join(nhanes_quest_pa_processed, by = c("SEQN", "Years"))

quest_vars <- nhanes_quest_processed %>% select(
  -c("SEQN", "Years", starts_with("q_told"))) %>% names() 


## =====================================================
## Prepare dietary data phenotypes
## =====================================================

## Dietary Indices: AHEI, HEI-2015/2020 and DII
nhanes_dietindex_processed <- nhanes_data_raw %>% 
  select(SEQN, Years, starts_with(c("HEI", "AHEI", "DII"))) %>% 
  rename_with(str_to_lower, -c(SEQN, Years)) %>%
  rename_with(., ~gsub("all", "total", .), c(hei2015_all, ahei_all, dii_all))
  

## Additional nutrient intakes from 24HR -------------
nutr_vars <- c("energy_kcal", "prot_g", "carb_g", "sugar_g", "fiber_g",
               "fat_g", "sfa_g", "mufa_g", "pufa_g", "atoc_mg")

# Write function to calculate average across RELIABLE/VALID days 
recode_valid_nutr.fun <- function(data = nhanes_data_raw, var) {
  
  # Set column names, based on dietvar suffix
  dr1_var <- sym(paste0("dr1_", var))
  dr2_var <- sym(paste0("dr2_", var))
  
  data %>% 
    mutate(
      nut_var_mean = case_when(
        dr1_recallstat == "Reported consuming breast-milk" | 
          dr2_recallstat == "Reported consuming breast-milk" ~ NA_real_,
        dr_intakedays == "Day 1 only" ~ !!dr1_var,
        dr_intakedays == "Day 1 and day 2" ~ rowMeans(
          pick(!!dr1_var, !!dr2_var), na.rm = TRUE),
        TRUE ~ NA_real_)) %>% 
    rename_with(., ~gsub("var", var, .), "nut_var_mean")
}

# Apply recode_valid_nutr function over all nutrient variables
nhanes_nutr_processed <- lapply(nutr_vars, function(x) {
  nhanes_data_raw %>% recode_valid_nutr.fun(x) %>% 
    select(SEQN, paste0("nut_", x, "_mean")) 
  }) %>% reduce(full_join, by = "SEQN") 


## Clean supplement and salt use variables
nhanes_diet_processed <- nhanes_data_raw %>% 
  select(SEQN, Years, dr_intakedays, contains("recallstat"),
         dr_addsalt_freq, dr_saltprep, contains("suppl")) %>%
  
  # Salt use habits -------------------
  mutate_at("dr_saltprep", ~case_when(
    . %in% c("Very often", "Very Often") ~ "Very often",
    . == "Don't know" | is.na(.) ~ NA,
    TRUE ~ .)) %>%
  mutate_at("dr_addsalt_freq", ~case_when(
    . == "Don't know" ~ NA,
    is.na(.) & !is.na(dr_saltprep) ~ "Never",
    TRUE ~ .)) %>% 
  mutate(
    addsalt_table_often = factor(case_when(
      dr_addsalt_freq %in% c("Occasionally", "Very often") ~ 1,
      is.na(dr_addsalt_freq) ~ NA,
      TRUE ~ 0)),
    addsalt_prep_often = factor(case_when(
      dr_saltprep %in% c("Occasionally", "Very often") ~ 1,
      is.na(dr_saltprep) ~ NA,
      TRUE ~ 0))) %>% 
  
  # Supplement use --------------------
  mutate(
    # If either day is "Yes", NA if both are missing, 0 otherwise
    dietsuppl_any = factor(case_when(
      dr1_anysuppl == "Yes" | dr2_anysuppl == "Yes" ~ 1,
      is.na(dr1_anysuppl) & is.na(dr2_anysuppl) ~ NA_real_,
      TRUE ~ 0)),
    # Max supplement count between Day 1 and Day 2
    dietsuppl_num = pmax(dr1_suppl_n, dr2_suppl_n, na.rm = TRUE)) %>% 
  
  select(SEQN, Years, starts_with("WTD"), starts_with(c("addsalt_", "dietsuppl_"))) %>% 
  
  # Merge in diet indices and nutrient intakes 
  full_join(nhanes_nutr_processed, by = c("SEQN")) %>% 
  full_join(nhanes_dietindex_processed, by = c("SEQN", "Years")) 

diet_vars <- nhanes_diet_processed %>% select(-c(SEQN, Years), starts_with("WT")) %>% names()


## =====================================================
## Prepare additional clinical outcomes
## =====================================================

# Merge required questionnaire + lab + exam variables 
nhanes_disease_processed <- full_join(
  nhanes_demo_processed %>% select(SEQN, Years, gender, age), 
  nhanes_quest_processed, by = c("SEQN", "Years")) %>%
  full_join(nhanes_lab_processed,  by =c("SEQN", "Years")) %>% 
  full_join(nhanes_exam_processed, by =c("SEQN", "Years")) %>% 
  
  # Calculated BMI
  mutate(bmi_calc = wt/((ht*.01)^2)) %>%
  
  ## Disease outcomes 
  mutate(
    
    # Obesity ----------------------
    obese = case_when(
      q_told_overwt_gte20 == "Yes" | q_told_overwt_lt20 == "Yes" | 
        q_wtloss_dietrx == "Yes" | bmi>=30 | bmi_calc >= 30 ~ 1,
      TRUE ~ 0),
    
    # CVD ----------------------
    cvd = case_when(
      q_told_chd == "Yes" | q_told_angina == "Yes" | 
        q_told_mi == "Yes" ~ 1,
      TRUE ~ 0 ), 
    
    # ASCVD --------------------
    ascvd = case_when(
      q_told_chd == "Yes" | q_told_angina == "Yes" | 
        q_told_mi == "Yes" | q_told_stroke == "Yes" ~ 1, 
      TRUE ~ 0),
    
    # Congestive heart failure ------------------
    chf = case_when(
      q_told_chf == "Yes" ~ 1,
      q_told_chf == "No" ~ 0,
      TRUE ~ NA),
    
    # COPD --------------------
    copd = case_when(
      q_told_bronch == "Yes" | q_told_anybronch == "Yes" | q_told_copd == "Yes" | 
        q_told_emphys == "Yes" ~ 1,
      TRUE ~ 0),
    
    # COPD, based on pulmonary function tests 
    pft_lt07 = case_when(
      fev1_pre / fvc_pre < 0.7 ~ 1,
      fev1_pre / fvc_pre >= 0.7 ~ 0,
      TRUE ~ NA),
    
    # CKD ---------------------
    ckd = case_when(
      q_told_kidfail == "Yes" | creatinine >= 1.4 ~ 1,
      TRUE ~ 0),
    
    # MDD (Depression) -------------------
    mdd = factor(case_when(
      diagn_depress_pre05 == 1 | q_med_depress == "Yes" |
        phq9_total_post05 >= 10 ~ 1,
      TRUE ~ 0)),
    
    # Hypertension --------------
    htn = case_when(
      q_told_htn == "Yes" | `q_told_htn_x2+` == "Yes" | sbp_mean >=130 | 
        dbp_mean >=80 ~ 1, 
      TRUE ~ 0)) %>% 
  
  # Diabetes (diagnosed) -----------------------
  mutate(
    diabetes = case_when(
      q_told_diab == "Yes" | # Doctor told you, you have diabetes
        q_med_insulin == "Yes" | # Taking insulin now
        q_med_diab == "Yes" | # Taking diabetic pills to lower blood sugar
        rx_use_diab == "Yes" |  # Using any antidiabetic medication 
        hba1c > 6.4 | # HbA1c > 6.4%
        glu > 199 | # Non-fasting glucose > 199 mg/dL
        fg > 125 ~ 1, # Fasting glucose >= 126 mg/dL (CHECK: LB2GLU > 199 | LB2SGL > 199)
      # Exclude likely T1D diagnosis
      age < 30 & q_told_diab == "Yes" & 
        q_med_insulin == "Yes" ~ 0, 
      # Exclude borderline T2D
      q_told_diab %in% c("No", "Borderline", "") &  
        (hba1c <= 6.4 | is.na(hba1c)) & 
        (fg <125 | is.na(fg)) ~ 0,
      TRUE ~ 0)) %>%
  
  # Diabetes, undiagnosed ------------------
  mutate(
    diabetes_undx = case_when(
      diabetes == 1 & # Have diabetes (as defined, above)
        q_told_diab != "Yes" & 
        q_med_insulin != "Yes" & 
        q_med_diab != "Yes" ~ 1,
      TRUE ~ 0)) %>%
  
  # Hypertension, undiagnosed ------------------
  mutate(
    htn_undx = case_when(
      (is.na(sbp_mean) & is.na(dbp_mean)) ~ NA,
      sbp_mean >= 130 | dbp_mean >= 80 | htn == 1 &
        q_told_htn != "Yes" & 
        q_med_bp != "Yes" & 
        rx_use_bp != "Yes" ~ 1,
      TRUE ~ 0)) %>%
  
  # Abdominal obesity ------------------
  mutate(
    obese_abd = case_when(
      gender == "Male" & waist >= 102 |
        gender == "Female" & waist >= 88 ~ 1,
      TRUE ~ 0)) %>%
  
  # Alternative Hypertension definition ------------------
  mutate(
    htn_stg1 = case_when(
      q_told_htn == "No" | 
      q_med_bp == "No" | 
      sbp_mean >= 130 | dbp_mean >=80 ~ 1,
    TRUE ~ 0)) %>%
  
  # Additional COPD definitions ------------------
  mutate(
    copd_pft = case_when(
      copd == 1 | pft_lt07 == 1 ~ 1,
      TRUE ~ 0)) %>%
  
  # CKD-related outcomes -----------
  mutate(
    ckd_gfr_level = factor(case_when(
      q_had_dialysis == "Yes" ~ "ESRD",
      egfr >= 90 ~ "Stage 0",
      egfr >=60 & egfr <90 ~ "Stage 2",
      egfr >= 45 & egfr < 60 ~ "Stage 3a",
      egfr >= 30 & egfr < 45 ~ "Stage 3b",
      egfr >= 15 & egfr <30 ~ "Stage 4",
      egfr < 15 ~ "Stage 5",
      TRUE ~ NA), levels = c("Stage 0", "Stage 2", "Stage 3a",
                             "Stage 3b", "Stage 4", "Stage 5", "ESRD"))) %>% 
  mutate(
    ckd_gfr_gte3 = case_when(
      ckd_gfr_level %in% c("Stage 0", "Stage 2") ~ 0,
      is.na(ckd_gfr_level) ~ NA,
      TRUE ~ 1),
    
    ckd_malb_level = factor(case_when(
      u_albumin <= 30 ~ 0,
      u_albumin > 30 & u_albumin <= 100 ~ 1,
      u_albumin > 100 & u_albumin <= 300 ~ 2,
      u_albumin > 300 ~ 3))) %>%
  
  # Any CKD, based on GFRs & u-albumin
  mutate(
    ckd_any = case_when(
      ckd_gfr_level != "Stage 0" | ckd_malb_level != 0 ~ 1,
      ckd_gfr_level == "Stage 0" | ckd_malb_level == 0 ~ 0,
    TRUE ~ NA)) %>% 
  
  # MASLD-related outcomes ------------------
  mutate(fib4 = (age*ast)/(plt*sqrt(alt))) %>%
  mutate(
    fib4_cat = case_when(
      age <65 & fib4 <1.30  | age >=65 & fib4 < 2 ~ "Low",
      age <65 & fib4 >=1.3 & fib4 <2.67 | 
        age >=65 & fib4 >=2 & fib4 <2.67 ~ "Moderate",
      fib4 >=2.67 ~ "High",
      TRUE ~ NA_character_)) %>% 
  # Fib4: Mod vs. low and High vs. low
  mutate(
    fib4_mod_vs_low = case_when(
      fib4_cat == "Moderate" ~ 1,
      fib4_cat == "Low" ~ 0,
      TRUE ~ NA),
    fib4_high_vs_low = case_when(
      fib4_cat == "High" ~ 1,
      fib4_cat == "Low" ~ 0,
      TRUE ~ NA)
  ) %>% 
  
  # NAFLD fibrosis score ------------------
  mutate(
    nfs = -1.675 + 0.037 * age + 0.094 * bmi + 1.13 * diabetes +
      0.99 * (ast / alt) - 0.013 * plt - 0.66 * alb) %>%
  mutate(
    nfs_cat = case_when(
      is.na(nfs) ~ NA,
      nfs < -1.455 ~ "Low",
      nfs > 0.676 ~ "High",
      nfs >= -1.455 & nfs <= 0.676 ~ "Moderate",
      TRUE ~ NA)) %>%
  mutate(
    nfs_mod_vs_low = case_when(
      nfs_cat == "Moderate" ~ 1,
      nfs_cat == "Low" ~ 0,
      TRUE ~ NA),
    nfs_high_vs_low = case_when(
      nfs_cat == "High" ~ 1,
      nfs_cat == "Low" ~ 0,
      TRUE ~ NA)) %>% 
  
  # Liver-CAP steatosis -------------------------
  mutate(
    liver_valid_elastography = case_when(
    !is.na(liver_stiff) & !is.na(liver_iqr) & 
      liver_iqr <= 30 ~ 1, 
    TRUE ~ 0)) %>%
  mutate(
    liver_cap_any = case_when( 
      liver_valid_elastography == 1 ~ case_when(
        liver_cap < 240 ~ 0,
        liver_cap >= 240 ~ 1,
        TRUE ~ NA),
      TRUE ~ NA),
    
    liver_cap_mod_sev = case_when(
      liver_valid_elastography == 1 ~ case_when(
        liver_cap >= 268 ~ 1,
        liver_cap < 268 ~ 0,
        TRUE ~ NA),
      TRUE ~ NA)) %>%
  
  # Convert 0, 1 to factor variables
  mutate(across(c(
    "diabetes", "diabetes_undx", "obese", "obese_abd", "cvd", "ascvd", "chf", 
    "copd", "pft_lt07", "copd_pft", "htn", "htn_undx", "htn_stg1", "ckd",
    "ckd_gfr_gte3", "ckd_any", "mdd", "fib4_mod_vs_low", "fib4_high_vs_low",
    "nfs_mod_vs_low", "nfs_high_vs_low", "liver_cap_any", "liver_cap_mod_sev"), 
    ~as.factor(.))) %>%
  
  # Select derived variables ----------------------
  select(SEQN, Years, bmi_calc, diabetes, diabetes_undx, 
         obese, obese_abd, cvd, ascvd, chf, copd, pft_lt07, copd_pft,
         htn, htn_stg1, htn_undx, ckd, ckd_gfr_level, ckd_gfr_gte3, ckd_any, 
         ckd_malb_level, mdd, fib4, fib4_cat, fib4_mod_vs_low, fib4_high_vs_low, 
         nfs, nfs_cat, nfs_mod_vs_low, nfs_high_vs_low, liver_cap_any, 
         liver_cap_mod_sev) 

disease_vars <- nhanes_disease_processed %>% 
  select(-c("SEQN", "Years", starts_with("WT"))) %>% names()


################################################################################
## Combine all nhanes variables
################################################################################

nhanes_processed <- full_join(
  nhanes_demo_processed, nhanes_exam_processed, by = c("SEQN", "Years")) %>%
  full_join(nhanes_lab_processed, by = c("SEQN", "Years")) %>%
  full_join(nhanes_quest_processed, by = c("SEQN", "Years")) %>%
  full_join(nhanes_diet_processed, by = c("SEQN", "Years")) %>%
  full_join(nhanes_disease_processed, by = c("SEQN", "Years")) %>%
   
  ## Filter to 1999 -- 2020 & delete 2017-2018 (redundant to 2017-2020) -----------
  filter(!Years %in% c("2017-2018", "2021-2023")) %>% 
  
  ## Clean up inerim variables ------------
  select(
    # interim disease diagnosis or diet component variables 
    -c(starts_with(c("q_told_", "q_med")), diagn_depress_pre05, 
       phq9_total_post05, starts_with(c("ahei_", "hei2015_", "dii_")) & 
         !any_of(c("ahei_total", "hei2015_total", "dii_total"))
    )
  )
  
nhanes_processed %>% saveRDS("../data/processed/nhanes_processed.rds")


## Delete large breadcrunmbs ...
rm(nhanes_demo_processed)
rm(nhanes_quest_behav_processed) 
rm(nhanes_lab_processed)
rm(nhanes_exam_processed)
rm(nhanes_quest_pa_processed)
rm(nhanes_quest_rx_processed)
rm(nhanes_csexam_processed)
rm(nhanes_quest_processed)
rm(nhanes_diet_processed)
rm(nhanes_dietindex_processed)
rm(nhanes_nutr_processed)
rm(nhanes_disease_processed)


## EOF
# Last Updated: 09-28-2026

