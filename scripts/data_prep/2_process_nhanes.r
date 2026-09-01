# Rscript for cleaning nhanes data 


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
nhanes_yrs <- nhanes_yrs[-c(2,7)] # remove "1999-2004" and 20007-2012

# Make array of years/labels for selecting tables
names(nhanes_yrs) <- c("", paste0("_", LETTERS[2:10]),"P_", "_L")

## Load project data dictionary to grab years, tables & variables of interest
project_datadict <- readxl::read_xlsx("./nhanes_pullrequest_IndVsArea_07.30.2026.xlsx")

## Load all nhanes tables & var descriptions
table_cats <- c("demo", "lab", "exam", "quest")
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
    x[x %in% c("Don't know", "Refused", "")] <- NA_character_
    return(x)
  } else {return(x)}
}


## ==================================
## Prepare DEMOGRAPHICS variables 
## ==================================

# racecat
racethn.labs <- c("NHW"="Non-Hispanic White", "NHB"="Non-Hispanic Black", "NHAsian"="Non-Hispanic Asian",
                  "Mexican-American"="Mexican American", "Other Hispanic"="Other Hispanic", 
                  "Other/Multi-Racial"="Other Race - Including Multi-Racial")

educ_level.labs <- c(
  "Less than 9th grade"="Less than 9th grade", 
  "9-11th grade (includes 12th grade with no diploma)"="9-11th grade", 
  "High school graduate/ged or equivalent"="HS graduate or GED", 
  "High school grad/ged or equivalent"="HS graduate or GED",
  "Some college or aa degree"="Some college or AA degree",
  "College graduate or above"="College graduate or above")


# income levels
inc.vals <- c("$0-$4,999", "$5,000-$9,999", "$10,000-$14,999", "$15,000-$19,999", "Under $20,000", 
              "Over $20,000", "$20,000-$24,999", "$25,000-$34,999", "$35,000-$44,999", "$45,000-$54,999",
              "$55,000-$64,999", "$65,000-$74,999", "Over $75,000", "$75,000-$99,999", "Over $100,000")


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
    female = ifelse(gender == "Female", 1, 0)) %>%
  
  # Race/ethnicity --------------------
  mutate_at("racethn_addNHA", ~ifelse(.=="", racethn1, .)) %>%
  mutate(
    racethn = add_descr_labels(., "racethn1", racethn.labs, ordered = T),       
    racethn_addNHA = add_descr_labels(., "racethn_addNHA", racethn.labs, ordered = T)) %>%
  mutate(racethn_combn = case_when(
    racethn_addNHA == "Other/Multi-Racial" & 
      Years %in% c(nhanes_yrs[1:6]) ~ "Other/Multi-Racial/NHAsian", # Definitions changed Pre-/Post-2011
    TRUE ~ racethn_addNHA)) %>%
  mutate(race_white = case_when(
    racethn == "NHW"~1, is.na(racethn) ~ NA, TRUE ~ 0)) %>%
  
  # Education levels --------------------
  mutate(across(c("educ_under20", "educ_20plus"), ~recode_nhanes_na.fun(str_to_sentence(.)))) %>%
  mutate(educ_level = case_when(
    age < 18 ~ NA, age >= 18 & age < 20 ~ str_to_sentence(educ_under20), 
    age >= 20 ~ str_to_sentence(educ_20plus),
    TRUE ~ NA)) %>% 
  mutate_at("educ_level", ~case_when(
    . %in% c(paste(c("9th", "10th", "11th"), "grade"), "12th grade, no diploma",
             "9-11th grade (includes 12th grade with no diploma)") ~ "9-11th grade",
    . %in% c("Ged or equivalent", "High school graduate") ~ "HS graduate or GED",
    . == "More than high school" ~ "Some college or AA degree",
    TRUE ~ unname(educ_level.labs[.]))) %>% 
  mutate_at("educ_level", ~ factor(
    ., levels=unique(unname(educ_level.labs)))) %>% 
    
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
      inc_to_pov < 1 ~"Low income",
      inc_to_pov >= 1 & inc_to_pov < 4 ~ "Middle income",
      inc_to_pov >= 4 ~ "High income",
      TRUE ~ NA)) %>%
  mutate_at("inc_to_pov_level", ~ factor(., levels=c(
    "Low income", "Middle income", "High income"))) %>%
  
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
    taste_tip2mouth_quinine = taste_tongue_quinine_glms / taste_mouth_quinine_glms,
    taste_tip2mouth_nacl_1M = taste_tongue_nacl_glms / taste_mouth_nacl_1M_glms
  ) %>% 
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
    smell_dysfun = case_when(
      smell_pst_total > 0 & smell_pst_total <= 5 ~ 1,
      !is.na(smell_pst_total) & smell_pst_total >5 ~ 0,
      TRUE ~ NA) ) %>% 
  mutate(
    smell_dysfun_cat = case_when(
      smell_pst_total %in% c(0,1,2,3) ~ "Anosmia/Severe hyposmia",
      smell_pst_total %in% c(4,5) ~ "Hyposmia",
      !is.na(smell_pst_total) & smell_pst_total > 5 ~ "Normal",
      TRUE ~ NA)
  ) %>% 
  select(SEQN, Years, taste_mouth_quinine_glms, taste_mouth_nacl_1M_glms, 
         taste_mouth_nacl_320mM_glms, contains("tip2mouth"), 
         smell_pst_total, smell_dysfun, smell_dysfun_cat)

# merge into basic exam vars
nhanes_exam_processed <- nhanes_data_raw %>% 
  # Calculate average sbp/dbp, by hand
  mutate(
    sbp_mean = rowMeans(pick(sbp1, sbp2, sbp3), na.rm = T),
    dbp_mean = rowMeans(pick(dbp1, dbp2, dbp3), na.rm = T)
  ) %>% 
  # Calculated BMI ---------------------
  mutate(bmi_calc = wt/((ht*.01)^2)) %>%
  # Abdominal obesity ---------
  mutate(whr = waist / hip) %>%
  mutate_at(c("sbp_mean", "dbp_mean"), ~ifelse(is.na(.), NA, .)) %>%
  select(SEQN, Years, bmi, bmi_calc, sbp_mean, dbp_mean, fev1_pre, fvc_pre,
         waist, hip, whr, ht, wt, starts_with("liver_"), 
         -starts_with(c("taste_","smell_"))) %>% 
  full_join(nhanes_csexam_processed, by = c("SEQN", "Years"))

exam_vars <- nhanes_exam_processed %>% select(-c("SEQN", "Years")) %>% names()

rm(nhanes_csexam_processed)

## =============================================================
## Prepare LABORATORY variables (& vars for PREVENT equation) 
## =============================================================

## Equation to calculate EGFR using CKD-EPI 2025 calculator
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


lab_cat_vars <- nhanes_vars_datadict %>% 
  filter(Category=="lab" & !(New.Variable.Name %in% base_vars)) %>% 
  filter(!startsWith(New.Variable.Name, "WT")) %>%
  pull(New.Variable.Name) %>% unique()

nhanes_lab_processed <- nhanes_data_raw %>%
  # Add binary var for black race (for egfr calculation)
  mutate(racethn_black = case_when(
    racethn1 == "Non-Hispanic Black" ~ 1,
    racethn1 == "" ~ NA,
    TRUE ~ 0)) %>%
  select(SEQN, Years, gender, age, racethn_black, all_of(lab_cat_vars)) %>%
  mutate(female = ifelse(gender == "Female",1,0)) %>%
  # Serum Creatinine, from 1999-2000
  mutate_at("creatinine", ~ifelse(Years == "1999-2000", 0.147+.*1.013, .))


## Apply corrections for urinary albumin/creatinine
nhanes_lab_processed <- nhanes_lab_processed %>%
  # U_creatinine: instrument change in 2007 (https://wwwn.cdc.gov/Nchs/Data/Nhanes/Public/2007/DataFiles/ALB_CR_E.htm)
  mutate_at("u_creatinine", ~ifelse(
    Years %in% c(as.vector(nhanes_yrs)[1:4]), # Adjust prior to 2007
    case_when(
      .<75 ~ (1.02*sqrt(.) - 0.36)^2,
      .>= 75 & . <250 ~ (1.05*sqrt(.)-0.74)^2,
      .>=250 ~ (1.01*sqrt(.)-0.10)^2,
      TRUE ~ .), .)) %>%
  ## Urinary albumin-to-creatinine ratio (mg/g) for PREVENT Equation
  # UACR (mg/g) = l_u_albumin (ug/mL) / l_u_creatinine (mg/dL)
  mutate(uacr = (u_albumin / u_creatinine) *100) %>%
  mutate(uacr_level = factor(case_when(
    uacr <30 ~ "uacr_lt30", 
    uacr >= 30 & uacr <100 ~ "uacr_30to100",
    uacr >= 100 & uacr <399 ~ "uacr_100to300",
    uacr >= 300 ~ "uacr_gte300",
    TRUE ~ NA),
    levels = c("uacr_lt30", "uacr_30to100", "uacr_100to300", "uacr_gte300"))
    ) %>%
  mutate(uacr_level.lab = factor(case_when(
    uacr_level == "uacr_lt30" ~ "Normal",
    uacr_level == "uacr_30to100" ~ "Low",
    uacr_level == "uacr_100to300" ~ "Moderate",
    uacr_level == "uacr_gte300" ~ "High"),
    level = c("Normal", "Low", "Moderate", "High"))
    ) %>% 
  ## eGFR, baed on CKD-EPI 2021 equation
  mutate(egfr = calc_egfr_ckdepi.fun(creatinine = creatinine, age=age, female=female)) %>%
  mutate(egfr_race = calc_egfr_ckdepi_race.fun(creatinine = creatinine, age=age, female=female, black=racethn_black)) %>%
  select(-c(age, female, gender, racethn_black))
  
lab_vars <- nhanes_lab_processed %>% select(-c("SEQN", "Years")) %>% names()


## ===================================================
## Prepare QUESTIONNAIRE variables 
## ====================================================

# ----------------------------------------------
## Medication use (Statins, BP, Diabetes) 
# -----------------------------------------------

rxq_url <- "https://wwwn.cdc.gov/Nchs/Data/Nhanes/Public/1988/DataFiles/RXQ_DRUG.xpt"
rxq_drug <- nhanesA::nhanesFromURL(rxq_url)


## 1) Create list of drug codes for each medication type 

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

# Merge all rxtypes into df
rx_alltypes <- rbind.data.frame(rx_statin, rx_bp, rx_diab) %>% distinct()


## 2) Match participants against target medications
rx_alluse <- nhanes_data_raw %>% 
  select(SEQN, starts_with("q_rx_drugid_")) %>% #Years, rx_use_any = q_rx_use_1,
  pivot_longer(
    cols=starts_with("q_rx_drugid_"), 
    names_to="drug_id", 
    values_to="RXDDRGID",
    values_drop_na = TRUE) %>%
  inner_join(rx_alltypes, by = "RXDDRGID") %>% 
  #left_join(., rxq_drug %>% select(RXDDRGID, RXDDRUG=RXDDRUG, RXUSE=RXDICN1A), by = "RXDDRGID") %>% 
  select(SEQN, RXDDRGID, RXDDRUG, RXUSE) %>%
  distinct() %>% 
  # Convert to wide-format to merge with nhanes_quest_processed
  group_by(SEQN, RXUSE) %>% 
  summarise(
    rx_use = "Yes",
    rx_id = paste0(unique(RXDDRGID), collapse="; "),
    rx_drug = paste0(unique(RXDDRUG), collapse="; "),
    .groups = "drop"
    ) %>%
  pivot_wider(
    id_cols = "SEQN", 
    names_from="RXUSE", 
    values_from = c(rx_use, rx_id, rx_drug),
    names_glue="{.value}_{RXUSE}"
    )

## 3) Merge back to main dataset & resolve No/NA values
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
      TRUE ~ .
    ))
  )
  
  
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
    total_pa_mets_wk = case_when(
      q_mvpa_anyvpa %in% c("No", "Unable to do activity") & 
        q_mvpa_anympa %in% c("No", "Unable to do activity") ~ 0,
      q_mvpa_anyvpa == "Yes" | q_mvpa_anympa == "Yes" ~ sum_slot_mets_wk,
      q_mvpa_anyvpa %in% c("Don't know", "Refused", "NA") & 
        q_mvpa_anympa %in% c("Don't know", "Refused", "NA") ~ NA)
    ) %>% 
  
  # Standard CDC sensitivity check: Cap extreme values at 16,800 MET-min/wk 
  # (Equivalent to ~5 hrs/day of vigorous exercise 7 days/wk)
  mutate(total_pa_mets_wk_cap = if_else(total_pa_mets_wk > 16800, 16800, total_pa_mets_wk)) %>% 
  select(SEQN, Years, total_pa_mets_wk, total_pa_mets_wk_cap)


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
    total_pa_mets_wk = vpa_work_mets + mpa_work_mets + commute_mets + 
      vpa_recr_mets + mpa_recr_mets) %>%
  
  # Binary Flag: Meeting CDC Guidelines (≥500 MET-min/week)
  mutate(pa_meets_guidelines = if_else(total_pa_mets_wk >= 500, 1, 0)) %>% 
  
  # Standard CDC sensitivity check: Cap extreme values at 16,800 MET-min/wk 
  # (Equivalent to ~5 hrs/day of vigorous exercise 7 days/wk)
  mutate(total_pa_mets_wk_cap = if_else(total_pa_mets_wk > 16800, 16800, total_pa_mets_wk)) %>% 
  select(SEQN, Years, total_pa_mets_wk, total_pa_mets_wk_cap)


## Bind cycle-stratified PA datasets 
nhanes_quest_pa_processed <- rbind.data.frame(
  nhanes_pa_pre07, nhanes_pa_post07) %>% 
  mutate(
    pa_level_mets = case_when(
      total_pa_mets_wk < 500 ~ "Low",
      total_pa_mets_wk >= 500 & total_pa_mets_wk < 1500 ~ "Moderate",
      total_pa_mets_wk >= 1500 ~ "High"
    )
  )

rm(nhanes_pa_pre07)
rm(nhanes_pa_post07)

# -----------------------------------------------------------------------
## Behavioral Variables: Smoking, Alcohol, Depression (PHQ), Insurance
# -----------------------------------------------------------------------

# Alcohol intake frequency categories ----------
alch_freq.labs <- c("Never in the last year"="Non-drinker", 
                    "1 to 2 times in the last year"="Less than 1 per month",
                    "3 to 6 times in the last year"="Less than 1 per month",
                    "7 to 11 times in the last year"="Less than 1 per month",
                    "Once a month"="Less than 1 per week" , 
                    "2 to 3 times a month"="Less than 1 per week" , 
                    "Once a week"="1-2 per week", "2 times a week"="1-2 per week",
                    "3 to 4 times a week"="3-6 per week","Nearly every day"="3-6 per week",
                    "Every day"="1 or more per day")

## Health Care Visit categories --------------------
hc_visit.labs <- c(
  "None"="None", "1" = "1 visit", "2 to 3" = "2-3 visits", 
  "4 to 5" = "4-5 visits", "4 to 9" = "4-5 visits",
  "6 to 7" = "6-9 visits", "8 to 9" = "6-9 visits",
  "10 to 12" = "10-12 visits", "13 or more" = "13 or more visits",
  "13 to 15" = "13 or more visits", "16 or more" = "13 or more visits"
  )

## Employment categories ------------------
employ.labs <- c("Looking for work, or" = "Looking for work", 
                 "Not working at a job or business?" = "Not working at a job/business", 
                 "With a job or business but not at work," = "With a job/business but not working",
                 "Working at a job or business," = "Working at a job/business")


## Function to recode PHQ9 for Depression 
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


nhanes_quest_behav_processed <- nhanes_data_raw %>% 
  rename(q_sob_incline=q_sob_inclilne) %>% # Correct typoe in data-dictionary
  
  ## Recode missing values for all questionnaire variables  -------------
  mutate(across(c(
    "q_feel_riskdiab", "q_genhealth_rating", "q_had_dialysis", "q_sob_incline", 
    "q_uninsur_pastyr", "q_nowork_reason", starts_with(c(
      paste0("q_", c("alc_", "hc_", "insur_", "phq_", "rest", "self", "smoke_", 
                     "told_", "tastechange", "csq", "work", "wtloss"))) )), 
    recode_nhanes_na.fun)) %>% 
  
  ## Smoking: current smoker, yes/no ---------------
  # a. Clean 'smoking cigarettes now' (SMQ020)
  mutate_at("q_smoke_current", ~gsub(",.*", "", gsub("[?]", "", .))) %>%
  # b. Define from smoke_history (smoked ≥100 cigarettes in your life?);
  #  - if smoke_history (≥100 cigs//life) == No, smoke_histrory not asked (N/A)
  #  - if smoke_status (do you smoke cigarettes now?; Every day, Smoke days, Not at all)
  mutate(
    smoke_status = case_when(
      q_smoke_ever == "No" ~ "Never smoker", # Never-smoker (<100 cigarettes/lifetime)
      q_smoke_ever == "Yes" & q_smoke_current == "Not at all" ~ "Former smoker", # Former-smoker (>100 cigarettes/life, but "Not at all" now)
      q_smoke_ever == "Yes" & q_smoke_current %in% c("Some days", "Every day") ~ "Current smoker",
      TRUE ~ NA)) %>%
  
  ## Alcohol frequency, times per week  ---------------
  #mutate(across(starts_with(c("q_alc_any", "q_alc_f", "q_alc_dr")), recode_nhanes_na.fun)) %>% 
  mutate(alch_everdrinker = case_when(
    Years %in% c("2017-2018", "2017-2020", "2021-2023") ~ q_alc_any_life,
    TRUE ~ case_when(
      q_alc_any_yr == "Yes" | q_alc_any_life == "Yes" ~ "Yes",
      q_alc_any_life == "No" ~ "No",
      is.na(q_alc_any_yr) & is.na(q_alc_any_life) ~ NA,
      TRUE ~ "No"))) %>% 
  # Code frequency (ocassions per week) for pre/post-2017 --
  mutate(
    # Drinking occassions, per week/month/yr (q_alc_freq_unit)
    alch_freq_wk_pre2017.num = case_when(
      Years %in% c("2017-2018", "2017-2020", "2021-2023") ~ NA,
      TRUE ~ case_when(
        q_alc_freq_units == "Week" ~ as.numeric(q_alc_freq),
        q_alc_freq_units == "Month" ~ as.numeric(q_alc_freq)/4.33,
        q_alc_freq_units == "Year" ~ as.numeric(q_alc_freq)/52.14,
        TRUE ~ as.numeric(q_alc_freq))),
    alch_freq_wk_post2017.lab = case_when(
      Years %in% c("2017-2018", "2017-2020", "2021-2023") ~ factor(
        unname(alch_freq.labs[q_alc_freq]), levels=unique(unname(alch_freq.labs))),
      TRUE ~ NA)) %>% 
  mutate(
    alch_freq_wk_pre2017.lab = factor(case_when(
      alch_freq_wk_pre2017.num == 0 ~ "Non-drinker",
      alch_freq_wk_pre2017.num > 0 & alch_freq_wk_pre2017.num < 1/4.33 ~ "Less than 1 per month",
      alch_freq_wk_pre2017.num >= 1/4.33 & alch_freq_wk_pre2017.num < 1.5 ~ "Less than 1 per week",
      alch_freq_wk_pre2017.num >= 1.5 & alch_freq_wk_pre2017.num < 2.5 ~ "1-2 per week",
      alch_freq_wk_pre2017.num >= 2.5 & alch_freq_wk_pre2017.num < 6.5 ~ "3-6 per week",
      alch_freq_wk_pre2017.num >= 6.5 ~ "1 or more per day",
      TRUE ~ NA), levels=unique(unname(alch_freq.labs)))) %>% 
  # Combine frequency/wk across exam cycles
  mutate(
    alch_freq_wk = case_when(
      alch_everdrinker == "No" ~ "Never drinker",
      Years %in% c("2017-2018", "2017-2020", "2021-2023") ~ alch_freq_wk_post2017.lab,
      TRUE ~ alch_freq_wk_pre2017.lab)) %>% 
 
  ## Health Insurance: covered by ANY health insurance, yes/no? ---------------
  mutate(
    insur_any = q_insur_any,
    insur_private = case_when(
      q_insur_private == "Covered by private insurance" ~ "Yes",
      q_insur_private == "Yes" ~ "No",
      TRUE ~ q_insur_private),
    uninsur_lastyr = q_uninsur_pastyr) %>% 
  
  ## Employment Status (if age ≥ 16 yrs) ------------------
  mutate(work_lastwk = unname(employ.labs[q_work_lastwk])) %>% 
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
  
  # Anxiety/Depression (self-report & PHQ score) ------------------
  mutate(across(c(phq_vars), recode_phq.fun)) %>% 
  mutate(phq9_total = rowSums(across(phq_vars))) %>% 
  mutate(across(c("q_anxiety", "q_depress"), ~case_when(
    . == "Positive Diagnosis" ~ 1,
    . == "Negative Diagnosis" ~ 0,
    . == "" ~ NA))) %>%
  rename_at(c("q_anxiety", "q_depress"), ~gsub("q_", "q_diagn_", .)) %>% 
  
  ## Told diabetes age; harmonize across cycles -------------
  mutate(
    told_diab_age = case_when(
      Years %in% c("1999-2000", "2001-2002", "2003-2004") ~ q_told_diab_age2,
      q_told_diab_age == 666 ~ 0.5, # 666 = Less than 1 year
      TRUE ~ q_told_diab_age)) %>% 
  
  ## General health rating -------------
  mutate(
    genhealth = factor(
      gsub(",.*", "", gsub("[?]", "", q_genhealth_rating)),
      levels=c("Poor", "Fair", "Good", "Very good", "Excellent"))) %>%
  
  ## Health care utilization -------------
  mutate(
    # Have a usual place to go for health care?
    hc_usualplace = case_when(
      q_hc_place == "There is no place" ~ "No",
      q_hc_place == "There is more than one place" ~ "More than 1 place",
      TRUE ~ q_hc_place),
    
    # n of Doctor visits last year
    hc_drvisits_lastyr = factor(
      unname(hc_visit.labs[as.character(q_hc_drvisit)]), levels=c(
        unique(unname(hc_visit.labs)))),

    # n (overnight or longer) hospital admissions last year; only pre-2017
    hc_admits_lastyr = case_when(
      q_hc_admissions_pastyr == "1" ~ "1 time",
      q_hc_admissions_pastyr %in% c(as.character(2:6)) ~ paste0(q_hc_admissions_pastyr, " times"),
      q_hc_admissions_pastyr %in% c(as.character(7:12)) | 
        q_hc_admissions_pastyr == "6 times or more" ~ "6+ times",
      TRUE ~ NA)) %>% 
  mutate_at("hc_admits_lastyr", ~factor(
    ., levels = c("1 visit", paste(c(2:6), "visits"), "6+ visits"))) %>% 
  
  ## n Restaurant meals per week ---------------
  mutate(
    restaur_freq_wk = case_when(
      q_restaur_week == 6666 ~ 0.5, # Less than weekly
      q_restaur_week == 5555 | q_restaur_week == 26 ~ 21, # More than 21 times per week; re-coded to 21
      TRUE ~ q_restaur_week)) %>% 
  
  # Why at risk for diabetes & How tried losing weight ------------
  mutate(across(starts_with(c("q_riskdiab", "q_wtloss_")), ~ifelse(
    !is.na(.) & . != "", 1, 0))) %>% 
  
  # Recode tasetchange variables as No Change (Reference)/Worse/Better
  mutate(across(paste0("q_tastechange_", c("sweet", "salt", "sour", "bitter")), 
                ~factor(., levels=c("No Change", "Worse", "Better")))) %>% 
  mutate_at("q_tastechange_time", ~factor(case_when(
    . == "Less than 3 Months Ago" ~ "Less than 3m",
    . == "3 to 12 Months (1 Yesr) Ago" ~ "3m to 1yr",
    . == "1 to 4 years ago" ~ "1 to 4yr",
    . == "5 to 9 years ago" ~ "5 to 9yr",
    . == "Ten or more years ago" ~ "More than 10yr",
    TRUE ~ NA),
    levels=c("Less than 3m", "3m to 1yr", "1 to 4yr", "5 to 9yr", "More than 10yr")
    )) %>% 
  
  # Rename q_csq_ vars as q_tasetq 
  rename_with(., ~gsub("q_csq_", "q_tasteq_", .), .cols=everything()) %>%
  
  ## Select cleaned questionnaire variables 
  select(SEQN, Years, smoke_status, employ_status, work_lastwk,
         alch_freq_wk,  alch_everdrinker, insur_any, insur_private,
         uninsur_lastyr, phq9_total, told_diab_age, genhealth, 
         hc_usualplace, hc_drvisits_lastyr, hc_admits_lastyr, restaur_freq_wk,
         q_feel_riskdiab, q_had_dialysis, q_sob_incline, starts_with(c(
         paste0("q_", c("told_", "riskdiab_", "wtloss_", "diagn_", "tastechange", 
                          "tasteq"))))
  )


## Combine all quest_processed dataframes
nhanes_quest_processed <- full_join(
  nhanes_quest_rx_processed, nhanes_quest_behav_processed, 
  by = c("SEQN", "Years")) %>% 
  full_join(nhanes_quest_pa_processed, by = c("SEQN", "Years"))

quest_vars <- nhanes_quest_processed %>% select(-c("SEQN", "Years")) %>% names() 


## =====================================================
## Prepare dietary data phenotypes
## =====================================================

nhanes_diet_processed <- nhanes_data_raw %>% 
  select(SEQN, Years, starts_with(c("HEI", "AHEI", "DII"))) %>% 
  rename_with(str_to_lower, -c(SEQN, Years)) %>%
  rename_with(., ~gsub("all", "total", .), c(hei2015_all, ahei_all, dii_all))

diet_vars <- nhanes_diet_processed %>% select(-c("SEQN", "Years")) %>% names()    


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
    
    # COPD --------------------
    copd = case_when(
      q_told_bronch == 1 | q_told_anybronch == 1 | q_told_copd == 1 | 
        q_told_emphys == 1 ~ 1,
      TRUE ~ 0),
    # COPD, based on pulmonary function tests 
    pft_lt07 = case_when(
      fev1_pre / fvc_pre < 0.7~1,
      fev1_pre / fvc_pre >= 0.7~0,
      TRUE ~ NA),
    
    # CKD ---------------------
    ckd = case_when(
      q_told_kidfail == "Yes" | creatinine >= 1.4 ~ 1,
      TRUE ~ 0),
    
    # MDD (Depression) -------------------
    mdd = case_when(
      q_diagn_depress == "Positive Diagnosis" | q_med_depress == "Yes" | 
        phq9_total >= 10 ~ 1,
      TRUE ~ 0),
    
    # Hypertension --------------
    htn = case_when(
      q_told_htn == "Yes" | `q_told_htn_x2+` == "Yes" | sbp_mean >=130 | 
        dbp_mean >=80 ~ 1, 
      TRUE ~ 0)
    ) %>% 
  
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
    obesity_abd = case_when(
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
  
  # Congestive heart failure ------------------
  mutate(chf = case_when(
    q_told_chf == "Yes" ~ 1,
    q_told_chf == "No" ~ 0,
    q_told_chf %in% c("Refused", "Don't know", "") ~ NA)) %>%
  
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
      egfr < 15 ~ "Stage 5, Kidney failure",
      TRUE ~ NA), levels = c("Stage 0", "Stage 2", "Stage 3a",
                             "Stage 3b", "Stage 4", "Stage 5", "ESRD")),
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
    fib4_cat = factor(case_when(
      age <65 & fib4 <1.30  | age >=65 & fib4 < 2 ~ "Low",
      age <65 & fib4 >=1.3 & fib4 <2.67 | 
        age >=65 & fib4 >=2 & fib4 <2.67 ~ "Moderately elevated",
      fib4 >=2.67 ~ "High",
      TRUE ~ NA_character_), 
      levels = c("Low", "Moderately elevated", "High"))) %>% 
  
  # NAFLD fibrosis score ------------------
  mutate(
    nfs = -1.675 + 0.037 * age + 0.094 * bmi + 1.13 * diabetes +
      0.99 * (ast / alt) - 0.013 * plt - 0.66 * alb) %>%
  mutate(
    nfs_cat = case_when(
      is.na(nfs) ~ NA,
      nfs < -1.455 ~ "Low",
      nfs > 0.676 ~ "High",
      nfs >= -1.455 & nfs <= 0.676 ~ "Intermediate",
      TRUE ~ NA)) %>%
  
  # Liver-CAP steatosis -------------------------
  mutate(
    liver_valid_elastography = case_when(
    !is.na(liver_stiff) & !is.na(liver_iqr) & 
      liver_iqr <= 30 ~ 1, 
    TRUE ~ 0)) %>%
  
  mutate(
    liver_cap_level = factor(case_when(
      liver_cap < 240 ~ "Normal",
      liver_cap >= 240 ~ "High",
      TRUE ~ NA), levels=c("Normal", "High")),
    liver_cap_steatosis = factor(case_when(
      is.na(liver_cap) ~ NA,
      liver_cap < 248 ~ "No steatosis",
      liver_cap >= 248 & liver_cap < 268 ~ "Mild steatosis",
      liver_cap >= 268 & liver_cap < 280 ~ "Moderate steatosis",
      liver_cap >= 280 ~ "Severe steatosis"), 
      levels=paste0(c("No", "Mild", "Moderate", "Severe"), " steatosis")),
    liver_cap_masld = case_when(
      is.na(liver_cap) ~ NA,
      liver_cap >= 248 ~ "1L",
      TRUE ~ "0L"),
    liver_cap_moderate_severe = case_when(
      is.na(liver_cap) ~ NA,
      liver_cap >= 268 ~ "1L",
      TRUE ~ "0L")) %>%
  
  # Convert 0,1 to factor variables
  mutate(across(c(
    "obese", "cvd", "ascvd", "copd", "pft_lt07", "ckd", "mdd", "htn", 
    "diabetes", "diabetes_undx", "htn_undx", "obesity_abd", "htn_stg1", "chf", 
    "copd_pft", "ckd_malb_level", "ckd_any", "liver_valid_elastography"), 
    ~as.factor(.))) %>%
  
  # Select derived variables ----------------------
  select(SEQN, Years, diabetes, diabetes_undx, htn_undx, obesity_abd, 
         obese, cvd, copd, copd_pft, htn, mdd, ascvd, ckd, ckd_gfr_level, ckd_malb_level, 
         ckd_any, htn_stg1, chf, copd_pft, fib4, fib4_cat, 
         nfs, nfs_cat, liver_cap_level, liver_valid_elastography, liver_cap_steatosis, 
         liver_cap_masld, liver_cap_moderate_severe)

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
  filter(!Years %in% c("2017-2018", "2021-2023"))
  
nhanes_processed %>% saveRDS("../data/processed/nhanes_processed.rds")

nhanes_vars_by_cat <- list(
  demo_vars = demo_vars, exam_vars = exam_vars, lab_vars = lab_vars,
  quest_vars = quest_vars, diet_vars = diet_vars,
  disease_vars = disease_vars)

nhanes_vars_by_cat %>% saveRDS("../data/raw/nhanes_vars_by_cat.rds")


## Delete large breadcrunmbs ...
#rm(nhanes_demo_processed)
#rm(nhanes_quest_behav_processed) 
#rm(nhanes_lab_processed)
#rm(nhanes_exam_processed)
#rm(nhanes_quest_pa_processed)
#rm(nhanes_quest_rx_processed)
#rm(nhanes_quest_processed)
#rm(nhanes_diet_processed)
#rm(nhanes_disease_processed)


## EOF
# Last Updated: 08-28-2026


