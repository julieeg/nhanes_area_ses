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
nhanes_vars_all <- fread("../data/raw/nhanes_vars_all.csv") 

## Load ALL nhanes variables
nhanes_data_all <- fread("../data/raw/nhanes_data_all.csv")

## List NHANES years ---------
nhanes_yrs <- names(table(nhanes_tables_all$Years))
nhanes_yrs <- nhanes_yrs[-2] # remove "1999-2004"

# Make array of years/labels for selecting tables
names(nhanes_yrs) <- c("", paste0("_", LETTERS[2:10]),"P_", "_L", "M_")

## Load project data dictionary to grab years, tables & variables of interest
project_datadict <- readxl::read_xlsx("./nhanes_pullrequest_IndVsArea_07.30.2026.xlsx")


################################################################################
## NHANES Variable Cleaning and Data Set Preparation 
################################################################################

base_vars <- names(nhanes_data_raw %>% select(
  "Years", "SEQN", "RIDSTATR", starts_with("SD"), starts_with("WT"), -"wt")
  )
  
## Apply age restriction: Adults, aged 18 or older
nhanes_data_raw <- nhanes_data_all %>% filter(age >= 18) %>%
  mutate_at("RIDSTATR", ~gsub("Both Int", "Both int", gsub("Only", "only", .))) 
dim(nhanes_data_raw) # N = 77050 

# Summarise N 
nhanes_data_raw %>%
  group_by(RIDSTATR) %>% reframe(n=n())
  # RIDSTATR                              n
  # Both interviewed and MEC examined 71669
  # Interviewed Only                   5381

## ==================================
## Prepare DEMOGRAPHICS variables 
## ==================================

# racecat
racethn.labs <- c("NHW"="Non-Hispanic White", "NHB"="Non-Hispanic Black", "NHAsian"="Non-Hispanic Asian",
                  "Mexican-American"="Mexican American", "Other Hispanic"="Other Hispanic", 
                  "Other/Multi-Racial"="Other Race - Including Multi-Racial")
# education levels
educ.labs <- c("9-11th grade"="9-11th grade (includes 12th grade with no diploma)", 
               "HS graduate or GED"="High school graduate/ged or equivalent", 
               "HS graduate or GED"="High school grad/ged or equivalent",
               "Some college or AA degree"="Some college or aa degree")

educ_hh.labs <- c(educ.labs[-2], "HS graduate, GED or AA degree"="High school grad/ged or some college/aa degree")
educ_levels <- c("Less than 9th grade", "9-11th grade", "HS graduate or GED", 
                 "Some college or AA degree", "College graduate or above") 

# income levels
inc.vals <- c("$0-$4,999", "$5,000-$9,999", "$10,000-$14,999", "$15,000-$19,999", "Under $20,000", 
              "Over $20,000", "$20,000-$24,999", "$25,000-$34,999", "$35,000-$44,999", "$45,000-$54,999",
              "$55,000-$64,999", "$65,000-$74,999", "Over $75,000", "$75,000-$99,999", "Over $100,000")

# working status levels
work.vals <- c("Working"="Working at a job or business", "Looking for work" = "Looking for work or", 
               "At a business, but not at work"="With a job or business but not at work",
               "Not working"="Not working at a job or business?", "Refused"="Refused", 
               "Don't know"="Don't know", "Age <16 y"="Age <16 y") 


## Build nhanes_demo_processed --------------------------
nhanes_demo_processed <- nhanes_data_raw %>% 
  # Age & sex --------------------
  mutate(
    age_gt65 = ifelse(age>65,1,0),  
    age_gte16 = ifelse(age>=16,1,0),
    age_gt65.lab = ifelse(age_gt65 == 1, "Above 65 years", "Below 65 years"),
    female = ifelse(gender == "Female", 1, 0)) %>%
  # Race/ethnicity --------------------
  mutate_at("racethn_addNHA", ~ifelse(.=="", racethn1, .)) %>%
  mutate(
    racethn = add_descr_labels(., "racethn1", racethn.labs, ordered = T),       
    racethn_addNHA = add_descr_labels(., "racethn_addNHA", racethn.labs, ordered = T)) %>%
  mutate(race_white = case_when(racethn == "NHW"~1, is.na(racethn) ~ NA, TRUE ~ 0)) %>%
  # Education levels --------------------
  mutate(across(c(educ, educ_hhref), str_to_sentence)) %>%
  mutate(educ_level = educ %>% 
           fct_recode(!!!educ.labs) %>% 
           fct_other(drop = c("", "Refused", "Don't know"), other_level=NA) %>% 
           fct_relevel(educ_levels)) %>%
  mutate(educ_level_hh = educ_hhref %>%
           fct_recode(!!!educ_hh.labs) %>%
           fct_other(drop = c("", "Refused", "Don't know"), other_level=NA) %>%
           fct_relevel(educ_levels)) %>%
  # Income level --------------------
  mutate_at(c("income_hh", "income_fam"), ~factor(gsub("er", "er ", gsub(" ", "", gsub(" to ", "-", .))))) %>% 
  mutate_at(c("income_hh", "income_fam"), ~factor(case_when(
    . == "$75,000andOver " ~ "Over $75,000",
    . == "$100,000andOver " ~ "Over $100,000",
    . == "Don'tknow" ~ "Don't know", 
    TRUE ~ .), levels=inc.vals)) %>%
  mutate_at(c("income_hh", "income_fam"), ~case_when(
    . %in% c("", "Refused", "Don't know") ~ NA, 
    TRUE ~ .)) %>%
  # Working status & hours (if age ≥ 16 yrs) --------------------
  mutate(
    working = case_when(
      age<16 ~ "Age <16 y", q_work_type == "" ~ NA, 
      TRUE ~ gsub(",.*", "", gsub("[?]", "",q_work_type))),
    working_gt35hr = case_when(
      age<16 ~ "Age <16 y", q_work_gt35hr %in% c("", "Refused", "Don't know") ~ NA,
      TRUE ~ q_work_gt35hr)) %>%
  mutate_at("working", ~factor(., levels=c(
    "Working at a job or business", "With a job or business but not at work",
    "Not working at a job or business", "Looking for work", "Age <16 y"))) %>%
  select(base_vars, age, age_gte16, age_gt65, age_gt65.lab, gender, female, racethn, racethn_addNHA, 
         educ_level, educ_hhref, income_hh, income_fam, inc_to_pov, working, working_gt35hr)


## ==================================
## Prepare EXAM variables 
## ==================================

exam_vars <- nhanes_vars_datadict %>% 
  filter(Category == "exam") %>%
  filter(!grepl("taste_", New.Variable.Name) & 
           !grepl("smell_", New.Variable.Name)) %>%
  pull(New.Variable.Name) %>% unique()


## Write function to create "id_correct" vars for taste/smell tests
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
  #filter(!is.na(taste_status)) %>%
  # Binary correct=1/incorrect=0/missing variables for taste ID tests ----------
  recode_chemos_correct.fun(test_var = "taste_tongue_quinine_id", correct_value = "Bitter") %>%
  recode_chemos_correct.fun(test_var = "taste_mouth_quinine_id", correct_value = "Bitter") %>%
  recode_chemos_correct.fun(test_var = "taste_tongue_nacl_id", correct_value = "Salty") %>%
  recode_chemos_correct.fun(test_var = "taste_mouth_nacl_1M_id", correct_value = "Salty") %>%
  recode_chemos_correct.fun(test_var = "taste_mouth_nacl_320mM_id", correct_value = "Salty") %>%
  recode_chemos_correct.fun(test_var = "taste_mouth_nacl_1M_id_repeat", correct_value = "Salty") %>%
  recode_chemos_correct.fun(test_var = "taste_mouth_nacl_320mM_id_repeat", correct_value = "Salty") %>%
    # Binary correct=1/incorrect=0/missing variables for smell ID tests ----------
  recode_chemos_correct.fun(test_var = "smell_chocolate", correct_value = "Chocolate") %>%
  recode_chemos_correct.fun(test_var = "smell_strawberry", correct_value = "Strawberry") %>%
  recode_chemos_correct.fun(test_var = "smell_smoke", correct_value = "Smoke") %>%
  recode_chemos_correct.fun(test_var = "smell_leather", correct_value = "Leather") %>%
  recode_chemos_correct.fun(test_var = "smell_soap", correct_value = "Soap") %>%
  recode_chemos_correct.fun(test_var = "smell_grape", correct_value = "Grape") %>%
  recode_chemos_correct.fun(test_var = "smell_onion", correct_value = "Onion") %>%
  recode_chemos_correct.fun(test_var = "smell_gas", correct_value = "Gas")

# merge into basic exam vars
nhanes_exam_processed <- nhanes_data_raw %>% 
  select(SEQN, Years, exam_vars) %>% 
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
  full_join(nhanes_csexam_processed, by = c("SEQN", "Years"))


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
  eGFR <- 142 *
    (pmin(scr, 1)^alpha) * 
    (pmax(scr, 1)^-1.200) *
    (0.9938^age) * 
    female_mult
  
  egfr_race <- 141 *
    (pmin(scr/kappa, 1) ^ alpha) *
    (pmax(scr/kappa, 1) ^ -1.209) *
    (0.993^age) * female_mult * race_mult
  
  return(egfr_race)
}


lab_vars <- nhanes_vars_datadict %>% 
  filter(Category=="lab" & !(New.Variable.Name %in% base_vars)) %>% 
  pull(New.Variable.Name) %>% unique()

nhanes_lab_processed <- nhanes_data_raw %>%
  # Add binary var for black race (for egfr calculation)
  mutate(racethn_black = case_when(
    racethn1 == "Non-Hispanic Black" ~ 1,
    racethn1 == "" ~ NA,
    TRUE ~ 0)) %>%
  select(SEQN, Years, gender, age, racethn_black, all_of(lab_vars)) %>%
  mutate(female = ifelse(gender == "Female",1,0)) %>%
  # Triglyceride for 2017-2020
  mutate_at("tg", ~ifelse(Years == "2021-2023", -12.19+(0.9785*.), .)) %>%
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
  # U_albumin: measurement change in 2021-2023 cycle 
  # Adjust urinary albumin for measurement change in 2021-2023 
  mutate_at("u_albumin", ~ifelse(Years == "2021-2023", -1.643+1.189*., .)) %>%
  # For values <0, use Lower Limits of Detection, u_alb = 0.02 mg/dL
  mutate_at("u_albumin", ~ifelse(.<0, 0.02, .)) %>%
  ## Urinary albumin-to-creatinine ratio (mg/g) for PREVENT Equation
  # UACR (mg/g) = l_u_albumin (ug/mL) / l_u_creatinine (mg/dL)
  mutate(uacr = (u_albumin / u_creatinine) *100) %>%
  ## eGFR, baed on CKD-EPI 2021 equation
  mutate(egfr = calc_egfr_ckdepi.fun(creatinine = creatinine, age=age, female=female)) %>%
  mutate(egfr_race = calc_egfr_ckdepi_race.fun(creatinine = creatinine, age=age, female=female, black=racethn_black)) %>%
  select(-age, -female, -gender)
  

## =======================================
## Prepare QUESTIONNAIRE variables 
## =======================================

## Medication use --------------------------

rxq_url <- "https://wwwn.cdc.gov/Nchs/Data/Nhanes/Public/1988/DataFiles/RXQ_DRUG.xpt"
rxq_drug <- nhanesA::nhanesFromURL(rxq_url)

# Create list of drug codes for STATIN medications
rx_statins <- rxq_drug %>% 
  filter(endsWith(RXDDRUG, "STATIN") & 
           RXDDCN1C == "HMG-COA REDUCTASE INHIBITORS (STATINS)") %>%
  select(RXDDRGID, RXDDRUG)

# Create list of drug codes for ANTI-HYPERTENSIVE (BP) medications
rx_bpx <- c("pril", "sartan", "dipine", "thiazide", "lol", "osin", 
            "Hydralazine", "Isosorbide", "Spironolactone", "Triamterene",
            "Eplerenone", "Amiloride", "Verapamil", "Diltiazem", "Chlorthalidone", 
            "Clonidine", "Imdapamide") ; 
rx_bp <- rxq_drug[rowSums(sapply(rx_bpx, function(rx) {
  grepl(rx, rxq_drug[["RXDDRUG"]], ignore.case = T) }))>0,] %>%
  filter(!grepl("TIMOLOL",RXDDRUG, ignore.case = T)) %>%
  # Remove non-BP medications
  filter(!grepl("ANTIARRHYTHMICS", RXDDCN1C, ignore.case = T)) %>%
  filter(!RXDDCN1C %in% c("NRTIS", "OPHTHALMIC GLAUCOMA AGENTS", "TOPICAL ANESTHETICS")) %>%
  select(RXDDRGID, RXDDRUG)


# Diabetes medications 
rx_diabx <- c("glipizide", "glyburide", "glimepiride", "gliclazide", "tolbutamide", 
              "chlorpropamide", "tolazamide", "acetohexamide", "glinide", 
              "glitazone", "gliptin")
rx_diab <- rxq_drug[rowSums(sapply(rx_diabx, function(rx) {
  grepl(rx, rxq_drug[["RXDDRUG"]], ignore.case = T) }))>0,] %>%
  select(RXDDRGID, RXDDRUG)
  

# Create binary (yes/no) Medication use variables for statins and BP-lowering drugs
nhanes_quest_rx_processed <- nhanes_data_raw %>% 
  select(SEQN, Years, rx_use_any = q_rx_use_1, starts_with("q_rx_drugid_")) %>%
  pivot_longer(cols=starts_with("q_rx_drugid_"), names_to="drug_id", values_to="q_rx_drugid") %>%
  #filter(q_rx_drugid != "") %>%
  group_by(SEQN, Years) %>%
  summarize(
    rx_use_statin = if_else(any(q_rx_drugid %in% rx_statins$RXDDRGID, na.rm = TRUE), "Yes",
                            if_else(any(rx_use_any %in% c("Don't know", "Refused")), NA, "No")),
    rx_statin_id = { 
      matched <- unique(rx_statins$RXDDRUG[q_rx_drugid %in% rx_statins$RXDDRGID])
      if (length(matched) > 0) paste(matched, collapse = "; ") else NA_character_ },
    rx_use_bpmed  = if_else(any(q_rx_drugid %in% rx_bp$RXDDRGID, na.rm = TRUE), "Yes",
                            if_else(any(rx_use_any %in% c("Don't know", "Refused")), NA, "No")),
    rx_bpmed_id = {
      matched <- unique(rx_bp$RXDDRUG[q_rx_drugid %in% rx_bp$RXDDRGID])
      if (length(matched) > 0) paste(matched, collapse = "; ") else NA_character_},
    rx_use_diabmed = if_else(any(q_rx_drugid %in% rx_diab$RXDDRGID, na.rm = TRUE), "Yes", 
                             if_else(any(rx_use_any %in% c("Don't know", "Refused")), NA, "No")),
    rx_diabmed_id = {
      matched <- unique(rx_diab$RXDDRUG[q_rx_drugid %in% rx_diab$RXDDRGID])
      if (length(matched) > 0) paste(matched, collapse = "; ") else NA_character_},
    .groups = "drop") %>% 
  select(-starts_with("q_rx_"))
 
 
# Create physical activity level variable --------------------
nhanes_quest_pa_processed <- nhanes_data_raw %>% 
  select(SEQN, Years, starts_with("q_mvpa_"), starts_with("q_vpa_"))


# Calculate PHQ depression score --------------------
recode_phq.fun <- function(x) {
  case_when(
    x == "Not at all" ~ 0,
    x == "Several days" ~ 1,
    x == "More than half the days" ~ 2,
    x == "Nearly every day" ~ 3,
    x %in% c("Refused", "Don't know", "Missing", "") ~ NA,
    TRUE ~ NA  # Catches any other unexpected values
  )
} ; phq_vars <- nhanes_data_raw %>% select(starts_with("q_phq_"), -"q_phq_work") %>% names()


## Health Insurance & Smoking Status ---------------
nhanes_quest_behav_processed <- nhanes_data_raw %>% 
  ## Health Insurance: covered by ANY health insurance, yes/no?
  mutate(
    healthinsure_any = case_when(
      q_insur_any %in% c("Don't know", "Refused", "") ~ NA,
      TRUE ~ q_insur_any)) %>% 
  ## Smoking: current smoker, yes/no
  # Step 1) clean smoking status variable, for smoking cigarettes now (SMQ020)
  mutate_at("q_smoke_current", ~case_when(
    . %in% c("Every day", "Every day,") ~ "Every day",
    . %in% c("Not at all", "Not at all?") ~ "Not at all",
    . %in% c("Some days", "Some days, or") ~ "Some days",
    TRUE ~ .)) %>%
  # Step 2) Define from smoke_history (smoked ≥100 cigarettes in your life?; if
  # (No --> smoke_status = N/A) & smoke_status (do you smoke cigarettes now?;
  # (Every day, Smoke days, Not at all)
  mutate(
    smoke_current = case_when(
      q_smoke_ever == "No" ~ "Never smoker", # Never-smoker (<100 cigarettes/lifetime)
      q_smoke_ever == "Yes" & q_smoke_current == "Not at all" ~ "Former smoker", # Former-smoker (>100 cigarettes/life, but "Not at all" now)
      q_smoke_ever == "Yes" & q_smoke_current %in% c("Some days", "Every day") ~ "Current smoker",
      TRUE ~ NA)) %>%
  # Calculate total PHQ depression score
  mutate(across(c(phq_vars), recode_dpq.fun)) %>%
  mutate(phq9_total = rowSums(across(phq_vars))) %>%
  ## Select raw & cleaned questionnaire variables 
  select(SEQN, Years, healthinsure_any, smoke_current, phq9_total, starts_with("q_"), 
         -starts_with("q_rx_"), -starts_with("q_mvpa"), -starts_with("q_vpa_"), 
         -starts_with("q_mpa"))

nhanes_quest_processed <- full_join(
  nhanes_quest_rx_processed, nhanes_quest_behav_processed, by = c("SEQN", "Years")
  ) ; names(nhanes_quest_processed)
 

## =====================================================
## Prepare dietary data phenotypes
## =====================================================

nhanes_diet_processed <- nhanes_data_raw %>% 
  select(SEQN, Years, starts_with("HEI"), starts_with("AHEI"))
    
## =====================================================
## Prepare additional clinical outcomes
## =====================================================

addn_outcomes <- c("obese", "cvd", "ascvd", "copd", "ckd", "mdd")
nhanes_addn_processed <- nhanes_data_raw %>% 
  full_join(nhanes_quest_rx_processed, by=c("SEQN", "Years")) %>%
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
    # COPD ---------------------
    ckd = case_when(
      q_told_kidfail == "Yes" | creatinine >= 1.4 ~ 1,
      TRUE ~ 0),
    # MDD (Depression) -------------------
    mdd = case_when(
      q_depress == "Positive Diagnosis" | q_med_depress == "Yes" ~ 1, # phq9 >=10
      TRUE ~ 0),
    # Hypertension --------------
    htn = case_when(
      q_told_htn == "Yes" | `q_told_htn_x2+` == "Yes" | sbp_avg >=130 | 
        dbp_avg >=80 ~ 1, 
      TRUE ~ 0)
    ) %>% 
  # Diabetes (diagnosed) -----------------------
  mutate(
    diabetes = case_when(
      q_told_diab == "Yes" | # Doctor told you, you have diabetes
        q_med_insulin == "Yes" | # Taking insulin now
        q_med_diab == "Yes" | # Taking diabetic pills to lower blood sugar
        rx_use_diabmed == "Yes" |  # Using any antidiabetic medication 
        hba1c >= 6.4 | # HbA1c > 6.4%
        fg > 125 ~ 1, # Fasting glucose >= 126 mg/dL (CHECK: LB2GLU > 199 | LB2SGL > 199)
      q_told_diab %in% c("No", "Borderline", "") &  
        (hba1c < 6.4 | is.na(hba1c)) & 
        (fg <126 | is.na(fg)) ~ 0,
      TRUE ~ NA)) %>%
  # Undiagnosed diabetes ------------------
  mutate(
    diabetes_undx = case_when(
      diabetes == 1 & # Have diabetes (as defined, above)
        q_told_diab != "Yes" & 
        q_med_insulin != "Yes" & 
        q_med_diab != "Yes" ~ 1,
      TRUE ~ 0)) %>%
  # Undiagnosed hypertension ------------------
  mutate(
    htn_undx = case_when(
      (!is.na(sbp_avg) & !is.na(dbp_avg)) ~ NA,
      sbp_avg >= 130 | dbp_avg >= 80 & 
        q_told_htn != "Yes" & 
        q_med_bp != "Yes" & 
        rx_use_bpmed != "Yes" ~ 1,
      TRUE ~ 0)) %>%
  # Abdominal obesity definitions ------------------
  mutate(
    obesity_abd = case_when(
      gender == "Male" & waist >= 102 ~ 1,
      gender == "Female" & waist >= 88 ~ 1,
      TRUE ~ 0)) %>%
  # Alternative Hypertension definition ------------------
  mutate(
    htn_stg1 = case_when(
      q_told_htn == "No" | 
      q_med_bp == "No" | 
      sbp_avg >= 130 | dbp_avg >=80 ~ 1,
    TRUE ~ 0)) %>%
  # Congestive heart failure ------------------
  mutate(chf = case_when(
    q_told_chf == "Yes" ~ 1,
    q_told_chf == "No" ~ 0,
    q_told_chf %in% c("Refused", "Don't know", "") ~ NA)) %>%
  # COPD-related outcomes ------------------
  mutate(
    copd_pft = case_when(
      !is.na(fev1_pre) & !is.na(fvc_pre) & 
        fev1_pre / fvc_pre < 0.7 | 
        q_told_bronch == "Yes" |
        q_told_copd == "Yes" |
        q_told_emphys == "Yes" ~ 1, 
      TRUE ~ 0)) %>%
  # MASLD-related outcomes ------------------
  mutate(fib4 = (age*ast)/(plt*sqrt(alt))) %>%
  mutate(
    fib4_cat = case_when(
      fib4 < 1.30 ~ "Low",
      fib4 <= 2.67 ~ "Intermediate",
      fib4 > 2.67 ~ "High",
      TRUE ~ NA_character_)) %>% 
  mutate(
    fib4_high = case_when(
      age < 65 & fib4 > 2.67 ~ "1L",
      age >= 65 & fib4 > 2.00 ~ "1L",
      !is.na(fib4) ~ "0L",
      TRUE ~ NA_character_)) %>%
  # NAFLD fibrosis score ------------------
  mutate(
    nfs = -1.675 + 0.037 * age + 0.094 * bmi + 1.13 * diabetes +
      0.99 * (ast / alt) - 0.013 * plt - 0.66 * alb) %>%
  mutate(
    nfs_cat = case_when(
      is.na(nfs) ~ NA,
      nfs < -1.455 ~ "Low",
      nfs > 0.676 ~ "High",
      nfs >= -1.455 & nfs <= 0.676 ~ "Indeterminate",
      TRUE ~ NA)) %>%
  mutate(
    liver_valid_elastography = case_when(
    !is.na(liver_stiff) & !is.na(liver_iqr) & 
      liver_iqr <= 30 ~ 1, 
    TRUE ~ 0)) %>%
  # Liver-CAP steatosis -------------------------
  mutate(
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
  select(SEQN, Years, diabetes, diabetes_undx, htn_undx, obesity_abd, 
         obese, cvd, copd, htn, mdd, ascvd, ckd,
         htn_stg1, chf, copd_pft, fib4, fib4_cat, fib4_high, nfs, nfs_cat, 
         liver_valid_elastography, liver_cap_steatosis, liver_cap_masld, 
         liver_cap_moderate_severe)


################################################################################
## Combine all nhanes variables
################################################################################

nhanes_processed <- full_join(
  nhanes_demo_processed, nhanes_exam_processed, by = c("SEQN", "Years")) %>%
  full_join(nhanes_lab_processed, by = c("SEQN", "Years")) %>%
  full_join(nhanes_quest_processed, by = c("SEQN", "Years")) %>%
  full_join(nhanes_diet_processed, by = c("SEQN", "Years")) %>%
  full_join(nhanes_addn_processed, by = c("SEQN", "Years"))

## =====================================================
## Prepare PREVENT variables 
## =====================================================

## load PREVENTR
library(preventr)

## Add prevent-specific age and sbp values based on max/min
nhanes_processed <- nhanes_processed %>% 
  mutate(
    prevent_age = ifelse(age>30 & age <80, age, NA),
    prevent_sex = ifelse(female == 1, "female", "male"),
    prevent_sbp = ifelse(sbp_mean>90 & sbp_mean<180, sbp_mean, NA),
    prevent_bprx = ifelse(rx_use_bpmed=="Yes",1,0),
    prevent_tc = ifelse(tc>130 & tc<320, tc, NA),
    prevent_hdl = ifelse(hdl>20 & hdl<100, hdl, NA),
    prevent_statin = ifelse(rx_use_statin=="Yes",1,0),
    prevent_diab = diabetes,
    prevent_smoking = ifelse(smoke_current == "Current smoker", 1, 0),
    prevent_egfr = ifelse(egfr >15 & egfr <140, egfr, NA),
    prevent_egfr_race = ifelse(egfr_race >15 & egfr_race <140, egfr_race, NA),
    prevent_bmi = ifelse(bmi>=18.5 & bmi<=39.9, bmi, NA),
    prevent_hba1c = ifelse(hba1c>=4.5 & hba1c <=15, hba1c, NA),
    prevent_uacr = ifelse(uacr >= 0.1 & uacr <= 25000, uacr, NA)
    ) 

nhanes_processed %>% saveRDS("../data/processed/nhanes_processed.rda")


## =====================================================
## Create analytical dataframes 
## =====================================================

# PREVENT variables ------------------------
# prevent inputs, outcomes and strata
prevent_nhanes_processed <- nhanes_processed %>%
  select(base_vars, age,  age_gt65, female, racethn, racethn_addNHA, 
         cvd, ascvd, ckd, copd, htn, diabetes, diabetes_undx, 
         starts_with("prevent_")) %>%
  ## Participant exclusions: Age range: <30 and >79 years
  # No existing CVD, CKD, ASCVD, Diabetes (diagnosed or undiagnosed)
  filter(!is.na(prevent_age) & 
           cvd == 0 & ckd == 0 & ascvd == 0 & 
           diabetes == 0 & diabetes_undx == 0
         )


## =====================================================
## Create data dictionary for derived variables
## =====================================================

nhanes_vars_datadict %>% head()

derived_vars_datadict <-
  rbind.data.frame(
    c("age_gt65", "female","racethn", "racethn_addNHA", "educ_level", "working")
    

## EOF
# Last Updated: 08-10-2026


