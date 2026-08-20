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

base_vars <- names(nhanes_data_all %>% select(
  "Years", "SEQN", "RIDSTATR", starts_with("SD"), starts_with("WT"), -"wt")
  )
  
## Apply age restriction: Adults, aged 18 or older
nhanes_data_raw <- nhanes_data_all %>% 
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
    age_gte16 = ifelse(age>=16,1,0),
    age_gt65 = ifelse(age>65,1,0),  
    age_3lvl = factor(
      case_when(age<40 ~ "under40y", 
                age >=40 & age<65 ~ "40to65y",
                age>=65 ~ "over65y",
                TRUE ~ NA), levels=c("under40y", "40to65y", "over65y")),
    female = ifelse(gender == "Female", 1, 0)) %>%
  mutate(
    age_gt65.lab = factor(ifelse(age_gt65 == 1, "Above 65 years", "Below 65 years"),
                          levels=c("Below 65 years", "Above 65 years"))) %>%
  # Race/ethnicity --------------------
  mutate_at("racethn_addNHA", ~ifelse(.=="", racethn1, .)) %>%
  mutate(
    racethn = add_descr_labels(., "racethn1", racethn.labs, ordered = T),       
    racethn_addNHA = add_descr_labels(., "racethn_addNHA", racethn.labs, ordered = T)) %>%
  mutate(racethn_combn = case_when(
    racethn_addNHA == "Other/Multi-Racial" & Years %in% c(nhanes_yrs[1:7]) ~ "Other/Multi-Racial/NHAsian",
    TRUE ~ racethn_addNHA)) %>%
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
  select(base_vars, age, age_gte16, age_gt65, age_gt65.lab, age_3lvl, 
         gender, female, racethn, racethn_addNHA, racethn_combn, educ_level, 
         educ_hhref, income_hh, income_fam, inc_to_pov, working, working_gt35hr)

demo_vars <- names(nhanes_demo_processed %>% select(-base_vars))


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
  filter(!startsWith(New.Variable.Name, "WT")) %>%
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
  # -For values <0, use Lower Limits of Detection, u_alb = 0.02 mg/dL
  mutate_at("u_albumin", ~ifelse(Years == "2021-2023", -1.643+1.189*., .)) %>%
  mutate_at("u_albumin", ~ifelse(.<0, 0.02, .)) %>%
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
  select(-age, -female, -gender)
  

## ===================================================
## Prepare QUESTIONNAIRE variables 
## ====================================================

# ------------------------------------------
## Medication use (Statins, BP, Diabetes) ##
# ------------------------------------------

rxq_url <- "https://wwwn.cdc.gov/Nchs/Data/Nhanes/Public/1988/DataFiles/RXQ_DRUG.xpt"
rxq_drug <- nhanesA::nhanesFromURL(rxq_url)

## 1) Create list of drug codes for each medication type 

# STATIN medications
rx_statin <- rxq_drug %>% 
  filter(endsWith(RXDDRUG, "STATIN") & 
           RXDDCN1C == "HMG-COA REDUCTASE INHIBITORS (STATINS)") %>%
  select(RXDDRGID, RXDDRUG) %>% mutate(RXUSE="statin")

# ANTI-HYPERTENSIVE (BP) medications
rx_bpx <- c("pril", "sartan", "dipine", "thiazide", "lol", "osin", 
            "Hydralazine", "Isosorbide", "Spironolactone", "Triamterene",
            "Eplerenone", "Amiloride", "Verapamil", "Diltiazem", "Chlorthalidone", 
            "Clonidine", "Imdapamide") ; rx_bp <- rxq_drug %>%
  filter(grepl(paste0(str_to_upper(rx_bpx), collapse = "|"), RXDDRUG, ignore.case = TRUE)) %>%
  # Remove non-BP medications (& explicitly TIMOLOL, more common as a topical agent)
  filter(!grepl("ANTIARRHYTHMICS|NRTIS|OPHTHALMIC GLAUCOMA AGENTS|TOPICAL ANESTHETICS", 
                RXDDCN1C, ignore.case = T)) %>%
  filter(!grepl("TIMOLOL",RXDDRUG, ignore.case = T)) %>%
  select(RXDDRGID, RXDDRUG) %>% mutate(RXUSE = "bp") %>% 
  distinct()

# GLUCOSE LOWERING/DIABETES medications
rx_diabx <- c("insulin", "metformin", "glipizide", "glyburide", "glimepiride", 
              "gliclazide", "tolbutamide", "chlorpropamide", "tolazamide", 
              "acetohexamide", "glinide", "glitazone", "gliptin") ; rx_diab <- rxq_drug %>%
  filter(grepl(paste0(str_to_upper(rx_diabx_pf), collapse = "|"), RXDDRUG, ignore.case = TRUE)) %>%
  select(RXDDRGID, RXDDRUG) %>% mutate(RXUSE = "diab") %>% 
  distinct()

# Merge all rxtypes into df
rx_alltypes <- rbind.data.frame(rx_statin, rx_bp, rx_diab)

## 2) Use rx_alltypes to find participants taking ANY medications 
rx_alluse <- nhanes_data_raw %>% 
  select(SEQN, Years, rx_use_any = q_rx_use_1, starts_with("q_rx_drugid_")) %>%
  pivot_longer(cols=starts_with("q_rx_drugid_"), names_to="drug_id", values_to="RXDDRGID") %>%
  full_join(rx_alltypes) %>% filter(!is.na(RXUSE)) %>%
  select(SEQN, rx_use_any, RXDDRGID, RXDDRUG, RXUSE) %>%
  distinct() %>% 
  # Convert to wide-format to merge with nhanes_quest_processed
  group_by(SEQN, RXUSE) %>% summarise(
    rx_use = "Yes",
    rx_id = paste0(unique(RXDDRGID), collapse="; "),
    rx_drug = paste0(unique(RXDDRUG), collapse="; "),
    .groups = "drop") %>%
  pivot_wider(id_cols = "SEQN", names_from="RXUSE", values_from = c(rx_use, rx_id, rx_drug),
              names_glue="{.value}_{RXUSE}")

# 3) Merge in medication data & recode No/NA values based on rx_use_any
nhanes_quest_rx_processed <- nhanes_data_raw %>% 
  select(SEQN, Years, rx_use_any = q_rx_use_1, starts_with("q_med_")) %>%
  left_join(rx_alluse, by = "SEQN") %>%
  mutate_at(c("rx_use_statin", "rx_use_bp", "rx_use_diab"),
            ~case_when(rx_use_any == "No" ~ "No",
                       rx_use_any %in% c("", "Yes") & is.na(.) ~ "No",
                       rx_use_any %in% c("Refused", "Don't know") ~ NA,
                       TRUE ~ .)
            )

# ----------------------------------
## Physical activity data (METs) 
# ----------------------------------

## To calculate METs (Metabolic Equivalents) per week = 
## MET-min/wk = MET Intensity x Frequency (times/week) x Duration (min/session)
## NOTE: MET estimates must be calcualted separately for 1999-2006 / 2007 onward
## to account for a change in PA survey 

# Download additional variables for engaging in ANY PA (MVPA or Transportation)
#anypa_tables.l <- build_nhanes_table(
#  cat="QUESTIONNAIRE", datadict = project_datadict %>%
#    filter(Varname %in% c("q_mvpa_anyvpa", "q_mvpa_anympa")))
#saveRDS(anypa_tables.l, "../data/raw/anypa_tables_all.rds")
anypa_tables.l <- readRDS("../data/raw/anypa_tables_all.rds")

## 1) Calculate METs in 1999-2006 cycles;
nhanes_pa_pre07 <- nhanes_data_raw %>% 
  
  # Gather & merge in required var over 1999-2006 cycles ----------
  select(SEQN, Years, starts_with("q_mvpa")) %>%
  filter(Years %in% nhanes_yrs[1:4]) %>%
  left_join(anypa_tables.l$data_table, by = c("SEQN", "Years")) %>%
  
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


# -----------------------------------------------------------------------
## Behavioral Variables: Health Insurance, Smoking & Depression (PHQ)
# -----------------------------------------------------------------------

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
  nhanes_quest_rx_processed, nhanes_quest_behav_processed, 
  by = c("SEQN", "Years")) %>% 
  full_join(nhanes_quest_pa_processed, by = c("SEQN", "Years"))
names(nhanes_quest_processed)
 

## =====================================================
## Prepare dietary data phenotypes
## =====================================================

nhanes_diet_processed <- nhanes_data_raw %>% 
  select(SEQN, Years, starts_with("HEI"), starts_with("AHEI"))
    
## =====================================================
## Prepare additional clinical outcomes
## =====================================================

addn_outcomes <- c("obese", "cvd", "ascvd", "copd", "ckd", "mdd")

nhanes_disease_processed <- full_join(
  nhanes_data_raw, nhanes_quest_rx_processed) %>%
  left_join(
    nhanes_quest_processed %>% select(SEQN, Years, phq9_total), 
            by = c("SEQN", "Years")
    ) %>%
  left_join(
    nhanes_lab_processed %>% select(SEQN, Years, egfr), 
    by = c("SEQN", "Years")
    ) %>% 
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
      q_depress == "Positive Diagnosis" | q_med_depress == "Yes" | 
        phq9_total >= 10 ~ 1,
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
      (is.na(sbp_avg) & is.na(dbp_avg)) ~ NA,
      sbp_avg >= 130 | dbp_avg >= 80 | htn == 1 &
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
      copd == 1 | pft_lt07 == 1 ~ 1,
      TRUE ~ 0)) %>%
  # CKD-related outcomes -----------
  mutate(
    ckdgfr_level = factor(case_when(
      q_had_dialysis == "Yes" ~ "ESRD",
      egfr >= 90 ~ "Stage 0",
      egfr >=60 & egfr <90 ~ "Stage 2",
      egfr >= 45 & egfr < 60 ~ "Stage 3a",
      egfr >= 30 & egfr < 45 ~ "Stage 3b",
      egfr >= 15 & egfr <30 ~ "Stage 4",
      egfr < 15 ~ "Stage 5, Kidney failure",
      TRUE ~ NA), levels = c("Stage 0", "Stage 2", "Stage 3a",
                             "Stage 3b", "Stage 4", "Stage 5", "ESRD")),
    ckdmalb_level = factor(case_when(
      u_albumin <= 30 ~ 0,
      u_albumin > 30 & u_albumin <= 100 ~ 1,
      u_albumin > 100 & u_albumin <= 300 ~ 2,
      u_albumin > 300 ~ 3))) %>%
  mutate(ckdany = case_when(
    ckdgfr_level != "Stage 0" | ckdmalb_level != 0 ~ 1,
    ckdgfr_level == "Stage 0" | ckdmalb_level == 0 ~ 0,
    TRUE ~ NA)) %>% # MASLD-related outcomes ------------------
  mutate(fib4 = (age*ast)/(plt*sqrt(alt))) %>%
  mutate(
    fib4_cat = factor(case_when(
      age <65 & fib4 <1.30  | age >=65 & fib4 < 2 ~ "Low",
      age <65 & fib4 >=1.3 & fib4 <2.67 | age >=65 & fib4 >=2 & fib4 <2.67 ~ "Moderately elevated",
      fib4 >=2.67 ~ "High",
      TRUE ~ NA_character_), levels = c("Low", "Moderately elevated", "High"))) %>% 
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
  select(SEQN, Years, diabetes, diabetes_undx, htn_undx, obesity_abd, 
         obese, cvd, copd, copd_pft, htn, mdd, ascvd, ckd, ckdgfr_level, ckdmalb_level, 
         ckdany, htn_stg1, chf, copd_pft, fib4, fib4_cat, 
         nfs, nfs_cat, liver_cap_level, liver_valid_elastography, liver_cap_steatosis, 
         liver_cap_masld, liver_cap_moderate_severe)


################################################################################
## Combine all nhanes variables
################################################################################

nhanes_processed <- full_join(
  nhanes_demo_processed, nhanes_exam_processed, by = c("SEQN", "Years")) %>%
  full_join(nhanes_lab_processed, by = c("SEQN", "Years")) %>%
  full_join(nhanes_quest_processed, by = c("SEQN", "Years")) %>%
  full_join(nhanes_diet_processed, by = c("SEQN", "Years")) %>%
  full_join(nhanes_disease_processed, by = c("SEQN", "Years"))
  
nhanes_processed %>% saveRDS("../data/processed/nhanes_processed.rda")


## ===================================================
## Create data.frame of nhanes variable weights
## ===================================================

## EOF
# Last Updated: 08-17-2026


