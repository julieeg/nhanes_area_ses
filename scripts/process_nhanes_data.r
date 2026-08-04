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
nhanes_data_raw <- fread("../data/raw/nhanes_data_all.csv")

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
  "Years", "SEQN", starts_with("SD"), starts_with("WT"), -"wt")
  )
  
## ==================================
## Prepare DEMOGRAPHICS variables 
## ==================================

# racecat
racethn.labs <- c("NHW"="Non-Hispanic White", "NHB"="Non-Hispanic Black", "NHAsian"="Non-Hispanic Asian",
                  "Mexican-American"="Mexican American", "Other Hispanic"="Other Hispanic", 
                  "Other/Multi-Racial"="Other Race - Including Multi-Racial")
# education levels
educ.labs <- c("9-11th grade"="9-11th grade (includes 12th grade with no diploma)", 
               "High school graduate or GED"="High school graduate/ged or equivalent", 
               "High school graduate or GED"="High school grad/ged or equivalent",
               "Some college or AA degree"="Some college or aa degree")
educ_hh.labs <- c(educ.labs[-2], "High school graduate, GED or AA"="High school grad/ged or some college/aa degree")

# income levels
inc.vals <- c("$0-$4,999", "$5,000-$9,999", "$10,000-$14,999", "$15,000-$19,999", "Under $20,000", 
              "Over $20,000", "$20,000-$24,999", "$25,000-$34,999", "$35,000-$44,999", "$45,000-$54,999",
              "$55,000-$64,999", "$65,000-$74,999", "Over $75,000", "$75,000-$99,999", "Over $100,000", 
              "Don't know", "Refused", "Missing")

# working status levels
work.vals <- c("Working"="Working at a job or business", "Looking for work" = "Looking for work or", 
               "At a business, but not at work"="With a job or business but not at work",
               "Not working"="Not working at a job or business?", "Refused"="Refused", 
               "Don't know"="Don't know", "Missing"="", "Age <16 y"="Age <16 y") 


## Build nhanes_demo_processed --------------------------
nhanes_processed <- nhanes_data_raw %>% 
  # Age & sex --------------------
  mutate(
    age_gte16 = ifelse(age>=16,1,0),                                            
    female = ifelse(gender == "Female", 1, 0)) %>%
  # Race/ethnicity --------------------
  mutate(
    racecat = add_descr_labels(., "racethn1", racethn.labs, ordered = F),       
    racecat_addNHA = add_descr_labels(., "racethn_addNHA", racethn.labs, ordered = F)) %>% 
  mutate(race_white = case_when(racecat == "NHW"~1, is.na(racecat) ~ NA, TRUE ~ 0)) %>%
  # Education level --------------------
  mutate_at(c("educ", "educ_hhref"),  ~str_to_sentence(.)) %>%
  mutate_at("educ",  ~do.call(fct_recode, c(list(.), setNames(educ.labs, names(educ.labs))))) %>%
  mutate_at("educ_hhref",  ~do.call(fct_recode, c(list(.), setNames(educ_hh.labs, names(educ_hh.labs))))) %>%
  # Income level --------------------
  mutate_at(c("income_hh", "income_fam"), ~factor(gsub("er", "er ", gsub(" ", "", gsub(" to ", "-", .))))) %>% 
  mutate_at(c("income_hh", "income_fam"), ~factor(case_when(
    . == "$75,000andOver " ~ "Over $75,000",
    . == "$100,000andOver " ~ "Over $100,000",
    . == "Don'tknow" ~ "Don't know",
    is.na(.) == T ~ "Missing",
    TRUE ~ .), levels=inc.vals)) %>%
  # Working status & hours (if age ≥ 16 yrs) --------------------
  mutate(
    working = case_when(age<16~"Age <16 y", is.na(q_work_type)~"Missing",
                        TRUE ~ gsub(",", "", q_work_type)), 
    workinghrs.cat = case_when(
      age<16 ~ "Age <16 y", is.na(q_work_hrs) ~ "Missing",
      q_work_hrs==77777 ~ "Don't know", q_work_hrs==99999 ~ "Refused",
      TRUE ~ "Hours Listed"),
    workinghrs = case_when(
      age<16 | q_work_hrs %in% c(77777, 99999) | is.na(q_work_hrs) ~ NA, 
      TRUE ~ q_work_hrs),
    work_gt35hr = case_when(
      age<16 ~ "Age <16 y", is.na(q_work_gt35hr) ~ "Missing")) %>% 
  mutate_at("working", ~do.call(fct_recode, c(list(.), setNames(work.vals, names(work.vals))))) %>%
  select(base_vars, age, age_gte16, gender, female, racecat, racecat_addNHA, 
         educ, educ_hhref, income_hh, income_fam, inc_to_pov,
         working, workinghrs, work_gt35hr)


## ==================================
## Prepare EXAM variables 
## ==================================

nhanes_processed <- nhanes_processed %>% full_join(
  nhanes_data_raw %>% 
    select(SEQN, bmi, wt, ht, waist, hip, sbp_avg, dbp_avg), 
  by="SEQN")

## Write function to create "id_correct" vars for taste/smell tests
make_csexam_id_correct.fun <- function(data=., csexam_var, correct_value) { 
  nhanes_csexam_processed %>% 
    select(var = all_of(csexam_var)) %>%
    mutate(var_correct = case_when(var == correct_value ~ 1,
                                   var == "" ~ NA, TRUE ~ 0)) %>%
  rename_at("var_correct", ~gsub("var", csexam_var, .)) %>% 
    select(ends_with("correct"))
}

recode_csexam_correct_id.fun <- function(data, csexam_test_var, correct_value) {
  var_correct <- paste0(csexam_test_var, "_correct")
  data %>% mutate(
    !!var_correct := case_when(
      .data[[csexam_test_var]] == correct_value ~ 1,
      .data[[csexam_test_var]] == "" ~ NA, TRUE ~ 0
      ))
  }

nhanes_processed <- nhanes_processed %>% full_join(
  nhanes_data_raw %>%
    select(SEQN, starts_with("taste"), starts_with("smell")) %>%
    mutate_at("taste_status", ~case_when(. %in% c("", "Not done") ~ NA,
                                         TRUE ~ .)) %>%
    filter(!is.na(taste_status)) %>%
    # Binary correct=1/incorrect=0/missing variables for taste ID tests ----------
    recode_csexam_correct_id.fun(csexam_test_var = "taste_tongue_quinine_id", correct_value = "Bitter") %>%
    recode_csexam_correct_id.fun(csexam_test_var = "taste_mouth_quinine_id", correct_value = "Bitter") %>%
    recode_csexam_correct_id.fun(csexam_test_var = "taste_tongue_nacl_id", correct_value = "Salty") %>%
    recode_csexam_correct_id.fun(csexam_test_var = "taste_mouth_nacl_1M_id", correct_value = "Salty") %>%
    recode_csexam_correct_id.fun(csexam_test_var = "taste_mouth_nacl_320mM_id", correct_value = "Salty") %>%
    recode_csexam_correct_id.fun(csexam_test_var = "taste_mouth_nacl_1M_id_repeat", correct_value = "Salty") %>%
    recode_csexam_correct_id.fun(csexam_test_var = "taste_mouth_nacl_320mM_id_repeat", correct_value = "Salty") %>%
    # Binary correct=1/incorrect=0/missing variables for smell ID tests ----------
    recode_csexam_correct_id.fun(csexam_test_var = "smell_chocolate", correct_value = "Chocolate") %>%
    recode_csexam_correct_id.fun(csexam_test_var = "smell_strawberry", correct_value = "Strawberry") %>%
    recode_csexam_correct_id.fun(csexam_test_var = "smell_smoke", correct_value = "Smoke") %>%
    recode_csexam_correct_id.fun(csexam_test_var = "smell_leather", correct_value = "Leather") %>%
    recode_csexam_correct_id.fun(csexam_test_var = "smell_soap", correct_value = "Soap") %>%
    recode_csexam_correct_id.fun(csexam_test_var = "smell_grape", correct_value = "Grape") %>%
    recode_csexam_correct_id.fun(csexam_test_var = "smell_onion", correct_value = "Onion") %>%
    recode_csexam_correct_id.fun(csexam_test_var = "smell_gas", correct_value = "Gas"),
  by="SEQN")


## =============================================================
## Prepare LABORATORY variables (& vars for PREVENT equation) 
## =============================================================

## Equation to calculate EGFR using CKD-EPI 2025 calculator
# Ref: https://github.com/hayden-farquhar/NHANES-EXWAS/blob/main/00_functions.R
calc_eGFR_CKD_EPI.fun <- function(creatinine, age, female) {
  # creatinine in mg/dL, age in years, female = 1/0
  kappa <- ifelse(female==1, 0.7, 0.9)
  alpha <- ifelse(female==1, -0.241, -0.302)
  female_mult <- ifelse(female==1, 1.012, 1.0)
  
  scr_ratio <- creatinine / kappa
  eGFR <- 142 *
    pmin(scr_ratio, 1)^alpha * 
    pmax(scr_ratio, 1)^(-1.200) *
    0.9938^age * 
    female_mult
  
  return(eGFR)
}

nhanes_processed <- nhanes_processed %>% full_join(
  nhanes_data_raw %>% 
    select(SEQN, gender, age, starts_with("l_")) %>%
    mutate(female = ifelse(gender == "Female",1,0)) %>%
    ## Urinary albumin-to-creatinine ratio (mg/g) for PREVENT Equation
    ## UACR (mg/g) = l_u_albumin (ug/mL) / l_u_creatinine (mg/dL)
    mutate(uacr = (l_u_albumin / l_u_creatinine) *100) %>%
    ## eGFR, baed on CKD-EPI 2021 equation
    mutate(egfr_ckdepi = calc_eGFR_CKD_EPI.fun(creatinine = l_creatinine, age=age, female=female)) %>%
    select(-age, -female),
  by="SEQN")
  


# HOLD FOR FUTURE LABORATORY VARIABLES TO ADD ================
vars_to_get_lab <- c('SEQN',"LBXGH","LB2GLU","LB2SGL","LBXGLU", "LBXHGB","LBXHCT","LB2HCT","LBXMCVSI","LB2MCVSI","LBXMC","LB2MC","LBXRDW","LB2RDW","LBXWBCSI","LBXNEPCT",
                     "LB2NEPCT","LBXLYPCT","LB2LYPCT","LBXEOPCT","LB2EOPCT","LBXPLTSI","LB2PLTSI",
                     "LBXSNASI","LB2SNASI","LBXSKSI","LBXSCLSI","LB2SCLSI","LBXSC3SI","LB2SC3SI","LBXSBU","LB2SBU","LB2SCR",
                     "LBDSCR","LBXSCR","LBXSAL","LB2SAL","LBXSTP","LB2STP","LBXSASSI","LB2SASSI","LBXSATSI","LB2SATSI","LBXSTB","LB2STB","LBDSTB",
                     "LBXTC","LB2TC","LB2HDL","LB2LDL","LBDLDL","LBXTR","LB2TR","LB2STR","LBXSTR",
                     "SSECPT","SSMHHT","SSMONP","SSURHIBP","SSURMHBP","URXCNP","URXCOP","URXDAZ","URXDMA","URXECP","URXECPT","URXEQU",
                     "URXETD","URXETL","URXGNS","URXHIBP","URXMBP","URXMC1","URXMCOH","URXMCP","URXMEP","URXMHBP","URXMHH","URXMHHT",
                     "URXMHNC","URXMHP","URXMIB","URXMNM","URXMNP","URXMOH","URXMONP","URXMOP","URXMZP",
                     "URXP01","URXP02","URXP03","URXP04","URXP05","URXP06","URXP07","URXP09","URXP10","URXUCR",
                     "HRDHG","HRXHG","LB2THG","LBXTHG","LBDTHGSI","LBDBGESI","LBDBGMSI","LBDIHGSI","LBXIHG","LBXBGE","LBXBGM",
                     "URXUHG","LB2BCD","LBDBCDSI","LBXBCD","LB2BPB","LBXBPB","LBDBMNSI","LBDBSESI","LBXBSE","LBDSELSI","LBXSEL","LBXBMN",
                     "LB2FOL","LBDFOL","LBXFOL","LB2RBF","LBXRBF","LB2B12","LBDB12","LBXB12","LB2HCY","LBDHCY","LBXHCY","LB2MMA",
                     "LBXMMA","LBDVIASI","LBXVIA","LBDVIESI","LBXVIE","LB2COT","LBXCOT","LBDGTCSI","LBXGTC","LBDRPLSI","LBXRPL","LBDRSTSI","LBXRST","LBXEPP",
                     "LB2IN", "LBXCRP","LB2CRP","LBXHSCRP","LBDHRPLC","LBDBAP","LBXBAP", "LBXFB","LBDFBSI","LBXPT21","LBDHI","LBDAPBSI",
                     "LBDIRNSI","LBXIRN","LB2FER","LBXFER","LBDTIBSI","LBXTIB","LBXPCT")


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
bp_suffixes <- c(
  "pril", "sartan", "dipine", "thiazide", "lol", "osin",
  "Hydralazine", "Isosorbide", "Spironolactone", "Triamterene", "Eplerenone", 
  "Amiloride", "Verapamil", "Diltiazem", "Chlorthalidone", "Clonidine", 
  "Imdapamide") ; rx_bp <- rxq_drug[rowSums(sapply(bp_suffixes, function(rx_suff) {
  grepl(rx_suff, rxq_drug[["RXDDRUG"]], ignore.case = T) }))>0,] %>%
  filter(!grepl("TIMOLOL",RXDDRUG, ignore.case = T)) %>%
  # Remove non-BP medications
  filter(!grepl("ANTIARRHYTHMICS", RXDDCN1C, ignore.case = T)) %>%
  filter(!RXDDCN1C %in% c("NRTIS", "OPHTHALMIC GLAUCOMA AGENTS", "TOPICAL ANESTHETICS")) %>%
  select(RXDDRGID, RXDDRUG)
  

# Compile raw NHANES rx data across all exam cycles -- in LONG format 
nhanes_tables.rx.l <- nhanes_tables_all.l$quest$raw_data_tables.l[
  grepl("RXQ_RX", names(nhanes_tables_all.l$quest$raw_data_tables.l))]
raw_rx_data.l <- lapply(1:length(nhanes_tables.rx.l), function(i) nhanes_tables.rx.l[[i]]$raw_table)
names(raw_rx_data.l) <- names(nhanes_tables.rx.l)


# Create binary (yes/no) Medication use variables for statins and BP-lowering drugs
nhanes_rx_use <- raw_rx_data.l[[i]] %>% 
  group_by(SEQN) %>%
  summarize(
    rx_statin.f = if_else(any(RXDDRGID %in% rx_ids.l$statin$RXDDRGID, na.rm = TRUE), "Yes", "No"),
    rx_bp.f  = if_else(any(RXDDRGID %in% rx_ids.l$bpmed$RXDDRGID, na.rm = TRUE), "Yes", "No"),
    rx_statin_drugid = {
      matched <- unique(RXDDRUG[RXDDRGID %in% rx_ids.l$statin$RXDDRGID])
      if (length(matched) > 0) paste(matched, collapse = "; ") else NA_character_ },
    rx_bp_drugid = {
      matched <- unique(RXDDRUG[RXDDRGID %in% rx_ids.l$bpmed$RXDDRGID])
      if (length(matched) > 0) paste(matched, collapse = "; ") else NA_character_},
    .groups = "drop"
    )


## Health Insurance & Smoking Status ---------------

nhanes_processed <- nhanes_processed %>% left_join( 
  nhanes_data_raw %>%
    ## Health Insurance: covered by ANY health insurance, yes/no?
    mutate(
      healthinsurance = case_when(
        q_hiq %in% c("Don't know", "Refused", "") ~ NA,
        TRUE ~ q_hiq)
    ) %>% 
    ## Smoking: current smoker, yes/no
    # Step 1) clean smoking status variable, for smoking cigarettes now (SMQ020)
    mutate_at("q_smoke_status", ~case_when(
      . %in% c("Every day", "Every day,") ~ "Every day",
      . %in% c("Not at all", "Not at all?") ~ "Not at all",
      . %in% c("Some days", "Some days, or") ~ "Some days",
      TRUE ~ .)) %>%
    # Step 2) Define from smoke_history (smoked ≥100 cigarettes in your life?; if
    # (No --> smoke_status = N/A) & smoke_status (do you smoke cigarettes now?;
    # (Every day, Smoke days, Not at all)
    mutate(
      smoke_curr = case_when(
        q_smoke_history == "No" ~ 0, # Never-smoker (<100 cigarettes/lifetime)
        q_smoke_history == "Yes" & q_smoke_status == "Not at all" ~ 0, # Former-smoker (>100 cigarettes/life, but "Not at all" now)
        q_smoke_history == "Yes" & q_smoke_status %in% c("Some days", "Every day") ~ 1,
        TRUE ~ NA)) %>%
    ## Select raw & cleaned questionnaire variables 
    select(SEQN, healthinsurance, smoke_curr, starts_with("OCQ"), starts_with("CID"), 
           starts_with("ALQ"), starts_with("SMQ"), starts_with("DBQ"), starts_with("CDQ"),
           starts_with("MCQ"), starts_with("q_"), starts_with("RX"), starts_with("DIQ")),
  by="SEQN")
  
    

## ===========================================
## Prepare additional PREVENT variables 
## ===========================================

## Diabetes Status -----------------------
nhanes_processed <- nhanes_processed %>% 
  mutate(diabetes = case_when(
    DIQ010 == "Yes" | # Doctor told you, you have diabetes
      DIQ050 == "Yes" | # Taking insulin now
      DIQ070 == "Yes" | # Taking diabetic pills to lower blood sugar
      l_gh >= 6.5 | # HbA1c >= 6.5%
      l_glu >= 126 ~ 1, # Fasting glucose >= 126 mg/dL
    DIQ010 %in% c("No", "Borderline", "") &  
      (l_gh < 6.5 | is.na(l_gh)) & 
      (l_glu <126 | is.na(l_glu)) ~ 0,
    TRUE ~ NA)
    )

nhanes_processed %>% group_by(Years) %>% 
  reframe(diabetes_status = n_pct(diabetes, level = 1))
#  A tibble: 12 × 2
#  Years     diabetes_status
#  <chr>     <chr>          
# 1 1999-2000 641 (6.4)      
# 2 2001-2002 707 (6.4)      
# 3 2003-2004 724 (7.2)      
# 4 2005-2006 759 (7.3)      
# 5 2007-2008 1136 (11.2)    
# 6 2009-2010 1148 (10.9)    
# 7 2011-2012 1041 (10.7)    
# 8 2013-2014 1103 (10.8)    
# 9 2015-2016 1202 (12.1)    
# 10 2017-2018 1125 (12.2)    
# 11 2017-2020 1794 (11.5)    
# 12 2021-2023 1329 (11.1)    


save(nhanes_processed, file = "../data/processed/nhanes_processed.rda")

