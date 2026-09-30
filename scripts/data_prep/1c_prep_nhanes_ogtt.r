## Rscript for compiling OGTT Data from NHANES 

## Variables required:
# Anthropometrics: sex, bmi, waist, height
# Fasting bloods: insulin, glucose, triglycerides, hdl
# OGTT: full 5 timepoints would be amazing (0/30/60/90/120min), but if not at least insulin 0/30min and glucose 0/30min.

## Sample inclusions
# -	Adults (>= 18 years)
# -	Complete data for at least fasting glucose, insulin, triglycerides, HDL 
# -	Include those with normoglycemia (FPG <95 mg/dL or 2hr OGTT glucose < 140 mg/dL), prediabetes (FPG 95-125 mg/dL & 2hr OGTT glucose 140-199 mg/dL ), and type 2 diabetes (no other meds but metformin)

## Outcomes: 
# - Anything CVD-related; MACE or anything caardiovascular, 
# - Diabetes

# Fasting: add in incadtor
# Fasting data alone

## Add in the definitoins for the outcomes
# diabetes
# MACE - definiiont from Zanetta

## Spearman correlations; 


## Units
# - 2h OGTT glucose (mg/dL) ; Also available in mmol/L
# Fasting glucose (mg/dL); also available in mmol/L
# Fasting plasma insulin (uU/mL)
# Fasting plasma triglyceride (mg/dL)
# [Fasting] plamsa HDL (mg/dL)


################################################################################
## Download publicly available NHANES tables from Demographics (DEMO), 
## Examination (EXAM), Laboratory (LAB) & Questionnaire (Q) data
################################################################################

## Load data dictionary for OGTT variables
ogtt_datadict <- readxl::read_xlsx("./nhanes_pullrequest_ogtt_09172026.xlsx") 

## Download all variables for OGTT/FG analyses -----------------------
nhanes_raw.l <- build_nhanes_table(
  data_dictionary = ogtt_datadict, start_year = 2007, 
  end_year = 2016, special_weights = "WTSAF|WTSOG")


## ============================================================
## Apply preliminary restrictions for data availability & age 
## ============================================================

## Subset to participants with EXAM data & >= 18y
nhanes_raw <- nhanes_raw.l$full_data_table %>% 
  filter(grepl("Both", RIDSTATR)) #%>% #N=48710
  #filter(age >= 18)  # N=19629 
  

################################################################################
## Process NHANES Phenotypes to prep Demographics, OGTT, Lab & Outcomes Data
################################################################################

base_vars <- names(nhanes_raw %>% select(
  "Years", "SEQN", "RIDSTATR", starts_with("SD"), starts_with("WT"), -"wt")
  )
 
## ===============================================
## Clean basic demographics & lab vars
## ===============================================

## Process phenotypes
nhanes_raw <- nhanes_raw %>%
  
  # Convert gender (Female/Male) to female (1/0) -----------------
  mutate(female = as.factor(ifelse(gender == "Female", 1, 0))) %>% 
  
  # Calculate mean SBP/DBP, by hand ------------------------------
  mutate(
    sbp_mean = rowMeans(pick(sbp1, sbp2, sbp3), na.rm = T),
    dbp_mean = rowMeans(pick(dbp1, dbp2, dbp3), na.rm = T)) %>% 
  mutate_at(c("sbp_mean", "dbp_mean"), ~ifelse(is.na(.), NA, .)) %>%
  
  ## Calculate ldl using Friedwald equation -------------
  mutate(ldl_friedewald = ifelse(ftg < 400, tc - hdl - (ftg/5), NA)) %>% 
  
  ## Process fasting blood measures (NA if fasting < 8hr) ----------
  mutate_at(c("fg", "fi", "hdl"), ~case_when(
    fasting_hr <9 ~ NA,
    TRUE ~ .)
  ) %>% 
  
  ## Process OGTT data 
  mutate(
    ogtt_2hg_cleaned = case_when(
      ogtt_drank_gtx == "All" & 
        grepl("complete", ogtt_complete_code, ignore.case = T) ~ ogtt_2hg,
      TRUE ~ NA)
  )


## =========================================================
## Prepare diabetes & CVD-related outcomes measures 
## =========================================================

# -------------------------
## Medication use    
# -------------------------

#rxq_url <- "https://wwwn.cdc.gov/Nchs/Data/Nhanes/Public/1988/DataFiles/RXQ_DRUG.xpt"
#rxq_drug <- nhanesA::nhanesFromURL(rxq_url)

## 1) Create list of drug codes for each medication type ----------------
rx_statin <- rxq_drug %>% 
  filter(endsWith(RXDDRUG, "STATIN") & RXDDCN1C == "HMG-COA REDUCTASE INHIBITORS (STATINS)") %>%
  select(RXDDRGID, RXDDRUG) %>% 
  mutate(RXUSE="statin")

rx_bpx <- c("\\b.*pril\\b", "\\b.*sartan\\b", "\\b.*dipine\\b", "\\b.*thiazide\\b", 
            "\\b.*lol\\b", "\\b.*osin\\b", "Hydralazine", "Isosorbide", 
            "Spironolactone", "Triamterene", "Eplerenone", "Amiloride", 
            "Verapamil", "Diltiazem", "Chlorthalidone", "Clonidine", 
            "Indapamide") ; rx_bp <- rxq_drug %>%
  filter(grepl(paste0(rx_bpx, collapse = "|"), RXDDRUG, ignore.case = TRUE)) %>%
  filter(!grepl("ANTIARRHYTHMICS|NRTIS|OPHTHALMIC GLAUCOMA AGENTS|TOPICAL ANESTHETICS", 
                RXDDCN1C, ignore.case = T)) %>%
  filter(!grepl("TIMOLOL", RXDDRUG, ignore.case = T)) %>% 
  select(RXDDRGID, RXDDRUG) %>% 
  mutate(RXUSE = "bp")

rx_diabx <- c("insulin", "metformin", "glipizide", "glyburide", "glimepiride", 
              "gliclazide", "tolbutamide", "chlorpropamide", "tolazamide", 
              "acetohexamide", "glinide", "glitazone", "gliptin") ; rx_diab <- 
  rxq_drug %>%
  filter(grepl(paste(paste0(rx_diabx, collapse = "|"), collapse = "|"), RXDDRUG, 
               ignore.case = TRUE)) %>%
  select(RXDDRGID, RXDDRUG) %>% 
  mutate(RXUSE = "diab")

# Merge all rxtypes into df
rx_alltypes <- rbind.data.frame(rx_statin, rx_bp, rx_diab) %>% 
  distinct() ; rx_alluse <- nhanes_raw %>% 
  select(SEQN, starts_with("q_rx_drugid_")) %>% 
  pivot_longer(cols=starts_with("q_rx_drugid_"), names_to="drug_id", 
               values_to="RXDDRGID", values_drop_na = TRUE) %>%
  inner_join(rx_alltypes, by = "RXDDRGID", relationship = "many-to-many") %>% 
  select(SEQN, RXDDRGID, RXDDRUG, RXUSE) %>% distinct() %>% 
  group_by(SEQN, RXUSE) %>% 
  summarise(
    rx_use = "Yes", rx_id = paste0(unique(RXDDRGID), collapse="; "),
    rx_drug = paste0(unique(RXDDRUG), collapse="; "), .groups = "drop") %>%
  pivot_wider(id_cols = "SEQN", names_from="RXUSE", names_glue="{.value}_{RXUSE}",
              values_from = c(rx_use, rx_id, rx_drug)
              ) ; nhanes_raw <- nhanes_raw %>% 
  rename(rx_use_any = q_rx_use_1) %>%
  select(-starts_with("q_rx")) %>%
  left_join(rx_alluse, by = "SEQN") %>%
  mutate(across(starts_with(c("q_med", "rx_")), ~recode_nhanes_na.fun(.))) %>%
  mutate(across(starts_with("rx_use_"), ~case_when(
    . == "Yes" ~ "1", rx_use_any == "No" ~ "0", 
    is.na(.) ~ "0", TRUE ~ .)))

# ---------------
# Outcomes 
# ----------------

nhanes_raw <- nhanes_raw %>% 
  mutate(
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
      q_told_chf == "Yes" ~ 1, q_told_chf == "No" ~ 0,
      TRUE ~ NA),
    
    # Hypertension --------------
    htn = case_when(
      `q_told_htn_x2+` == "Yes" | sbp_mean >=130 | dbp_mean >=80 ~ 1, 
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
        `q_told_htn_x2+` != "Yes" & 
        rx_use_bp != "Yes" ~ 1,
      TRUE ~ 0))


# =====================================================
## Add flag for inclusion criteria
# =====================================================

nhanes_raw <- nhanes_raw %>% mutate(
    include_ng = case_when(
      fg < 95 | ogtt_2hg_cleaned < 140 ~ 1,
      TRUE ~ 0),
    include_predm = case_when(
      fg >=95 & fg <125 | ogtt_2hg_cleaned >= 140 & ogtt_2hg_cleaned < 200 ~ 1,
      TRUE ~ 0),
    include_dm = case_when(
      is.na(diabetes) | diabetes == 0 ~ 1,
      diabetes == 1 & rx_drug_diab == "METFORMIN" ~ 1,
      TRUE ~ 0)) %>% 
  mutate(
    include = case_when(
      include_ng == 1 | include_predm == 1 | include_dm == 1 ~ "Include",
      TRUE ~ "Exclude")
  )

# =====================================================
## Add nYRS to calculate sample weights
# =====================================================

nhanes_raw <- nhanes_raw %>% 
  mutate(
    WTMEC.COMBN = WTMEC2YR,
    WTSAF.COMBN = WTSAF2YR, 
    WTYRS = case_when(
      Years %in% c("1999-2000", "2001-2002") ~ 4,
      Years == "2017-2020" ~ 3.2, 
      TRUE ~ 2),
    nYRS = ifelse(Years == "2017-2020", 3.2, 2)
  )


################################################################################
## Preliminary descriptive tables 
################################################################################

source("../scripts/functions/nhanes_survdesign_functions.r")

vars_to_summarise <- c(
  age="Age, years", 
  female="Sex, female",
  bmi="BMI, kg/m2",
  waist="Waist circumference, cm",
  sbp_mean = "SBP, mmHg",
  dbp_mean = "DBP, mmHg",
  fg="Fasting Glucose (mg/dL)",
  fi="Fasting Insulin (uU/mL",
  ftg="Fasting Triglyceride (mg/dL)",
  hdl="Fasting HDL (mg/dL)",
  ogtt_2hg_cleaned = "Two Hour OGTT Glucose (md/dL)",
  diabetes = "Type 2 diabetes (diagnosed)",
  diabetes_undx = "Type 2 diabetes (undiagnosed)",
  htn="Hypertension (diagnosed)",
  htn_undx = "Hypertension (undiagnosed)",
  cvd="CVD", ascvd="ASCVD", chf="CHF"
)

build_nhanes_summarytable.fun(
  vars_to_summarise = vars_to_summarise, strata = "include", 
  data = nhanes_raw
  )













          