## Exploratory analysis of taste-related data in NHANES

################################################################################
## Set Up: Load required packages & Compile NHANES table/variable index files
################################################################################

library(tidyverse) ; library(data.table) ; library(nhanesA)

## Load NHANES Manifest for Variables & Tables -------------------
manifest_nhanes_vars <- fread("./manifest_nhanes_vars.txt")
manifest_nhanes_tables <- fread("./manifest_nhanes_tables.txt")
rdc_manifest <- nhanesManifest(which = "limitedaccess")

## Search for taste/smell variables
manifest_nhanes_tables %>% 
  filter(grepl("taste", TableDesc, ignore.case = T))
  #        Component  Table     TableDesc BeginYear EndYear     Years
  #           <char> <char>        <char>     <int>   <int>    <char>
  # 1:   Examination  CSX_H Taste & Smell      2013    2014 2013-2014 
  # 2: Questionnaire  CSQ_G Taste & Smell      2011    2012 2011-2012
  # 3: Questionnaire  CSQ_H Taste & Smell      2013    2014 2013-2014


## Look for additional taste/smell exams (2012?) and genetic data
rdc_manifest %>% 
  filter(grepl("taste", Description, ignore.case = T))
  #     Table   Description                                              DocURL     Years Date.Published
  # 1 CSX_G_R Taste & Smell /Nchs/Data/Nhanes/Public/2012/DataFiles/CSX_G_R.htm 2012-2012     April 2014

chemos_datadict <- manifest_nhanes_vars %>% 
  filter(grepl("taste", TableDesc, ignore.case = T)) %>% 
  select(category = Component, Table, variable = VarName, description = VarDesc, 
         Varname = VarName) %>% 
  mutate(Weight = ifelse(category == "Examination", "WTMEC2YR", "WTINT2YR")) %>% 
  mutate_at("category", ~toupper(.))

################################################################################
## Download publicly available NHANES tables from Demographics (DEMO), 
## Examination (EXAM), Laboratory (LAB) & Questionnaire (Q) data
################################################################################

## Load data dictionary for OGTT variables
nhanes_datadict <- readxl::read_xlsx("./nhanes_pullrequest_IndVSArea_09012026.xlsx")

source("../scripts/functions/nhanes_survdesign_functions.r")
source("../scripts/functions/build_nhanes_tables.r")

## LOAD raw NHANES data (from InvXArea proj) -----------------------
nhanes_data_raw <- fread("../data/raw/nhanes_data_raw.csv") %>% 
  filter(Years %in% c("2011-2012", "2013-2014"))

#nhanes_raw.l <- build_nhanes_table.fun(
#  data_dictionary = nhanes_datadict, start_year = 2011, 
#  end_year = 2014, special_weights = "WTSAF")

#chemos_raw.l <- build_nhanes_table.fun(
#  data_dictionary = chemos_datadict, start_year = 2011, 
#  end_year = 2014)


################################################################################
## Process NHANES Phenotypes to prep Demographics, OGTT, Lab & Outcomes Data
################################################################################

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












