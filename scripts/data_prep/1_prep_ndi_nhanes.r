## Rscript to prepare NDI data

## Primary code for cleaning NDI data are adapted from the R_ReadInProgramAllSurveys.R
## file provided in the CDC FTP containing the NDI data:

# *****************************************************************************************
# May 2022
# 
# ** PUBLIC-USE LINKED MORTALITY FOLLOW-UP THROUGH DECEMBER 31, 2019 **
#
# The following R code can be used to read the fixed-width format ASCII public-use Linked
# Mortality Files (LMFs) from a stored location into a R data frame.  Basic frequencies
# are also produced.  
# 
# NOTE:   With the exception of linkage eligibility-status (ELIGSTAT), the other discrete
#         variables (e.g., MORTSTAT) are designated as integers. We provide the definitions
#         of the variable values in the comments but leave it up to the user to decide 
#         whether integer or factor variables is/are preferred for their analyses.  
#
# NOTE:   As some variables are survey specific, we have created two versions of the program: 
#         one for NHIS and another for NHANES.
# 
# *****************************************************************************************   


################################################################################
## Download and merge the NDI (National Dealth Index) Data 
################################################################################
# Link: https://ftp.cdc.gov/pub/Health_Statistics/NCHS/datalinkage/linked_mortality/

library(haven)

# List NHANES cycles with NDI data (1999-2000 through 2017-2018)
nhanes_ndi_cycles <- sapply(0:9, function(i) sprintf("%s-%s", 1999+(i*2), 2000+(i*2)))

nhanes_ndi_filenames <- sapply(nhanes_ndi_cycles, function(cycle) {
  sprintf("NHANES_%s_MORT_2019_PUBLIC.dat", gsub("[-]", "_", cycle)) })


# 1. Define base CDC FTP URL for mortality linked files & filenames
ndi_url <- "https://ftp.cdc.gov/pub/Health_Statistics/NCHS/datalinkage/linked_mortality/"


## ==============================================================
## Build function to download and prepare NDI_NHANES files
## ==============================================================
#dir.create("../data/raw/ndi_nhanes")

build_ndi_nhanes_data.fun <- function(base_url = ndi_url, file_name) {
  
  # Set ndi_nhanes file name and path
  nhanes_cycle <- names(nhanes_ndi_filenames)[which(nhanes_ndi_filenames == file_name)]
  file_path <- paste0("../data/raw/ndi_nhanes/", file_name)
  
  # Download file from remote server using wget --> save to ../data/raw/ folder
  download.file(url = paste0(base_url, file_name), destfile = file_path, method = "wget")
  
  # read in the fixed-width format ASCII file
  nhanes_ndi_dat <- read_fwf(
    file=file_path,
    col_types = "iiiiiiii",
    fwf_cols(SEQN = c(1,6),
             ndi_eligstat = c(15,15),
             ndi_mortstat = c(16,16),
             ndi_ucod_leading = c(17,19),
             ndi_diabetes = c(20,20),
             ndi_htn = c(21,21),
             ndi_permth_int = c(43,45),
             ndi_permth_exm = c(46,48)),
    na = c("", ".")) %>% 
    # Add in cycle & descriptive variables 
    mutate(
      Years = nhanes_cycle,
      ndi_eligstat.lab = case_when(
        ndi_eligstat == 1 ~ "Eligible", 
        ndi_eligstat == 2 ~ "Under 18 (not available)", 
        ndi_eligstat == 3 ~ "Ineligible", 
        TRUE = NA_character_),
      ndi_mortstat.lab = case_when(
        ndi_mortstat == 0 ~ "Assumed alive", 
        ndi_mortstat == 1 ~ "Assumed deceased",
        TRUE = NA_character_), # NA = ineligible or under age 18
      ndi_ucod_leading.lab = case_match(
        ndi_ucod_leading == 1 ~ "Heart diseases (I00-I09, I11, I13, I20-I51)",
        ndi_ucod_leading == 2 ~ "Malignant neoplasms (C00-C97)",
        ndi_ucod_leading == 3 ~ "Chronic lower respiratory diseases (J40-J47)",
        ndi_ucod_leading == 4 ~ "Accidents (unintentional injuries) (V01-X59, Y85-Y86)",
        ndi_ucod_leading == 5 ~ "Cerebrovascular diseases (I60-I69)",
        ndi_ucod_leading == 6 ~ "Alzheimers disease (G30)",
        ndi_ucod_leading == 7 ~ "Diabetes mellitus (E10-E14)", 
        ndi_ucod_leading == 8 ~ "Influenza and pneumonia (J09-J18)",
        ndi_ucod_leading == 9 ~ "Nephritis, nephrotic syndrome and nephrosis (N00-N07, N17-N19, N25-N27)",
        ndi_ucod_leading == 10 ~ "All other causes (residual)",
        TRUE = NA_character_), # NA = Ineligible, <18yr, assumed alive, or COD data available
      ndi_diabetes.lab = case_match(
        ndi_diabetes == 0 ~ "No", # Condition not listed as a multiple cause of death
        ndi_diabetes == 1 ~ "Yes", # Condition listed as a multiple cause of death
        TRUE = NA_character_), #Assumed alive, <18yr, ineligible for mortality follow-up, or MCOD not available
      ndi_htn.lab = case_match(
        ndi_htn == 0 ~ "No", # Condition not listed as a multiple cause of death
        ndi_htn == 1 ~ "Yes", # Condition listed as a multiple cause of death
        TRUE = NA_character_) #Assumed alive, <18yr, ineligible for mortality follow-up, or MCOD not available
    ) 
  
  return(nhanes_ndi_dat)
  
}

## =================================================================
## Run function over all ndi_nhanes files and build single df 
## =================================================================

nhanes_ndi_raw <- lapply(
  nhanes_ndi_filenames, function(x) build_ndi_nhanes_data.fun(file_name=x)) %>% 
  do.call(rbind.data.frame, .)

## Save csv for merging -------------------
nhanes_ndi_raw %>% fwrite("../data/raw/ndi_nhanes/nhanes_ndi_raw.csv")


## EOF
# Last Updated: 08-27-2026

