# Rscript to merge in ACS and SDI data from sjcromer GH repo


################################################################################
## Set Up & Load Required Packages 
################################################################################


## load pre-built pantry functions (for data wrangling)
get_github_files <-function(user, repo, path="", files, ext = c("txt", "csv")) {
  api <- sprintf("https://api.github.com/repos/%s/%s/contents/%s", user, repo, path)
  toload <- grep(paste0("\\.",ext,"$"), jsonlite::fromJSON(api)$name, value = TRUE, ignore.case = TRUE) 
  if(is.null(files)) {
    URLs <- lapply(toload, function(f) {
      sprintf("https://raw.githubusercontent.com/%s/%s/main/%s/%s", user, repo, path, f)
    }) } else {
      toload <- toload[grepl(files, toload)]
      URLs <- sprintf("https://raw.githubusercontent.com/%s/%s/main/%s/%s", user, repo, path, toload)
    } ; github_files.l <- invisible(lapply(URLs, fread))
  names(github_files.l) <- gsub(paste0("[.]", ext), "", toload)
  return(github_files.l)
} 

## Load all prepared ACS_SES variables from sjcromer GH repo
acs_ses_dat.l <- get_github_files(user="sjcromer", repo="ACS_SES", path="", files="ACS_", ext="csv")
acs_ses_09 <- rbind.data.frame(acs_ses_dat.l$ACS_2009_01, acs_ses_dat.l$ACS_2009_02) %>% 
  rename_with(., ~gsub("estimate_", "estimate.", .)) %>% 
  rename_with(., ~gsub("moe_", "moe.", .))
acs_ses_19 <-rbind.data.frame(acs_ses_dat.l$ACS_2019_01, acs_ses_dat.l$ACS_2019_02)
acs_ses_14 <- rbind.data.frame(acs_ses_dat.l$ACS_2014_01, acs_ses_dat.l$ACS_2014_02)
acs_ses_merged <- rbind.data.frame(acs_ses_09, acs_ses_14, acs_ses_19)


# Rename ACS measures to merge into nhanes_dat
acs_ses_use <- acs_ses_merged %>% 
  select(V1, contains(c("urbrur", "ltcoll", "lths", "unemp", "MHI", "fpl200", "sdi")),
         "svi", "adi", "year")

acs_ses_use <- acs_ses_use %>%          
  rename(
    acs_urbrur.bin = urbrurbin, acs_urbrur.cat = urbrurcat,
    acs_educ_ltcoll_pct = estimate.pct_ltcoll, acs_educ_lths_pct = estimate.pct_lths,
    acs_unemp_pct = estimate.pct_unemp, acs_incpov_mhi = estimate.MHI,
    acs_incpov_fpl200_pct = estimate.pct_fpl200, 
    Year = year) %>% 
  select(V1, Year, sdi, svi, adi, starts_with("acs_"), sdi_zip, sdizip_preventcat)

acs_ses_use %>% fwrite("../data/processed/acs_ses_to_merge.csv")

## EOF
# Last Updated: 09-28-2026


  