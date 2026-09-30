# Rscript to merge in ACS and SDI data from sjcromer GH repo


################################################################################
## Set Up & Load Required Packages 
################################################################################

## Load all prepared ACS_SES variables from sjcromer GH repo
acs_ses_dat.l <- get_github_files("sjcromer", "ACS_SES", path="", load_fn=fread, 
                                  file_pf="ACS_", file_ext="csv")

acs_ses_09 <- rbind.data.frame(acs_ses_dat.l$ACS_2009_01, acs_ses_dat.l$ACS_2009_02) %>% 
  rename_with(., ~gsub("estimate_", "estimate.", .)) %>% 
  rename_with(., ~gsub("moe_", "moe.", .))
acs_ses_19 <-rbind.data.frame(acs_ses_dat.l$ACS_2019_01, acs_ses_dat.l$ACS_2019_02)
acs_ses_14 <- rbind.data.frame(acs_ses_dat.l$ACS_2014_01, acs_ses_dat.l$ACS_2014_02)

acs_ses_merged <- rbind.data.frame(acs_ses_09, acs_ses_14, acs_ses_19) %>%  
  # Clean up urban/rural variables to replace "" with missing
  mutate_at(c("urbrurbin", "urbrurcat"), ~ifelse(. == "", NA, .)) %>% 
  select(
    V1, contains(c("urbrur", "ltcoll", "lths", "unemp", "MHI", "fpl200", "sdi")),
    "svi", "adi", "year"
    ) %>% 
  rename(
    acs_urbrur = urbrurbin, acs_urbrur_cat = urbrurcat,
    acs_educ_ltcoll_pct = estimate.pct_ltcoll, acs_educ_lths_pct = estimate.pct_lths,
    acs_unemp_pct = estimate.pct_unemp, acs_incpov_mhi = estimate.MHI,
    acs_incpov_fpl200_pct = estimate.pct_fpl200, 
    Year = year) %>% 
  select(V1, Year, sdi, svi, adi, starts_with("acs_"), sdi_zip, sdizip_preventcat) %>% 
  # Set levels for categorical exposures as highest SES level
  mutate_at("acs_urbrur", ~factor(., levels = c("urban", "rural"))) %>% 
  mutate_at("acs_urbrur_cat", ~ factor(., levels = c(
    "metropolitan", "micropolitan", "town", "rural")))

acs_ses_merged %>% fwrite(., "../data/processed/acs_ses_to_merge.csv")

rm(acs_ses_09) ; rm(acs_ses_14) ; rm(acs_ses_19)
rm(acs_ses_dat.l)

## EOF
# Last Updated: 09-30-2026


  