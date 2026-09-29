# Run descriptive analyses

################################################################################
## Set Up; Load Required Packages & Data Files
################################################################################

## set local directory ------------------------
setwd('~/Documents/GitHub/nhanes_area_ses/run') 

## load required base pacakges
list_of_packages <- c(
  "tidyverse", "data.table", "nhanesA", "progress", "sociome", "jsonlite", "haven", 
  "forcats", "parallel") ; invisible(lapply(list_of_packages, function(pkg) {
    if(!requireNamespace(pkg, quietly = TRUE)) { 
      install.packages(pkg) } ; library(pkg, character.only = TRUE)
  }))


## load pre-built pantry functions (for data wrangling)
get_github_scripts <-function(user, repo, path, file=NULL) {
  api <- sprintf("https://api.github.com/repos/%s/%s/contents/%s", user, repo, path)
  scripts <- grep("*.R", jsonlite::fromJSON(api)$name, value = T, ignore.case = T)
  if(is.null(file)) {
    URLs <- lapply(scripts, function(f) sprintf("https://raw.githubusercontent.com/%s/%s/main/%s/%s", user, repo, path, f))
  } else {
    URLs <- sprintf("https://raw.githubusercontent.com/%s/%s/main/%s/%s", user, repo, path, file)
  } ; invisible(lapply(URLs, source))
} ; get_github_scripts("julieeg", "pantry", "functions")


## Load nhanes_area_ses scripts -------------------
get_github_scripts("julieeg", "nhanes_area_ses", "scripts/analysis/build_nhanes_survdesign.r")
source("../scripts/functions/nhanes_survdesign_functions.r")


## Load nhanes data & prevent variables ------------------
nhanes_dat <- readRDS("../data/processed/nhanes_postprocessed_linked_ndi_prvnt_sdi.rds")

path_to_output <- "../data/output/"


################################################################################
## Define lists of exposures, outcomes, and strata
################################################################################

# ===================
## SES Exposures
# ===================

ses_indiv_exposures <- c(educ="educ_level", incpov="inc_to_pov", incpovlvl="inc_to_pov_level", 
                         employ="employ_status", foodinsecure = "foodinsecure")

ses_area_exposures <- c(urbrur = "acs_urbrur.bin", urbrurcat = "acs_urbrur.cat",
                        pctlths="acs_educ_lths_pct", pctltcoll="acs_educ_ltcoll_pct",
                        pctunemp = "acs_unemp_pct", mhi="acs_incpov_mhi", 
                        pctfpl200="acs_incpov_fpl200_pct", sdi="sdi", svi="svi", adi="adi")

ses_exposures <- c(ses_indiv_exposures, ses_area_exposures)


# ==========================
## Stratifying variables
# ==========================

strata_vars <- c(sex="gender", agecat="age_4lvl", racethn="racethn_combn")


# ===================
## All outcomes 
# ===================

all_outcomes.l <- list(
  
  behav = c( ## Behavioral traits --------------------------------
             "smoke_current", "smoke_ever", "alch_drink_curr", "alch_drink_daily",
             "alch_drink_weekly", "pa_total_mets_wk", "pa_meets_guidelines", 
             "hei2015_total", "ahei_total", "dii_total", "dietsuppl_any",
             "dietsuppl_num", "addsalt_prep_often", "addsalt_table_often",
             "restaur_freq_gt2", "govtmeal_any", "foodinsecure", 
             "genhealth_low_vs_other", "hc_drvisit", "hc_hospadmit", 
             "insur_any", "uninsur_lastyr"),
  
  biomark = c( ## Annthrop/BP, clinical labs, metals/toxins, ----------------------
               "bmi", "waist", "whr", "wt", "sbp_mean", "dbp_mean",
               "fg", "fi", "tg", "tc", "hdl", "ldl_friedewald", "glu", "tgglu",
               "ogtt_2hg", "hba1c", "uacr", "egfr", "crp", "creatinine",
               "vitd", "b_carot","g_tocoph", "t_lycop", "manganese", "selenium",
               "mercury", "lead", "cadmium", "hcb", "hcb_adj", "hepox", "pcb180", 
               "pcbdiox", "u_bpa", "testosterone", "estradiol", "hpv_oral"),
  
  disease = c( ## Disease outcomes ---------------------------
               "diabetes", "diabetes_undx", "htn", "htn_undx", "htn_stg1", 
               "obese", "obese_abd", "cvd", "ascvd", "chf", "mdd",
               "ckd", "ckd_gfr_gte3", "ckd_any", "copd", "copd_pft", "pft_lt07", 
               "nfs", "nfs_mod_vs_low", "nfs_high_vs_low",
               "liver_cap_any", "liver_cap_mod_sev"),
  
  riskpred = c( ## Disease risk predictions: FIB4 & PREVENT est  ----------
                "fib4", "fib4_mod_vs_low", "fib4_high_vs_low", 
                paste0(rep("prevent_",40), 
                       rep(c("cvd", "ascvd", "hf", "chd", "stroke"), each=8), 
                       rep(c("_10yr", "_30yr"), each=4), 
                       c("_base", "_hba1c", "_uacr", "_full"))),
  
  chemosen = c( # Taste/Smell outcomes ----------------------
                "taste_mouth_quinine_glms", "taste_mouth_nacl_1M_glms", 
                "taste_mouth_nacl_320mM_glms", "smell_pst_total", "smell_dysfun_any",
                "smell_dysfun_severe", 
                paste0("tastechange_", c("sweet", "salt", "bitter", "sour"), "_worse"))
)

# Make separate list of NDI outcomes
ndi_outcomes <- list(
  ndi = c(
    "ndi_mortstat", "ndi_mortstat_hd", "ndi_mortstat_cbvd", "ndi_mortstat_cvd", 
    "ndi_mortstat_diab", "ndi_mortstat_diab_any" 
  )
)


################################################################################
## Descriptive summaries by Age, Sex and Race/Ethnicity 
################################################################################

#dir.create(paste0(path_to_output, "descr"))

# =====================================================================
## Descriptive Table 1s, by EXPOSURES & STRATA for ALL variables
# =====================================================================

## Apply over all EXPOSURES & STRATA, on all OUTCOMES
addn_tab1_vars <- c("smoke_status", "alch_freq_wk", "pa_level_mets", "genhealth",
                    "income_hh", "income_fam", "uacr_level", "ckd_gfr_level", 
                    "fib4_cat", "nfs_cat")

strata_descr <- c(cycle="Years", strata_vars, ses_exposures)
vars_tab1.l <- c(years="Years", list(strata = unname(strata_vars)), 
                 list(ses = unname(ses_exposures)), c(all_outcomes.l, ndi_outcomes))

lapply(seq_along(strata_tab1), function(x) {
  mclapply(vars_tab1.l, function(vars) {
    build_nhanes_summarytable.fun(vars, strata=strata_tab1[[x]], digits = 3) 
  }) %>% do.call(rbind.data.frame, .) %>% 
    fwrite(paste0(path_to_output, "/descr/tab_descr_allvars_by", names(strata_tab1[x]), ".csv"))
}) 


################################################################################
## Distributions of indiv and area-level SES measures by each STRATA
################################################################################

strata_descr <- c(strata_vars, ses_exposures)

## Quantiles of continuous SES exposures x strata

lapply(seq_along(strata_descr), function(z) {
  mclapply(seq_along(ses_exposures), function(x) {
    if(is.numeric(nhanes_dat %>% pull(ses_exposures[[x]]))) {
      get_nhanes_quantiles.fun(ses_exposures[[x]], strata = strata_descr[[z]], 
                               data = nhanes_dat, probs=c(0,0.05,0.25,0.50,0.75,0.95,1)) 
    } else { get_nhanes_distrib.fun(ses_exposures[[x]], strata = strata_descr[[z]], 
                                    data = nhanes_dat) }
  }) %>% do.call(rbind.data.frame, .) %>% 
    fwrite(paste0("../data/output/descr/tab_descr_sesdistrib_by", names(strata_descr[z]), ".csv"))
})

## Additional intersectionality

nhanes_dat <- nhanes_dat %>%
  mutate(
    intersect_raceXeduc = case_when(
      !is.na(racethn_combn) & !is.na(educ_level) ~ paste0(racethn_combn, "_x_", educ_level),
      TRUE ~ NA),
    intersect_raceXincpov = case_when(
      !is.na(racethn_combn) & !is.na(inc_to_pov_level) ~ paste0(racethn_combn, "_x_", inc_to_pov_level),
      TRUE ~ NA),
    intersect_sexXeduc = case_when(
      !is.na(gender) & !is.na(educ_level) ~ paste0(gender, "_x_", educ_level),
      TRUE ~ NA),
    intersect_sexXincpov = case_when(
      !is.na(gender) & !is.na(inc_to_pov_level) ~ paste0(gender, "_x_", inc_to_pov_level),
      TRUE ~ NA),
    intersect_agecatXeduc = case_when(
      !is.na(age_4lvl) & !is.na(educ_level) ~ paste0(age_4lvl, "_x_", educ_level),
      TRUE ~ NA),
    intersect_agecatXincpov = case_when(
      !is.na(age_4lvl) & !is.na(inc_to_pov_level) ~ paste0(age_4lvl, "_x_", inc_to_pov_level),
      TRUE ~ NA),
    #intersect_urbruXeduc = case_when(
    #  !is.na(age_4lvl) & !is.na(urbrur.bin) ~ paste0(urbrur.bin, "-", educ_level),
    #  TRUE ~ NA),
    #intersect_urbruXincpovlvl = case_when(
    #  !is.na(urbrur.bin) & !is.na(inc_to_pov_level) ~ paste0(urbrur.bin, "-", inc_to_pov_level),
    #  TRUE ~ NA),
  )

intersect_vars <- c(raceXeduc="intersect_raceXeduc", raceXincpov="intersect_raceXincpov",
                    sexXeduc="intersect_sexXeduc", sexXincpov="intersect_sexXincpov",
                    agecatXeduc="intersect_agecatXeduc", agecatXincpov="intersect_agecatXincpov",
                    urbrurXeduc="intersect_urbrurXeduc", urbrurXincpov="intersect_urbrurXincpov")

# --> All continuous area-level ses meausres 

## Check for intersections by Education level
vars_intersect_educ <- c(intersect_vars[endsWith(names(intersect_vars), "educ")])
vars_intersect_educ <- vars_intersect_educ[-4]

lapply(seq_along(vars_intersect_educ), function(z) {
  intersect <- get_nhanes_quantiles.fun("inc_to_pov", strata = vars_intersect_educ[[z]], data = nhanes_dat)
  intersect %>% 
    select(-contains(c("total", "teststat")), -"p_value") %>% 
    pivot_longer(cols = -c(Variable, Level),
                 names_sep="_x_", names_to=c("Var1_level", "Var2_level")) %>%
    mutate(stat = ifelse(startsWith(Var1_level, "mean_"), "mean", "se")) %>% 
    mutate_at("Var1_level", ~gsub("mean_", "", gsub("se_","", .))) %>% 
    pivot_wider(names_from="stat") %>% 
    mutate(Intersect = names(vars_intersect_educ)[z], .before=1) %>% 
    mutate(teststat = intersect$teststat_df[1], teststat_val = intersect$teststat_val[1],
           P_value = intersect$p_value[1]) %>% 
    fwrite(paste0("../data/output/descr/tab_descr_distrib_incpov_by",names(vars_intersect_educ)[z],".csv"))
})


################################################################################
## Pearson correlations of indiv and area-level SES measures, overall by STRATA
################################################################################

## Add integer versions of categorical SES exposures
nhanes_dat <- nhanes_dat %>% 
  mutate(
    educ_level.int = as.integer(educ_level),
    inc_to_pov_level.int = as.integer(inc_to_pov_level),
    employ_status.int = as.integer(employ_status))

ses_exposures.corr <- c("educ_level.int", "inc_to_pov_level.int", "inc_to_pov", 
                        "employ_status.int")

## Correlations in total sample ---------------
cor_total.l <- get_nhanes_cormat.df(vars = ses_exposures.corr)
full_join(cor_total.l$corr, cor_total.l$pvals, by = c("strata", "variable")) %>% 
  fwrite("../data/output/descr/tab_descr_corr_ses_bytotal.csv")


## Correlations, by strata ---------------
mclapply(seq_along(strata_descr), function(z) {
  corres <- get_nhanes_cormat.dßf(vars = ses_exposures.corr, strata=strata_descr[[z]]) 
  full_join(corres$corr, corres$pvals, by = c("strata", "level", "variable")) %>% 
    fwrite(paste0("../data/output/descr/tab_descr_corr_ses_by", names(strata_descr)[z], ".csv"))
})



## EOF



## FINAL STEPS;

# * list of pa ckages requied
# * update the saving directory (SC to access scripts on github?)
# * separate the scripts for ind vs area level models
#

## Water fall plot for prevent equation outcomes; each IND is an up or down on waterfal, based on how their prediction changes hen you add SDI
#and color code by race

# HOLD ON THIS: but think about how to consolidate the risk estimates when adding SDI information
# calcualte delta in prevent equation, base to sdi; base to full; did they meet a threshod in each strata
# % pppl in each stratum that met a criterium

# multi-stratify by age, sex, and race/ethn
# JUST THE PREDICTION; how does the prediction change when SDI is added (IRRESPECTIVE of how well it connects to the outcome)
# Just crious about the change in prediction, 

## BUILD-IN CODE TO ADD THE AREA-LEVEL VARIABLES
# AND THE SDI & ChOOSE category**

# take SDI --> assign SDI cat --> assign true predict 


