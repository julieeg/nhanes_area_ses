# Rscript to run in the RDC to prepare & tabulate descriptive statistics of 
# area-level SES measures in NHANES

path_to_output = "../data/output/RDCtest"
source("../scripts/analysis/0.run_prep.r")

################################################################################
## Additional RDC prep to merge and construct final data sets
################################################################################

#/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/
## Use Income-Level as proxy for SDI decile (LowInc=3, MiddleInc=2, HighInc=1) 
nhanes_dat <- nhanes_dat %>% mutate(
  sdi_decile = case_when(
    inc_to_pov_level == "HighInc" ~ 1,
    inc_to_pov_level == "MiddleInc" ~ 5,
    inc_to_pov_level == "LowInc" ~ 10,
    ))

## TEMPORARY: Create indicator for seqn in acs_ses to merge
acs_ses <- fread("../data/processed/acs_ses_to_merge.csv") %>% 
  filter(complete.cases(.))

nhanes_seqn_tomerge <- nhanes_dat %>% 
  filter(Years %in% c("2009-2010", "2013-2014", "2017-2020")) %>% 
  pull(SEQN)

set.seed(314159)
nhanes_shuffled <- tibble(
  SEQN = sample(nhanes_seqn_tomerge, replace = FALSE),
  row_id = seq_along(SEQN)
)
acs_ses <- acs_ses %>% 
  mutate(row_id = row_number()) %>% 
  left_join(nhanes_shuffled, by = "row_id") %>% 
  select(-row_id) %>% 
  filter(!is.na(SEQN))

# Merge back into acs_ses
head(acs_ses) ; dim(acs_ses)
nhanes_merged <- nhanes_dat %>% full_join(acs_ses, by = "SEQN")
dim(nhanes_merged)
#\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\

## START WITH: nhanes_merged

# ==============================================================================
## Derive PREVENT SDI estimates, based on newly added SDI variable from RDC
# ==============================================================================

# 1. Grab names of all prevent prediction columns
prevent_sdi.vars <- unique(sub("sdi[123]", "sdi", nhanes_merged %>% select(
  starts_with("prevent_") & contains(c("_sdi", "_full.sdi")) & 
    contains("yr"), -contains("zip")) %>% names()))

# 2. Use lapply to derive final prevent_sdi_etc vars
prevent_sdi.dat <- lapply(prevent_sdi.vars, function(var) {
  nhanes_merged %>% 
    select(SEQN, sdi_decile, matches(paste0(gsub("sdi", "sdi[1-3]", var),"$"))) %>%
    rename_with(~ sub("^.*(sdi[1-3]+).*$", "\\1", .x), -SEQN) %>% 
    mutate(
      x = case_when(
        sdi_decile == 1 ~ sdi1,
        sdi_decile == 5 ~ sdi2,
        sdi_decile == 10 ~ sdi3,
        TRUE ~ NA)) %>% 
    select(SEQN, x) %>%
    rename_with(., ~gsub("x", var, .)) 
}) %>% reduce(., full_join, by="SEQN")

# 3. Merge into working dataframe & delete sdi[1-3] vars
nhanes_merged <- nhanes_merged %>% 
  select(-c(contains("prevent") & contains("yr") & contains("sdi"))) %>% 
  left_join(prevent_sdi.dat, by="SEQN")


# ==========================================================
## Re-code AREA-LEVEL factor variables (Ref = highest SES)
# ==========================================================

urbrur.lvls <- c("urban", "rural")
urbrurcat.lvls <- c("metropolitan", "micropolitan", "town", "rural")

nhanes_merged <- nhanes_merged %>% 
  mutate_at("acs_urbrur", ~factor(., levels=urbrur.lvls)) %>% 
  mutate_at("acs_urbrur_cat", ~factor(., levels=urbrurcat.lvls)) 
   
## Add intersection variables for 
nhanes_merged <- nhanes_merged %>% 
  mutate(
    intx_urbruXeduc = case_when(
      !is.na(acs_urbrur) & !is.na(educ_level) ~ paste0(acs_urbrur, "_x_", educ_level),
      TRUE ~ NA),
    intx_urbruXincpov = case_when(
      !is.na(acs_urbrur) & !is.na(inc_to_pov_level) ~ paste0(acs_urbrur, "_x_", inc_to_pov_level),
      TRUE ~ NA),
    intx_sexXagecatXraceXurbru = case_when(
      !is.na(gender) & !is.na(age_4lvl) & !is.na(racethn_combn) & !is.na(acs_urbrur) ~
        paste0(gender, age_4lvl, racethn_combn, acs_urbrur, collapse="_x_"),
      TRUE ~ NA)
    ) 


################################################################################
## Descriptive summaries by Age, Sex and Race/Ethnicity 
################################################################################

# ==============================================================================
## TABLE 1s. Descriptive table by AREA-LEVEL exposures/strata for ALL variables
# ==============================================================================

descr_tab1_vars.l <- c(years="Years", 
                       list(strata = c("gender", "age_4lvl", "racethn_combn")), 
                       list(ses = unname(ses_exposures.indiv)),
                       list(ses_area = unname(ses_exposures.area)), 
                       list(descr = addn_table1_vars),
                       c(all_outcomes.l, ndi_outcomes.l)
)

## Summarize all variables by categorical area-level SES exposures
lapply(seq_along(strata_vars), function(z) {
  lapply(seq_along(descr_tab1_vars.l), function(y) {
    
    xvar=names(descr_tab1_vars.l)[y] ; zvar=names(strata_vars.area)[z]
    cat(sprintf("DESCRIBE | Var ~ Strata | %s ~ %s \n", xvar, zvar))
    
    tryCatch({
      build_nhanes_summarytable.fun(
          vars_to_summarise = descr_tab1_vars.l[[y]],
          strata = strata_vars.area[[z]], subsets = alist(age >= 18),
          data = nhanes_merged, digits=3) 
    }, error = function(e) {
      message(sprintf("FAILED:  %s", conditionMessage(e))) ; return(NULL)
    })
    
  }) %>% do.call(bind_rows, .) %>% 
    fwrite(paste0(path_to_output, "/descr/tab_descr_allvars_by", 
                  names(strata_vars.area[z]), ".csv"))  
  
  ## Finished: printing output
  cat(sprintf("Finished. | Saving table to %s \n", paste0(
    path_to_output, "/descr/tab_descr_allvars_by", 
    names(strata_vars.area[z]), ".csv")))
  
}) 


# -------------------------------------------------------------------------------
## Table 1 (cont.) Distributions of Area-level exposures by Indiv-level exposures
# -------------------------------------------------------------------------------

descr_tab1_area_vars.l <- list(ses_area=descr_tab1_vars.l$ses_area)

lapply(seq_along(strata_vars.indiv), function(z) {
  lapply(seq_along(descr_tab1_area_vars.l), function(y) {
    
    xvar=names(descr_tab1_area_vars.l)[y] ; zvar=names(strata_vars.indiv)[z]
    cat(sprintf("DESCRIBE | Var ~ Strata | %s ~ %s \n", xvar, zvar))
    
    tryCatch({
      build_nhanes_summarytable.fun(
        vars_to_summarise = descr_tab1_area_vars.l[[y]],
        strata = strata_vars.indiv[[z]], subsets = alist(age >= 18),
        data = nhanes_merged, digits = 3) 
    }, error = function(e) {
      message(sprintf("FAILED:  %s", conditionMessage(e))) ; return(NULL)
    })
    
  }) %>% do.call(bind_rows, .) %>% fwrite(
    paste0(path_to_output, "/descr/tab_descr_areavars_by", 
           names(strata_vars.indiv[z]), ".csv"))
  
  ## Finished: printing output
  cat(sprintf("Finished. | Saving table to %s \n", paste0(
    path_to_output, "/descr/tab_descr_areavars_by", 
    names(strata_vars.indiv[z]), ".csv")))
  
}) 



################################################################################
## TABLE 2s. Distributions of continuous AREA-level exposures by STRATA
################################################################################

## Describe intersection of SES measures by strata & Education/Income
lapply(seq_along(intx_vars), function(z) { # For each intersection ... 
  intx_res <- lapply(seq_along(ses_exposures), function(x) { # For each SES exposure ... 
    
    xvar=names(ses_exposures)[x] ; zvar=names(intx_vars)[z]
    cat(sprintf("DESCRIBE | Var ~ Strata | %s ~ %s ... \n", xvar, zvar))
    
    intx_res <- tryCatch({ # skip if exposure is part of the intersection
      if(xvar %in% names(ses_exposures.indiv) & zvar %in% names(intx_vars.indiv)) {
        cat(sprintf("   SKIP | Run indiv-level comparisons, locally. \n"))
        return(NULL)
      } else {
        if(is.numeric(nhanes_merged %>% pull(ses_exposures[[x]]))) {
          get_nhanes_quantiles.fun(
            ses_exposures[[x]], strata = intx_vars[[z]], 
            subsets = alist(age >= 18),
            probs=c(0.05,0.25,0.50,0.75,0.95), data = nhanes_merged)
        } else { 
          get_nhanes_distrib.fun(
            ses_exposures[[x]], strata = intx_vars[[z]], 
            subsets = alist(age>=18), data = nhanes_merged) }
      }
    }, error = function(e) {
      message(sprintf("   FAILED | %s", conditionMessage(e)))
      return(NULL)
    }) 
    
  }) %>% do.call(rbind.data.frame, .)
  
  # Reformat intersection output
  intx_res %>% 
    #select(-contains(c("teststat")), -"p_value") %>% 
    pivot_longer(cols = -c(Variable, Level, p_value, contains(c("teststat", "total"))),
                 names_sep="_x_", names_to=c("Var1_level", "Var2_level")) %>%
    pivot_wider(names_from=Var1_level) %>%
    mutate(
      Intersect = names(strata_intx_vars[8:17])[z], 
      teststat = intx_res$teststat_df[1], 
      teststat_val = intx_res$teststat_val[1],
      p_value = intx_res$p_value[1]) %>% 
    
    select(
      Intersect, Variable, Level, Var2_level, ends_with("total"), 
      starts_with(c("mean", "se", "teststat")), 
      p_value) %>%
    
    fwrite(paste0(path_to_output, "/descr/tab_distr_areases_by", 
                  names(intx_vars[z]), ".csv"))
})



################################################################################
## TABLE 3. Pearson correlations of area-level SES measures, overall by STRATA
################################################################################

## Add integer versions of categorical SES exposures
nhanes_merged <- nhanes_merged %>% 
  mutate(
    educ_level.int = as.integer(educ_level),
    inc_to_pov_level.int = as.integer(inc_to_pov_level),
    employ_status.int = as.integer(employ_status),
    foodsecure_level.int = as.integer(foodsecure_level),
    acs_urbrur.int = as.integer(acs_urbrur),
    acs_urbrur_cat.int = as.integer(acs_urbrur_cat))

ses_exposures.corr <- c("educ_level.int", "inc_to_pov_level.int", "inc_to_pov", 
                        "employ_status.int", "foodsecure_level.int", 
                        "acs_urbrur.int", "acs_urbrur_cat.int",
                        unname(ses_exposures.area)[-c(1:2)])

## Correlations in total sample ---------------
cor_total.l <- get_nhanes_cormat.fun(vars=ses_exposures.corr, data=nhanes_merged)

full_join(cor_total.l$corr, cor_total.l$pvals, by = c("level", "variable")) %>% 
  fwrite(paste0(path_to_output, "/descr/tab_cormat_allses_bytotal.csv"))


## Correlations, by strata ---------------
mclapply(seq_along(strata_vars), function(z) {
  corres <- get_nhanes_cormat.fun(vars = ses_exposures.corr, strata=strata_vars[[z]]) 
  full_join(corres$corr, corres$pvals, by = c("strata", "level", "variable")) %>% 
    fwrite(paste0(
      path_to_output, "/descr/tab_corr_allses_by", 
      names(strata_vars)[z],".csv")
      )
})



################################################################################
## TABLE 4. Summary of PREVENT estimates, before/after adding SDI, by strata
################################################################################

# ==============================================================================
## I) Summarise risk estimates in base/full/sdi models, by strata & intersections
# ==============================================================================

prevent_vars_all <- nhanes_merged %>% 
  select(starts_with("prevent_") & contains("yr")) %>% 
  names()

## List ALL prevent vars to describe: raw estimates, riskclass
prevent_risk_vars <- list(
  "10yr_CVD", "30yr_CVD", "10yr_ASCVD", "30yr_ASCVD", "10yr_HF", "30yr_HF" 
) ; prevent_descr_vars.l <- lapply(prevent_risk_vars, function(y) {
  nhanes_merged %>% select(contains(y)) %>% select(
    ends_with(y), contains("diff"), contains("riskclass"), contains("reclass")
    ) %>% names()
}) ; names(prevent_descr_vars.l) = prevent_risk_vars
  

## Summarise risk estimates, categories, differences and changes when -/+ SDI
lapply(seq_along(strata_vars), function(z) {
  lapply(seq_along(prevent_descr_vars.l), function(y) {
    
    cat(sprintf("Describing %s risk estimates | By strata ~ %s \n", 
                toupper(names(prevent_descr_vars.l)[y]), 
                toupper(names(strata_vars)[z])))
    
    build_nhanes_summarytable.fun( 
      vars_to_summarise = prevent_descr_vars.l[[y]], strata = strata_vars[[z]], 
      subsets = alist(age >= 18), data = nhanes_merged, digits=3)
    
    }) %>% 
    do.call(bind_rows, .) %>% 
  fwrite(paste0(path_to_output, "/descr/tab_descr_prevdescr_by", 
               names(strata_vars[z]), ".csv"))  
  
  ## Finished: printing output
  cat(sprintf("Finished. | Saving table to %s \n", paste0(
    path_to_output, "/descr/tab_descr_prevest_by", names(strata_vars[z]), ".csv"))
  )
})

# ------------------------------------------------------------------------
## Re-run descriptive analysis, subsetting to prevent_use_htn/dyslip == 1
# ------------------------------------------------------------------------
prevent_subsets <- list(
  alist(age >= 18 & prevent_use_htn ==1),
  alist(age >= 18 & prevent_use_dyslip == 1)
  )

lapply(seq_along(strata_vars), function(z) {
  lapply(seq_along(prevent_descr_vars.l), function(y) {
    lapply(seq_along(prevent_subsets), function(j) {
      
      cat(sprintf("DESCRIBE | PREVENT ~ Strata | %s ~ %s | FILTER [%s] \n", 
                  toupper(names(prevent_descr_vars.l)[y]), 
                  toupper(names(strata_vars)[z]),
                  prevent_subsets[j]))
    
    build_nhanes_summarytable.fun(prevent_descr_vars.l[[y]], 
                                  strata = strata_vars[[z]], 
                                  subsets = prevent_subsets[[j]],
                                  data = nhanes_merged, d=3) %>% 
      mutate(Filter = paste(prevent_filters[j], "==", 1), .before=1)
    
  }) %>% do.call(bind_rows, .)
  }) %>% do.call(bind_rows, .) %>%
    fwrite(paste0(path_to_output, "/descr/tab_descr_prevdescr_guidelines_by", 
                  names(strata_vars[z]), ".csv"))  
  
  ## Finished: printing output
  cat(sprintf("Finished. | Saving table to %s \n", paste0(
    path_to_output, "/descr/tab_descr_prevest_by", names(strata_vars.indiv[z]), ".csv"))
  )
})


# ==========================================================
## Summarize quantiles of differences, by cat strata/intx
# ==========================================================

prevent_riskdiff_vars <- list(
  "10yr_CVD", "30yr_CVD", "10yr_ASCVD", "30yr_ASCVD", "10yr_HF", "30yr_HF" 
) ; prevent_riskdiff_vars.l <- lapply(prevent_riskdiff_vars, function(y) {
  nhanes_merged %>% select(contains(y) & contains("diff")) %>% names()
}) ; names(prevent_riskdiff_vars.l) = prevent_riskdiff_vars


## Summarize by each individual strata 
prevent_sdi_diff_dist.l <- lapply(seq_along(strata_vars), function(z) {
  
  lapply(seq_along(prevent_riskdiff_vars.l), function(x) {
    riskdiff_vars <- prevent_riskdiff_vars.l[[x]]
    lapply(seq_along(riskdiff_vars), function(i) {
      xvar = riskdiff_vars[i] ; zvar=names(strata_vars.indiv)[z]
      cat(sprintf("DESCRIBE | PREVENT ~ Strata | %s ~ %s \n", xvar, zvar))
      
      tryCatch({
        get_nhanes_quantiles.fun(
          xvar, strata = strata_vars[[z]], 
          probs=c(0.05,0.25,0.50,0.75,0.95), data = nhanes_merged) %>% 
          select(-contains(c("teststat")), -"p_value") %>% 
          pivot_longer(cols = -c(Variable, Level, contains("total")),
                       names_to="stat_strata") %>%
          separate(stat_strata, sep="_", into = c("stat", "stratalvl")) %>%
          pivot_wider(values_from = c("value"), names_from="stat") %>%
          select(Variable, quantile=Level, stratalvl, ends_with("total"), mean, se) 
      }, error = function(e) {
        cat(sprintf("SKIPPING | %s x %s \n", xvar, zvar))
        message(sprintf("    Error details: %s", conditionMessage(e)))
        return(NULL)
      })
      
  }) %>% do.call(rbind.data.frame, .)
  }) %>% bind_rows()
}) ; names(prevent_sdidiff_dist.l) <- names(strata_vars.indiv)


# Compile & save prevent_sdi change estimates, by strata
prevent_sdi_diff_dist.df <- lapply(seq_along(prevent_sdi_diff_dist.l), function(x) {
  prevent_sdi_diff_dist.l[[x]] %>% 
    mutate(Strata = names(prevent_sdi_diff_dist.l[x]), .before=1)
}) %>% do.call(rbind.data.frame, .) 


## Summarize by intersection variables
prevent_sdi_diff_intx_dist.l <- lapply(seq_along(intx_vars), function(z) {
  
  lapply(seq_along(unlist(prevent_riskdiff_vars.l)), function(x) {
    
    xvar = unlist(prevent_riskdiff_vars.l)[x] ; zvar=names(intx_vars)[z]
    cat(sprintf("DESCRIBE | PREVENT ~ Strata | %s ~ %s \n", xvar, zvar))
    
    tryCatch({
      get_nhanes_quantiles.fun(
        xvar, strata = intx_vars[[z]], 
        subsets = alist(age >= 18),
        probs=c(0.05,0.25,0.50,0.75,0.95), data = nhanes_merged) %>% 
        select(-contains(c("teststat")), -"p_value") %>% 
        pivot_longer(cols = -c(Variable, Level, contains("total")),
                     names_to="stat_strata") %>%
        # Add for intersection strata, which have _x_ built in to strat levels 
        mutate_at("stat_strata", ~gsub("_x_", "x", .)) %>% 
        separate(stat_strata, sep="_", into = c("stat", "stratalvl")) %>%
        pivot_wider(values_from = c("value"), names_from="stat") %>%
        select(Variable, quantile=Level, stratalvl, ends_with("total"), mean, se) 
    }, error = function(e) {
      cat(sprintf("SKIPPING | %s x %s \n", xvar, zvar))
      message(sprintf("    Error details: %s", conditionMessage(e)))
      return(NULL)
    })
    
  }) %>% do.call(rbind.data.frame, .)
}) ; names(prevent_sdidiff_intx_dist.l) <- names(intx_vars)


# Compile & save prevent_sdi_intx change estimates, by intx strata
prevent_sdi_diff_intx_dist.df <- lapply(seq_along(prevent_sdi_diff_intx_dist.l), function(x) {
  prevent_sdi_diff_intx_dist.l[[x]] %>% 
    mutate(Strata = names(prevent_sdi_diff_intx_dist.l[x]), .before=1)
  }) %>% do.call(rbind.data.frame, .) 

# Bind summary tables & save as .csv
prevent_sdi_diff_all_dist.df <- rbind.data.frame(
  prevent_sdi_diff_dist.df, prevent_sdi_diff_intx_dist.df)

prevent_sdidiff_all_dist.df %>% fwrite(
  paste0(path_to_output, "/descr/tab_distr_prevdiff_bystrata.csv")
)


# -----------------------------------------------------------
## Prepare boxplots of PREVENT changes, by strata
# -----------------------------------------------------------

# Reshape for plotting
prevent_sdidiff_dist_toplot <- prevent_sdidiff_all_dist.df %>% 
  mutate(Quantile = paste0("p", as.numeric(quantile) * 100),
         Outcome = gsub(".*yr_", "", gsub("_diff.*", "", Variable)),
         Time = gsub(".*_", "", gsub("yr_.*", "yr", Variable)),
         Model = str_to_sentence(gsub("_.*", "", gsub("prevent_", "", Variable)))) %>% 
  # note: Select mean or mean_total depending on which variable you want to plot
  select(Model, Outcome, Time, Strata, Level = stratalvl, Quantile, mean) %>% 
  pivot_wider(names_from = Quantile, values_from = mean) %>% 
  mutate_at("Strata", ~factor(., levels = c(
    "sex", "agecat", "racethn", "educ", "incpovlvl", "employ", "foodinsec", "sexXagecatXrace"))) %>% 
  mutate(intxlvl = ifelse(Strata == "sexXagecatXrace", Level, NA)) %>% 
  mutate_at("Level", ~factor(., levels = c(
    "Female", "Male", 
    "30to39y", "40to64y", "over65y", ## Missing under30y ??
    "NHW", "NHB",  "MexAm", "OthHis", "Oth/Mult/NHA", "NHA", "Oth/Mult",
    "HighInc", "MiddleInc", "LowInc",
    "LessThan9th", "SomeHS", "HSgrad", "SomeColl", "Collgrad",
    "Full-time employed", "Part-time employed", "Student", "Unemployed",
    "0", "1")
  ))


## Generate TEST Plots
prevent_sdidiff_dist_toplot %>% 
  filter(Model == "Base" & Time == "10yr" & Strata != "sexXagecatXrace") %>%
  distinct(Outcome, Time, Strata, Level, .keep_all = TRUE) %>%
  ggplot(aes(x = Level, fill = Level)) +
  facet_grid(Outcome ~ Strata, space="free_x", scale="free_x") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") + 
  geom_boxplot(aes(ymin = p5, lower = p25, middle = p50, upper = p75, ymax = p95,
                   group = interaction(Level, Strata)), 
               stat = "identity", 
               position = position_dodge(0.4), width=0.55) +
  scale_x_discrete(expand = expansion(add = c(1.25, 1.25))) +
  scale_fill_manual(values = c(
    palettes$NatExt$Oranges[c(4,6)], palettes$NatExt$Purples[c(5,4,3)],
    palettes$NatExt$Blues[4:2], palettes$NatExt$Teals[5:2],
    palettes$NatExt$Greens[c(3,4,5)], palettes$NatExt$Reds[2:6],
    palettes$NatExt$Yellows[2:5], palettes$NatExt$Olives[c(4,6)])) +
  ggplot_theme +
  guides(fill = "none") + # <-- Removes the legend for fill / Level
  theme(axis.text.x = element_text(angle = 45, color = "black", 
                                   hjust=1, vjust = 1),
        panel.border = element_blank(),
        panel.spacing.x = unit(0, "mm"), 
        panel.grid = element_blank()) +
  labs(
    title = "Boxplot from Pre-calculated Summary Statistics",
    x = "Variable",
    y = "Change in Estimate (Base - Base+SDI)"
  )



## SECOND test plot, of intersection: sexXageXrace
prevent_sdidiff_dist_toplot %>% 
  filter(Model == "Base" & Time == "10yr" & Strata == "sexXagecatXrace") %>%
  distinct(Outcome, Time, Strata, intxlvl, .keep_all = TRUE) %>%
  mutate(Sex=gsub("x.*","",intxlvl),
         Race=gsub(".*yx", "", intxlvl)) %>%
  ggplot(aes(x = intxlvl, fill = intxlvl)) +
  facet_grid(Outcome ~ Race, space="free_x", scale="free_x") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") + 
  geom_boxplot(aes(ymin = p5, lower = p25, middle = p50, upper = p75, ymax = p95,
                   group = interaction(intxlvl)), 
               stat = "identity", 
               position = position_dodge(0.4), width=0.55) +
  scale_x_discrete(expand = expansion(add = c(1.25, 1.25))) +
  #scale_fill_manual(values = c(
  #  palettes$NatExt$Oranges[c(4,6)], palettes$NatExt$Purples[c(5,4,3)],
  #  palettes$NatExt$Blues[4:2], palettes$NatExt$Teals[5:2],
  #  palettes$NatExt$Greens[c(3,4,5)], palettes$NatExt$Reds[2:6],
  #  palettes$NatExt$Yellows[2:5], palettes$NatExt$Olives[c(4,6)])) +
  ggplot_theme +
  guides(fill = "none") + # <-- Removes the legend for fill / Level
  theme(axis.text.x = element_text(angle = 45, color = "black", 
                                   hjust=1, vjust = 1),
        panel.border = element_blank(),
        panel.spacing.x = unit(0, "mm"), 
        panel.grid = element_blank()) +
  labs(
    title = "Boxplot from Pre-calculated Summary Statistics",
    x = "Variable",
    y = "Change in Estimate (Base - Base+SDI)"
  )


## EOF



## FINAL STEPS;

## Water fall plot for prevent equation outcomes; each IND is an up or down on waterfal, based on how their prediction changes hen you add SDI
#and color code by race

# HOLD ON THIS: but think about how to consolidate the risk estimates when adding SDI information
# calcualte delta in prevent equation, base to sdi; base to full; did they meet a threshod in each strata
# % pppl in each stratum that met a criterium

# multi-stratify by age, sex, and race/ethn
# JUST THE PREDICTION; how does the prediction change when SDI is added (IRRESPECTIVE of how well it connects to the outcome)
# Just crious about the change in prediction, 

## 
