## Special R functions for NHANES Data Download, Cleaning & Analysis 
## Written by: Julie Gervis
## Last Updated: 9/17/2026


################################################################################
## Set Up: Load required packages & Compile NHANES table/variable index files
################################################################################

library(tidyverse) ; library(data.table) ; library(nhanesA)

## Load NHANES Manifest for Variables & Tables -------------------
manifest_nhanes_vars <- nhanesManifest(which = "variables") %>% 
  mutate(Years = paste0(BeginYear, "-", EndYear)) %>% 
  filter(UseConstraints == "None")
#manifest_nhanes_vars %>% fwrite("./manifest_nhanes_vars.txt")

manifest_nhanes_tables <- manifest_nhanes_vars %>% 
  select(Component, Table, TableDesc, BeginYear, EndYear, Years) %>% 
  distinct()
#manifest_nhanes_tables %>% fwrite("./manifest_nhanes_tables.txt")

################################################################################
## Data Download Function
################################################################################

## ================================================================
## Download & build NHANES data tables using a data dictionary
## ================================================================

build_nhanes_table.fun <- function(data_dictionary, category = NULL,
                                   start_year = "2007", end_year = "2016",
                                   special_weights = NULL,
                                   incl_long_tabs = T) {
  
  ## 0. Initialize error log
  error_log <- list() ; log_error <- function(context, table=NA, variable=NA, message) {
    error_log[[length(error_log) + 1]] <<- data.frame(
      context  = context, table    = table, variable = variable,
      message  = message, stringsAsFactors = FALSE)
    cat(sprintf("  [WARNING] %s | table: %s | %s\n", context, table, message))
  }
  
  ## List the data categories for Download; if categories == NULL, only 1 data 
  ## category will be downloaded
  data_categories <- unique(data_dictionary$category)
  tab_data.l <- lapply(data_categories, function(cat) {
  
  ## 1. Create List of UNIQUE tables & variables to load, by data category
  # Filter by NHANES data category (DEMO, LABORATORY, EXAM, QUESTIONNAIRE, DIET)
  datadict_cat <- data_dictionary %>% filter(category == cat)
  
  # Note: variables could appear in multiple tables, so list UNIQUE tables, only
  vars_by_cat <- manifest_nhanes_vars %>% 
    filter(grepl(cat, toupper(Component))) %>%
    filter(toupper(VarName) %in% toupper(datadict_cat$variable)) 
  
  # If start/end_years specified, apply restriction to selected variables
  if(!is.null(start_year)) { vars_by_cat <- vars_by_cat %>% 
    filter(as.numeric(BeginYear) >= as.numeric(start_year)) } 
  if(!is.null(end_year)) {
    vars_by_cat <- vars_by_cat %>% 
      filter(as.numeric(EndYear) <= as.numeric(end_year)+1)
  }
  
  # Gather all tables by category that have SEQN + ≥1 other requested variable;
  tables_by_cat <- vars_by_cat %>% select(Table, BeginYear, EndYear, Years) %>% unique() 
 
  # Gather sample weights & add to vars_by_cat
  sample_weights <- c("WTINT|WTMEC|")
  vars_by_cat <- vars_by_cat %>% bind_rows(
    manifest_nhanes_vars %>% filter(
      grepl(paste0(sample_weights, paste0(special_weights, collapse="|"),
                   collapse="|"), VarName)) %>% 
      filter(Table %in% tables_by_cat$Table)
  )
  
  # 2. Use lapply to download all requested variables for each TABLE/YEAR:
  cat_tab_data.l <- lapply(1:nrow(tables_by_cat), function(i) {
    
    tab <- tables_by_cat$Table[i] ; years = tables_by_cat$Years[i]
    cat(sprintf("Downloading %s | %s [%s]\n", cat, tab, years))
    
    # B) Download table and requested variables
    tab_raw <- tryCatch({
      tab_all <- nhanes(tab)
      
      # Compile list of selected variables, 
      vars_to_select <- unique(vars_by_cat$VarName)
      
      # Check for variables with mis-matched cases 
      vars_match_orig <- names(tab_all)[names(tab_all) %in% vars_to_select]
      vars_match_lower <- names(tab_all)[tolower(names(tab_all)) %in% tolower(vars_to_select)]
      
      # IF lowercasing captures additional variables, convert column names to lower
      if (length(vars_match_lower) > length(vars_match_orig)) {
        tab_all <- tab_all %>% select(SEQN, all_of(vars_match_lower)) 
      } else {
        tab_all <- tab_all %>% select(SEQN, all_of(vars_match_orig)) 
      } ; tab_all <- tab_all %>% 
        mutate(Years=years, .before=1) 
      
    }, error = function(e) {
      log_error("nhanes() download", table=tab, message=conditionMessage(e))
      return(NULL)
    })
    
    if (is.null(tab_raw)) return(NULL)   # skip remainder for this table
    
    # Compile data dictionary for table
    vars_by_tab <- names(tab_raw)
    datadict_tab <- datadict_cat %>% filter(variable %in% c(vars_by_tab))  
    
    # C) Add descriptive names (to facilitate merging across dataset/years)
    new_var_names <- names(tab_raw) 
    names(new_var_names) <- datadict_tab$Varname[match(new_var_names, datadict_tab$variable)]
    names(new_var_names)[is.na(names(new_var_names))] <- make.names(new_var_names[is.na(names(new_var_names))])
    tab_named <- tab_raw %>% rename(all_of(new_var_names)) 
    
    # D) Check for variable missingness >90% and remove
    missingness <- colMeans(is.na(tab_named))
    high_missing <- missingness[which(missingness>0.90)]
    if(length(high_missing)>0) {
      miss_summary <- cbind(
        category=cat, Table=tab, Years=years, cbind.data.frame(high_missing) %>% 
          rownames_to_column("Variable")
      ) #; tab_named <- tab_named %>% select(-names(high_missing))
    } else {
      miss_summary <- cbind(category=cat, Table=tab, Years=years, Variable="NONE", high_missing=NA)
    }
    
    # Compile list of outputs
    list(table=tab_named, raw_table=tab_raw, missing_table=miss_summary)
    
  }) ; names(cat_tab_data.l) <- tables_by_cat$Table
  
  cat(sprintf("Creating dataset for %s with variables: %s \n", cat, paste0(unique(vars_by_cat$VarName), collapse=", ")))
  
  ## 3. Remove any tables/table variables that failed to load
  cat_tab_data.l <- cat_tab_data.l[sapply(cat_tab_data.l, is.list)]
  vars_by_cat <- vars_by_cat %>% filter(Table %in% names(cat_tab_data.l))
  
  # function to keep variables with non-missingness?
  keepval <- function(x) { 
    idx <- which(!is.na(x))
    if (length(idx) > 0) x[idx[1]] else x[1]
  }
  
  ## 4. Collapse all tables - group by Year/SEQN to join common variables across datasets
  # Coerce all factor variables into character to facilitate binding
  clean_tab_data.l <- lapply(cat_tab_data.l, function(df) {
    df[["table"]] %>%
      mutate(across(where(is.factor), as.character))
  })
  
  # 5. Convert long tables (with multiple SEQN entries) to wide format (e.g., RSQ_RX & PAQ)
  long_tabs <- names(clean_tab_data.l)[
    sapply(clean_tab_data.l, function(tab) nrow(tab) != length(unique(tab[["SEQN"]])) )]
  
  if(length(long_tabs) > 1 & incl_long_tabs == T) {
    long_tab_data.l <- clean_tab_data.l[long_tabs]
    wide_tab_data.l <- lapply(long_tab_data.l, function(tab) {
      tab %>% 
        group_by(SEQN, Years) %>%
        mutate(entry_num = row_number()) %>%
        ungroup() %>%
        pivot_wider(names_from=entry_num, values_from = starts_with("q_"), 
                    names_glue = "{.value}_{entry_num}")
    }) ; clean_tab_data.l <- c(
      clean_tab_data.l[!names(clean_tab_data.l) %in% long_tabs],
      wide_tab_data.l)
  } else {
    clean_tab_data.l <- clean_tab_data.l[!names(clean_tab_data.l) %in% long_tabs]
  }
  
  # Convert data frames to data.tables and bind
  data_by_cat <- rbindlist(lapply(clean_tab_data.l, setDT), fill = TRUE)
  data_by_cat.df <- data_by_cat[, lapply(.SD, keepval), by = .(Years, SEQN)]
  
  # 6. Create detailed summary of all variable definitions, tables, and availability at each time point
  var_summary <- vars_by_cat %>%
    filter(Table %in% tables_by_cat$Table) %>% 
    mutate(New.VarName = datadict_cat$Varname[match(VarName, datadict_cat$variable)], .after=VarName) %>%
    group_by(New.VarName, VarDesc) %>%
    reframe(Raw.VarNames = paste0(unique(VarName), collapse=", "), 
            VarDesc = paste(VarDesc), 
            Years = ifelse(min(BeginYear) == max(BeginYear), sprintf("%s-%s", mean(as.numeric(BeginYear)), mean(as.numeric(EndYear))), 
                           sprintf("%s-%s", min(as.numeric(BeginYear)), max(as.numeric(EndYear)))),
            Raw.Tables = paste0(unique(Table), collapse=", ")) %>%
    mutate_at("New.VarName", ~ifelse(is.na(.), Raw.VarNames, .)) %>%
    unique()
  
  # 7. Compile table of missing variables
  missing_table <- lapply(names(cat_tab_data.l), function(tab) cat_tab_data.l[[tab]]$missing_table) %>% 
    do.call(rbind.data.frame, .) %>% 
    filter(Variable != "NONE") %>%
    unique() 
  
  ## Save error log
  error_log <- error_log %>% do.call(rbind.data.frame, .)
  
  ## 8. Return list of outputs, for EACH data category
  return(list(data_table=data_by_cat.df,
              var_summary=var_summary,  
              var_missingness = missing_table,
              long_data_tables = if(length(long_tabs)>1) {long_tabs} else {"None"},
              error_log = error_log, 
              raw_data_tables.l=cat_tab_data.l))
  
  }) ; names(tab_data.l) <- data_categories
  
  ## Return list of outputs, across ALL data cateogries
  return(list(full_data_table = tab_data.l %>%
                map("data_table") %>% 
                reduce(full_join),
              data_by_category.l = tab_data.l)
         )
}


################################################################################
## Data Cleaning & Processing
################################################################################

## =================================================
# Generic function to recode missingness as NA
## =================================================

recode_nhanes_na.fun <- function(x) {
  if(is.numeric(x)) {
    x[x %in% c(777, 7777, 77777, 999, 9999, 99999)] <- NA_real_
    return(x)
  } else if(is.character(x)) {
    x[x %in% c("Don't know", "Refused", "", 
               "777", "7777", "77777", "999", "9999", "99999")] <- NA_character_
    return(x)
  } else {return(x)}
}















