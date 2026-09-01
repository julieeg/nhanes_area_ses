# Rscript to build NHANES summary tables


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


################################################################################
## Build index of all NHANES tables/vars across time points (1999-2023)
################################################################################

#list data categories with labels
data_categories <- c("DEMO", "LABORATORY", "EXAM", "QUESTIONNAIRE", "DIET")
data_categories.labs <- c("Demographics"="DEMO", "Laboratory"="LABORATORY", 
                          "Examination"="EXAM", "Questionnaire"="QUESTIONNAIRE", 
                          "Dietary"="DIET")

## =====================================================
## Make index of all nhanes TABLES, by category & year
## =====================================================

## All NHANES Tables (1999-2023)
nhanes_tables_all <- nhanesSearch("Respondent sequence number", namesonly=F) %>%
  mutate(Category=data_categories.labs[Component], Years=sprintf("%s-%s", Begin.Year, EndYear)) %>%
    select(Category, Component, Data.File.Name, Data.File.Description, Begin.Year, "End.Year"=EndYear, Years) %>%
    arrange(Category, Begin.Year) ; dim(nhanes_tables_all) # 1540 7

## Add Tables NOT indexed on CDC website (included in nhanesSearch)
nhanes_tables_addn <- rbind.data.frame(
  c("BIOPRO_J", "Standard Biochemistry Profile", "2017-2018"),
  c("P_BIOPRO", "Standard Biochemistry Profile", "2017-2020"),
  c("BIOPRO_L", "Standard Biochemistry Profile", "2021-2023"),
  c("TCHOL_J", "Cholesterol Total", "2017-2018"),
  c("P_TCHOL", "Cholesterol Total", "2017-2020"),
  c("TCHOL_L", "Cholesterol Total", "2021-2023"),
  c("HDL_J", "Cholesterol - High - Density Lipoprotein (HDL)", "2017-2018"),
  c("P_HDL", "Cholesterol - High - Density Lipoprotein (HDL)", "2017-2020"),
  c("HDL_L", "Cholesterol - High - Density Lipoprotein (HDL)", "2021-2023"),
  c("TRIGLY_L"))

names(nhanes_tables_addn) <- c("Data.File.Name", "Data.File.Description", "Years") 
nhanes_tables_addn <- nhanes_tables_addn %>% mutate(
  Category = "LABORATORY", Component = "Laboratory", .before=1
    ) %>% mutate(
      Begin.Year = gsub("-.*","", Years), End.Year = gsub(".*-","", Years),
      .before="Years"
  )

nhanes_tables_all <- nhanes_tables_all %>% bind_rows(nhanes_tables_addn)
#nhanes_tables_all %>% fwrite("../data/raw/nhanes_tablelist_all.csv")
#nhanes_tables_all <- fread("../data/raw/nhanes_tablelist_all.csv")


## =========================================================
## Make index of all nhanes VARIABLES, by category & year
## =========================================================

list_nhanes_vars.fun <- function(nhanes_table_summary) {
  lapply(nhanes_table_summary[["Data.File.Name"]], function(tab) {
    category <- nhanes_table_summary %>% filter(Data.File.Name == tab) %>% pull(Category)
    years <- nhanes_table_summary %>% filter(Data.File.Name == tab) %>% pull(Years)
    tryCatch({
      codebook.l <- nhanesCodebook(nh_table=tab) 
      codebook <- lapply(1:length(codebook.l), function(i) { cbind.data.frame(
        Variable.Name=codebook.l[[i]][[1]], 
        Variable.Description=codebook.l[[i]][[3]]) }) %>% 
          do.call(rbind.data.frame, .) %>% 
        as.data.table() %>% mutate(Table=tab, Years=years, .before=1)
    }, error = function(e) {
      data.table(Category = category, Year=year, Table=tab, Error = conditionMessage(e))
      return(NULL)
      }) }) %>% 
    bind_rows() 
}

# List nhanes variables for 'ALL' nhanes tables 
nhanes_vars_all <- list_nhanes_vars.fun(nhanes_tables_all) ; dim(nhanes_vars_all) # 47685 5 
nhanes_vars_lab_post2017 <- list_nhanes_vars.fun(
  nhanes_tables_all %>% 
    filter(Category == "LABORATORY" & 
             Years %in% c("2017-2018", "2017-2020", "2021-2023")))
  
nhanes_vars_all <- nhanes_vars_all %>% 
  bind_rows(nhanes_vars_lab_post2017) %>% unique()
  
# Merge in Categeories 
nhanes_vars_all <- nhanes_vars_all %>% 
  left_join(nhanes_tables_all %>% select(Category, Table=Data.File.Name)) %>%
  select(Category, names(nhanes_vars_all))
nhanes_vars_all %>% fwrite("../data/raw/nhanes_varlist_all.csv")
#nhanes_vars_all <- fread("../data/raw/nhanes_varlist_all.csv")


################################################################################
## Download publicly available NHANES tables from Demographics (DEMO), 
## Examination (EXAM), Laboratory (LAB) & Questionnaire (Q) data
################################################################################

nhanes_yrs <- names(table(nhanes_tables_all$Years))
nhanes_yrs <- nhanes_yrs[-c(2,7)] # remove "1999-2004" and 2007-2012

# Make array of years/labels for selecting tables
names(nhanes_yrs) <- c("", paste0("_", LETTERS[2:10]),"P_", "_L")


## =========================================================================
## Write function to build NHANES tables from a data dictionary
## =========================================================================

## Function to build nhanes table with variables, by category
build_nhanes_table <- function(cat, datadict = project_datadict) {
  
  ## 0. Initialize error log
  error_log <- list() ; log_error <- function(context, table=NA, variable=NA, message) {
    error_log[[length(error_log) + 1]] <<- data.frame(
      context  = context, table    = table, variable = variable,
      message  = message, stringsAsFactors = FALSE)
    cat(sprintf("  [WARNING] %s | table: %s | %s\n", context, table, message))
  }
  
  ## 1. Create List of UNIQUE tables & variables to load by data category
  # Filter by NHANES data category (DEMO, LABORATORY, EXAM, QUESTIONNAIRE, DIET)
  datadict_cat <- datadict %>% filter(category == cat)
  
  # Note: variables could appear in multiple tables, so list UNIQUE tables, only
  vars_by_cat <- nhanes_vars_all %>% 
    filter(Category == cat & Variable.Name %in% datadict_cat$variable) 
  vars_by_cat <- bind_rows(vars_by_cat,
                           # Add rows with SEQN for each required Table
                           nhanes_vars_all %>% filter(
                             Table %in% vars_by_cat$Table & Variable.Name == "SEQN"))
  
  # Gather all tables by category that i) have SEQN + ≥1 other requested variable
  tables_by_cat <- vars_by_cat %>% select(Table, Years) %>% unique()
  
  # Gather sample weights & add to vars_by_cat
  vars_by_cat <- vars_by_cat %>% bind_rows(
    nhanes_vars_all %>% filter(Table %in% tables_by_cat$Table) %>%
      filter(grepl("WTINT|WTMEC|WTSAF|WTDR", Variable.Name))
      #filter(grepl("sample|weight", Variable.Description, ignore.case = T)) %>%
  )
  
  # 2. Use lapply to download all requested variables for each TABLE/YEAR:
  tab_data.l <- lapply(1:nrow(tables_by_cat), function(i) {
    
    tab <- tables_by_cat$Table[i] ; years = tables_by_cat$Years[i]
    cat(sprintf("Downloading %s | %s [%s]\n", cat, tab, years))
    
    # B) Download table and requested variables
    tab_raw <- tryCatch({
      tab_all <- nhanes(tab)
      # Vars to select from table
      vars_to_select <- names(tab_all)[c(which(names(tab_all) %in% unique(vars_by_cat$Variable.Name)))] #c(datadict_cat %>% pull(variable))))]
      # Add relevant sample weights 
      #vars_to_select <- unique(c(vars_by_cat$Variable.Name, vars_to_select))
        #unique(c(names(tab_all)[which(startsWith(names(tab_all), "WT"))], vars_to_select)) 
      tab_all %>% select("SEQN", all_of(vars_to_select)) %>%
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
    
  }) ; names(tab_data.l) <- tables_by_cat$Table
  
  cat(sprintf("Creating dataset for %s", cat))
  
  ## E) Remove any tables/table variables that failed to load
  tab_data.l <- tab_data.l[sapply(tab_data.l, is.list)]
  vars_by_cat <- vars_by_cat %>% filter(Table %in% names(tab_data.l))
  
  # function to keep variables with non-missingness?
  keepval <- function(x) { 
    idx <- which(!is.na(x))
    if (length(idx) > 0) x[idx[1]] else x[1]
  }
  
  ## F) Collapse all tables - group by Year/SEQN to join common variables across datasets
  
  # Coerce all factor variables into character to facilitate binding
  clean_tab_data.l <- lapply(tab_data.l, function(df) {
    df[["table"]] %>% mutate(across(where(is.factor), as.character))
  })
  
  # Convert long tables (with multiple SEQN entries) to wide format (e.g., RSQ_RX & PAQ)
  long_tabs <- names(clean_tab_data.l)[sapply(clean_tab_data.l, function(tab) nrow(tab) != length(unique(tab[["SEQN"]])) )]
  if(length(long_tabs) >1) {
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
    } 
  
  # i) Convert data frames to data.tables and bind
  data_by_cat <- rbindlist(lapply(clean_tab_data.l, setDT), fill = TRUE)
  data_by_cat.df <- data_by_cat[, lapply(.SD, keepval), by = .(Years, SEQN)]
  
  # Create detailed summary of all variable definitions, tables, and availability at each time point
  var_summary <- vars_by_cat %>%
    filter(Table %in% tables_by_cat$Table) %>% 
    mutate(New.Variable.Name = datadict_cat$Varname[match(Variable.Name, datadict_cat$variable)], .after=Variable.Name) %>%
    separate(Years, into = c("Begin.Year", "End.Year")) %>%
    group_by(New.Variable.Name, Variable.Description) %>%
    reframe(Raw.Variable.Names = paste0(unique(Variable.Name), collapse=", "), 
            Variable.Description = paste(Variable.Description), 
            Years = ifelse(min(Begin.Year) == max(Begin.Year), sprintf("%s-%s", mean(as.numeric(Begin.Year)), mean(as.numeric(End.Year))), 
                           sprintf("%s-%s", min(as.numeric(Begin.Year)), max(as.numeric(End.Year)))),
            Raw.Table.Names = paste0(unique(Table), collapse=", ")
    ) %>%
    mutate_at("New.Variable.Name", ~ifelse(is.na(.), Raw.Variable.Names, .)) %>%
    unique()
  
  # Compile table of missing variables
  missing_table <- lapply(names(tab_data.l), function(tab) tab_data.l[[tab]]$missing_table) %>% 
    do.call(rbind.data.frame, .) %>% 
    filter(Variable != "NONE") %>%
    unique() 
  
  ## Save error log
  error_log <- error_log %>% do.call(rbind.data.frame, .)
  
  ## Return list of outputs
  return(list(data_table=data_by_cat.df,
              var_summary=var_summary,  
              var_missingness = missing_table,
              long_data_tables = if(length(long_tabs)>1) {long_tabs} else {"None"},
              error_log = error_log, 
              raw_data_tables.l=tab_data.l))
}


## ===========================================================
## Apply build_nhanes_table over each variable category 
## ===========================================================

## Load project data dictionary to grab years, tables & variables of interest
project_datadict <- readxl::read_xlsx("./nhanes_pullrequest_IndVsArea_07.30.2026.xlsx")

## Demographics data ---------------------
demo_tables.l <- build_nhanes_table(cat = "DEMO", datadict = project_datadict)
saveRDS(demo_tables.l, "../data/raw/nhanes_demo_tables.rds") 

## Laboratory data -----------------------
lab_tables.l <- build_nhanes_table(cat = "LABORATORY", datadict = project_datadict) 
saveRDS(lab_tables.l, "../data/raw/nhanes_lab_tables.rds")

## Exam data ---------------------------
exam_tables.l <- build_nhanes_table(cat = "EXAM", datadict = project_datadict)
saveRDS(exam_tables.l, "../data/raw/nhanes_exam_tables.rds")

## Questionnaire data ------------------
quest_tables.l <- build_nhanes_table(cat = "QUESTIONNAIRE", datadict = project_datadict)
saveRDS(quest_tables.l, "../data/raw/nhanes_quest_tables.rds")

## ====================================
## Create complete NHANES dataframe 
## ======================================

table_cats <- c("demo", "lab", "exam", "quest")
names(table_cats) <- names(data_categories.labs)[1:4]

nhanes_tables_raw.l <- lapply(table_cats, function(cat) {
  readRDS(sprintf("../data/raw/nhanes_%s_tables.rds", cat)) 
}) ; names(nhanes_tables_raw.l) <- table_cats

demo_tables.l <- nhanes_tables_raw.l$demo
lab_tables.l <- nhanes_tables_raw.l$lab
exam_tables.l <- nhanes_tables_raw.l$exam
quest_tables.l <- nhanes_tables_raw.l$quest

nhanes_vars_datadict <- lapply(table_cats, function(cat) {
  nhanes_tables_raw.l[[cat]][["var_summary"]] %>% mutate(Category = cat, .before=1)
}) %>% do.call(rbind.data.frame, .)


################################################################################
## Prepare NHANES dietary data, to build AHEI and HEI diet scores
################################################################################

## ===================================================================
## Load NHANES dietary data & apply build_nhanes_dietaryindex 
## ===================================================================

#remove.packages("dietaryindex")
#remotes::install_github("jamesjiadazhan/dietaryindex", force = TRUE)
library(dietaryindex)

## Load all dietary_data as a list ----------------------
dr1iff_files <- c(nhanes_tables_all %>% filter(grepl("DR1IFF", Data.File.Name)) %>% pull(Data.File.Name))
nhanes_dr1iff.l <- lapply(dr1iff_files, nhanes) ; names(nhanes_dr1iff.l) <- dr1iff_files

dr2iff_files <- c(nhanes_tables_all %>% filter(grepl("DR2IFF", Data.File.Name)) %>% pull(Data.File.Name))
nhanes_dr2iff.l <- lapply(dr2iff_files, nhanes) ; names(nhanes_dr2iff.l) <- dr2iff_files

dr1tot_files <- c(nhanes_tables_all %>% filter(grepl("DR1TOT", Data.File.Name)) %>% pull(Data.File.Name))
nhanes_dr1tot.l <- lapply(dr1tot_files, nhanes) ; names(nhanes_dr1tot.l) <- dr1tot_files

dr2tot_files <- c(nhanes_tables_all %>% filter(grepl("DR2TOT", Data.File.Name)) %>% pull(Data.File.Name))
nhanes_dr2tot.l <- lapply(dr2tot_files, nhanes) ; names(nhanes_dr2tot.l) <- dr2tot_files

nhanes_nutrient_data.l <- list(dr1iff=nhanes_dr1iff.l, dr2iff=nhanes_dr2iff.l, dr1tot=nhanes_dr1tot.l, dr2tot=nhanes_dr2tot.l)
saveRDS(nhanes_nutrient_data.l, "../data/raw/nhanes_nutrient_tables.rds")
#nhanes_nutrient_data.l <- readRDS("../data/raw/nhanes_nutrient_tables.rds")


# =========================================================================
## Write wrapper function to build AHEI and HEI diet scores
# =========================================================================

build_nhanes_dietaryindex <- function(index, years = "all") {
  
  # 1. Check dietary index argument
  valid_indices <- c("AHEI", "HEI", "HEI2015", "HEI2020", "DII", "EDII")
  if (!toupper(index) %in% valid_indices) {
    stop(sprintf("Invalid index '%s'. Must be one of: %s", index, paste(valid_indices, collapse = ", ")))
  }
  
  # 2. Map exam cycles & label with exam codes
  all_cycles <- c("_D", "_E", "_F", "_G", "_H", "_I", "_J", "P_")
  names(all_cycles) <- c("0506","0708", "0910", "1112", "1314", "1516", "1718", "1720")
  diet_sample_weights <- c("WTDRD1", "WTDR2D")
  
  if (years == "all") { 
    cycles <- all_cycles ; #diet_sample_weights <- c(diet_sample_weights, "WTDRD1PP", "WTDR2DPP") 
  } else {
    cycles <- all_cycles[years]}
  
  # Pre-define dietary data files
  fped_files <- list(driff = c("fped_dr1iff_", "fped_dr2iff_"), 
                     drtot = c("fped_dr1tot_", "fped_dr2tot_"))
  
  # 3. Iterate over each exam cycle
  index.l <- lapply(1:length(cycles), function(i) {
    
    cycle_name <- names(cycles)[i]
    cycle_code <- cycles[[i]]
    cat(sprintf("CALCULATING | %s for cycle %s \n", toupper(index), names(cycles)[i]))
    
    # Process DEMO file and re-code RIAGNDR as 1=Male/2=Female
    demo_file <- ifelse(cycle_code == "P_", "P_DEMO", paste0("DEMO", cycle_code))
    demo <- demo_tables.l$raw_data_tables.l[[demo_file]]$raw_table %>% 
      select("SEQN", "RIAGENDR", "RIDAGEYR", starts_with("SD")) %>% 
      mutate_at("RIAGENDR", ~as.numeric(case_when(
        .=="Male"~1, .=="Female"~2))
        )
    
    # ============================
    # AHEI Calculation
    # ============================
    if(toupper(index) == "AHEI") {
      load("../data/raw/FPED_files/SSB_FNDDS_1718.rda")
      
      # Load FPED datasets for each cycle
      cat("READING | fped_dr1iff & fped_dr2iff \n")
      fped_dr1iff <- read_sas(sprintf("../data/raw/FPED_files/%s%s.sas7bdat", 
                                      fped_files$driff[1], cycle_name)) %>% select(-"RIAGENDR")
      fped_dr2iff <- read_sas(sprintf("../data/raw/FPED_files/%s%s.sas7bdat", 
                                      fped_files$driff[2], cycle_name)) %>% select(-"RIAGENDR")
      
      # Grab NUTRIENT information for each cycle
      cat("DOWNLOADING | nutrient_dr1iff & nutrient_dr2iff \n")
      nutrient_dr1iff_file <- as.character(
        ifelse(cycle_code == "P_", "P_DR1IFF", paste0("DR1IFF", cycle_code))
        )
      
      nutrient_dr1iff <- nhanes_nutrient_data.l$dr1iff[[nutrient_dr1iff_file]] %>% 
        left_join(demo, by = "SEQN") %>% 
        mutate_at("DR1DRSTZ", ~case_when(
          .=="Reliable and met the minimum criteria"~1, 
          .=="Reported consuming breast-milk"~0))
      
      nutrient_dr2iff <- nhanes_nutrient_data.l$dr2iff[[
        gsub("1", "2", nutrient_dr1iff_file)]] %>% 
        left_join(demo, by = "SEQN") %>%
        mutate_at("DR2DRSTZ", ~case_when(
          .=="Reliable and met the minimum criteria"~1, 
          .=="Reported consuming breast-milk"~0))
      
      index_data <- dietaryindex::AHEI_NHANES_FPED(
        FPED_IND_PATH = fped_dr1iff, NUTRIENT_IND_PATH = nutrient_dr1iff,
        FPED_IND_PATH2 = fped_dr2iff, NUTRIENT_IND_PATH2 = nutrient_dr2iff,
        SSB_code = SSB_FNDDS_1718) %>%
        left_join(fped_dr1iff %>% select(SEQN, starts_with("WTDR")) %>% 
                    distinct(), by="SEQN")
      
      return(index_data)
    }
    
    # =====================================
    # HEI (2015/2020) or DII Calculation
    # =====================================
    if(grepl("HEI|DII", toupper(index))) {
      
      # Load FPED datasets for each cycle
      cat("READING | fped_dr1tot & fped_dr2tot \n")
      
      fped_dr1tot <- read_sas(sprintf(
        "../data/raw/FPED_files/%s%s.sas7bdat", fped_files$drtot[1], cycle_name)) %>% 
        select(-"RIAGENDR", "RIDAGEYR")
      
      fped_dr2tot <- read_sas(sprintf(
        "../data/raw/FPED_files/%s%s.sas7bdat", fped_files$drtot[2], cycle_name)) %>% 
        select(-"RIAGENDR", "RIDAGEYR")
      
      # Load NUTRIENT information for each cycle
      cat("DOWNLOADING | nutrient_dr1tot & nutrient_dr2tot \n")
      
      nutrient_dr1tot_file <- ifelse(cycle_code == "P_", "P_DR1TOT", 
                                     paste0("DR1TOT", cycle_code))
      
      nutrient_dr1tot <- nhanes_nutrient_data.l$dr1tot[[nutrient_dr1tot_file]] %>% 
        left_join(demo, by="SEQN") %>% 
        mutate_at("DR1DRSTZ", ~case_when(
          .=="Reliable and met the minimum criteria"~1, 
          .=="Reported consuming breast-milk"~0))
      
      nutrient_dr2tot <- nhanes_nutrient_data.l$dr2tot[[gsub("1","2",nutrient_dr1tot_file)]] %>% 
        left_join(demo, by="SEQN") %>%
        mutate_at("DR2DRSTZ", ~case_when(
          .=="Reliable and met the minimum criteria"~1, 
          .=="Reported consuming breast-milk"~0))
    
      ## HEI Calculation -----------------------
      if(grepl("HEI", toupper(index))) {
       index_data <- HEI2015_NHANES_FPED(
          FPED_PATH = fped_dr1tot, NUTRIENT_PATH = nutrient_dr1tot,
          FPED_PATH2 = fped_dr2tot, NUTRIENT_PATH2 = nutrient_dr2tot,
          DEMO_PATH = demo) %>%
          left_join(fped_dr1tot %>% select(SEQN, starts_with("WTD")) %>% 
                      distinct(), by="SEQN")
      }
      
      ## DII Calculation --------------------
      if(grepl("DII", toupper(index))) {
        
        data("DII_OTHER_INGREDIENTS_day1")
        data("DII_OTHER_INGREDIENTS_day2")
        
        index_data <-  DII_NHANES_FPED(
          FPED_PATH = fped_dr1tot, NUTRIENT_PATH = nutrient_dr1tot,
          FPED_PATH2 = fped_dr2tot, NUTRIENT_PATH2 = nutrient_dr2tot,
          OTHER_INGREDIENTS1 = DII_OTHER_INGREDIENTS_day1, 
          OTHER_INGREDIENTS2 = DII_OTHER_INGREDIENTS_day2, 
          DEMO_PATH = demo) %>% 
          left_join(fped_dr1tot %>% select(SEQN, starts_with("WTD")) %>% 
                      distinct(), by = "SEQN")
      }
      
      return(index_data)
      
    }
  })

  index_data <- index.l %>% do.call(bind_rows, .)
  return(index_data)
  
}


## =============================================
## Build tables with diet quality indices 
## =============================================

diet_weights <- c("WTDRD1", "WTDR2D", "WTDRD1PP", "WTDR2DPP")

# Calcualte HEI and AHEI -------------
nhanes_dietindices.l <- lapply(c("hei", "ahei"), function(index) {
  build_nhanes_dietaryindex(index=toupper(index), years="all") %>%
    left_join(demo_tables.l$data_table %>% select(Years, SEQN), by = "SEQN") 
}) ; names(nhanes_dietindices.l) <- c("hei", "ahei")

# Add DII ---------------
nhanes_dietindex_dii <- build_nhanes_dietaryindex(index="DII", years="all") %>%
  left_join(demo_tables.l$data_table %>% select(Years, SEQN), by = "SEQN") %>% 
  rename_with(~paste0("DII_", .), -c(SEQN, Years, diet_weights, DII_ALL, DII_NOETOH))

# Merge & save diet indices ---------
nhanes_dietindices.l <- c(nhanes_dietindices.l, dii=list(nhanes_dietindex_dii))
saveRDS(nhanes_dietindices.l, "../data/raw/nhanes_dietindex_tables.rds")

nhanes_dietindices <- nhanes_dietindices.l$hei %>% full_join(
  nhanes_dietindices.l$ahei, by=c("SEQN", "Years", diet_weights)) %>% 
  full_join(nhanes_dietindices.l$dii, by=c("SEQN", "Years", diet_weights)) 
nhanes_dietindices %>% fwrite("../data/raw/nhanes_dietindices_raw.csv")


## =========================================================
## Additional Variable Sets Added After Initial Merge
## =========================================================

## Additional Questionnaire Variables ============
## Alcohol intake frequency -----------------
alch_tables.l <- build_nhanes_table(
  cat = "QUESTIONNAIRE", datadict = project_datadict %>% 
    filter(grepl("q_alc", Varname))
  ) ; saveRDS(alch_tables.l, file = "../data/raw/nhanes_quest_alch_tables.rda")


## Health Care interactions --------------
hc_tables.l <- build_nhanes_table(
  cat = "QUESTIONNAIRE", datadict = project_datadict %>% 
    filter(grepl("q_hc", Varname))
  ) ; saveRDS(hc_tables.l, file = "../data/raw/nhanes_quest_hc_tables.rds")


# Any physical activity (MVPA or Transportation)
anypa_tables.l <- build_nhanes_table(
  cat="QUESTIONNAIRE", datadict = project_datadict %>%
    filter(Varname %in% c("q_mvpa_anyvpa", "q_mvpa_anympa"))
  ) ; saveRDS(anypa_tables.l, "../data/raw/nhanes_quest_anypa_tables.rds")


## Employment type ------------------
employ_tables.l <- build_nhanes_table(
  cat = "QUESTIONNAIRE", datadict = project_datadict %>% 
    filter(grepl("q_work|q_nowork", Varname) & Varname != "q_phq_work")
  ) ; saveRDS(employ_tables.l, file = "../data/raw/nhanes_quest_employ_tables.rds")


## Taste/Smell Questionnaire ------------------
tasteq_tables.l <- build_nhanes_table(
  cat = "QUESTIONNAIRE", datadict = project_datadict %>% 
    filter(grepl("q_tastechange|q_csq", Varname))
) ; saveRDS(tasteq_tables.l, file = "../data/raw/nhanes_quest_taste_tables.rds")


add_quest_tables <- full_join(
  alch_tables.l$data_table, hc_tables.l$data_table, by = c("SEQN", "Years")) %>% 
  full_join(anypa_tables.l$data_table, by = c("SEQN", "Years")) %>% 
  full_join(employ_tables.l$data_table, by = c("SEQN", "Years")) %>% 
  full_join(tasteq_tables.l$data_table, by = c("SEQN", "Years"))
  
add_quest_varsummary.l <- list(
  alch_tables.l$var_summary, hc_tables.l$var_summary, 
  anypa_tables.l$var_summary, employ_tables.l$var_summary,
  tasteq_tables.l$var_summary)

################################################################################
## Build complete NHANES dataframe with all requested variables, over time
################################################################################

nhanes_data_raw <- full_join(
  demo_tables.l$data_table, lab_tables.l$data_table, by=c("SEQN", "Years")) %>% 
  full_join(exam_tables.l$data_table, by=c("SEQN", "Years")) %>%
  full_join(quest_tables.l$data_table, by=c("SEQN", "Years")) %>%
  full_join(nhanes_dietindices, by=c("SEQN", "Years")) %>%
   
  ## Remove overlapping & add additional QUESTIONNAIRE variables
  select(-starts_with(c("q_alc", "q_hc", "q_work"))) %>% 
  full_join(add_quest_tables, by = c("SEQN", "Years")) %>% 
  
  ## Filter to 1999 -- 2020 & delete 2017-2018 (redundant to 2017-2020) -----------
  filter(!Years %in% c("2017-2018", "2021-2023")) 

nhanes_data_raw %>% fwrite(., "../data/raw/nhanes_data_raw.csv")


## =========================================================
## Build NHANES variable index table & search function 
## =========================================================

nhanes_vars_datadict <- lapply(table_cats, function(cat) {
  nhanes_tables_raw.l[[cat]][["var_summary"]] %>% 
    mutate(Category = cat, .before=1)
  }) %>% do.call(rbind.data.frame, .) %>% 
  # Add additional quest variables, loaded separately 
  rbind.data.frame(
    add_quest_varsummary.l %>% do.call(rbind.data.frame, .)  %>% 
      mutate(Category = "quest", .before=1)
  )

nhanes_vars_datadict %>% fwrite("../data/raw/nhanes_vars_datadict.csv")

## Write wrapper functions to search for NHANES variables ---------------
search_nhanes_variables <- function(search_keywords, 
                                    search_columns = "Variable.Description",
                                    search_as = "AND", ignore_case=T) {
  
  # Check that nhanes_vars_all df is loaded into Environment
  if(!exists("nhanes_vars_all")) {
    nhanes_varlist_all <- fread("../data/raw/nhanes_varlist_all.csv")
  } 
  
  # Choose which rows have BOTH or EITHER var_keywords
  match_keywords <- sapply(search_keywords, function(word) grepl(
    word, nhanes_vars_all[[search_columns]], ignore.case = T))
  
  return_rows <- if (toupper(search_as) == "AND") {
    rowSums(match_keywords) == length(search_keywords)} else {
      rowSums(match_keywords) > 0 }
  
  nhanes_vars_all[return_rows, ] %>%
    arrange(Variable.Name, Table) 
}



## EOF

