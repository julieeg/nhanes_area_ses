## Rscript to build NHANES table and variable summary tables


library(tidyverse) ; library(data.table)
library(nhanesA)

setwd("~/Documents/GitHub/nhanes_area_ses/run/")


################################################################################
## Build index of all NHANES tables/vars across time points (1999-2023)
################################################################################

#list data categories with labels
data_categories <- c("DEMO", "LABORATORY", "EXAM", "QUESTIONNAIRE", "DIET")
data_categories.labs <- c("Demographics"="DEMO", "Laboratory"="LABORATORY", 
                          "Examination"="EXAM", "Questionnaire"="QUESTIONNAIRE", 
                          "Dietary"="DIET")

## ==========================================================
## Make index of all nhanes TABLES, by category & year
## ==========================================================

## All NHANES Tables with SEQN (1999-2023) -------------------------------------
nhanes_tables_all <- nhanesSearch("Respondent sequence number", namesonly=F) %>%
  mutate(Category=data_categories.labs[Component], 
         Years=sprintf("%s-%s", Begin.Year, EndYear)) %>%
  select(Category, Component, Data.File.Name, Data.File.Description, 
         Begin.Year, "End.Year"=EndYear, Years) %>%
  arrange(Category, Begin.Year) ; dim(nhanes_tables_all) # 1540 7


## Manually download LAB tables missing from the CDC Manifest
nhanes_manifest <- nhanesManifest()
lab_p <- nhanesTables('LAB', 'P') %>% 
  rename(Data.File.Description = Data.File.Name) %>%
  mutate(Data.File.Name = gsub(" Doc", "", Doc.File))
lab_p$Data.File.Name[which(!lab_p$Data.File.Name %in% nhanes_tables_all$Data.File.Name)]

nhanes_addn_lab_tables <- rbind.data.frame(
  c("P_BIOPRO", "Standard Biochemistry Profile", "2017-2020"),
  c("BIOPRO_L", "Standard Biochemistry Profile", "2021-2023"),
  c("P_HSCRP", "High-Sensitivity C-Reactive Protein", "2017-2020"),
  c("HSCRP_L", "High-Sensitivity C-Reactive Protein", "2021-2023"),
  c("P_TCHOL", "Cholesterol Total", "2017-2020"),
  c("TCHOL_L", "Cholesterol Total", "2021-2023"),
  c("P_HDL", "Cholesterol - High - Density Lipoprotein (HDL)", "2017-2020"),
  c("HDL_L", "Cholesterol - High - Density Lipoprotein (HDL)", "2021-2023")
) ; names(nhanes_addn_lab_tables) <- c("Data.File.Name", "Data.File.Description", "Years") 
nhanes_addn_lab_tables <- nhanes_addn_lab_tables %>% 
  mutate(Category = "LABORATORY", Component = "Laboratory", .before=1) %>% 
  mutate(
    Begin.Year = gsub("-.*","", Years), End.Year = gsub(".*-","", Years),
    .before="Years"
  )

## Combine all nhanes tables & save ------------------
nhanes_tables_all <- nhanes_tables_all %>% bind_rows(nhanes_addn_lab_tables)
nhanes_tables_all %>% fwrite("../data/index/nhanes_tablelist_all.csv")


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


## Gather NHANES variables using the Manifest function -------------------
manifest_nhanes_vars <- nhanesManifest("variables") %>% 
  mutate(Years = paste0(BeginYear, "-", EndYear))
manifest_nhanes_vars %>% fwrite("../data/index/nhanes_manifest_variables.csv")

## EOF
## Last Updated: 09-17-2026