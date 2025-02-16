library(tidyverse)

# This scripts groups steps used for veryfying the quality of the structure of the data
# (checks of trait values to be found in another script)

# Import data ####
core <-  data.table::fread("output/TIDY_6_ID_field_campaign.csv",encoding = "UTF-8")

# Class of columns ####
# check the class of the different columns
str(core) # everything is a character, except verbatimTraitValue, info on plot altitude, latitude and longitude, and date

class(core$verbatimTraitValue)

# Values of attributes ####
# Check that the values taken by each attribute (= within each column)
# remove numeric columns, and occurrence
col_names <- core %>% 
  select(!(where(is.numeric))) %>% 
  select(- c("verbatimOccurrenceID" ,"verbatimOccurrenceID_sample" ,"verbatimOccurrenceID_population" )) %>% 
  colnames()

get_column_values <- function(col_name){
  core %>% 
    pull(col_name) %>% 
    unique()
}

list_values_per_column <- lapply(X = col_names,FUN = get_column_values)
names(list_values_per_column) <- col_names


## trait ####
list_values_per_column$verbatimTraitName %>% sort()
list_values_per_column$verbatimTraitName_old %>% sort()
list_values_per_column$traitName %>% sort()

list_values_per_column$verbatimTraitUnit %>% sort()

list_values_per_column$traitEntityDataFile  %>% sort()
list_values_per_column$traitEntityValid  %>% sort()

list_values_per_column$variableType
list_values_per_column$traitID %>% sort()

list_values_per_column$traitQuality %>% sort()

list_values_per_column$basisOfRecord %>% sort()
list_values_per_column$measurementMethod %>% sort() 
list_values_per_column$samplingProtocol %>% sort() # pourquoi des fois : "All stems or sheaths collected on the \"\"\"\"\"\"\"\"cored\"\"\"\"\"\"\"\" individual"


## species ####
list_values_per_column$Species %>% sort()

## environment ####
list_values_per_column$traitPlot %>% sort()
list_values_per_column$treatmentOriginal %>% sort()
core %>% filter(treatmentOriginal == "")

list_values_per_column$envPlot %>% sort()
list_values_per_column$countryCode %>% sort()

## data structure ####
list_values_per_column$Rep %>% sort()
list_values_per_column$feuillet %>% sort()

## Date and hour

# Control that Year, Month, Day, is (correctly) filled in

# NAs ?
core %>% filter(is.na(Year)) %>% View # /!\ pb pheno Marie-Laure
core %>% filter(is.na(Month)) %>% View
core %>% filter(is.na(Day))

# Check if impossible values:
core %>% pull(Year) %>% unique() %>% sort()
core %>% pull(Month) %>% unique() %>% sort()
core %>% pull(Day) %>% unique() %>% sort()

## metadata ####
list_values_per_column$measurementDeterminedBy
list_values_per_column$nameOfProject
list_values_per_column$Site
list_values_per_column$Block


# Bivariate ####

# Site and country
core %>% select(Site,countryCode) %>% unique()

# Site and plot
core %>% select(Site,traitPlot) %>% unique()


# Problems ####

## fields empty ####
empty_block <- core %>% filter(Block == "") %>% pull(verbatimOccurrenceID) # Block empty
empty_project <- core %>% filter(nameOfProject == "") %>% pull(verbatimOccurrenceID) # nameOfProject empty
empty_determinedBy <- core %>% filter(measurementDeterminedBy == "") %>% pull(verbatimOccurrenceID) # measurementMethod empty

core_empty_fields <- core %>% 
  filter(verbatimOccurrenceID %in% c(empty_block,empty_project,empty_determinedBy))
core_empty_fields %>% dim()

# due to pb in MoFTraits (?) : none observed
empty_quality <- core %>% filter(traitQuality == "") %>% pull(verbatimOccurrenceID) # Quality empty : many (110460) entries !
empty_method <- core %>% filter(measurementMethod == "") %>% pull(verbatimOccurrenceID) # measurementMethod empty

core_empty_fields_MoFTraits <- core %>% 
  filter(verbatimOccurrenceID %in% c(empty_quality,empty_method))
core_empty_fields_MoFTraits %>% dim()

# Are there empty fields in any column?
DIM <- c()
for (i in c(1:length(colnames(core)))){
  fcol <- colnames(core)[i]
  dim_empty <- core %>% filter(get(fcol) == "") %>% dim()
  DIM <- c(DIM,dim_empty[1])
}
DIM
data.frame(colnames(core),DIM)



## Into spreadsheets ####

source("scripts/functions/regenerate_spreadsheet.R")

SITES <- core_empty_fields %>% 
  pull(siteName) %>% 
  unique()

# Supprimer les fichiers excel avant (le code ne parvient pas à les écraser)
for (fsite in SITES){
  # names of spreadsheets
  names_spreadsheet <- core_empty_fields %>%
    filter(siteName == fsite) %>% 
    arrange(feuillet) %>% 
    pull(feuillet) %>% 
    unique()
  # mutate(site_feuillet = paste(Site,feuillet,sep = "_")) %>% 
  # mutate(site_feuillet = str_sub(site_feuillet,start = 1, end = 31)) %>%  # # maximum length of excel worksheet names
  # pull(site_feuillet)
  
  # list of spreadsheets to export
  list_tidy_spreadsheet <- core_empty_fields %>% 
    filter(siteName == fsite) %>% 
    group_split(siteName,feuillet) %>% 
    setNames(names_spreadsheet)
  
  list_spreadsheet <- lapply(list_tidy_spreadsheet,regenerate_spreadsheet)
  
  lst_data <- list_spreadsheet
  wb <- openxlsx::createWorkbook()
  purrr::imap(
    .x = lst_data,
    .f = function(df, object_name) {
      openxlsx::addWorksheet(wb = wb, sheetName = object_name)
      openxlsx::writeData(wb = wb, sheet = object_name, x = df)
    }
  )
  saveWorkbook(wb = wb, file = paste0("output/WorkingFiles/empty_fields",fsite,".xlsx"))
}

