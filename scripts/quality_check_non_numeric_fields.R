library(tidyverse)

# This scripts groups steps used for veryfying the quality of the structure of the data
# (checks of trait values to be found in another script)

# Import data ####
core <-  data.table::fread("output/TIDY_occurrenceID.csv",encoding = "UTF-8")

# Class of columns ####
# check the class of the different columns
str(core) # everything is a character, except verbatimTraitValue, info on plot altitude, latitude and longitude, and date

class(core$verbatimTraitValue)

# Values of attributes ####
# Check that the values taken by each attribute (= within each column)
# remove numeric columns, and occurrence
col_names <- core %>% 
  select(!(is.numeric)) %>% 
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
list_values_per_column$traitEntity  %>% sort()

list_values_per_column$variableType
list_values_per_column$traitID %>% sort()

list_values_per_column$Quality %>% sort()

list_values_per_column$basisOfRecord %>% sort()
list_values_per_column$measurementMethod %>% sort() 
list_values_per_column$samplingProtocol %>% sort() # pourquoi des fois : "All stems or sheaths collected on the \"\"\"\"\"\"\"\"cored\"\"\"\"\"\"\"\" individual"


## species ####
list_values_per_column$Species %>% sort()

## environment ####
list_values_per_column$traitPlot %>% sort()
list_values_per_column$Treatment %>% sort()
list_values_per_column$envPlot %>% sort()
list_values_per_column$countryCode %>% sort()

## data structure ####
list_values_per_column$Rep %>% sort()
list_values_per_column$feuillet %>% sort()

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
empty_quality <- core %>% filter(Quality == "") %>% pull(verbatimOccurrenceID) # Quality empty : many (110460) entries !
empty_method <- core %>% filter(measurementMethod == "") %>% pull(verbatimOccurrenceID) # measurementMethod empty
empty_determinedBy <- core %>% filter(measurementDeterminedBy == "") %>% pull(verbatimOccurrenceID) # measurementMethod empty

