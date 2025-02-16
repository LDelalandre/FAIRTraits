library(tidyverse)

# This script :
# - updates trait names
# - adds information on trait measurement and sampling method to the core of the database

# Import data ####
# TIDY4 <-  read.csv2("output/TIDY_trait_entity_updated.csv",fileEncoding = "latin1",sep="\t",dec = ".")
TIDY4 <- data.table::fread("output/TIDY_2_corrected_typos.csv",encoding="UTF-8") %>% 
  rename(verbatimTraitName_old = verbatimTraitName) %>% 
  rename(traitEntityDataFile = traitEntity)

MoFTraits <- readxl::read_excel("data/MoFTraitsFull_may2024_clean.xlsx", sheet = "MoFTraitsFull") %>% 
  rename(verbatimTraitName_new = verbatimTraitName_new_new) %>% 
  mutate(Site = if_else (Site == "HGM", "Hautes Garrigues",Site)) %>% 
  mutate_all(trimws)


# Combine core data with MoFTraits ####
# (information on traits : names, measurement method, etc.)

# This is performed separately for traits which were measured the same way whatever
# the site, and the other traits

## traits whose samplingProtocol and measurementMethod differ depending on the site ####
info_differingtraits_tomerge <- MoFTraits %>% 
  filter(!(Site == "All"))

TIDY4_differingtraits_completed <- TIDY4 %>% 
  left_join(info_differingtraits_tomerge, by = c("Site","verbatimTraitName_old","traitEntityDataFile"))

# Only the completed lines
DF_differingtraits_completed <- TIDY4_differingtraits_completed %>% 
  filter(!is.na(traitName)) # les lignes qui ont été bien complétées

# Only lines to be completed = where traitName is NA so far
DF_commontraits <- TIDY4_differingtraits_completed %>% 
  filter(is.na(traitName)) %>% 
  select(-c( traitName,   samplingProtocol, measurementMethod,
             inFinalFile,traitEntityAbbreviation,
             verbatimTraitName_new, traitEntityValid,
             variableType,      entityCategory,    traitQuality,     
             verbatimTraitUnit, termSource,        localIdentifier,   traitID,basisOfRecord )) # remove the columns added by left-joining with MoFTraits !!


## traits whose samplingProtocol and measurementMethod are identical whatever the site ####
info_commontraits_tomerge <- MoFTraits %>% 
  filter(Site == "All") %>% 
  select(-Site) %>% 
  unique()

DF_commontraits_completed <- DF_commontraits %>% 
  left_join(info_commontraits_tomerge, by = c("verbatimTraitName_old","traitEntityDataFile"))

## Check before binding the two files ####
# Do the two completed data frames have the same columns?
setdiff(colnames(DF_commontraits_completed),colnames(DF_differingtraits_completed))
setdiff(colnames(DF_differingtraits_completed),colnames(DF_commontraits_completed ))

## Bind the two files ####
TIDY5 <- rbind(DF_differingtraits_completed,DF_commontraits_completed) %>% 
  rename(verbatimTraitName = verbatimTraitName_new)

# Quality check ####

## Was trait name  updated for every trait?
DF_commontraits_completed %>% 
  filter(is.na(traitName))

## Were methods described for every trait?
DF_commontraits_completed %>% 
  filter(is.na(measurementMethod))

# Does row number remain identical?
dim(TIDY4)
dim(TIDY5)
dim(TIDY5 %>% unique())

# Export ####
data.table::fwrite(TIDY5,"output/TIDY_3_MoFTraits.csv",sep="\t")

