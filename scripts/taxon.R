library("openxlsx")
library(tidyverse)

source("scripts/functions/TAXREF.R")

# Import data completed from Flore Méditerranéenne (Tison) ####
# sp_info <- read.xlsx("data/Species_completed_Tison.xlsx", startRow = 1, colNames = TRUE)
sp_info <- read.csv2("data/FAIRTraits_taxon_raw.csv")

# Add info from TAXREF ####
taxref <- read.table("data/TAXREF_v16/TAXREFv16.txt",header=T,sep = "\t")

sp_info_scientificName <- sp_info %>% 
  mutate(scientificName = map_chr(Species,get_scientificName,taxref)) 

sp_info_name_URL <- sp_info_scientificName %>% 
  mutate(nameAccordingTo = map_chr(Species,get_CD_NOM,taxref))  %>% 
  mutate(nameAccordingTo = paste0("taxRefv16_CD_NOM: ",nameAccordingTo))

sp_info_name_URL_id <- sp_info_name_URL %>% 
  mutate(scientificNameID = map_chr(Species,get_URL,taxref))

taxon_extension <- sp_info_name_URL_id %>% 
  select(-Remarques)

data.table::fwrite(taxon_extension,"output/FAIRTraits_Taxon.csv",sep="\t")
