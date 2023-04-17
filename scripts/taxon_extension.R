library("openxlsx")
library(tidyverse)

source("scripts/functions/TAXREF.R")

# Import data completed from Flore Méditerranéenne (Tison) ####
sp_info <- read.xlsx("data/species/Fichier_sp_flores/Species_completed_Tison.xlsx", startRow = 1, colNames = TRUE)

# Add info from TAXREF ####
taxref <- read.table("data/TAXREF_v16/TAXREFv16.txt",header=T,sep = "\t")

sp_info_scientificName <- sp_info %>% 
  mutate(scientificName = map_chr(Species,get_scientificName,taxref)) 

sp_info_name_URL <- sp_info_scientificName %>% 
  mutate(acceptedNameUsageID = map_chr(Species,get_CD_NOM,taxref))

sp_info_name_URL_id <- sp_info_name_URL %>% 
  mutate(nameAccordingTo = map_chr(Species,get_URL,taxref))

# checké avec TAXREF: les familles botaniques sont bien renseignées.

write.csv2(sp_info_name_URL_id %>% 
             select(-Remarques),"output/taxon_extension.csv",row.names=F,fileEncoding = "Latin1")

