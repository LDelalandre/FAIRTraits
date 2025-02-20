library(tidyverse)

source("scripts/functions/TAXREF.R")

taxref <- read.table("data/TAXREF_v16/TAXREFv16.txt",header=T,sep = "\t")

sp_ann_bot <- read.csv2("data/Delalandre2025AnnBot_species_info.csv")
sp_ann_bot_taxref <- sp_ann_bot %>% 
  mutate(nameAccordingTo = map_chr(scientificName_short,get_CD_NOM,taxref))  %>% 
  mutate(nameAccordingTo = paste0("taxRefv16_CD_NOM: ",nameAccordingTo)) %>% 
  mutate(scientificNameID = map_chr(scientificName_short,get_URL,taxref)) %>% 
  rename(species = scientificName_short,
         breeding_system = Breeding.system,
         family = Family) %>% 
  dplyr::select(species, code_sp, family, scientificName, scientificNameID, nameAccordingTo) %>% 
  rename(scientificNameShort = species)
  
j.mN <- c(3.75, 5.69, 4.63, 4.45, 
          4.35, 3.67, 4.19, 6.02, 4.11, 
          4.62, 5.29, 3.82, 6.61, 6.43, 
          5.65, 5.79, 5.54)

sp_ann_bot_taxref$j.mN <- j.mN

write.csv2(sp_ann_bot_taxref,
           "output/Delalandre2025AnnBotSpecies.csv",
           row.names = F,
           fileEncoding = "UTF-8")
