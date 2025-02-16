library(tidyverse)
source("scripts/functions/TAXREF and TOP.R")

taxref <- read.table("data/TAXREF_v16/TAXREFv16.txt",header=T,sep = "\t")

table <- read.csv2("scripts/paper_annuals_in_situ/Table_species.csv")
table_completed <- table %>% 
  mutate(species = if_else(species=="Festuca christianii-bernardii","Festuca christiani-bernardi",species)) %>% 
  mutate(species = if_else(species=="Vicia sativa sativa","Vicia sativa subsp. sativa",species)) %>% 
  mutate(scientificName = map_chr(species,get_scientificName,taxref))  %>%
  mutate(taxonURL = map_chr(species,get_URL,taxref)) %>% 
  mutate(CD_nom = map_chr(species,get_CD_NOM,taxref)) 



table_final <- table_completed %>% 
  select(-X,-species) %>% 
  rename(Species = scientificName) %>% 
  select(Species,LifeForm1,LifeHistory,Intensive,Extensive,taxonURL) %>% 
  select(-c(Intensive,Extensive)) %>% 
  kableExtra::kable( escape = F,
                     col.names = c("Species", "Life Form (Raunk.)", "Life History",
                                   # "Presence (Int.)","Presence (Ext.)",
                                   "Taxon ID")) %>%
  kableExtra::kable_styling("hover", full_width = F)

cat(table_final, file = "scripts/paper_annuals_in_situ/Appendix_list_of_species.doc")

dim(table_completed)
