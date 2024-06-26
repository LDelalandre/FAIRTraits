library(tidyverse)

core <-  data.table::fread("output/TIDY_occurrenceID.csv",encoding = "UTF-8")

dupl<-DUPLcplet %>% 
  pull(verbatimOccurrenceID)

core %>% 
  filter(Site == "CRE_PDM" & feuillet == "Biomass_WholePlant_Root") %>% 
  filter(verbatimOccurrenceID %in% dupl) %>% 
  View

# Manque verbatimTraitName
core %>% 
  filter(verbatimOccurrenceID == "VEROPERS_CRE_PDM_CRP_MO3J01_HiN_2005_3_1_Rcc3_") %>% 
  View
