library(tidyverse)

# This script:
# - imports a csv file with one record per row (output of `core_1_import_data.R`), 
# - updates the values in some columns (Plot, Treatment), and corrects typos

# TIDY2 <- read.csv2("output/TIDY.csv",fileEncoding = "latin1",sep="\t",dec = ".")
TIDY2 <- data.table::fread("output/TIDY.csv",encoding="UTF-8")

# Operations on columns ####
TIDY3 <- TIDY2 %>% 
  # rename Entity
  rename(traitEntity = Entity) %>% 

  # Change values of Plot and Treatment
  mutate(Plot = case_when(Site == "La Fage" ~ paste("FAG",Plot,sep = "_"),
                          Site == "Cazarils" ~ paste("CAZ",Plot,sep = "_"),
                          Site == "PDM" ~ paste("CRP",Plot,sep = "_"),
                          Site == "O2LA" ~ paste("CRO",Plot,sep = "_"),
                          Site == "Garraf" ~ paste("GAR",Plot,sep = "_"),
                          Site == "Hautes Garrigues" ~ paste("HGM",Plot,sep = "_"),
                          Site == "Les Agros" ~ paste("AGR",Plot,sep = "_"))) %>% 
  mutate(Treatment = paste("Treatment",Treatment,sep="_")) %>% 
  
  # correct typos
  mutate(verbatimTraitName = str_replace_all(verbatimTraitName, " ", "")) %>% 
  mutate(verbatimTraitName = str_replace_all(verbatimTraitName, " ", "")) %>% 
  mutate(verbatimTraitName = case_when(verbatimTraitName == "R_prod" ~ "Rprod",
                                           verbatimTraitName == "R_DM_84" ~ "R_DM_ab_84",
                                           verbatimTraitName == "R_DM_0" ~ "R_DM_ab_0",
                                           TRUE ~ verbatimTraitName)) #verbatimTraitName == "R_Cellulose" ~"R_cellulose",

# Export ####
data.table::fwrite(TIDY3,"output/TIDY_corrected_typos.csv",sep="\t")








