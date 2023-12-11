library(tidyverse)

# This script:
# - imports a csv file with data in row (output of `core_1_import_data.R`), 
# - updates the values in some columns (Plot, Treatment), and corrects typos

# TIDY2 <- read.csv2("output/TIDY.csv",fileEncoding = "latin1",sep="\t",dec = ".")
TIDY2 <- data.table::fread("output/TIDY.csv",encoding="UTF-8")

MoFTraits <- read.csv2("data/MoFTraitsFull.csv",fileEncoding = "latin1") %>% 
  mutate_all(trimws) %>% 
  mutate(verbatimTraitName_new = case_when(verbatimTraitName_new == "RDM _ab_0" ~ "RDM_ab_0",
                                           verbatimTraitName_new == "RDM _ab_84" ~ "RDM_ab_84",
                                           TRUE ~ verbatimTraitName_new)) %>% 
  filter(!verbatimTraitName_new == "check in data file")
# colnames(MoFTraits)[1] <- gsub('^...','',colnames(MoFTraits)[1]) # remove ï.., qu'Eric a mis je sais pas comment.
# Enlever les espaces qui se sont glissés un peu partout dans les noms de traits
# MoFTraits <- MoFTraits %>% 
#   mutate(verbatimTraitName_old = str_replace_all(verbatimTraitName_old, " ", "")) %>% 
#   mutate(verbatimTraitName_new = str_replace_all(verbatimTraitName_new, " ", "")) %>% 
#   mutate(verbatimTraitName_old = str_replace_all(verbatimTraitName_old, "Â", ""))

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

data.table::fwrite(TIDY3,"output/TIDY_corrected_typos.csv")

#____________________________________________________________________
# check 
dim(TIDY3)
dim(TIDY4.0)
correspondence_traits_old_new <- correspondence_traits_old_new %>% 
  mutate(test = paste(traitEntityDataFile , verbatimTraitName_old, sep = "_"))

correspondence_traits_old_new[which(duplicated(correspondence_traits_old_new$test)),] %>% 
  arrange(test)

correspondence_traits_old_new[which(duplicated(correspondence_traits_old_new$verbatimTraitName_new)),] %>% 
  arrange(verbatimTraitName_new)

correspondence_traits_old_new %>% 
  select(traitEntityDataFile , verbatimTraitName_old) %>%
  # unique() %>%
  dim()

TIDY4.0 %>% 
  filter(is.na(verbatimTraitName_new)) %>% View() # c'est normal, c'était déjà vide avant MoFTraitsFull
# (et les champs d'espèce et autres sont vides = pb d'Excel, ou bien le trait n'est pas retenu)


# TEMPORARY missing traits names ####
# Problème résolu
missing_in_moftraits <- TIDY4.0 %>% 
  filter(is.na(verbatimTraitName_new)) %>% # traits that did not match MoFTraits
  select(verbatimTraitName_old,traitEntityDataFile,traitEntityValid,Site,feuillet) %>%  # names of these traits
  unique() %>% 
  arrange(verbatimTraitName_old)
# missing_in_moftraits %>% View

write.csv2(missing_in_moftraits,"output/WorkingFiles/2023_07_12_missing_in_MoFTraits.csv",fileEncoding = "latin1",row.names=F)

vbtn <- missing_in_moftraits %>% pull(verbatimTraitName_old) %>% unique()
submof <- MoFTraits %>% 
  filter(verbatimTraitName_old %in%  vbtn) 

#____________________________________________________________________




dim(TIDY2)
dim(TIDY3)
