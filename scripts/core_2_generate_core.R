library(tidyverse)

# This script:
# - imports a csv file with data in row (output of `core_1_import_data.R`), 
# - updates trait names and the values in some other columns (Plot, Treatment)

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
  mutate(Treatment = paste("Treatment",Treatment,sep="_"))


# Traits ####
## Update names of traits and entity ####
correspondence_traits_old_new <- MoFTraits %>% 
  select(verbatimTraitName_old,traitEntityDataFile,verbatimTraitName_new,traitEntityValid) %>% 
  unique() # some trait names are duplicated because of different measurement methods in MoFTraits

TIDY4.0 <- TIDY3 %>%
  rename(verbatimTraitName_old = verbatimTraitName) %>% 
  rename(traitEntityDataFile = traitEntity) %>% 
  # correct typos
  mutate(verbatimTraitName_old = str_replace_all(verbatimTraitName_old, " ", "")) %>% 
  mutate(verbatimTraitName_old = str_replace_all(verbatimTraitName_old, " ", "")) %>% 
  mutate(verbatimTraitName_old = case_when(verbatimTraitName_old == "R_Cellulose" ~"R_cellulose",
                                           verbatimTraitName_old == "R_prod" ~ "Rprod",
                                           verbatimTraitName_old == "R_DM_84" ~ "R_DM_ab_84",
                                           verbatimTraitName_old == "R_DM_0" ~ "R_DM_ab_0",
                                           TRUE ~ verbatimTraitName_old)) %>%  
  # update pbs organ Graminoid (sheath) and others (stem)...
  left_join(correspondence_traits_old_new)
  

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

## Select traits to keep ####

# Names of traits that we keep
trait_names <- MoFTraits %>% 
  filter(inFinalFile == "yes") %>% 
  pull(verbatimTraitName_new)

TIDY4 <- TIDY4.0 %>% 
  # select(-verbatimTraitName_old) %>%
  rename(verbatimTraitName = verbatimTraitName_new) %>%
  rename(traitEntity = traitEntityValid) %>% 
  # filter(!is.na(verbatimTraitName))  # idem que ligne en-dessous
  filter(verbatimTraitName %in% trait_names)  # remove traits that are not listed in MeasurementOrFact(traits)

data.table::fwrite(TIDY4,"output/TIDY_trait_entity_updated.csv")
# write.table(TIDY4 ,"output/TIDY_trait_entity_updated.csv",fileEncoding = "latin1",row.names=F,sep="\t",dec = ".")



dim(TIDY2)
dim(TIDY3)
dim(TIDY4.0)
dim(TIDY4)


