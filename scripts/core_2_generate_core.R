library(tidyverse)

# This script imports data in row, and generates the core of the database, with verbatimOccurrenceIDs
# It also updates trait names and values in some other columns

# PBs :
# - Enormément de décimales dans les valeurs de traits ! 
# NB: c'est déjà le cas dans les fichiers bruts. Pourquoi ?
# --> Remonter l'historique, et voir à quel moment ça apparaît. Fausse manip pdt ouverture des fichiers excel ?
# Observé dès les versions de novembre ! (2021_11_02_LaFage_PlantTraitsDP_vp.xlsx, présents sur le MyCore)

TIDY2 <- read.csv2("output/TIDY.csv",fileEncoding = "latin1",sep="\t",dec = ".")

# Operations on columns ####
TIDY3 <- TIDY2 %>% 
  # rename Entity
  rename(traitEntity = Entity) %>% 

  # Change values of Plot and Treatment
  mutate(Plot = case_when(Site == "La Fage" ~ paste("FAG",Plot,sep = "_"),
                          Site == "Cazarils" ~ paste("CAZ",Plot,sep = "_"),
                          Site == "PDM" ~ paste("CRE",Plot,sep = "_"),
                          Site == "O2LA" ~ paste("CRE",Plot,sep = "_"),
                          Site == "Garraf" ~ paste("GAR",Plot,sep = "_"),
                          Site == "Hautes Garrigues" ~ paste("HGM",Plot,sep = "_"),
                          Site == "Les Agros" ~ paste("AGR",Plot,sep = "_"))) %>% 
  mutate(Treatment = paste("Treatment",Treatment,sep="_"))


# Traits ####
## Update names of traits ####
MoFTraits <- read.csv2("data/MoFTraits.csv",fileEncoding = "latin1")
correspondence_traits_old_new <- MoFTraits %>% 
  select(verbatimTraitName_old,verbatimTraitName_new) %>% 
  unique() # some trait names are duplicated because of different measurement methods in MoFTraits

TIDY4.0 <- TIDY3 %>%
  rename(verbatimTraitName_old = verbatimTraitName) %>% 
  left_join(correspondence_traits_old_new) 

#____________________________________________________________________
# TEMPORARY missing traits names ####
# TRAITS OF THE CORE ABSENT FROM MOF_traits (checker une dernière fois)
TIDY4.0 %>% 
  filter(is.na(verbatimTraitName_new)) %>% # traits that did not match MoFTraits
  pull(verbatimTraitName_old) %>%  # names of these traits
  unique() %>%
  sort()
Toccur <- TIDY3_occurrenceID %>% 
  pull(verbatimTraitName) %>% 
  unique() %>% 
  sort()
setdiff(Toccur, MoFTraits$verbatimTraitName_old) %>% sort()

# Traits dans MoFTraits mais pas dans nos données!
setdiff(MoFTraits$verbatimTraitName_old, Toccur) %>% sort() # REGARDER LES ESPACES DANS MoFTraits$verbatimTraitName!!!!
  
# Je ne trouve pas : "LRF" "LTS"   "S_Var"
#____________________________________________________________________

## Select traits to keep ####
trait_names <- MoFTraits$verbatimTraitName_new # Names of traits that we keep
TIDY4 <- TIDY4.0 %>% 
  select(-verbatimTraitName_old) %>%
  rename(verbatimTraitName = verbatimTraitName_new) %>%
  filter(verbatimTraitName %in% trait_names)  # remove traits that are not listed in MeasurementOrFact(traits)


# OccurrenceIDs ####
TIDY4_occurrenceID <- TIDY4 %>%
  mutate(verbatimOccurrenceID = paste(Code_Sp,Site,Block,Plot,Treatment,Year,Month,Day,Rep,verbatimTraitName,traitEntity,sep = "_")) %>% 
  mutate(verbatimOccurrenceID_echantillon = paste(Code_Sp,Site,Block,Plot,Treatment,Year,Month,Day,Rep,sep = "_")) %>% 
  mutate(verbatimOccurrenceID_population = paste(Code_Sp,Site,Block,Plot,Treatment,Year,Month,Day,sep = "_")) %>% 
  # remove columns already present in MoF_traits
  select(-c(Code_Sp,Family,LifeForm1,LifeForm2))

#____________________________________
# TEMPORARY duplicated occurrence ####
source("scripts/functions/regenerate_spreadsheet.R")

DUPL <- TIDY4_occurrenceID[which(duplicated(TIDY4_occurrenceID$verbatimOccurrenceID)),]
DUPL %>% 
  pull(Site) %>% 
  unique()

DUPLcplet <- TIDY4_occurrenceID %>% 
  filter(verbatimOccurrenceID %in% DUPL$verbatimOccurrenceID) %>% 
  arrange(Site,feuillet)
write.csv2(DUPLcplet,"data/AFaire_Avril2023/occurrences/duplicated_occurrenceID_format_core.csv",row.names=F,fileEncoding = "latin1")

sum_DUPL <- DUPL %>% 
  group_by(Site, feuillet) %>% 
  summarize(n = n())
write.csv2(sum_DUPL,"data/AFaire_Avril2023/occurrences/sites_feuillets_with_duplicated_occurrenceID.csv",row.names=F,fileEncoding = "latin1")

names_feuillets <- DUPLcplet %>%
  select(Site,feuillet) %>% 
  unique() %>% 
  mutate(site_feuillet = paste(Site,feuillet,sep = "_")) %>% 
  mutate(site_feuillet = str_sub(site_feuillet,start = 1, end = 31)) %>%  # # maximum length of excel worksheet names
  pull(site_feuillet)

list_site_feuillet <- DUPLcplet %>% 
  select(-c(verbatimOccurrenceID,verbatimOccurrenceID_echantillon,verbatimOccurrenceID_population)) %>% 
  group_split(Site,feuillet) %>% 
  setNames(names_feuillets)

list_feuillet <- lapply(list_site_feuillet,regenerate_spreadsheet)

# list_site_feuillet[[1]] %>% 
#   View()
# list_feuillet[[1]] %>% 
#   View()

# Export it into an excel file
lst_data <- list_feuillet
wb <- openxlsx::createWorkbook()
purrr::imap(
  .x = lst_data,
  .f = function(df, object_name) {
    openxlsx::addWorksheet(wb = wb, sheetName = object_name)
    openxlsx::writeData(wb = wb, sheet = object_name, x = df)
  }
)
# saveWorkbook(wb = wb, file = "data/AFaire_Avril2023/occurrences/duplicated_occurrenceID_format_excel.xlsx")


# Export ####
TIDY4_occurrenceID_nodupl <- TIDY4_occurrenceID %>% 
  filter(!verbatimOccurrenceID %in% DUPL$verbatimOccurrenceID)

write.table(TIDY4_occurrenceID_nodupl ,"output/TIDY_occurrenceID.csv",fileEncoding = "latin1",row.names=F,sep="\t",dec = ".")
