library(tidyverse)

# This script:
# - imports a csv file with data in row (output of `core_1_import_data.R`), 
# - generates the core of the database, with verbatimOccurrenceIDs
# - updates trait names and the values in some other columns (Plot, Treatment)

TIDY2 <- read.csv2("output/TIDY.csv",fileEncoding = "latin1",sep="\t",dec = ".")

MoFTraits <- read.csv2("data/MoFTraits_vmai2023.csv",fileEncoding = "latin1")
colnames(MoFTraits)[1] <- gsub('^...','',colnames(MoFTraits)[1]) # remove ï.., qu'Eric a mis je sais pas comment.
# Enlever les espaces qui se sont glissés un peu partout dans les noms de traits
MoFTraits <- MoFTraits %>% 
  mutate(verbatimTraitName_old = str_replace_all(verbatimTraitName_old, " ", "")) %>% 
  mutate(verbatimTraitName_new = str_replace_all(verbatimTraitName_new, " ", "")) %>% 
  mutate(verbatimTraitName_old = str_replace_all(verbatimTraitName_old, "Â", ""))

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
# TEMPORARY missing traits names ####
# Problème résolu
TIDY4.0 %>% 
  filter(is.na(verbatimTraitName_new)) %>% # traits that did not match MoFTraits
  select(verbatimTraitName_old,traitEntityDataFile,traitEntityValid) %>%  # names of these traits
  unique() %>% 
  View()

#____________________________________________________________________

## Select traits to keep ####
trait_names <- MoFTraits$verbatimTraitName_new # Names of traits that we keep
TIDY4 <- TIDY4.0 %>% 
  select(-verbatimTraitName_old) %>%
  rename(verbatimTraitName = verbatimTraitName_new) %>%
  rename(traitEntity = traitEntityValid) %>% 
  filter(!is.na(verbatimTraitName)) # idem que ligne en-dessous
  # filter(verbatimTraitName %in% trait_names)  # remove traits that are not listed in MeasurementOrFact(traits)


# OccurrenceIDs ####
TIDY4_occurrenceID <- TIDY4 %>%
  mutate(verbatimOccurrenceID = paste(Code_Sp,Site,Block,Plot,Treatment,Year,Month,Day,Rep,verbatimTraitName,traitEntity,sep = "_")) %>% 
  mutate(verbatimOccurrenceID_echantillon = paste(Code_Sp,Site,Block,Plot,Treatment,Year,Month,Day,Rep,sep = "_")) %>% 
  mutate(verbatimOccurrenceID_population = paste(Code_Sp,Site,Block,Plot,Treatment,Year,Month,Day,sep = "_")) %>% 
  # remove columns already present in taxon
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
  setNames(names_feuillets) %>% 
  unique()

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
saveWorkbook(wb = wb, file = "data/AFaire_Avril2023/occurrences/duplicated_occurrenceID_format_excel.xlsx")


# Export ####
TIDY4_occurrenceID_nodupl <- TIDY4_occurrenceID %>% 
  filter(!verbatimOccurrenceID %in% DUPL$verbatimOccurrenceID)

write.table(TIDY4_occurrenceID_nodupl ,"output/TIDY_occurrenceID_nodupl.csv",fileEncoding = "latin1",row.names=F,sep="\t",dec = ".")
write.table(TIDY4_occurrenceID ,"output/TIDY_occurrenceID.csv",fileEncoding = "latin1",row.names=F,sep="\t",dec = ".")


# Info on treatments
# TIDY4_occurrenceID <- read.csv2("output/TIDY_occurrenceID.csv",sep="\t")
# treatment <- TIDY4_occurrenceID_nodupl %>% 
#   select(Site, Treatment) %>% 
#   unique() 
# write.csv2(treatment,"output/treatments_per_site.csv",row.names=F)
