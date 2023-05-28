library(tidyverse)

# This script adds info on trait measurement and sampling method to the core of the database

# Join occurrence and MoFTraits

TIDY4 <-  read.csv2("output/TIDY_trait_entity_updated.csv",fileEncoding = "latin1",sep="\t",dec = ".")
MoFTraits <- read.csv2("data/MoFTraits_vmai2023.csv",fileEncoding = "latin1") %>% 
  mutate(Site = if_else (Site == "HGM", "Hautes Garrigues",Site)) %>% 
  mutate_all(trimws)
# colnames(MoFTraits)[1] <- gsub('^...','',colnames(MoFTraits)[1]) # remove ï.., qu'Eric a mis je sais pas comment.


info_traits <- MoFTraits %>% 
  select(-c(verbatimTraitName_old,traitEntityDataFile)) %>% 
  rename(verbatimTraitName = verbatimTraitName_new,
         traitEntity = traitEntityValid) %>% 
  unique()

#_________________________
# temporaire
tdupl <- info_traits %>% 
  mutate(T_E = paste(verbatimTraitName,traitEntity,sep="_")) %>% 
  filter(duplicated(T_E)) %>% 
  pull(T_E)

info_traits %>% 
  mutate(T_E = paste(verbatimTraitName,traitEntity,sep="_")) %>%
  filter(T_E %in% tdupl) %>% 
  arrange(verbatimTraitName) 
#_____________________________

### traits whose samplingProtocol and measurementMethod differ depending on the site ####
info_differingtraits_tomerge <- info_traits %>% 
  filter(!(Site == "All"))

TIDY4_differingtraits_completed <- TIDY4 %>% 
  left_join(info_differingtraits_tomerge, by = c("Site","verbatimTraitName","traitEntity"))

DF_differingtraits_completed <- TIDY4_differingtraits_completed %>% 
  filter(!is.na(traitName)) # les lignes qui ont été bien complétées

# les lignes à compléter : traitName vaut NA
DF_commontraits <- TIDY4_differingtraits_completed %>% 
  filter(is.na(traitName)) %>% 
  select(-c(traitName, Quality, verbatimTraitUnit, LocalIdentifier, traitID, samplingProtocol, measurementMethod))


### traits whose samplingProtocol and measurementMethod are identical whatever the site ####
info_commontraits_tomerge <- info_traits %>% 
  filter(Site == "All") %>% 
  select(-Site) %>% 
  unique()

# -- temporaire
# pas de duplication de ligne dans ce MoFTraits
info_commontraits_tomerge %>% 
  filter(duplicated(paste0(verbatimTraitName,traitEntity)))
# --

DF_commontraits_completed <- DF_commontraits %>% 
  left_join(info_commontraits_tomerge, by = c("verbatimTraitName", "traitEntity"))

# -- temporaire
# pb: encore des lignes non ocmplétées
DF_commontraits_pb <- DF_commontraits_completed %>% 
  filter(is.na(traitName))

DF_commontraits_pb%>% 
  group_by(Site) %>% 
  summarize(n = n())

list_trait_entity_to_add <- DF_commontraits_pb %>% 
  select(Site, verbatimTraitName_old,verbatimTraitName , traitEntityDataFile,traitEntity) %>% 
  unique()

write.csv2(list_trait_entity_to_add, "output/list_trait_entity_to_add.csv",row.names=F)

# Pourquoi pas complété avant ?
TEST <- TIDY4_differingtraits_completed %>% 
  filter(Site == "La Fage" & verbatimTraitName == "RDMC") %>%
  select(Site,verbatimTraitName,traitEntity) %>% 
  unique()


TOMERGE <- info_differingtraits_tomerge %>% 
  select("Site","verbatimTraitName","traitEntity","traitName")
TEST %>% 
  left_join(TOMERGE, by = c("Site","verbatimTraitName","traitEntity"))
TOMERGE %>% 
  filter(traitName == "Root dry matter content")
  filter(Site=="La Fage" & verbatimTraitName == "RDMC")

# --
#____________________________________________________________
TIDY4_commontraits <- TIDY4 %>% 
  filter(verbatimTraitName %in% info_commontraits_tomerge$verbatimTraitName)

# add columns of the MeasurementOrFact(traits) extension (to be splitted into extension later)
TIDY4_commontraits_ext <- TIDY4_commontraits %>% 
  right_join(info_commontraits_tomerge,by = c("verbatimTraitName","traitEntity"))
#
# TEMPORAIRE
TIDY4_commontraits_ext %>% 
  filter(is.na(traitEntity))
#



TIDY5 <- rbind(TIDY4_commontraits_ext,TIDY4_differingtraits_ext)
dim(TIDY4_occurrenceID)
dim(TIDY5)
# on a perdu des lignes! Pourquoi ? Lesquelles ?
V1 <- TIDY4_occurrenceID$verbatimOccurrenceID
V2 <- TIDY5$verbatimOccurrenceID
lost_occ <- setdiff(V1,V2)
# separate(occ, into = c("Code_Sp","Site","Block","Plot","Treatment","Year","Month","Day","Rep","verbatimTraitName"), sep = "_", remove = F)

LOST <- TIDY4_occurrenceID %>% 
  filter(verbatimOccurrenceID %in% lost_occ )

LOST %>% 
  pull(verbatimTraitName) %>% 
  unique()

no_corresp <- LOST %>% 
  select(verbatimTraitName,traitEntity) %>% unique()

write.csv2(no_corresp , "data/AFaire_Avril2023/MoFTraits/trait_entity_absent_in_MoFTraits.csv",row.names=F)

no_corresp_site_feuillet <- LOST %>% 
  select(verbatimTraitName,traitEntity,Site,feuillet) %>% unique()

write.csv2(no_corresp_site_feuillet , "data/AFaire_Avril2023/MoFTraits/trait_entity_absent_in_MoFTraits_site_feuillet.csv",row.names=F)


# Export ####
write.table(TIDY5 ,"output/TIDY_MoFTraits.csv",fileEncoding = "latin1",row.names=F,sep="\t",dec = ".")

