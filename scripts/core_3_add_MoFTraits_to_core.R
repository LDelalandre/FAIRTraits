library(tidyverse)

# This script adds info on trait measurement and sampling method to the core of the database

# Join occurrence and MoFTraits

TIDY4_occurrenceID <-  read.csv2("output/TIDY_occurrenceID.csv",fileEncoding = "latin1",sep="\t",dec = ".")
MoFTraits <- read.csv2("data/MoFTraits_vmai2023.csv",fileEncoding = "latin1")
colnames(MoFTraits)[1] <- gsub('^...','',colnames(MoFTraits)[1]) # remove ï.., qu'Eric a mis je sais pas comment.

info_traits <- MoFTraits %>% 
  select(-verbatimTraitName_old) %>% 
  rename(verbatimTraitName = verbatimTraitName_new)

### traits whose samplingProtocol and measurementMethod are identical whatever the site ####
# subset of core with these traits
info_commontraits_tomerge <- info_traits %>% 
  filter(Site == "All") %>% 
  select(-Site)

TIDY4_commontraits <- TIDY4_occurrenceID %>% 
  filter(verbatimTraitName %in% info_commontraits_tomerge$verbatimTraitName)

# add columns of the MeasurementOrFact(traits) extension (to be splitted into extension later)
TIDY4_commontraits_ext <- TIDY4_commontraits %>% 
  merge(info_commontraits_tomerge,by = c("verbatimTraitName","traitEntity"))



### traits whose samplingProtocol and measurementMethod differ depending on the site ####
info_traits_tomerge <- info_traits %>% 
  filter(!(Site == "All"))

TIDY4_differingtraits <- TIDY4_occurrenceID %>% 
  filter(verbatimTraitName %in% info_traits_tomerge$verbatimTraitName)

# add columns of the MeasurementOrFact(traits) extension (to be splitted into extension later)
# PROBLEM
info_traits_tomerge %>% pull(Site) %>% unique()
TIDY4_differingtraits %>% pull(Site) %>% unique()

TIDY4_differingtraits_ext <- TIDY4_differingtraits %>% 
  merge(info_traits_tomerge, by = c("Site","verbatimTraitName","traitEntity"))

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

