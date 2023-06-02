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

### traits whose samplingProtocol and measurementMethod differ depending on the site ####
info_differingtraits_tomerge <- info_traits %>% 
  filter(!(Site == "All"))

TIDY4_differingtraits_completed <- TIDY4 %>% 
  left_join(info_differingtraits_tomerge, by = c("Site","verbatimTraitName","traitEntity"))

# Only the completed lines
DF_differingtraits_completed <- TIDY4_differingtraits_completed %>% 
  filter(!is.na(traitName)) # les lignes qui ont été bien complétées

# Only lines to be completed = where traitName equals NA
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

write.csv2(list_trait_entity_to_add, "output/WorkingFiles/list_trait_entity_to_add.csv",row.names=F)
# --


# bind the two
TIDY5 <- rbind(DF_differingtraits_completed,DF_commontraits_completed)
dim(TIDY4)
dim(TIDY5)

TIDY5 %>% 
  filter(is.na(traitName)) %>% dim() # compléter MoFTraits (Eric)


# Export ####
write.table(TIDY5 ,"output/TIDY_MoFTraits.csv",fileEncoding = "latin1",row.names=F,sep="\t",dec = ".")

