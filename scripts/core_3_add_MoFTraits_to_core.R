library(tidyverse)

# This script adds info on trait measurement and sampling method to the core of the database

# Join occurrence and MoFTraits

TIDY4 <-  read.csv2("output/TIDY_trait_entity_updated.csv",fileEncoding = "latin1",sep="\t",dec = ".")

MoFTraits <- read.csv2("data/MoFTraitsFull.csv",fileEncoding = "latin1") %>% 
  mutate(Site = if_else (Site == "HGM", "Hautes Garrigues",Site)) %>% 
  mutate_all(trimws) %>% 
  mutate(verbatimTraitName_new = case_when(verbatimTraitName_new == "RDM _ab_0" ~ "RDM_ab_0",
                                         verbatimTraitName_new == "RDM _ab_84" ~ "RDM_ab_84",
                                         TRUE ~ verbatimTraitName_new))
# colnames(MoFTraits)[1] <- gsub('^...','',colnames(MoFTraits)[1]) # remove ï.., qu'Eric a mis je sais pas comment.

MoFTraitsLight <- readxl::read_excel("data/MoFTraitsLight.xlsx", sheet = "MoFTraitsLight") %>% 
  mutate_all(trimws) %>% 
  rename(verbatimTraitName = verbatimTraitName_new)

info_traits <- MoFTraits %>% 
  select(-c(verbatimTraitName_old,traitEntityDataFile)) %>% 
  rename(verbatimTraitName = verbatimTraitName_new,
         traitEntity = traitEntityValid) %>% 
  unique()

#________________________
# TEMPORAIRE : check MoFTraitsLight
A <- MoFTraits %>% pull(verbatimTraitName_new) %>% unique()

B <- MoFTraitsLight %>% pull(verbatimTraitName) %>% unique()

setdiff(A,B)
setdiff(B,A)

is.element("RDM_ab_84",B)

#______________________

### traits whose samplingProtocol and measurementMethod differ depending on the site ####
info_differingtraits_tomerge <- info_traits %>% 
  filter(!(Site == "All"))

TIDY4_differingtraits_completed <- TIDY4 %>% 
  left_join(info_differingtraits_tomerge, by = c("Site","verbatimTraitName","traitEntity"))

# Only the completed lines
DF_differingtraits_completed <- TIDY4_differingtraits_completed %>% 
  filter(!is.na(traitName)) # les lignes qui ont été bien complétées

# Only lines to be completed = where traitName is NA so far
DF_commontraits <- TIDY4_differingtraits_completed %>% 
  filter(is.na(traitName)) %>% 
  select(-c( traitName,Quality,   samplingProtocol, measurementMethod)) #

# problème : les deux data frame suivants devraient avoir le même nombre de lignes !
dim(TIDY4)
dim(TIDY4_differingtraits_completed)
# en complétant, on ajoute des lignes
# c'est qu'il doit y avoir des doublons dans info_traits
info_differingtraits_tomerge %>% 
  select(Site,verbatimTraitName,traitEntity) %>% unique() %>% 
  dim()
info_differingtraits_tomerge %>% 
  select(Site,verbatimTraitName,traitEntity) %>% 
  filter(verbatimTraitName == "RtRM84" & Site == "O2LA")
  duplicated()

dim(DF_differingtraits_completed) + dim(DF_commontraits)


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
# pb: encore des lignes non complétées
DF_commontraits_pb <- DF_commontraits_completed %>% 
  filter(is.na(traitName))

DF_commontraits_pb%>% 
  group_by(Site) %>% 
  summarize(n = n())

list_trait_entity_to_add <- DF_commontraits_pb %>% 
  select(Site, verbatimTraitName_old,verbatimTraitName , traitEntityDataFile,traitEntity,traitName) %>% 
  unique()

write.csv2(list_trait_entity_to_add, "output/WorkingFiles/2023_08_21_list_site_trait_name_to_add.csv",row.names=F)
# --


# bind the two
TIDY5 <- rbind(DF_differingtraits_completed,DF_commontraits_completed)
dim(TIDY4)
dim(TIDY5)

#_______________
# Pourquoi augmentation du nombre de lignes ?
D1 <- DF_differingtraits_completed %>% 
  mutate(verbatimOccurrenceID = paste(Code_Sp,Site,Block,Plot,Treatment,Year,Month,Day,Rep,verbatimTraitName,traitEntity,sep = "_"))

D2 <- DF_commontraits_completed %>% 
  mutate(verbatimOccurrenceID = paste(Code_Sp,Site,Block,Plot,Treatment,Year,Month,Day,Rep,verbatimTraitName,traitEntity,sep = "_"))

intersect(D1$verbatimOccurrenceID, D2$verbatimOccurrenceID)


info_differingtraits_tomerge %>% select(Site,verbatimTraitName,traitEntity)
info_commontraits_tomerge %>% select(Site,verbatimTraitName,traitEntity)
#______________


TIDY5 %>% 
  filter(is.na(traitName)) %>% dim() # compléter MoFTraits (Eric)


# Add column variableType
# TIDY5 <- TIDY5 %>% 
#   mutate(verbatimOccurrenceID = paste(Code_Sp,Site,Block,Plot,Treatment,Year,Month,Day,Rep,verbatimTraitName,traitEntity,sep = "_"))
#   
TIDY6 <- TIDY5 %>% 
  merge(MoFTraitsLight %>%  select(verbatimTraitName, verbatimTraitUnit, traitID, variableType)
  )

MoFTraitsLight[which(duplicated(MoFTraitsLight$verbatimTraitName)),]


TIDY6 %>% filter(is.na(traitName))


# Export ####
write.table(TIDY6 ,"output/TIDY_MoFTraits.csv",fileEncoding = "latin1",row.names=F,sep="\t",dec = ".")

# vérifs
dim(TIDY4)
dim(TIDY5)
dim(TIDY6)

