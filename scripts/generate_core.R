library(tidyverse)

TIDY2bis <- read.csv2("output/TIDY2.csv",fileEncoding = "latin1",sep="\t",dec = ".")

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


# OccurrenceIDs ####
TIDY3_occurrenceID <- TIDY3 %>%
  mutate(verbatimOccurrenceID = paste(Code_Sp,Site,Block,Plot,Treatment,Year,Month,Day,Rep,verbatimTraitName,sep = "_")) %>% 
  mutate(verbatimOccurrenceID_echantillon = paste(Code_Sp,Site,Block,Plot,Treatment,Year,Month,Day,Rep,sep = "_")) %>% 
  mutate(verbatimOccurrenceID_population = paste(Code_Sp,Site,Block,Plot,Treatment,Year,Month,Day,sep = "_")) %>% 
  # remove columns already present in MoF_traits
  select(-c(Code_Sp,Family,LifeForm1,LifeForm2))


# Traits ####
## Update names of traits ####
MoFTraits <- read.csv2("data/MoFTraits.csv",fileEncoding = "latin1")
correspondence_traits_old_new <- MoFTraits %>% 
  select(verbatimTraitName_old,verbatimTraitName_new) %>% 
  rename(verbatimTraitName = verbatimTraitName_old)
trait_names <- MoFTraits$verbatimTraitName_new # Names of traits that we keep

TIDY4.0 <- TIDY3_occurrenceID %>%
  merge(correspondence_traits_old_new) %>% 
  select(-verbatimTraitName) %>% 
  rename(verbatimTraitName = verbatimTraitName_new) 


## Select traits to keep ####
TIDY4 <- TIDY4.0 %>% 
  filter(verbatimTraitName %in% trait_names)  # remove traits that are not listed in MeasurementOrFact(traits)



#____________________________________
# temporaire
source("scripts/functions/regenerate_feuillet.R")

DUPL <- TIDY4[which(duplicated(TIDY4$verbatimOccurrenceID)),]
DUPL %>% 
  pull(Site) %>% 
  unique()

DUPLcplet <- TIDY4 %>% 
  filter(verbatimOccurrenceID %in% DUPL$verbatimOccurrenceID) %>% 
  arrange(Site,feuillet)
write.csv2(DUPLcplet,"output/DUPLcplet.csv",row.names=F,fileEncoding = "latin1")

sum_DUPL <- DUPL %>% 
  group_by(Site, feuillet) %>% 
  summarize(n = n())
write.csv2(sum_DUPL,"data/AFaire_Avril2023/duplicated/sites_feuillets_with_duplicated_occurrenceID.csv",row.names=F,fileEncoding = "latin1")

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

list_feuillet <- lapply(list_site_feuillet,regenerate_feuillet)

list_site_feuillet[[1]] %>% View()
list_feuillet[[1]] %>% 
  View()

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
saveWorkbook(wb = wb, file = "data/AFaire_Avril2023/duplicated_occurrenceID.xlsx")




#____________________________________



#_____________________________________
# TEMPORAIRE
# old trait names 
Told <- correspondence_traits_old_new$verbatimTraitName

# trait included
Tincl <- TIDY4 %>% pull(verbatimTraitName) %>% unique() %>% sort()

# traits excluded
Texcl <- TIDY3 %>% 
  filter(!(verbatimTraitName %in% Told)) %>% 
  filter(!(verbatimTraitName %in% Tincl)) %>% 
  pull(verbatimTraitName) %>% 
  unique()
EXCL <- data.frame(verbatimTraitName = Texcl,
                   removed = 1)

Tto_add <- read.csv2("data/Traits_à_ajouter_MoFTraits.csv") %>% 
  rename(verbatimTraitName = column_removed)

# Voir si on a bien inclus tous les traits qu'on voulait
full_join(EXCL,Tto_add) %>% View
#_____________________________________



#_______________________________________________________________________________
## Join occurrence and MoFTraits ####
info_traits <- MoFTraits %>% 
  select(-verbatimTraitName_old) %>% 
  rename(verbatimTraitName = verbatimTraitName_new)

### traits whose samplingProtocol and measurementMethod are identical whatever the site ####
# subset of core with these traits
traits_identical_all_sites <-  info_traits %>% 
  filter(Site == "All") %>% 
  pull(verbatimTraitName)

TIDY4_commontraits <- TIDY4 %>% 
  filter(verbatimTraitName %in% traits_identical_all_sites)

# add columns of the MeasurementOrFact(traits) extension (to be splitted into extension later)
info_commontraits_tomerge <- info_traits %>% 
  filter(Site == "All") %>% 
  select(-Site)

TIDY4_commontraits_ext <- TIDY4_commontraits %>% 
  merge(info_commontraits_tomerge,by = c("verbatimTraitName","traitEntity"))



### traits whose samplingProtocol and measurementMethod differ depending on the site ####
# subset of core with these traits
traits_f_site <- info_traits %>% 
  filter(!(Site == "All")) %>% 
  pull(verbatimTraitName)

TIDY4_differingtraits <- TIDY4 %>% 
  filter(verbatimTraitName %in% traits_f_site)

# add columns of the MeasurementOrFact(traits) extension (to be splitted into extension later)
info_traits_tomerge <- info_traits %>% 
  filter(!(Site == "All"))

info_traits_tomerge %>% pull(Site) %>% unique()
TIDY4_differingtraits %>% pull(Site) %>% unique()

TIDY4_differingtraits_ext <- TIDY4_differingtraits %>% 
  merge(info_traits_tomerge, by = c("Site","verbatimTraitName","traitEntity"))

TIDY5 <- rbind(TIDY4_commontraits_ext,TIDY4_differingtraits_ext)
dim(TIDY4)
dim(TIDY5)
# on a perdu des lignes! Pourquoi ? Lesquelles ?
V1 <- TIDY4$verbatimOccurrenceID
V2 <- TIDY5$verbatimOccurrenceID
lost_occ <- setdiff(V1,V2)
# separate(occ, into = c("Code_Sp","Site","Block","Plot","Treatment","Year","Month","Day","Rep","verbatimTraitName"), sep = "_", remove = F)

LOST <- TIDY4 %>% 
  filter(verbatimOccurrenceID %in% lost_occ )

LOST %>% pull(verbatimTraitName) %>% unique()

no_corresp <- LOST %>% 
  select(verbatimTraitName,traitEntity) %>% unique()

write.csv2(no_corresp , "output/trait_entity_absent_in_MoFTraits.csv",row.names=F)

#_______________________________________________________________________________
## Envt info  #### 
# (plot latitude, longitude, altitude)

Plots <- read.csv2("data/Plots.csv",fileEncoding = "latin1",sep=";")

Infos_Plots <- Plots %>% 
  select(Site,Plot,plotLatitude,	plotLongitude,	plotAltitude)
# NB: A COMPLETER POUR LES PLOTS qu'on n'a pas (pas de mesure de sol)
# Mettre des bons noms de colonnes

# TRES PEU DE CONGRUENCE ENTRE LES PLOTS DES METADONNEES ET DES DONNEES
# --> A compléter
plots_core <- TIDY5 %>% 
  filter(!(Site %in% c("O2LA","PDM"))) %>% 
  select(Site,Plot) %>% 
  unique() %>% 
  arrange(Site)

write.csv2(plots_core,"output/plots_per_site.csv",row.names=F,fileEncoding = "Latin1")

# TIDY6 <- merge(TIDY5,Infos_Plots) # je perds de l'info en mergeant


TIDY %>% filter(Site == "Les Agros") %>% pull(Plot)

#________________________________________________
# GBIF ####
## New columns for GBIF ####
TIDY7 <- TIDY6 %>% 
  mutate(countryCode = if_else(Site == "Garraf", "ES","FR"),
         basisOfRecord = "Human Observation",
         dynamicProperties = paste(verbatimTraitName,verbatimTraitValue,verbatimTraitUnit,sep="_"),
         plotAltitude_min = plotAltitude,
         plotAltitude_max = plotAltitude) 


#_______________________________________________
# generate core and extensions MoF traits ####
mapping <- read.csv2("data/Mapping.csv",header=T)


##  Core (with mapping file) ####
mapping_core <- mapping %>% filter(GBIFFile == "Occurrences")

Occurrences <- TIDY7 %>% 
  select(all_of(mapping_core$Variable))
colnames(Occurrences) <- mapping_core$Term

write.table(Occurrences ,"output/Core_vavr2023.csv",fileEncoding = "UTF-8",
            row.names=F,sep="\t",dec = ".")


## TraitValues ####
mapping_traits <- mapping %>% filter(Module == "TraitValues")

TraitValues <- TIDY7 %>% 
  select(all_of(mapping_traits$Variable))
colnames(TraitValues) <- mapping_traits$Term

head(TraitValues) %>% View

write.table(TraitValues ,"output/TraitValues_vavr2023.csv",fileEncoding = "UTF-8",
            row.names=F,sep="\t",dec = ".")


## Subsample ####
core_subsample <- core[sample(10000, ), ]
MeasurementOrFact_subsample <- MeasurementOrFact[sample(10000, ), ]
write.csv2(core_subsample,"output/core_subsample.csv",fileEncoding = "Latin1",row.names=F)
write.csv2(MeasurementOrFact_subsample,"output/MeasurementOrFact_subsample(traits).csv",fileEncoding = "Latin1",row.names=F)



# MeasurementOrFact <- TIDY5 %>% 
#   select(verbatimOccurrenceID,Site,	verbatimTraitName,	traitName,	traitEntity,	Quality,	verbatimTraitUnit,
#          LocalIdentifier,	traitID	,samplingProtocol,	measurementMethod
#   )
# 
# write.csv2(MeasurementOrFact,"output/MeasurementOrFact(traits).csv",fileEncoding = "Latin1",row.names=F)


# Taxon Core ####
taxon <- read.csv2("output/taxon_extension.csv") %>% 
  mutate(event = "Flowering range") %>% 
  mutate(Height1 = 10*Height1,
         Height2=10*Height2) %>% 
  mutate(sizeInMillimeters  = paste(Height1,Height2,sep ="-")) 
mapping_taxon <- mapping %>% filter(GBIFFile == "Taxon")

# ATTENTION, ONE PERD LifeCycle2, LifeForm2
setdiff(colnames(taxon),mapping_taxon$Variable)

taxon2 <- taxon %>% 
  select(all_of(mapping_taxon$Variable))
colnames(taxon2) <- mapping_taxon$Term

write.table(taxon2 ,"output/Taxon_vavr2023.csv",fileEncoding = "UTF-8",
            row.names=F,sep="\t",dec = ".")



#_______________________________________________________________________________
# vérifications ####

TIDY3 <- read.csv2("output/Core_vavr2023.csv")
dim(core)


## checker que l'extension taxons correspond bien aux espèces du core ####
taxon <- read.csv2("output/taxon_extension.csv")
sp_core <- core %>% 
  select(Species) %>% 
  unique()

test_taxon <- sp_core%>% 
  merge(taxon)

test_taxon %>% dim()
taxon %>% dim()

# espèces dans l'extension taxon, mais pas dans le core
setdiff(taxon$Species,sp_core$Species)
# "Lolium perenne"

# espèces dans le core, mais pas dans l'extension taxon
setdiff(sp_core$Species,taxon$Species)
# "Festuca ovina (sp.)?"






# 
# write.table(MoF_traits ,"data/MoFTraits_test.csv",fileEncoding = "UTF-8",
#             row.names=F,sep="\t",dec = ".")

traits_dupl <- MoF_traits %>% 
  group_by(verbatimTraitName_new) %>% 
  summarize(n = n()) %>% 
  filter(n>1) %>% 
  pull(verbatimTraitName_new)

MoF_traits %>% 
  filter(verbatimTraitName_new %in% traits_dupl) %>% 
  View


TIDY5 %>% 
  # filter(Site == "Hautes Garrigues") %>%
  filter(verbatimTraitName=="SLA") %>% 
  select(Site,verbatimTraitName,Entity) %>% 
  unique()


# Pourquoi pas LNC sur whole plant??
TIDY5 %>% 
  filter(Site == "Hautes Garrigues") %>% 
  filter(verbatimTraitName=="LNC") %>% 
  select(Site,verbatimTraitName,Entity) %>% 
  unique()
