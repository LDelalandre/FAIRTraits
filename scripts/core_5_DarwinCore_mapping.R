library(tidyverse)

TIDY4_occurrenceID <-  read.csv2("output/TIDY_occurrenceID.csv",fileEncoding = "latin1",sep="\t",dec = ".")

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
