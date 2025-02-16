library(tidyverse)

# This script changes column names of the database to DarwinCore names

# importer TIDY_plot, mais pour l'instant pas au point
core <-  data.table::fread("output/TIDY_6_ID_field_campaign.csv",encoding = "UTF-8") %>% 
  rename(site = Site) %>% 
  rename(traitEntity = traitEntityValid) %>% 
  rename(traitPlotLatitude = plotLatitude,traitPlotLongitude = plotLongitude, traitPlotAltitude = plotAltitude)

taxon <- data.table::fread("output/FAIRTraits_Taxon.csv",encoding = "UTF-8")

mapping_occurrence <- readxl::read_excel("data/Mapping&Ordre_DwCArchives.xlsx", sheet = "Mapping_Occurence")
mapping_taxa <- readxl::read_excel("data/Mapping&Ordre_DwCArchives.xlsx", sheet = "Mapping_Taxa")

# Occurrence ####

A <- colnames(core)
B <- mapping_occurrence %>% pull(Colonne)

setdiff(A,B)
setdiff(B,A)

core_InDoRES <- core %>% 
  select(all_of(mapping_occurrence$Colonne))
core_GBIF <- core_InDoRES

colnames(core_GBIF) <- mapping_occurrence$`DwC Term`

# Taxa ####
A <- colnames(taxon)
B <- mapping_taxa %>% pull(Colonne)

setdiff(A,B)
setdiff(B,A)

taxon_InDoRES <- taxon %>% select(all_of(mapping_taxa$Colonne))

mapping_taxa$Colonne[11] <- "NaturalHistoryUnstructured"
mapping_taxa <- mapping_taxa %>% 
  filter(!(Colonne == "Height2"))
taxon_GBIF <- taxon %>% select(all_of(mapping_taxa$Colonne))
colnames(taxon_GBIF) <- mapping_taxa$`DwC Term`


# Export ####
data.table::fwrite(core_InDoRES ,"output/InDoRES_occurrence.csv",sep="\t")
data.table::fwrite(core_GBIF ,"output/GBIF_occurrence.csv",sep="\t")
data.table::fwrite(taxon_InDoRES ,"output/InDoRES_taxa.csv",sep="\t")
data.table::fwrite(taxon_GBIF ,"output/GBIF_taxa.csv",sep="\t")

#_____________



mapping_core <- readxl::read_excel("data/FAIRTraits_MappingGBIF.xlsx", sheet = "Core")
mapping_taxon <- readxl::read_excel("data/FAIRTraits_MappingGBIF.xlsx", sheet = "Taxon")

column_order_indores <- readxl::read_excel("data/OrdreColonnes_TraitValues.xlsx", sheet = "Feuil1") %>% 
  select(Colonne,Ordre,Status)



# Update taxon in core ####
intersect(colnames(core), colnames(taxon))

core_upd_taxon <- core %>% 
  select(-c("Code_Sp"  , "Family" ,   "LifeForm1", "LifeForm2")) %>% 
  merge(taxon %>% select("Species","Code_Sp"  , "Family" ,   "LifeForm1", "LifeForm2"))

A<-taxon %>% pull(Species)
B<-core %>% pull(Species) %>% unique()
setdiff(A,B) # in taxon, but not in core (not a problem?)
setdiff(B,A) # in core, but not in taxon (generates a loss of rows in core)

# update names of columns in the files ####
core_upd_nm <- core_upd_taxon %>% 
  rename(traitEntity = traitEntityValid) %>% 
  rename(traitPlotLatitude = plotLatitude) %>% 
  rename(traitPlotLongitude = plotLongitude) %>% 
  rename(traitPlotAltitude = plotAltitude) 
  



# Core ####

# Are columns of the core described in the mapping file?
setdiff(mapping_core$verbatimVariableName , colnames(core) )
setdiff(colnames(core) , mapping_core$verbatimVariableName )


## InDoRES ####
# col_indores <- mapping_core %>% 
#   filter(Status_InDoRES == "keep") %>% 
#   arrange(orderInFinalOccurenceFile) %>% 
#   pull(verbatimVariableName)

col_indores <- column_order_indores %>% 
  filter(Status == "keep") %>% 
  arrange(Ordre) %>%
  pull(Colonne)

core_indores <- core_upd_nm %>% 
  select(any_of(col_indores)) 

setdiff(col_indores , colnames(core_indores)) # identiques

## GBIF ####
### Manually ####
core_GBIF1 <- core_indores %>% 
  mutate(measurementType = paste(traitEntity,traitQuality,sep="_")) %>% 
  mutate(measurementID = paste(termSource, localIdentifier,sep="_"))

### With mapping file ####

col_GBIF_names <- mapping_core %>% 
  filter(Status_GBIF == "keep") %>% 
  filter(!(verbatimVariableName %in% c("traitEntityValid","traitQuality","termSource","locaIdentifyer"))) %>% 
  arrange(as.numeric(orderInFinalOccurenceFile)) %>% 
  select(verbatimVariableName,variableNameStandard)

setdiff(col_GBIF_names$verbatimVariableName,colnames(core_GBIF1))
setdiff(colnames(core_GBIF1),col_GBIF_names$verbatimVariableName) # OK, columns that should not be kept in GBIF

core_GBIF2 <- core_GBIF1 %>% 
  select(any_of(col_GBIF_names$verbatimVariableName),measurementType,measurementID)

colnames(core_GBIF2) <- c(col_GBIF_names$variableNameStandard,"measurementType","measurementID")

core_GBIF2

# Taxon ####

# Are columns of the taxon file described in the mapping file?
setdiff(mapping_taxon$verbatimVariableName , colnames(taxon) )
setdiff(colnames(taxon) , mapping_taxon$verbatimVariableName )

## GBIF ####
var_taxon_GBIF <- mapping_taxon %>% 
  filter(Status_GBIF == "keep") %>% 
  arrange(orderInFinalTaxonFile) %>% 
  select(verbatimVariableName,variableNameStandard)

taxon_GBIF <- taxon %>% 
  select(all_of(var_taxon_GBIF$verbatimVariableName))

colnames(taxon_GBIF) <- var_taxon_GBIF$variableNameStandard





# Export ####
data.table::fwrite(core_upd_nm,"output/core_raw.csv",sep="\t")

data.table::fwrite(core_indores,"output/core_InDoRES.csv",sep="\t")

data.table::fwrite(core_GBIF2,"output/core_GBIF.csv",sep="\t")

data.table::fwrite(taxon_GBIF,"output/taxon_GBIF.csv",sep="\t")



#ANCIEN, A VIRER (?) ####

## New columns for GBIF ####
GBIF <- core %>% 
  mutate(countryCode = if_else(Site == "Garraf", "ES","FR"),
         basisOfRecord = "Human Observation",
         dynamicProperties = paste(verbatimTraitName,verbatimTraitValue,verbatimTraitUnit,sep="_"),
         plotAltitude_min = plotAltitude,
         plotAltitude_max = plotAltitude
  ) 

#_____________________________________
#  Core Occurrences and extension ####
## Occurrences ###
mapping_core <- mapping %>% 
  filter(GBIFFile == "Occurrences")

setdiff(mapping_core$Variable, GBIF %>% colnames())

Occurrences <- GBIF %>% 
  select(any_of(mapping_core$Variable)) # changer pour all_of
colnames(Occurrences) <- mapping_core$Term # vérifier qu'on conserve bien le même ordre

data.table::fwrite(Occurrences,"output/GBIF/Occurrences.csv")


## TraitValues ####
mapping_traits <- mapping %>% 
  filter(GBIFFile == "TraitValues")

setdiff(mapping_traits$Variable, GBIF %>% colnames())

TraitValues <- GBIF %>% 
  select(any_of(mapping_traits$Variable)) # changer pour all_of
colnames(TraitValues) <- mapping_traits$Term

data.table::fwrite(TraitValues,"output/GBIF/TraitValues.csv")



## Subsample ####
# core_subsample <- core[sample(10000, ), ]
# MeasurementOrFact_subsample <- MeasurementOrFact[sample(10000, ), ]
# write.csv2(core_subsample,"output/core_subsample.csv",fileEncoding = "Latin1",row.names=F)
# write.csv2(MeasurementOrFact_subsample,"output/MeasurementOrFact_subsample(traits).csv",fileEncoding = "Latin1",row.names=F)
# 


#______________________________________________________
# Taxon Core and extensions ####
taxon <- read.csv2("output/taxon.csv") %>% 
  mutate(event = "Flowering range") %>% 
  mutate(Height1 = 10*Height1,
         Height2=10*Height2) %>% 
  mutate(sizeInMillimeters  = paste(Height1,Height2,sep ="-")) 

mapping_taxon <- mapping %>% 
  filter(GBIFFile == "Taxon")

# ATTENTION, ONE PERD LifeCycle2, LifeForm2
setdiff(mapping_taxon$Variable,colnames(taxon))

taxon2 <- taxon %>% 
  select(all_of(mapping_taxon$Variable))
colnames(taxon2) <- mapping_taxon$Term

data.table::fwrite(taxon2,"output/GBIF/Taxon_vavr2023.csv")


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
