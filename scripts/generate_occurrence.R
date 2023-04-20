library(tidyverse)
library("openxlsx")

sites <- c("LaFage","Cazarils","PDM","O2LA","Garraf","HGM","LesAgros")





# colmuns other than traits used for building the "tidy" csv (i.e. data in row)
columns_other_than_traits <- 
  c("Site", "Block" ,"Plot", "Treatment", "Year" ,  "Month"  ,   "Day"       ,       "Species" , "Code_Sp"   ,    "Family" ,
    "LifeForm1" ,"LifeForm2" , 
    "Entity",    "Rep",
    "nameOfProject" ,"measurementDeterminedBy",
    "feuillet" # a supprimer à la fin, juste pour la traçabilité
    #"Published in" # for MLoss_Roots
  )

# functions ####
read_file <- function(fsite){
  # function to import all excel sheets of a site in a list
  
  # Define files to read in a data frame whose columns are:
  # - site, the name of the site
  # - file, the name of the excel file to import
  DATA_FILES <- c("Data_LaFage_FAIRTraits_vavril2023.xlsx",
                  "Data_Cazarils_FAIRTraits_vavril2023.xlsx",
                  "Data_CRE_PDM_FAIRTraits_vavril2023.xlsx",
                  "Data_CRE_O2LA_FAIRTraits_vavril2023.xlsx",
                  "Data_Garraf_FAIRTraits_vavril2023.xlsx",
                  "Data_HGM_FAIRTraits_vavril2023.xlsx",
                  "Data_LesAgros_FAIRTraits_vavril2023.xlsx"
  )
  sites <- c("LaFage","Cazarils","PDM","O2LA","Garraf","HGM","LesAgros")
  SITES <- data.frame(cbind(sites,DATA_FILES))
  colnames(SITES) <- c("site","file")
  
  # Extract the name of the excel file corresponding to the site on which we work (i.e., fsite)
  filename <- SITES %>% 
    filter(site == fsite) %>% 
    pull(file)
  # get the path to that file
  path_filename <- paste0("data/Versions_Avril2023/",filename)
  
  # Import the sheets of that file in a list
  sheets <- readxl::excel_sheets(path_filename)
  sheets2 <- sheets[-c(1,2)] # remove Metadata and Climate data, to keep only trait data
  
  x <- lapply(sheets2, function(X) readxl::read_excel(path_filename, sheet = X))
  names(x) <- sheets2
  x
}

# Centralize data in one "tidy" data frame with info in row ####

feuillets_to_remove <- c("LeafDimensions (àsupprimer)","Climate data","Leaf_Thickness")

# COL_REMOVED <- NULL # columns discarded from the database
TIDY <- NULL

for (focalsite in sites){
  
  files <- read_file(focalsite)
  TIDY_site <- NULL
  
  for (i in c(1:length(files))){ 
    # work on each excel sheet successively
    focus <- files[[i]]
    focus$feuillet <- names(files[i])
    if(focalsite == "PDM") {focus$Site <- "PDM"}
    if(focalsite == "O2LA") {focus$Site <- "O2LA"}
    
    if (!(names(files[i]) %in% feuillets_to_remove)){
      # names of the traits in the focal sheet
      focus_traits <- focus %>%
        select(!any_of(columns_other_than_traits)) %>%
        colnames()  # extract the name of the traits available on that sheet
      
      # which are the columns that are missing in that sheet?
      Missing <- setdiff(columns_other_than_traits, names(focus))  # Find names of missing columns
      focus[Missing] <- NA  # Add them, filled with NAs
      
      # Make the dataset in a tidy form
      for (trait in focus_traits){
        focus_trait <- focus %>% 
          select(any_of(columns_other_than_traits),all_of(trait)) %>% 
          # some columns, i.e. those in new_columns_sp_file and incolumns_gas_exchange_metadata, are not there
          gather(all_of(trait) ,key=verbatimTraitName,value=verbatimTraitValue)
        focus_trait$Day <- as.character(focus_trait$Day)
        
        TIDY_site <- rbind(TIDY_site,focus_trait)
      }
      
      TIDY_now <- TIDY_site %>% 
        filter(!(feuillet %in% feuillets_to_remove))
      
      # # columns removed 
      # col_removed <- TIDY_now %>% 
      #   filter(!(verbatimTraitName %in% trait_names)) %>% 
      #   select(verbatimTraitName) %>% 
      #   unique() %>% 
      #   rename(column_removed = verbatimTraitName)
      # col_removed$feuillet <- unique(focus$feuillet)
      # col_removed$site <- unique(focus$Site)
    }
    
    
  }
  TIDY <- rbind(TIDY,TIDY_site)
  # COL_REMOVED <- rbind(COL_REMOVED,col_removed)
}

# write.csv2(COL_REMOVED,"output/columns_removed.csv",row.names = F)

TIDY_backup <- TIDY

TIDY2 <- TIDY %>%
## generate occurrenceID ####
  mutate(verbatimOccurrenceID = paste(Code_Sp,Site,Block,Plot,Treatment,Year,Month,Day,Rep,verbatimTraitName,sep = "_")) %>% 
  mutate(verbatimOccurrenceID_echantillon = paste(Code_Sp,Site,Block,Plot,Treatment,Year,Month,Day,Rep,sep = "_")) %>% 
  mutate(verbatimOccurrenceID_population = paste(Code_Sp,Site,Block,Plot,Treatment,Year,Month,Day,sep = "_")) %>% 
## # remove columns already present in MoF_traits ####
  select(-c(Code_Sp,Family,LifeForm1,LifeForm2)) %>%  
## remove NAs in trait values ####
  filter(!(is.na(verbatimTraitValue))) %>% 
## change values of, e.g., Plots ####
  mutate(Plot = case_when(Site == "La Fage" ~ paste("FAG",Plot,sep = "_"),
                          Site == "Cazarils" ~ paste("CAZ",Plot,sep = "_"),
                          Site == "PDM" ~ paste("CRE",Plot,sep = "_"),
                          Site == "O2LA" ~ paste("CRE",Plot,sep = "_"),
                          Site == "Garraf" ~ paste("GAR",Plot,sep = "_"),
                          Site == "Hautes Garrigues" ~ paste("HGM",Plot,sep = "_"),
                          Site == "La Fage" ~ paste("Fag",Plot,sep = "_")))



# Corrections (typos) ####
TIDY3 <- TIDY2 %>% 
  filter(!(Species %in% c("Geranium dissectum - p\xe9tiole","Geranium dissectum - pétiole"))) %>% 
  mutate(Species = case_when(Species == "Thymus serpyllum" ~ "Thymus sp.",
                             Species == "Taraxacum officinale" ~ "Taraxacum sp.",
                             Species == "Carex humilis?" ~"Carex sp.",
                             Species == "Cratægus monogyna" ~ "Crataegus monogyna",
                             Species == "Chamærops humilis" ~ "Chamaerops humilis",
                             Species == "Erophila verna" ~ "Draba verna",
                             Species == "Bromus steriis" ~ "Bromus sterilis",
                             Species == "Festuca christiani-bernardii" ~ "Festuca christiani-bernardi",
                             Species == "Geranium dissectum - limbe" ~ "Geranium dissectum",
                             Species == "Festuca ovina (sp)?" ~ "Festuca christiani-bernardi",
                             Species == "Linum tenuifolium subsp. tenuifolium" ~ "Linum tenuifolium",
                             TRUE ~ Species)) %>% 
  mutate(Species = case_when( Species == "Potentilla reptens" ~ "Potentilla reptans" ,
                              Species == "Vicia heteophylla" ~ "Vicia heterophylla",
                              Species == "Ampelodesmos mauritanica" ~ "Ampelodesmos mauritanicus",
                              Species == "Kickxia spruria" ~ "Kickxia spuria", # r en trop
                              Species == "Convovulus arvensis" ~ "Convolvulus arvensis", # manque un l
                              Species == "Cratægus monogyna" ~ "Crataegus monogyna",
                              Species == "Carex humilis?" ~ "Carex sp.",
                              Species == "Myosostis ramosissima subsp. ramosissima" ~ "Myosotis ramosissima subsp. ramosissima", # s en trop
                              Species == "Helichrysum stoechas ssp. stoechas" ~ "Helichrysum stoechas subsp. stoechas",
                              Species == "Viola alba ssp. scotophylla" ~ "Viola alba subsp. scotophylla",
                              Species == "Carex hallerana" ~  "Carex halleriana",
                              Species == "Plantago lanceola" ~ "Plantago lanceolata",
                              Species == "Catananche coerulea" ~ "Catananche caerulea",
                              Species == "Chamærops humilis" ~ "Chamaerops humilis",
                              Species == "Cirsium acaule" ~ "Cirsium acaulon",
                              Species == "Inula conyza" ~ "Inula conyzae",
                              # "Linum tenuifolium subsp. tenuifolium" n'existe pas dans TAXREF
                              TRUE ~ Species)) %>% 
  mutate(Treatment = paste("Treatment",Treatment,sep="_"))


#_______________________________________________________________________________
#  Modifs ####

# Names of traits that we keep
MoFTraits <- read.csv2("data/Versions_Avril2023/MoFTraits.csv",fileEncoding = "latin1")

correspondence_traits_old_new <- MoFTraits %>% 
  select(verbatimTraitName_old,verbatimTraitName_new) %>% 
  rename(verbatimTraitName = verbatimTraitName_old)
trait_names <- MoFTraits$verbatimTraitName_new

TIDY4.0 <- TIDY3 %>%
## Update values of verbatimTraitName ####
  merge(correspondence_traits_old_new) %>% 
  select(-verbatimTraitName) %>% 
  rename(verbatimTraitName = verbatimTraitName_new) 

TIDY4.0 %>% 
  filter(verbatimTraitName == "LCC")

## Select traits to keep ####
TIDY4 <- TIDY4.0 %>% 
  filter(verbatimTraitName %in% trait_names) %>% # remove traits that are not listed in MeasurementOrFact(traits)
## change some column names ####
  rename(traitEntity = Entity)

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

Tto_add <- read.csv2("data/Versions_Avril2023/Traits_à_ajouter_MoFTraits.csv") %>% 
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

TIDY4_commontraits %>% dim()
TIDY4_commontraits_ext %>% dim()
# ok, mêmes dimensions

#--
### traits whose samplingProtocol and measurementMethod differ depending on the site ####
# PB1: dans le CORE, l'info PDM et O2LA est dégradée en CAmp Redon. Changer à la lecture de ces sites le nom du site, puis le rechanger à la toute fin.
# PB2: il manque des sites dans le fichier de mapping des traits!!



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

# left join (pour database sans perte de données)
TIDY4_differingtraits_ext <- TIDY4_differingtraits %>% 
  left_join(info_traits_tomerge, by = c("Site","verbatimTraitName","traitEntity"))

# merge (pour voir ce qui manque)
TIDY4_differingtraits_ext_check <- TIDY4_differingtraits %>% 
  merge(info_traits_tomerge, by = c("Site","verbatimTraitName","traitEntity"))

TIDY4_differingtraits %>% dim()
TIDY4_differingtraits_ext_check %>% dim()
# on a plein de trucs qui disparaissent ?

missing_MoF <- TIDY4_differingtraits_ext %>% 
  filter(is.na(samplingProtocol))
write.csv2(missing_MoF,"output/missing_MoF_avr2023.CSV",row.names=F)

# IL MANQUE ENVIRON 667 lignes... (= on n'a pas les métadonnées de mesures de trait pour ces données)

# Concerne les sites Cazarils et Les Agros
# concerne uniquement le trait LCC
# Hautes Garrigues et PDM : whole plant
# Autres sites : mature leaf


missing <- setdiff(TIDY4_differingtraits$verbatimOccurrenceID, TIDY4_differingtraits_ext_check$verbatimOccurrenceID) %>% # plein de lignes manquantes dans info_traits_tomerge
  as.data.frame()
colnames(missing) <- "verbatimOccurrenceID"

# isoler Species, Site, verbatimTraitName
missing2 <- missing %>% 
  separate(verbatimOccurrenceID, into = c("Code_Sp","Site","Block","Plot","Treatment","Year","Month","Day","Rep","verbatimTraitName"),
           sep = "_")
missing2 %>% pull(Site) %>% unique()
missing2 %>% pull(verbatimTraitName) %>% unique()
missing2 %>% pull(Code_Sp) %>% unique()

setdiff(TIDY4_differingtraits_ext_check$verbatimOccurrenceID, TIDY4_differingtraits$verbatimOccurrenceID) # 0




TIDY5 <- rbind(TIDY4_commontraits_ext,TIDY4_differingtraits_ext_check)
dim(TIDY4)
dim(TIDY5)
# on a perdu des lignes! Pourquoi ? Lesquelles ?


#_______________________________________________________________________________
## Envt info  #### 
# (plot latitude, longitude, altitude)

Plots <- read.csv2("data/Versions_Avril2023/Plots.csv",fileEncoding = "latin1",sep=";")

Infos_Plots <- Plots %>% 
  rename(Site = Site.name,
         Plot = plot) %>% 
  select(Site,Plot,plotLatitude,	plotLongitude,	plotAltitude)
# NB: A COMPLETER POUR LES PLOTS qu'on n'a pas (pas de mesure de sol)
# Mettre des bons noms de colonnes

# TRES PEU DE CONGRUENCE ENTRE LES PLOTS DES METADONNEES ET DES DONNEES
# --> A compléter
plots_core <- TIDY5 %>% 
  filter(!(Site %in% c("O2LA","PDM"))) %>% 
  pull(Plot) %>% unique()
plots_info <- Infos_Plots %>% pull(Plot) %>% unique()
setdiff(plots_core,plots_info)
setdiff(plots_info,plots_core)
intersect(plots_info,plots_core)

TIDY6 <- merge(TIDY5,Infos_Plots) # je perds de l'info en mergeant



TIDY5 %>% filter(Plot == "Q3") %>% View
#________________________________________________
## New columns ####
TIDY7 <- TIDY6 %>% 
  mutate(countryCode = if_else(Site == "Garraf", "ES","FR"),
         basisOfRecord = "Human Observation",
         dynamicProperties = paste(verbatimTraitName,verbatimTraitValue,verbatimTraitUnit,sep="_"),
         plotAltitude_min = plotAltitude,
         plotAltitude_max = plotAltitude) 


#_______________________________________________
# generate core and extensions MoF traits ####
mapping <- read.csv2("data/Versions_Avril2023/Mapping.csv",header=T)


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
