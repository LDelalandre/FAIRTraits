library(tidyverse)
library("openxlsx")

# Names of traits that we keep
MeasurementOrFact_traits <- openxlsx::read.xlsx("data/Versions_Avril2023/Supp_Extensions_FAIRTraits_vavril2023.xlsx", sheet = "MeasurementOrFact(traits)", startRow = 1, colNames = TRUE)
trait_names <- MeasurementOrFact_traits$verbatimTraitName_old
correspondence_traits_old_new <- MeasurementOrFact_traits %>% 
  select(verbatimTraitName_old,verbatimTraitName_new) %>% 
  rename(verbatimTraitName = verbatimTraitName_old)


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

feuillets_to_remove <- c("LeafDimensions (àsupprimer)","Climate data")

COL_REMOVED <- NULL # columns discarded from the database
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
      
      # columns removed 
      col_removed <- TIDY_now %>% 
        filter(!(verbatimTraitName %in% trait_names)) %>% 
        select(verbatimTraitName) %>% 
        unique() %>% 
        rename(column_removed = verbatimTraitName)
      col_removed$feuillet <- unique(focus$feuillet)
      col_removed$site <- unique(focus$Site)
    }
    
    
  }
  TIDY <- rbind(TIDY,TIDY_site)
  COL_REMOVED <- rbind(COL_REMOVED,col_removed)
}

write.csv2(COL_REMOVED,"output/columns_removed.csv",row.names = F)


TIDY2 <- TIDY %>%
  # generate occurrenceID
  mutate(verbatimOccurrenceID = paste(Code_Sp,Site,Block,Plot,Treatment,Year,Month,Day,Rep,verbatimTraitName,sep = "_")) %>% 
  mutate(verbatimOccurrenceID_echantillon = paste(Code_Sp,Site,Block,Plot,Treatment,Year,Month,Day,Rep,sep = "_")) %>% 
  mutate(verbatimOccurrenceID_population = paste(Code_Sp,Site,Block,Plot,Treatment,Year,Month,Day,sep = "_")) %>% 
  select(-c(Code_Sp,Family,LifeForm1,LifeForm2)) %>% # remove columns that we do not want to keep
  filter(verbatimTraitName %in% trait_names) # remove traits that are not listed in MeasurementOrFact(traits)



# Corrections ####
TIDY3 <- TIDY2 %>% 
  filter(!(is.na(verbatimTraitValue))) %>% 
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




#_____________________________
# Change trait names ####
TIDY4 <- TIDY3 %>% 
  merge(correspondence_traits_old_new) %>% 
  select(-verbatimTraitName) %>% 
  rename(verbatimTraitName = verbatimTraitName_new)

# write.csv2(TIDY4,"output/Core_vavr2023.csv",row.names=F,fileEncoding = 'Latin1')
# TIDY4 <- read.csv2("output/Core_vavr2023.csv") %>% 
#   select(-X)

#_____________________________
# Add samplingProtocol and measurementMethod ####
info_traits <- MeasurementOrFact_traits %>% 
  select(-verbatimTraitName_old) %>% 
  rename(verbatimTraitName = verbatimTraitName_new)

# CHANGE MERGE for FULL_JOIN ?
# (Il y a des lignes qui disparaissent dans la procédure, alors que normalement j'ai séléectionné uniquement els traits listés dans MeasurementOrFact. Lesquels ?)

traits_identical_all_sites <-  info_traits %>% 
  filter(Site == "All") %>% 
  pull(verbatimTraitName)

traits_f_site <- info_traits %>% 
  filter(!(Site == "All")) %>% 
  pull(verbatimTraitName)

# quels traits sont mesurés à la fois dans les sites où il y a des différences, et dans "All"
intersect(traits_f_site,traits_identical_all_sites)
# Il y a juste LCC et TotStDMC pour lesquels il y a des méthodes de mesure différentes entre "All" et les sites particuliers
# Voir à quels sites ça correspond et dupliquer les lignes en conséquence

# TotRpDM: il y a des problèmes: 1)même chose appelée différemment dans traitName
# 2) ça s'appelle TotRpDM, alors que dans certains cas, mesuré juste sur l'organe

#__
## traits whose samplingProtocol and measurementMethod are identical whatever the site ####


# subset of core with these traits
TIDY4_commontraits <- TIDY4 %>% 
  filter(verbatimTraitName %in% traits_identical_all_sites) %>% 
  filter(!verbatimTraitName %in% c("LCC","TotStDM"))

intersect(TIDY4_commontraits %>% colnames(),
          info_traits %>% colnames())

# add columns of the MeasurementOrFact(traits) extension (to be splitted into extension later)
info_commontraits_tomerge <- info_traits %>% 
  filter(Site == "All") %>% 
  select(-Site)

TIDY4_commontraits_ext <- TIDY4_commontraits %>% 
  merge(info_commontraits_tomerge,by = c("verbatimTraitName"))

TIDY4_commontraits %>% dim()
TIDY4_commontraits_ext %>% dim()
# ok, mêmes dimensions

#--
## traits whose samplingProtocol and measurementMethod differ depending on the site ####
# PB1: dans le CORE, l'info PDM et O2LA est dégradée en CAmp Redon. Changer à la lecture de ces sites le nom du site, puis le rechanger à la toute fin.
# PB2: il manque des sites dans le fichier de mapping des traits!!



# subset of core with these traits
TIDY4_differingtraits <- TIDY4 %>% 
  filter(verbatimTraitName %in% traits_f_site)

intersect(TIDY4_differingtraits %>% colnames(),
          info_traits %>% colnames())

# add columns of the MeasurementOrFact(traits) extension (to be splitted into extension later)
info_traits_tomerge <- info_traits %>% filter(!(Site == "All"))

info_traits_tomerge %>% pull(Site) %>% unique()
TIDY4_differingtraits %>% pull(Site) %>% unique()

# left join (pour database sans perte de données)
TIDY4_differingtraits_ext <- TIDY4_differingtraits %>% 
  left_join(info_traits_tomerge, by = c("Site","verbatimTraitName"))

# merge (pour voir ce qui manque)
TIDY4_differingtraits_ext_check <- TIDY4_differingtraits %>% 
  merge(info_traits_tomerge, by = c("Site","verbatimTraitName"))

TIDY4_differingtraits %>% dim()
TIDY4_differingtraits_ext_check %>% dim()

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




TIDY5 <- rbind(TIDY4_commontraits_ext,TIDY4_differingtraits_ext)
dim(TIDY4)
dim(TIDY5)
# on a perdu des lignes! ... ou rajouté, au choix.

dim(TIDY5)

TIDY5 %>% 
  group_by(nameOfProject) %>% 
  summarize(n = n())




# generate core and extension ####

core <- TIDY5 %>% 
  select(Site,	Block	,Plot,	Treatment,	Year,	Month,	Day,	Species,
         Entity,	Rep,	feuillet ,verbatimTraitName,	verbatimTraitValue	, nameOfProject,	measurementDeterminedBy,
         verbatimOccurrenceID, verbatimOccurrenceID_echantillon,	verbatimOccurrenceID_population	
)


write.table(core,"output/Core_vavr2023.csv",fileEncoding = "UTF-8",
            row.names=F,sep="\t")


MeasurementOrFact <- TIDY5 %>% 
  select(verbatimOccurrenceID,Site,	verbatimTraitName,	traitName,	traitEntity,	Quality,	verbatimTraitUnit,
         LocalIdentifier,	traitID	,samplingProtocol,	measurementMethod
)

write.csv2(MeasurementOrFact,"output/MeasurementOrFact(traits).csv",fileEncoding = "Latin1",row.names=F)


core_subsample <- core[sample(10000, ), ]
MeasurementOrFact_subsample <- MeasurementOrFact[sample(10000, ), ]
write.csv2(core_subsample,"output/core_subsample.csv",fileEncoding = "Latin1",row.names=F)
write.csv2(MeasurementOrFact_subsample,"output/MeasurementOrFact_subsample(traits).csv",fileEncoding = "Latin1",row.names=F)



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
