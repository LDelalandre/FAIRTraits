library(tidyverse)
library("openxlsx")

# Names of traits that we keep
MeasurementOrFact_traits <- openxlsx::read.xlsx("data/Versions_Avril2023/Supp_Extensions_FAIRTraits_vavril2023.xlsx", sheet = "MeasurementOrFact(traits)", startRow = 1, colNames = TRUE)
trait_names <- MeasurementOrFact_traits$verbatimTraitName

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
                             Species == "Festuca christiani-bernardi" ~ "Festuca christiani-bernardii",
                             Species == "Geranium dissectum - limbe" ~ "Geranium dissectum",
                             Species == "Festuca ovina (sp)?" ~ "Festuca christiani-bernardii",
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
                              TRUE ~ Species)) 


write.csv2(TIDY3,"output/Core_vavr2023.csv",row.names=F,fileEncoding = 'Latin1')

#_______________________________________________________________________________
# vérifications ####

core <- read.csv2("output/Core_vavr2023.csv")
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
