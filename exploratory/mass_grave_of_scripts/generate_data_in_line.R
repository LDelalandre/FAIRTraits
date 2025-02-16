library(tidyverse)
library("openxlsx")

# functions
source("scripts/functions/1_read_files.R")

library(tidyverse)
library("openxlsx")

sites <- c("LaFage","Bargemon","Cazarils","CampRedon","PDM","Garraf","HGM")


# En cours: Importer automatiquement tous les feuillets pour un site ####
# A utiliser quand les fichiers source seront propores (noms de colonnes définitifs...)
read_file <- function(fsite){
  # Define files to read in a data frame whose columns are:
  # - site, the name of the site
  # - file, the name of the excel file to import
  DATA_FILES <- c("LaFage_PlantTraitsDP_vjan2023.xlsx",
                  "Cazarils_PlantTraitsDP_vjan2023.xlsx" ,
                  "CRE_PDM_PlantTraitsDP_vjan2023.xlsx" ,
                  "CRE_O2LA_PlantTraitsDP_vjan2023.xlsx",
                  "Garraf_PlantTraitsDP_vjan2023.xlsx" ,
                  "HGM_PlantTraitsDP_vjan2023.xlsx" ,
                  "LesAgros_PlantTraitsDP_vjan2023.xlsx"
  )
  sites <- c("LaFage","Cazarils","PDM","O2LA","Garraf","HGM","LesAgros")
  SITES <- data.frame(cbind(sites,DATA_FILES))
  colnames(SITES) <- c("site","file")
  
  # Extract the name of the excel file corresponding to the site on which we work (i.e., fsite)
  filename <- SITES %>% 
    filter(site == fsite) %>% 
    pull(file)
  # get the path to that file
  path_filename <- paste0("data/",filename)
  
  # Import the sheets of that file in a list
  sheets <- readxl::excel_sheets(path_filename)
  sheets2 <- sheets[-c(1,2)] # remove Metadata and Climate data, to keep only trait data
  
  x <- lapply(sheets2, function(X) readxl::read_excel(path_filename, sheet = X))
  names(x) <- sheets2
  x
}



columns_other_than_traits <- 
  c("Site", "Block" ,"Plot", "Treatment", "Year" ,  "Month"  ,   "Day"       ,       "Species" , "Code_Sp"   ,    "Family" ,
    "LifeForm1" ,"LifeForm2" , 
    "Entity",    "Rep",
    "nameOfProject" ,"measurementDeterminedBy"
    #"Published in" # for MLoss_Roots
  )
#,"verbatimOccurrenceID")


focalsite <- "PDM"
fsheets <- read_file(fsite = focalsite)

fsheets$MLoss_Roots %>% View()



fsites <- sites # focal sites

TIDY <- NULL
# NB : il faut peut-être ajouter le paddock dans verbatim occurrence id pour tous les sites. Fait pour la Fage.
for (focalsite in fsites){ # site
  files <- read_file(focalsite)
  
  for (i in c(1:length(files))){ 
    # work on each excel sheet successively
    focus <- files[[i]]
    
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
      
      TIDY <- rbind(TIDY,focus_trait)
    }
    
  }
}

TIDY2 <- TIDY %>% 
  mutate(verbatimOccurrenceID = paste(Code_Sp,Site,Block,Plot,Treatment,Day,Rep,sep = "_"))


# correct typos in species names (good correction of names ; in the file list of species, just temporary)
# IL Y A SANS DOUTE ENCORE BEAUCOUP D'ERREURS A CORRIGER
TIDY3 <- TIDY2 %>% 
  mutate(Code_Sp = case_when( Species == "Carex humilis?" & Code_Sp == "CAREHUMI" ~ "CARESP",
                              TRUE ~ Code_Sp)) %>% 
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
                              Species == "Festuca christiani-bernardii" ~ "Festuca christiani-bernardi",
                              # "Linum tenuifolium subsp. tenuifolium" n'existe pas dans TAXREF
                              TRUE ~ Species)) %>% 
  filter(!(Species == "Geranium dissectum - pétiole")) %>% 
  mutate(Code_Sp = case_when(Code_Sp == "AMPEMAURI" ~ "AMPEMAUR",
                             Code_Sp == "AVENBRO" ~ "AVENBROM",
                             Code_Sp == "CATACOER" ~ "CATACAER",
                             Code_Sp == "DACTGLOM" ~ "DACTGLOM-HIS",
                             Code_Sp == "HELISTOE" ~ "HELISTOE-STO",
                             Code_Sp == "RUBUSPEC" ~ "RUBUSP",
                             Code_Sp == "VIOLALBA" ~ "VIOLALBA-SCO",
                             Code_Sp == "XEREINAP" ~ "XERAINAP",
                             TRUE ~Code_Sp
  ))

write.csv2(TIDY3,"output/Core_vavr2023.csv",row.names=F,fileEncoding = 'Latin1')
