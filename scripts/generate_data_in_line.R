library(tidyverse)
library("openxlsx")

# functions
source("scripts/1_read_files.R")


#_______________________________________________________________________________
# Tidy ####
# generate data frame in the good format for the data paper (= lines)
columns_other_than_traits <- 
  c("Site", "Block" ,"Plot", "Treatment", "Year"     ,   "Day"       ,       "Species" , "Code_Sp"   ,    "Family" ,
    "LifeForm1" ,"LifeForm2" , 
    "Rep",
    "nameOfProject" ,"measurementDeterminedBy","verbatimOccurrenceID")

# columns to add in the species file
new_columns_sp_file <- c(    "plantType",	"leafType")

#columns for gas exchange:
columns_gas_exchange_metadata <- c(  "dataType"  , "instrumentOutputStatus", 
                                     "leafStatus" , "leafAreaBasis", "leafAreaMethod"  ,
                                     "plantAge" , "leafAge", "canopyPosition" ,"lightExposure" , "timeOfDay"   )
# /!\ these columns have to be added in the methods, linked to measurementOrFact,  and associated with
# verbatimOccurrenceID

# To this end, make a code that extracts all the columns linked tomeasurementOrFact and/or occurrence,
# and verbatimOccurrenceID, to have it in a different file.
# OR, add empty columns (with either "none" or NA) to the other sheets that do not have it
# Le plus simple : pour les feuillets d'échange gazeux, 


TIDY <- NULL

fsites <- sites[c(2,3,6)] # focal sites

# NB : il faut peut-être ajouter le paddock dans verbatim occurrence id pour tous les sites. Fait pour la Fage.
for (site in fsites){ # site
  files <- read_files(site)
  
  for (i in c(1:length(files))){ # excel sheet
    if (length(files))
      focus <- files[[i]]
    
    focus_traits <- focus %>%
      select(!all_of(columns_other_than_traits)) %>%
      colnames()  # extract the name of the traits available on that sheet
    
    
    # which are the colmuns that are missing? 
    missing_plant_leaf_type <- setdiff(new_columns_sp_file,colnames(focus))
    missing_metadata <- setdiff(columns_gas_exchange_metadata,colnames(focus))
    
    # Remove metadata shared by only few sheets (ie gas exchange)
    if ( !(length(intersect(focus_traits,columns_gas_exchange_metadata)) == 0) ){
      focus_traits <- setdiff(focus_traits,c(columns_gas_exchange_metadata,new_columns_sp_file))
    }
    
    for (trait in focus_traits){
      focus_trait <- focus %>% 
        select(all_of(columns_other_than_traits),all_of(trait)) %>% 
        # some columns, i.e. those in new_columns_sp_file and incolumns_gas_exchange_metadata, are not there
        gather(all_of(trait) ,key=verbatimTraitName,value=verbatimTraitValue)
      focus_trait$Day <- as.character(focus_trait$Day)
      
      TIDY <- rbind(TIDY,focus_trait)
    }
    
  }
}

# correct typos in species names (good correction of names ; in the file list of species, just temporary)
TIDY2 <- TIDY %>% 
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

write.csv2(TIDY2,"output/ETS_format_Bar_Caz_Gar.csv",row.names=F)