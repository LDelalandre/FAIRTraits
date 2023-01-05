library(tidyverse)
library("openxlsx")


# functions
source("scripts/functions/TAXREF and TOP.R")
source("scripts/1_read_files.R")


# TaXREF and TOP
taxref <- read.csv2("data/TAXREF/TAXREF14.0_FR_Continental_13_07_2021.csv") # long to charge
TOP <- read.csv2("data/traits/traits_V2.csv",fill=T)


#_______________________________________________________________________________
# Tidy ####
# generate data frame in the good format for the data paper
columns_other_than_traits <- 
  c("Site", "Block" ,"Plot", "Treatment", "Year"     ,   "Day"       ,       "Species" , "Code_Sp"   ,    "Family" ,
    "LifeForm1" ,"LifeForm2" , 
    "plantType",	"leafType",
    "Rep",
    "nameOfProject" ,"measurementDeterminedBy","verbatimOccurrenceID",
    #columns for gas exchange:
    "dataType"  , "instrumentOutputStatus", "leafStatus" , "leafAreaBasis", "leafAreaMethod"  ,
    "plantAge" , "leafAge", "canopyPosition" ,"lightExposure" , "timeOfDay"   )

TIDY <- NULL

fsites <- sites[c(1,2,3,4,6)] # focal sites
fsites <- sites[c(2)] # focal sites

# NB : il faut peut-être ajouter le paddock dans verbatim occurrence id pour tous les sites. Fait pour la Fage.
for (site in sites){ # site
  files <- read_files(site)
  
  for (i in c(1:length(files))){ # excel sheet
    focus <- files[[i]]
    
    # which of columns_other_than_traits are present in the focal sheet
    fcolnames <- colnames(focus)
    fcolumns_other_than_traits <- intersect(fcolnames,columns_other_than_traits)
    
    focus_traits <- focus %>%
      select(!all_of(fcolumns_other_than_traits)) %>%
      colnames() # extract the name of the traits available on that sheet
    
    for (trait in focus_traits){
      focus_trait <- focus %>% 
        select(all_of(fcolumns_other_than_traits),all_of(trait)) %>% 
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
                              TRUE ~ Species)) %>% 
  filter(!(Species == "Geranium dissectum - pétiole"))

write.csv2(TIDY2,"output/Traitdata_test.csv",row.names=F)

#_______________________________________________________________________________
# TOP ####
TIDY <- read.csv2("output/Traitdata_test.csv")
TIDY[1:10,] %>% 
  mutate(site2 = str_replace(Site," ",""))


# III.2) Add Standard trait name from TOP
TOP2 <- TOP %>% 
  select(c("verbatimTraitName","traitName","LocalIdentifier","traitID","verbatimTraitUnit"))
focus_trait_TOP <- merge(focus_trait,TOP2,by="verbatimTraitName")
# focus_trait_TOP$Day <- as.character(focus_trait_TOP$Day)


#_______________________________________________________________________________
# TAXREF ####
list_sp <- read.csv2("output/Species_Code_Sp.csv")

list_sp_scientificName <- list_sp %>% 
  mutate(scientificName = map_chr(Species,get_scientificName,taxref)) 

list_sp_name_id <- list_sp_scientificName %>% 
  mutate(taxonID = map_chr(Species,get_taxon_id,taxref)) %>% 
  mutate(scientificName = if_else(Species == "Myosostis ramosissima subsp. ramosissima","Myosotis ramosissima Rochel, 1814 subsp. ramosissima",scientificName)) %>% 
  mutate(taxonID = if_else(Species == "Myosostis ramosissima subsp. ramosissima","https://inpn.mnhn.fr/espece/cd_nom/137934",taxonID))

write.csv2(list_sp_name_id,"output/list of species_TAXREF.csv",row.names=F)

# Add taxref info ####
# NB: in fine, I will add all the info from the species data frame (LifeForm1, etc.), 
# with the info from the floras, etc.
list_sp_name_id_short <- list_sp_name_id %>% 
  select(Species,scientificName,taxonID)

TIDY_TAXREF <- TIDY %>% 
  merge(list_sp_name_id_short,by="Species") %>% # add TAXREF info
  rename(verbatimScientificName = Species)

write.csv2(TIDY_TAXREF,"output/Traitdata_test_taxref.csv",row.names=F)


#_______________________________________________________________________________
# List of all the traits ####
TIDY <- read.csv2("output/Traitdata_test.csv")

list_of_traits <- TIDY %>% 
  pull(verbatimTraitName) %>% 
  unique() 

df_traits <- data.frame(verbatimTraitName = list_of_traits)
write.csv2(df_traits,"output/list of traits.csv",row.names=F)
