library(tidyverse)
library("openxlsx")


# functions
source("scripts/functions/TAXREF and TOP.R")
source("scripts/1_read_files.R")


# TaXREF and TOP
# taxref <- read.csv2("data/TAXREF/TAXREF14.0_FR_Continental_13_07_2021.csv") # long to charge
taxref <- read.table("data/TAXREF_v16/TAXREFv16.txt",header=T,sep = "\t")
TOP <- read.csv2("data/traits/list of traits_jan2023_part_per_thousand.csv",fill=T,fileEncoding="latin1")


#_______________________________________________________________________________
# Tidy ####
# generate data frame in the good format for the data paper
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
                              TRUE ~ Species)) %>% 
  filter(!(Species == "Geranium dissectum - pétiole"))

write.csv2(TIDY2,"output/ETS_format_Bar_Caz_Gar.csv",row.names=F)

#_______________________________________________________________________________
# TOP ####
TIDY <- read.csv2("output/ETS_format_Bar_Caz_Gar.csv")

focus_trait_TOP <- merge(focus_trait,TOP,by="verbatimTraitName") %>% 
  select(all_of(colnames(TIDY)),everything())

TIDY_traits <- merge(TIDY,TOP,by="verbatimTraitName") %>% 
  select(all_of(colnames(TIDY)),everything()) %>% 
  relocate("verbatimTraitValue", .after = last_col()) %>% 
  relocate("verbatimTraitUnit", .after = last_col())%>% 
  relocate("measurementMethod", .after = last_col())

#_______________________________________________________________________________
# TAXREF ####
# list_sp <- read.csv2("output/Species_Code_Sp.csv")
list_sp <- TIDY %>% 
  select(Species,Code_Sp) %>% 
  unique()

list_sp_scientificName <- list_sp %>% 
  mutate(scientificName = map_chr(Species,get_scientificName,taxref)) 

list_sp_name_id <- list_sp_scientificName %>% 
  mutate(taxonID = map_chr(Species,get_taxon_id,taxref)) %>% 
  mutate(scientificName = if_else(Species == "Myosostis ramosissima subsp. ramosissima","Myosotis ramosissima Rochel, 1814 subsp. ramosissima",scientificName)) %>% 
  mutate(taxonID = if_else(Species == "Myosostis ramosissima subsp. ramosissima","https://inpn.mnhn.fr/espece/cd_nom/137934",taxonID))

list_sp_name_id_cd <- list_sp_name_id %>% 
  mutate(CD_NOM = map_chr(Species,get_CD_NOM,taxref)) 

write.csv2(list_sp_name_id,"output/list of species_TAXREF_Bar_Caz_Gar.csv",row.names=F)

# Add taxref info ####
# NB: in fine, I will add all the info from the species data frame (LifeForm1, etc.), 
# with the info from the floras, etc.

TIDY_traits_TAXREF <- TIDY_traits %>% 
  merge(list_sp_name_id,by=c("Species","Code_Sp")) %>% # add TAXREF info
  select(Species,scientificName,taxonID,everything(),verbatimTraitValue,verbatimTraitUnit) %>% 
  rename(verbatimScientificName = Species)

write.csv2(TIDY_traits_TAXREF,"output/ETS_format_Bar_Caz_Gar_traits_TAXREF.csv",row.names=F,fileEncoding="latin1")


#_______________________________________________________________________________
# List of all the traits ####
TIDY <- read.csv2("output/Traitdata_test.csv")

list_of_traits <- TIDY %>% 
  pull(verbatimTraitName) %>% 
  unique() 

df_traits <- data.frame(verbatimTraitName = list_of_traits)
write.csv2(df_traits,"output/list of traits.csv",row.names=F)
