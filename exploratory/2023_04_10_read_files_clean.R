library(tidyverse)
library("openxlsx")

sites <- c("LaFage","Bargemon","Cazarils","CampRedon","PDM","Garraf","HGM")

columns_other_than_traits <- 
  c("Site", "Block" ,"Plot", "Treatment", "Year" ,  "Month"  ,   "Day"       ,       "Species" , "Code_Sp"   ,    "Family" ,
    "LifeForm1" ,"LifeForm2" , 
    "Entity",    "Rep",
    "nameOfProject" ,"measurementDeterminedBy"
    #"Published in" # for MLoss_Roots
  )

# functions ####
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


change_format <- function(data_imported){
  # to change date format
  data_imported %>% 
    mutate(Day = as.Date(Day- 25569, origin = "1970-01-01")) %>% 
    mutate(Day2 = str_replace_all(Day,"-","")) %>% 
    mutate(Site2 = str_replace(Site," ","")) %>% 
    mutate(Treatment2 = str_replace_all(Treatment,"_","")) %>% 
    mutate(verbatimOccurrenceID = paste(Code_Sp,Site2,Block,Plot,Treatment2,Day2,Rep,sep = "_")) %>% 
    select(-c(Day2,Site2,Treatment2))
}





# Centralize data in one data frame with info in row ####

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

write.csv2(TIDY2,"output/Core_vavr2023.csv",row.names=F,fileEncoding = 'Latin1')



