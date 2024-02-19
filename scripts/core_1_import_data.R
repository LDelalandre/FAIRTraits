library(tidyverse)
library("openxlsx")

# This script imports raw data from excel files and writes a single csv file with data in row

# functions ####
read_file <- function(fsite){
  # function to import all excel sheets of a site in a list
  
  # Define files to read in a data frame whose columns are:
  # - site, the name of the site
  # - file, the name of the excel file to import
  DATA_FILES <- c("Data_LaFage_FAIRTraits.xlsx",
                  "Data_Cazarils_FAIRTraits.xlsx",
                  "Data_CRE_PDM_FAIRTraits.xlsx",
                  "Data_CRE_O2LA_FAIRTraits.xlsx",
                  "Data_Garraf_FAIRTraits.xlsx",
                  "Data_HGM_FAIRTraits.xlsx",
                  "Data_LesAgros_FAIRTraits.xlsx"
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
  sheets2 <- sheets[!sheets %in% c("Metadata","Climate data")] # remove Metadata and Climate data, to keep only trait data
  
  x <- lapply(sheets2, function(X) readxl::read_excel(path_filename, sheet = X))
  names(x) <- sheets2
  x
}


# Centralize data in one "tidy" data frame with one record per row ####

sites <- c("LaFage","Cazarils","PDM","O2LA","Garraf","HGM","LesAgros")

feuillets_to_remove <- c("LeafDimensions (àsupprimer)","Climate data","Leaf_Thickness","Soil data",
                         "PDM Climatic data 1975-2006")

# colmuns other than traits used for building the "tidy" csv (i.e. data in row)
columns_other_than_traits <- 
  c("Site", "Block" ,"Plot", "Treatment", "Year" ,  "Month"  ,   "Day"       ,       "Species" , "Code_Sp"   ,    "Family" ,
    "LifeForm1" ,"LifeForm2" , 
    "Entity",    "Rep",
    "nameOfProject" ,"measurementDeterminedBy",
    "feuillet" # a supprimer à la fin, juste pour la traçabilité
    #"Published in" # for MLoss_Roots
  )

TIDY <- NULL # file that will contain all trait measurements in rows
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
          gather(all_of(trait) ,key=verbatimTraitName,value=verbatimTraitValue)
        focus_trait$Day <- as.character(focus_trait$Day)
        
        TIDY_site <- rbind(TIDY_site,focus_trait)
      }
    }
  }
  TIDY <- rbind(TIDY,TIDY_site)
}

# Change verbatimTraitValue to numeric
TIDY$verbatimTraitValue <- as.numeric(TIDY$verbatimTraitValue)
TIDY$Year <- as.numeric(TIDY$Year)
TIDY$Month <- as.numeric(TIDY$Month)
TIDY$Day <- as.numeric(TIDY$Day)

# Corrections (typos) ####
TIDY2 <- TIDY %>% 
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
                             Species == "Potentilla reptens" ~ "Potentilla reptans" ,
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
# remove NAs ####
  filter(!is.na(verbatimTraitValue)) %>% 
  unique()

# Export ####
# write.table(TIDY2 ,"output/TIDY.csv",fileEncoding = "latin1",row.names=F,sep="\t",dec = ".")
data.table::fwrite(TIDY2,"output/TIDY.csv",sep="\t")