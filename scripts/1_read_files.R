library(tidyverse)
library("openxlsx")

# Traits ####
DATA_FILES <- c("LaFage_PlantTraitsDP_vp.xlsx",
                "Bargemon_PlantTraitsDP_vp.xlsx",
                "Cazarils_PlantTraitsDP_vp.xlsx" ,
                "CampRedon_PlantTraitsDP_vp.xlsx",
                "CRE_PDM_PlantTraitsDP_vp.xlsx" ,
                "Garraf_PlantTraitsDP_vp.xlsx" ,
                "HGM_PlantTraitsDP_vp.xlsx" 
)
sites <- c("LaFage","Bargemont","Cazarils","CampRedon","PDM","Garraf","HGM")

SITES <- data.frame(cbind(sites,DATA_FILES))
colnames(SITES) <- c("site","file")


read_files <- function(site){ # Calls the following (site-specific) functions
  if (site == "LaFage"){
    read_LaFage(SITES)
  } else if (site == "Bargemont"){
    read_Bargemont(SITES)
  } else if (site == "Cazarils"){
    read_Cazarils(SITES)
  } else if (site == "CampRedon"){
    read_CampRedon(SITES)
  } else if (site == "Garraf"){
    read_Garraf(SITES)
  } else if (site == "HGM"){
    read.table("data/species_HGM.txt",sep="\t",header=T) %>% 
      unique() # species from HGM
  } else if (site == "PDM"){
    read.table("data/species_PDM.txt",sep="\t",header=T) %>% 
      unique() # species from PDM
  } else{
    print("sites must belong to:")
    print(sites)
  }
} 


read_LaFage <- function(SITES){
  data_file <- SITES %>% 
    filter(site=="LaFage") %>% 
    pull(file)
  
  LeafMorpho <-  read.xlsx(paste0("data/",data_file), sheet = "LeafMorpho_traits", startRow = 1, colNames = TRUE) %>% 
    mutate(Day = as.Date(Day- 25569, origin = "1970-01-01")) %>% 
    mutate(verbatimOccurrenceID = paste(Code_Sp,Site,Block,Plot,Treatment,Day,Rep,sep = "_")) 

  LeafCN <- read.xlsx(paste0("data/",data_file), sheet = "LeafC&N", startRow = 1, colNames = TRUE) %>% 
    mutate(Day = as.Date(Day- 25569, origin = "1970-01-01")) %>% 
    mutate(verbatimOccurrenceID = paste(Code_Sp,Site,Block,Plot,Treatment,Day,Rep,sep = "_")) 
  
  LeafP <- read.xlsx(paste0("data/",data_file), sheet = "LeafP", startRow = 1, colNames = TRUE) %>% 
    mutate(Day = as.Date(Day- 25569, origin = "1970-01-01")) %>% 
    mutate(verbatimOccurrenceID = paste(Code_Sp,Site,Block,Plot,Treatment,Day,Rep,sep = "_")) 
  
  Leaf13C15N <- read.xlsx(paste0("data/",data_file), sheet = "Leaf13C", startRow = 1, colNames = TRUE) %>% 
    mutate(Day = as.Date(Day- 25569, origin = "1970-01-01")) %>% 
    mutate(verbatimOccurrenceID = paste(Code_Sp,Site,Block,Plot,Treatment,Day,Rep,sep = "_")) 
  
  Biovolume <- read.xlsx(paste0("data/",data_file), sheet = "Biovolume", startRow = 1, colNames = TRUE) %>% 
    mutate(Day = as.Date(Day- 25569, origin = "1970-01-01")) %>% 
    mutate(verbatimOccurrenceID = paste(Code_Sp,Site,Block,Plot,Treatment,Day,Rep,sep = "_")) 
  
  Pheno <- read.xlsx(paste0("data/",data_file), sheet = "Pheno", startRow = 1, colNames = TRUE) %>% 
    mutate(Rep = "none") %>% 
    mutate(Day = "none") %>%
    mutate(verbatimOccurrenceID = paste(Code_Sp,Site,Block,Plot,Treatment,Day,Rep,sep = "_")) 
  
  Seed <- read.xlsx(paste0("data/",data_file), sheet = "Seed", startRow = 1, colNames = TRUE) %>% 
    mutate(Day = as.Date(Day- 25569, origin = "1970-01-01")) %>% 
    mutate(verbatimOccurrenceID = paste(Code_Sp,Site,Block,Plot,Treatment,Day,Rep,sep = "_")) 
  
  list(LeafMorpho,LeafCN,LeafP,Leaf13C15N,Biovolume,Pheno,Seed)
}

read_Bargemont <- function(SITES){
  data_file <- SITES %>% 
    filter(site=="Bargemont") %>% 
    pull(file)
  
  LeafMorpho <-  read.xlsx(paste0("data/",data_file), sheet = "LeafMorpho_traits", startRow = 1, colNames = TRUE) %>% 
    mutate(Day = as.Date(Day- 25569, origin = "1970-01-01")) %>% 
    mutate(verbatimOccurrenceID = paste(Code_Sp,Site,Block,Plot,Treatment,Day,Rep,sep = "_")) 
  
  LeafCN <- read.xlsx(paste0("data/",data_file), sheet = "LeafC&N", startRow = 1, colNames = TRUE) %>% 
    mutate(Day = as.Date(Day- 25569, origin = "1970-01-01")) %>% 
    mutate(verbatimOccurrenceID = paste(Code_Sp,Site,Block,Plot,Treatment,Day,Rep,sep = "_")) 
  
  Leaf13C15N <- read.xlsx(paste0("data/",data_file), sheet = "Leaf13C&15N", startRow = 1, colNames = TRUE) %>% 
    mutate(Day = as.Date(Day- 25569, origin = "1970-01-01")) %>% 
    mutate(verbatimOccurrenceID = paste(Code_Sp,Site,Block,Plot,Treatment,Day,Rep,sep = "_")) 
  
  list(LeafMorpho,LeafCN,Leaf13C15N)
}

read_Cazarils <- function(SITES){
  data_file <- SITES %>% 
    filter(site=="Cazarils") %>% 
    pull(file)
  
  LeafMorpho <-  read.xlsx(paste0("data/",data_file), sheet = "LeafMorpho_traits", startRow = 1, colNames = TRUE) %>% 
    mutate(Day = as.Date(Day- 25569, origin = "1970-01-01")) %>% 
    mutate(verbatimOccurrenceID = paste(Code_Sp,Site,Block,Plot,Treatment,Day,Rep,sep = "_")) 
  
  LeafDimensions <- read.xlsx(paste0("data/",data_file), sheet = "LeafDimensions", startRow = 1, colNames = TRUE) %>% 
    mutate(Day = as.Date(Day- 25569, origin = "1970-01-01")) %>% 
    mutate(verbatimOccurrenceID = paste(Code_Sp,Site,Block,Plot,Treatment,Day,Rep,sep = "_")) 
  
  LeafCN <- read.xlsx(paste0("data/",data_file), sheet = "LeafC&N", startRow = 1, colNames = TRUE) %>% 
    mutate(Day = as.Date(Day- 25569, origin = "1970-01-01")) %>% 
    mutate(verbatimOccurrenceID = paste(Code_Sp,Site,Block,Plot,Treatment,Day,Rep,sep = "_")) 
  
  LeafP <- read.xlsx(paste0("data/",data_file), sheet = "LeafP", startRow = 1, colNames = TRUE) %>% 
    mutate(Day = as.Date(Day- 25569, origin = "1970-01-01")) %>% 
    mutate(verbatimOccurrenceID = paste(Code_Sp,Site,Block,Plot,Treatment,Day,Rep,sep = "_")) 
  
  Leaf13C15N <- read.xlsx(paste0("data/",data_file), sheet = "Leaf13C&15N", startRow = 1, colNames = TRUE) %>% 
    mutate(Day = as.Date(Day- 25569, origin = "1970-01-01")) %>% 
    mutate(verbatimOccurrenceID = paste(Code_Sp,Site,Block,Plot,Treatment,Day,Rep,sep = "_")) 
  
  Biovolume <- read.xlsx(paste0("data/",data_file), sheet = "Biovolume", startRow = 1, colNames = TRUE) %>% 
    # mutate(Day = as.Date(Day- 25569, origin = "1970-01-01")) %>%
    # Problem : Day parfois de la forme xx/5 --> Lecture non possible
    mutate(verbatimOccurrenceID = paste(Code_Sp,Site,Block,Plot,Treatment,Day,Rep,sep = "_")) 
  
  Pheno <- read.xlsx(paste0("data/",data_file), sheet = "Pheno", startRow = 1, colNames = TRUE) %>% 
    mutate(Rep = "none") %>% 
    mutate(Day = as.Date(Day- 25569, origin = "1970-01-01")) %>% 
    mutate(verbatimOccurrenceID = paste(Code_Sp,Site,Block,Plot,Treatment,Day,Rep,sep = "_")) 
  
  Seed <- read.xlsx(paste0("data/",data_file), sheet = "SeedM", startRow = 1, colNames = TRUE) %>% 
    mutate(Day = as.Date(Day- 25569, origin = "1970-01-01")) %>% 
    mutate(verbatimOccurrenceID = paste(Code_Sp,Site,Block,Plot,Treatment,Day,Rep,sep = "_")) 
  
  SeedS <- read.xlsx(paste0("data/",data_file), sheet = "SeedS", startRow = 1, colNames = TRUE) %>% 
    mutate(Day = as.Date(Day- 25569, origin = "1970-01-01")) %>% 
    mutate(verbatimOccurrenceID = paste(Code_Sp,Site,Block,Plot,Treatment,Day,Rep,sep = "_")) 
  
  list(LeafMorpho,LeafDimensions,LeafCN,LeafP,Leaf13C15N,Biovolume,Pheno,Seed,SeedS)
}

read_CampRedon <- function(SITES){
  data_file <- SITES %>% 
    filter(site=="CampRedon") %>% 
    pull(file)
  
  LeafMorpho <-  read.xlsx(paste0("data/",data_file), sheet = "LeafMorpho_traits", startRow = 1, colNames = TRUE) %>% 
    mutate(Day = as.Date(Day- 25569, origin = "1970-01-01")) %>% 
    mutate(verbatimOccurrenceID = paste(Code_Sp,Site,Block,Plot,Treatment,Day,Rep,sep = "_")) 
  
  LeafElements <- read.xlsx(paste0("data/",data_file), sheet = "LeafElements", startRow = 1, colNames = TRUE) %>% 
    mutate(Day = as.Date(Day- 25569, origin = "1970-01-01")) %>% 
    mutate(verbatimOccurrenceID = paste(Code_Sp,Site,Block,Plot,Treatment,Day,Rep,sep = "_")) 
  
  LeafIsotopes <- read.xlsx(paste0("data/",data_file), sheet = "LeafIsotopes", startRow = 1, colNames = TRUE) %>% 
    mutate(Day = as.Date(Day- 25569, origin = "1970-01-01")) %>% 
    mutate(verbatimOccurrenceID = paste(Code_Sp,Site,Block,Plot,Treatment,Day,Rep,sep = "_")) 
  
  LeafOtherElements <- read.xlsx(paste0("data/",data_file), sheet = "LeafOtherElements", startRow = 1, colNames = TRUE) %>% 
    mutate(Day = as.Date(Day- 25569, origin = "1970-01-01")) %>% 
    mutate(verbatimOccurrenceID = paste(Code_Sp,Site,Block,Plot,Treatment,Day,Rep,sep = "_")) 
  
  list(LeafMorpho,LeafElements,LeafIsotopes,LeafOtherElements)
}

read_Garraf <- function(SITES){
  data_file <- SITES %>% 
    filter(site=="Garraf") %>% 
    pull(file)
  
  LeafMorpho <-  read.xlsx(paste0("data/",data_file), sheet = "LeafMorpho_traits", startRow = 1, colNames = TRUE) %>% 
    mutate(Day = as.Date(Day- 25569, origin = "1970-01-01")) %>% 
    mutate(verbatimOccurrenceID = paste(Code_Sp,Site,Block,Plot,Treatment,Day,Rep,sep = "_")) 
  
  LeafDimensions <- read.xlsx(paste0("data/",data_file), sheet = "LeafDimensions", startRow = 1, colNames = TRUE) %>% 
    mutate(Day = as.Date(Day- 25569, origin = "1970-01-01")) %>% 
    mutate(verbatimOccurrenceID = paste(Code_Sp,Site,Block,Plot,Treatment,Day,Rep,sep = "_")) 
  
  LeafCN <- read.xlsx(paste0("data/",data_file), sheet = "LeafC&N", startRow = 1, colNames = TRUE) %>% 
    mutate(Day = as.Date(Day- 25569, origin = "1970-01-01")) %>% 
    mutate(verbatimOccurrenceID = paste(Code_Sp,Site,Block,Plot,Treatment,Day,Rep,sep = "_")) 
  
  LeafP <- read.xlsx(paste0("data/",data_file), sheet = "LeafP", startRow = 1, colNames = TRUE) %>% 
    mutate(Day = as.Date(Day- 25569, origin = "1970-01-01")) %>% 
    mutate(verbatimOccurrenceID = paste(Code_Sp,Site,Block,Plot,Treatment,Day,Rep,sep = "_")) 
  
  Leaf13C15N <- read.xlsx(paste0("data/",data_file), sheet = "Leaf13C&15N", startRow = 1, colNames = TRUE) %>% 
    mutate(Day = as.Date(Day- 25569, origin = "1970-01-01")) %>% 
    mutate(verbatimOccurrenceID = paste(Code_Sp,Site,Block,Plot,Treatment,Day,Rep,sep = "_")) 
  
  Biovolume <- read.xlsx(paste0("data/",data_file), sheet = "Biovolume", startRow = 1, colNames = TRUE) %>% 
    # mutate(Day = as.Date(Day- 25569, origin = "1970-01-01")) %>%
    # Problem : Day parfois de la forme xx/5 --> Lecture non possible
    mutate(verbatimOccurrenceID = paste(Code_Sp,Site,Block,Plot,Treatment,Day,Rep,sep = "_")) 
  
  Seed <- read.xlsx(paste0("data/",data_file), sheet = "SeedM", startRow = 1, colNames = TRUE) %>% 
    mutate(Day = as.Date(Day- 25569, origin = "1970-01-01")) %>% 
    mutate(verbatimOccurrenceID = paste(Code_Sp,Site,Block,Plot,Treatment,Day,Rep,sep = "_"))
  
  list(LeafMorpho,LeafDimensions,LeafCN,LeafP,Leaf13C15N,Biovolume,Seed)
}






