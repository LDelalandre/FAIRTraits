library(tidyverse)
library("openxlsx")

# Problems : 
# Biovolume à Cazarils : date pas bonne. Et du coup pas bon verbatimOccurrenceID
# Idem à Garraf

# Pour tous les Gas exchange: 
# mutate(nameOfProject = NA, measurementDeterminedBy = NA)
# A virer quand l'info sera complétée


# Traits ####
DATA_FILES <- c("LaFage_PlantTraitsDP_vp.xlsx",
                "Bargemon_PlantTraitsDP_vp.xlsx",
                "Cazarils_PlantTraitsDP_vp.xlsx" ,
                "CampRedon_PlantTraitsDP_vp.xlsx",
                "CRE_PDM_PlantTraitsDP_vp.xlsx" ,
                "Garraf_PlantTraitsDP_vp.xlsx" ,
                "HGM_PlantTraitsDP_vp.xlsx" 
)
sites <- c("LaFage","Bargemon","Cazarils","CampRedon","PDM","Garraf","HGM")

SITES <- data.frame(cbind(sites,DATA_FILES))
colnames(SITES) <- c("site","file")


#_______________________________________________________________________________
# Version antérieure ####
read_files <- function(site){ # Calls the following (site-specific) functions
  if (site == "LaFage"){
    read_LaFage(SITES)
  } else if (site == "Bargemon"){
    read_Bargemon(SITES)
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



change_format <- function(data_imported){
  data_imported %>% 
    mutate(Day = as.Date(Day- 25569, origin = "1970-01-01")) %>% 
    mutate(Day2 = str_replace_all(Day,"-","")) %>% 
    mutate(Site2 = str_replace(Site," ","")) %>% 
    mutate(Treatment2 = str_replace_all(Treatment,"_","")) %>% 
    mutate(verbatimOccurrenceID = paste(Code_Sp,Site2,Block,Plot,Treatment2,Day2,Rep,sep = "_")) %>% 
    select(-c(Day2,Site2,Treatment2))
}

read_LaFage <- function(SITES){
  data_file <- SITES %>% 
    filter(site=="LaFage") %>% 
    pull(file)
  
  LeafMorpho <-  read.xlsx(paste0("data/",data_file), sheet = "LeafMorpho_traits", startRow = 1, colNames = TRUE) %>% 
    change_format()

  LeafCN <- read.xlsx(paste0("data/",data_file), sheet = "LeafC&N", startRow = 1, colNames = TRUE) %>% 
    change_format()
  
  LeafP <- read.xlsx(paste0("data/",data_file), sheet = "LeafP", startRow = 1, colNames = TRUE) %>% 
    change_format()
  
  Leaf13C15N <- read.xlsx(paste0("data/",data_file), sheet = "Leaf13C", startRow = 1, colNames = TRUE) %>% 
    change_format()
  
  Biovolume <- read.xlsx(paste0("data/",data_file), sheet = "Biovolume", startRow = 1, colNames = TRUE) %>% 
    change_format()
  
  Pheno <- read.xlsx(paste0("data/",data_file), sheet = "Pheno", startRow = 1, colNames = TRUE) %>% 
    mutate(Rep = "none") %>% 
    mutate(Day = "none") %>%
    mutate(Day2 = str_replace_all(Day,"-","")) %>% 
    mutate(Site2 = str_replace(Site," ","")) %>% 
    mutate(Treatment2 = str_replace_all(Treatment,"_","")) %>% 
    mutate(verbatimOccurrenceID = paste(Code_Sp,Site2,Block,Plot,Treatment2,Day2,Plot,Rep,sep = "_")) %>% 
    select(-c(Day2,Site2,Treatment2))
  
  Seed <- read.xlsx(paste0("data/",data_file), sheet = "Seed", startRow = 1, colNames = TRUE) %>% 
    change_format()
  
  list(LeafMorpho,LeafCN,LeafP,Leaf13C15N,Biovolume,Pheno,Seed)
}

read_Bargemon <- function(SITES){
  data_file <- SITES %>% 
    filter(site=="Bargemon") %>% 
    pull(file)
  
  LeafMorpho <-  read.xlsx(paste0("data/",data_file), sheet = "LeafMorpho_traits", startRow = 1, colNames = TRUE) %>% 
    change_format()
  
  LeafCN <- read.xlsx(paste0("data/",data_file), sheet = "LeafC&N", startRow = 1, colNames = TRUE) %>% 
    change_format()
  
  Leaf13C15N <- read.xlsx(paste0("data/",data_file), sheet = "Leaf13C&15N", startRow = 1, colNames = TRUE) %>% 
    change_format()
  
  LLS <- read.xlsx(paste0("data/",data_file), sheet = "LLS", startRow = 1, colNames = TRUE) %>% 
    mutate(verbatimOccurrenceID = paste(Code_Sp,Site,Block,Plot,Treatment,Day,Rep,sep = "_")) 
  # Day = "none" --> I don't change the format
  
  GasExchange <- read.xlsx(paste0("data/",data_file), sheet = "GasExchange", startRow = 1, colNames = TRUE) %>% 
    change_format() %>% 
    mutate(nameOfProject = "MELODY", measurementDeterminedBy = "Catherine Roumet")
  
  list(LeafMorpho,LeafCN,Leaf13C15N,LLS,GasExchange)
}

read_Cazarils <- function(SITES){
  data_file <- SITES %>% 
    filter(site=="Cazarils") %>% 
    pull(file)
  
  LeafMorpho <-  read.xlsx(paste0("data/",data_file), sheet = "LeafMorpho_traits", startRow = 1, colNames = TRUE) %>% 
    change_format()
  
  LeafCN <- read.xlsx(paste0("data/",data_file), sheet = "LeafC&N", startRow = 1, colNames = TRUE) %>% 
    change_format()
  
  LeafP <- read.xlsx(paste0("data/",data_file), sheet = "LeafP", startRow = 1, colNames = TRUE) %>% 
    change_format()
  
  Leaf13C15N <- read.xlsx(paste0("data/",data_file), sheet = "Leaf13C&15N", startRow = 1, colNames = TRUE) %>% 
    change_format()
  
  Biovolume <- read.xlsx(paste0("data/",data_file), sheet = "Biovolume", startRow = 1, colNames = TRUE) %>% 
    # mutate(Day = as.Date(Day- 25569, origin = "1970-01-01")) %>%
    # Problem : Day parfois de la forme xx/5 --> Lecture non possible
    mutate(verbatimOccurrenceID = paste(Code_Sp,Site,Block,Plot,Treatment,Day,Rep,sep = "_")) 
  
  Pheno <- read.xlsx(paste0("data/",data_file), sheet = "Pheno", startRow = 1, colNames = TRUE) %>% 
    mutate(Rep = "none") %>% 
    mutate(verbatimOccurrenceID = paste(Code_Sp,Site,Block,Plot,Treatment,Day,Rep,sep = "_"))
  
  Seed <- read.xlsx(paste0("data/",data_file), sheet = "SeedM", startRow = 1, colNames = TRUE) %>% 
    change_format()
  
  SeedS <- read.xlsx(paste0("data/",data_file), sheet = "SeedS", startRow = 1, colNames = TRUE) %>% 
    change_format()
  
  LLS <- read.xlsx(paste0("data/",data_file), sheet = "LLS", startRow = 1, colNames = TRUE) %>% 
    mutate(verbatimOccurrenceID = paste(Code_Sp,Site,Block,Plot,Treatment,Day,Rep,sep = "_"))
  
  GasExchange <- read.xlsx(paste0("data/",data_file), sheet = "GasExchange", startRow = 1, colNames = TRUE) %>% 
    change_format() %>% 
    mutate(nameOfProject = "MELODY", measurementDeterminedBy = "Catherine Roumet")
  
  list(LeafMorpho,LeafCN,LeafP,Leaf13C15N,Biovolume,Pheno,Seed,SeedS,LLS,GasExchange,LLS,GasExchange)
}

read_CampRedon <- function(SITES){
  data_file <- SITES %>% 
    filter(site=="CampRedon") %>% 
    pull(file)
  
  LeafMorpho <-  read.xlsx(paste0("data/",data_file), sheet = "LeafMorpho_traits", startRow = 1, colNames = TRUE) %>% 
    change_format()
  
  LeafElements <- read.xlsx(paste0("data/",data_file), sheet = "LeafElements", startRow = 1, colNames = TRUE) %>% 
    change_format() 
  
  LeafIsotopes <- read.xlsx(paste0("data/",data_file), sheet = "LeafIsotopes", startRow = 1, colNames = TRUE) %>% 
    change_format()
  
  LeafOtherElements <- read.xlsx(paste0("data/",data_file), sheet = "LeafOtherElements", startRow = 1, colNames = TRUE) %>% 
    change_format()
  
  list(LeafMorpho,LeafElements,LeafIsotopes,LeafOtherElements)
}

read_Garraf <- function(SITES){
  data_file <- SITES %>% 
    filter(site=="Garraf") %>% 
    pull(file)
  
  LeafMorpho <-  read.xlsx(paste0("data/",data_file), sheet = "LeafMorpho_traits", startRow = 1, colNames = TRUE) %>% 
    change_format()
  
  LeafCN <- read.xlsx(paste0("data/",data_file), sheet = "LeafC&N", startRow = 1, colNames = TRUE) %>% 
    change_format()
  
  LeafP <- read.xlsx(paste0("data/",data_file), sheet = "LeafP", startRow = 1, colNames = TRUE) %>% 
    change_format() 
  
  Leaf13C15N <- read.xlsx(paste0("data/",data_file), sheet = "Leaf13C&15N", startRow = 1, colNames = TRUE) %>% 
    change_format()
  
  Biovolume <- read.xlsx(paste0("data/",data_file), sheet = "Biovolume", startRow = 1, colNames = TRUE) %>% 
    # mutate(Day = as.Date(Day- 25569, origin = "1970-01-01")) %>%
    # Problem : Day parfois de la forme xx/5 --> Lecture non possible
    mutate(verbatimOccurrenceID = paste(Code_Sp,Site,Block,Plot,Treatment,Day,Rep,sep = "_")) 
  
  Seed <- read.xlsx(paste0("data/",data_file), sheet = "SeedM", startRow = 1, colNames = TRUE) %>% 
    change_format()
  
  GasExchange <- read.xlsx(paste0("data/",data_file), sheet = "GasExchange", startRow = 1, colNames = TRUE) %>% 
    change_format() %>% 
    mutate(nameOfProject = "DynEcoMed", measurementDeterminedBy = "Catherine Roumet")
  
  list(LeafMorpho,LeafCN,LeafP,Leaf13C15N,Biovolume,Seed,GasExchange)
}



read_PDM <- function(SITES){
  data_file <- SITES %>% 
    filter(site=="PDM") %>% 
    pull(file)
  
  LeafMorpho <-  read.xlsx(paste0("data/",data_file), sheet = "LeafMorpho_traits", startRow = 1, colNames = TRUE) %>% 
    change_format()
  
  LeafCN <- read.xlsx(paste0("data/",data_file), sheet = "LeafC&N", startRow = 1, colNames = TRUE) %>% 
    change_format()
  
  LeafP <- read.xlsx(paste0("data/",data_file), sheet = "LeafP", startRow = 1, colNames = TRUE) %>% 
    change_format() 
  
  Leaf13C15N <- read.xlsx(paste0("data/",data_file), sheet = "Leaf13C&15N", startRow = 1, colNames = TRUE) %>% 
    change_format()
  
  Biovolume <- read.xlsx(paste0("data/",data_file), sheet = "Biovolume", startRow = 1, colNames = TRUE) %>% 
    # mutate(Day = as.Date(Day- 25569, origin = "1970-01-01")) %>%
    # Problem : Day parfois de la forme xx/5 --> Lecture non possible
    mutate(verbatimOccurrenceID = paste(Code_Sp,Site,Block,Plot,Treatment,Day,Rep,sep = "_")) 
  
  Seed <- read.xlsx(paste0("data/",data_file), sheet = "SeedM", startRow = 1, colNames = TRUE) %>% 
    change_format()
  
  GasExchange <- read.xlsx(paste0("data/",data_file), sheet = "GasExchange", startRow = 1, colNames = TRUE) %>% 
    change_format() %>% 
    mutate(nameOfProject = "DynEcoMed", measurementDeterminedBy = "Catherine Roumet")
  
  list(LeafMorpho,LeafCN,LeafP,Leaf13C15N,Biovolume,Seed,GasExchange)
}




