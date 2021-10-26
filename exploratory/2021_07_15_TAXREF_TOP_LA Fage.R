library(tidyverse)
library("xlsx") 
# /!\ with this package, NAs are replaced by NaN within numeric columns. Il faudra que je les retire dans le fichier final

# function
source("functions/TAXREF and TOP.R")
site <- "La Fage"
# I) Import datasets ####
# I.1) TaXREF and TOP
taxref <- read.csv2("data/TAXREF/TAXREF14.0_FR_Continental_13_07_2021.csv") # long to charge
TOP <- read.csv2("data/TOP/TOP_info.csv",fill=T)

# I.2) Traits
# NB : pas besoin de LeafDimensions (info redondante avec LeafMorpho: que L_Area!)

LeafMorpho <-  read.xlsx2("data/La Fage/LaFage_PlantTraits.xls",sheetName = "LeafMorpho_traits",colClasses=NA)
  read.csv(paste0("data/",site,"/LeafMorpho_traits.csv"),sep=";" ,dec=",") %>% 
  mutate(verbatimOccurrenceID = paste("FAG",Treatment,Day,Code_Sp,Rep,sep = "_")) 

LeafDimensions <-read.csv(paste0("data/",site,"/LeafDimensions.csv"),sep=";",dec=",") %>% 
  mutate(verbatimOccurrenceID = paste("FAG",Treatment,Day,Code_Sp,Rep,sep = "_"))

LeafCN <- read.csv(paste0("data/",site,"/LeafC&N.csv"),sep=";",dec=",") %>% 
  mutate(verbatimOccurrenceID = paste("FAG",Treatment,Day,Code_Sp,Rep,sep = "_"))

LeafP <- read.csv(paste0("data/",site,"/LeafP.csv"),sep=";",dec=",") %>% 
  mutate(verbatimOccurrenceID = paste("FAG",Treatment,Day,Code_Sp,Rep,sep = "_")) 

Biovolume <- read.csv(paste0("data/",site,"/Biovolume.csv"),sep=";",dec=",") %>% 
  mutate(verbatimOccurrenceID = paste("FAG",Treatment,Day,Code_Sp,Rep,sep = "_")) 

Seed <- read.csv(paste0("data/",site,"/Seed.csv"),sep=";",dec=",") %>% 
  mutate(verbatimOccurrenceID = paste("FAG",Treatment,Day,Code_Sp,Rep,sep = "_")) 

# NB pour l'occurrenceID, je ferais peut-être mieux de ne pas pooler en une colonne, mais de faire comme pour measurementOfFact:
# 1) Inclure les colonnes de l'extension Occurrence dans le tableau Traitdata
# 2) Pour chaque ligne différente, donner un identifiant unique (commençant par FAG pour ne pas confondre les jeux de données)
# 3) Séparer les tableaux ensuite.
# 4) Supprimer les lignes répétée du tableau Occurrence (une ligne par observation ou individu suffit).
# 5) Quand tout ça sera propre, je pourrai donner un occurrenceID plus court, et qui correspondra à mon 
# verbatimOccurrenceID (bijection), pour faire la correspondance.
# 6) Veiller à la correspondance entre cet occurrenceID du tableau Traitdata et dee l'extension Occurrence.


# II) TAXREF info ####
TRAITS <- list(c("SLA","LDMC","L_Area"), # list of traits for each group of traits
               c("L_Area"), # Pas besoin, déjà dans LeafMorpho !!! (à checker quand même)
               c("LNC","LCC"),
               c("LPC"),
               c("Hveg","Hrepro"),
               c("SeedMass"))
GROUPS_OF_TRAITS <- list(LeafMorpho,LeafDimensions,LeafCN,LeafP,Biovolume,Seed) # datasets
groups_of_traits <- c("LeafMorpho","LeafDimensions","LeafCN","LeafP","Biovolume","Seed") # names of the datasets

for (i in 1:length(groups_of_traits)){ # index of the trait sheet used
  group_of_traits <- groups_of_traits[i]
  focus <- GROUPS_OF_TRAITS[[i]]
  
  # II.1) scientificName
  focus_scientificName <- focus %>% 
    mutate(scientificName = map_chr(Species,get_scientificName,taxref))
  write.csv2(focus_scientificName,paste0("data/",site,"/processed/I.1) ",group_of_traits,"_scientificName.csv"),row.names=F)
  
  no_scientificName <- focus_scientificName %>% 
    filter(is.na(scientificName)) %>% 
    pull(Species) %>% 
    unique()
  write.table(no_scientificName,
              paste0("data/",site,"/processed/I.1) ",group_of_traits,"_noScientificName.txt"),
              col.names=F,row.names=F)
  
  # II.2) taxonID
  focus_scientificName <- read.csv2(paste0("data/",site,"/processed/I.1) ",group_of_traits,"_scientificName.csv"))
  
  focus_name_id <- focus_scientificName %>% mutate(taxonID = map_chr(Species,get_taxon_id,taxref))
  write.csv2(focus_name_id,paste0("data/",site,"/processed/I.2) ",group_of_traits,"_name_id.csv"),row.names=F)
}


# III) Tidy and TOP info ####
TIDY <- NULL
for (i in 1:length(groups_of_traits)){ # index of the trait sheet used
  group_of_traits <- groups_of_traits[i]
  focus <- GROUPS_OF_TRAITS[[i]]
  
  focus_name_id <-read.csv2(paste0("data/",site,"/processed/I.2) ",group_of_traits,"_name_id.csv"))
  focus_traits <- TRAITS[[i]]
  

  for (trait in focus_traits){
    # III.1) Gather into tidy form
    focus_trait <- focus_name_id %>% 
      select(Species,all_of(trait),scientificName,taxonID,verbatimOccurrenceID) %>% 
      # Ici, il faudra que je sélectionne aussi TOUTES LES COLONNES DES EXTENSIONS, avant de les évacuer.
      rename(verbatimScientificName = Species) %>% 
      # filter(verbatimScientificName=="Bromus erectus") %>% #/!\ just for a test with a subset of the data
      # head() %>%
      gather(trait,key=verbatimTraitName,value=verbatimTraitValue)
    # Automate verbatimTraitUnit for each trait (get it from the metadata)
    
    # III.2) Add Standard trait name from TOP
    colsWanted <- c("traitName","LocalIdentifier","traitID","verbatimTraitUnit")
    for(columnWanted in colsWanted){
      focus_trait <- focus_trait %>%  mutate(!!columnWanted:= map2_chr(trait,columnWanted,get_info_TOP,TOP))
    }
    TIDY <- rbind(TIDY,focus_trait)
  }
}
write.csv2(TIDY,paste0("data/",site,"/processed/III) Traitdata_test.csv"),row.names=F)
  
