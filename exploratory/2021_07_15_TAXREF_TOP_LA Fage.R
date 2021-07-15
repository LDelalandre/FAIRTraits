library(dplyr)

# function
source("functions/TAXREF and TOP.R")

# I) Import datasets ####
# I.1) TaXREF and TOP
taxref <- read.csv2("data/TAXREF/TAXREF14.0_FR_Continental_13_07_2021.csv")
TOP <- read.csv2("data/TOP/TOP_info.csv",fill=T)

# I.2) Traits
leafmorpho <-  read.table("data/La Fage/LeafMorpho_traits.txt",header=T,sep="\t") %>% 
  mutate(occurrenceID = paste("FAG",Treatment,Day,Code_Sp,"1",sep = "_")) # 1 = group of traits 1 (i.e. leafmorpho)
CN <- read.table("data/La Fage/LeafC&N.txt",header=T,sep="\t") %>% 
  mutate(occurrenceID = paste("FAG",Treatment,Day,Code_Sp,"2",sep = "_"))



# II) TAXREF info ####
TRAITS <- list(c("SLA","LDMC","L_Area"),
               c("LNC","LCC")) # list of traits for each group of traits
GROUPS_OF_TRAITS <- list(leafmorpho,CN) # datasets
groups_of_traits <- c("leafmorpho","CN") # names of the datasets

for (i in c(1,2)){ # index of the trait sheet used
  group_of_traits <- groups_of_traits[i]
  focus <- GROUPS_OF_TRAITS[[i]]
  
  # II.1) scientifisName
  focus_scientificName <- focus %>% 
    mutate(scientificName = map_chr(Species,get_scientificName,taxref))
  write.csv2(focus_scientificName,paste0("data/La Fage/processed/I.1) ",group_of_traits,"_scientificName.csv"),row.names=F)
  
  no_scientificName <- focus_scientificName %>% 
    filter(is.na(scientificName)) %>% 
    pull(Species) %>% 
    unique()
  
  # II.2) taxonID
  focus_scientificName <- read.csv2(paste0("data/La Fage/processed/I.1) ",group_of_traits,"_scientificName.csv"))
  
  focus_name_id <- focus_scientificName %>% mutate(taxonID = map_chr(Species,get_taxon_id,taxref))
  write.csv2(focus_name_id,paste0("data/La Fage/processed/I.2) ",group_of_traits,"_name_id.csv"),row.names=F)
}


# III) Tidy and TOP info ####
TIDY <- NULL
for (i in c(1,2)){ # index of the trait sheet used
  group_of_traits <- groups_of_traits[i]
  focus <- GROUPS_OF_TRAITS[[i]]
  
  focus_name_id <-read.csv2(paste0("data/La Fage/processed/I.2) ",group_of_traits,"_name_id.csv"))
  focus_traits <- TRAITS[[i]]
  

  for (trait in focus_traits){
    # III.1) Gather into tidy form
    focus_trait <- focus_name_id %>% 
      select(Species,trait,scientificName,taxonID,occurrenceID) %>%
      rename(verbatimScientificName = Species) %>% 
      filter(verbatimScientificName=="Bromus erectus") %>% #/!\ just for a test with a subset of the data
      head() %>%
      gather(trait,key=verbatimTraitName,value=verbatimTraitValue)
    # Automate verbatimTraitUnit for each trait (get it from the metadata)
    
    # III.2) Add Standard trait name from TOP
    colsWanted <- c("traitName","TOPLocalIdentifier","traitID","verbatimTraitUnit")
    for(columnWanted in colsWanted){
      leafmorpho_trait <- focus_trait %>%  mutate(!!columnWanted:= map2_chr(trait,columnWanted,get_info_TOP,TOP))
    }
    TIDY <- rbind(TIDY,leafmorpho_trait)
  }
}
  
