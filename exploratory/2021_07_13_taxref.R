library(tidyverse)



# taxref <- read.table("data/TAXREF_v14/TAXREFv14.txt",header=T,sep="\t",fill=T) # beaucoup moins d'info... ?!
taxref <- read.csv2("data/TAXREF/TAXREF14.0_FR_Continental_13_07_2021.csv")
# # taxref %>%
# #   filter(grepl("Bromus hordeaceus", NOM_VALIDE))
taxref %>% 
  pull(RANG) %>% 
  unique()

# Example on the leafmorpho sheet ####
# leafmorpho <-  read.table("data/La Fage/LeafMorpho_traits.txt",header=T,sep="\t") %>% 
#   mutate(occurrenceID = paste("FAG",Treatment,Day,Code_Sp,"1",sep = "_")) # 1 = group of traits 1 (i.e. leafmorpho)

leafmorpho <-  read.csv("data/La Fage/LeafMorpho_traits.csv",sep=";") %>% 
  mutate(verbatimOccurrenceID = paste("FAG",Treatment,Day,Code_Sp,Rep,sep = "_"))

# I) TAXREF: standard taxon name ####
# I.1) Add scientificName ####
# Example: "Aphyllanthes monspeliensis L., 1753"
get_scientificName <- function(verbatim_name,taxref){
  valid_name <- taxref %>% 
    filter(LB_NOM == verbatim_name) %>%
    pull(NOM_VALIDE)
  valid_name[1]
}

leafmorpho_scientificName <- leafmorpho %>% mutate(scientificName = map_chr(Species,get_scientificName,taxref))
write.csv2(leafmorpho_scientificName,"data/La Fage/processed/I.1) leafmorpho_scientificName.csv")

# Species whose scientificName was not found
no_scientificName <- leafmorpho_scientificName %>% filter(is.na(scientificName)) %>% 
  pull(Species) %>% 
  unique()
write.table(no_scientificName,"data/La Fage/processed/I.1) no_scientificName.txt",row.name=F,col.names=F)
# Dans taxref, LB_NOM: 
# Myosotis ramosissima subsp. ramosissima
# Linum tenuifolium subsp. tenuifolium: n'existe pas... Soit Linum biforme subsp. tenuifolium, soit Linum tenuifolium
# NB: je n'ai pas le genre "GN" dans la colonne RANG!! --> Re-télécharger TAXREF en entier?

# I.2) Add taxon id ####
get_taxon_id <- function(verbatim_name,taxref){
  taxon_id <- taxref %>% 
    filter(LB_NOM == verbatim_name) %>%
    pull(URL)
  taxon_id[1]
}

leafmorpho_scientificName <- read.csv2("data/La Fage/processed/I.1) leafmorpho_scientificName.csv")

leafmorpho_name_id <- leafmorpho_scientificName %>% mutate(taxonID = map_chr(Species,get_taxon_id,taxref))
write.csv2(leafmorpho_name_id,"data/La Fage/processed/I.2) leafmorpho_name_id.csv",row.names=F)

# II) Tidy format ON A GIVEN TRAIT ####
leafmorpho_name_id <- read.csv2("data/La Fage/processed/I.2) leafmorpho_name_id.csv")

# Output: a tidy table for each trait. Then pool them (rbind).
traits <- c("SLA","LDMC","L_Area")
# NB: autres feuilles du doc excel: "L_Length","L_Width","LCC","LNC","LPC","Hveg","SeedMass"


DATABASE <- NULL
for (trait in traits){
  # Verbatim trait name
  leafmorpho_trait <- leafmorpho_name_id %>% 
    select(Species,trait,scientificName,taxonID) %>%
    rename(verbatimScientificName = Species) %>% 
    filter(verbatimScientificName=="Bromus erectus") %>% #/!\ just for a test with a subset of the data
    head() %>%
    gather(trait,key=verbatimTraitName,value=verbatimTraitValue)
  # Automate verbatimTraitUnit for each trait (get it from the metadata)
  
  # III) TOP: Standard trait name ####
  TOP <- read.csv2("data/TOP/TOP_info.csv",fill=T)
  # write.csv2(TOP,"data/TOP/TOP_info2.csv",row.names=F)
  
  get_info_TOP <- function(trait,columnWanted){
    TOP %>% 
      filter(verbatimTraitName == trait) %>% 
      pull(columnWanted)
  }
  
  colsWanted <- c("traitName","TOPLocalIdentifier","traitID","verbatimTraitUnit")
  for(columnWanted in colsWanted){
    leafmorpho_trait <- leafmorpho_trait %>%  mutate(!!columnWanted:= map2_chr(trait,columnWanted,get_info_TOP))
  }
  DATABASE <- rbind(DATABASE,leafmorpho_trait)
}

write.csv2(DATABASE,"data/La Fage/processed/III) leafmorpho_test.csv",row.names=F)
                  





