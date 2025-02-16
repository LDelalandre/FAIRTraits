library(dplyr)

LeafMorpho <-  read.csv("data/La Fage/LeafMorpho_traits.csv",sep=";")

LeafMorpho %>% 
  head() %>% 
  mutate(verbatimOccurrenceID = paste("FAG",Treatment,Day,Code_Sp,Rep,sep = "_"))

# --> Ensuite, fournir un identifiant unique pour toutes les combinaisons identiques de ces colonnes à l'échelle de toutes les données.
# Le mieux est peut-être de 1) faire ça AVANT de faire le tableau central Traitdata, en l'y intégrant,
# 2) de séparer tout ça ensuite.