library(dplyr)

LeafMorpho <-  read.csv("data/La Fage/LeafMorpho_traits.csv",sep=";")

LeafMorpho %>% 
  head() %>% 
  mutate(verbatimMeasurementID = paste("FAG",Treatment,Day,Code_Sp,"1",sep = "_"), # 1 for the group of trait. To be separated further 
         # for each trait ???
         basisOfRecord="LivingSpecimen", 
         measurementMethod=NA, # URI to provide
         measurementDeterminedBy=NA, # Correspondence between dates and person to establish.
         measurementDeterminedDate=Day,
         aggregatedMeasure=F,
         individualCount=1)

# --> Ensuite, fournir un identifiant unique pour toutes les combinaisons identiques de ces colonnes à l'échelle de toutes les données.
# Le mieux est peut-être de 1) faire ça AVANT de faire le tableau central Traitdata, en l'y intégrant,
# 2) de séparer tout ça ensuite.
