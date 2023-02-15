library("openxlsx")
library(tidyverse)

source("scripts/functions/TAXREF and TOP.R")

# Import data completed from Flore Méditerranéenne (Tison) ####
sp_info <- read.xlsx("data/species/Fichier_sp_flores/Species_completed_Tison.xlsx", startRow = 1, colNames = TRUE)


sp_info %>% 
  group_by(LifeCycle) %>% 
  summarize(n = n())

sp_info %>% 
  ggplot(aes(x = Height1,y=Height2))+
  geom_point()

sp_info %>% 
  ggplot(aes(x = Flowering_start,y=Flowering_end))+
  geom_point()

# complete both baseflor and sp_info with names from taxref, and merge them
baseflor <- read.xlsx("data/species/baseflor.xlsx",startRow = 1, colNames = TRUE)


# Add info from TAXREF ####
taxref <- read.table("data/TAXREF_v16/TAXREFv16.txt",header=T,sep = "\t")

sp_info_scientificName <- sp_info %>% 
  mutate(scientificName = map_chr(Species,get_scientificName,taxref)) 

sp_info_name_URL <- sp_info_scientificName %>% 
  mutate(taxonID = map_chr(Species,get_CD_NOM,taxref))

sp_info_name_URL_id <- sp_info_name_URL %>% 
  mutate(taxonURL = map_chr(Species,get_URL,taxref))

# checké avec TAXREF: les familles botaniques sont bien renseignées.

write.csv2(sp_info_name_URL_id,"output/taxon_extension.csv",row.names=F,fileEncoding = "Latin1")


