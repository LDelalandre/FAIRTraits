library(tidyverse)

# This script adds info on altitude, longitude, and latitude, to the core of the database

TIDY5 <-  read.csv2("output/TIDY_MoFTraits.csv",fileEncoding = "latin1",sep="\t",dec = ".")

## Envt info  #### 
# (plot latitude, longitude, altitude)

Plots <- read.csv2("data/Plots.csv",fileEncoding = "latin1",sep=";")

Infos_Plots <- Plots %>% 
  rename(Site = Site.name,Plot=plot) %>% 
  select(Site,Plot,plotLatitude,	plotLongitude,	plotAltitude) %>% 
  mutate(Plot = case_when(Site == "La Fage" ~ paste("FAG",Plot,sep = "_"),
                          Site == "Cazarils" ~ paste("CAZ",Plot,sep = "_"),
                          Site == "PDM" ~ paste("CRE",Plot,sep = "_"),
                          Site == "O2LA" ~ paste("CRE",Plot,sep = "_"),
                          Site == "Garraf" ~ paste("GAR",Plot,sep = "_"),
                          Site == "Hautes Garrigues" ~ paste("HGM",Plot,sep = "_"),
                          Site == "Les Agros" ~ paste("AGR",Plot,sep = "_")))
# NB: A COMPLETER POUR LES PLOTS qu'on n'a pas (pas de mesure de sol)
# Mettre des bons noms de colonnes

# TRES PEU DE CONGRUENCE ENTRE LES PLOTS DES METADONNEES ET DES DONNEES
# --> A compléter
plots_core <- TIDY5 %>% 
  filter(!(Site %in% c("O2LA","PDM"))) %>% 
  select(Site,Plot) %>% 
  unique() %>% 
  arrange(Site)

write.csv2(plots_core,"output/plots_per_site.csv",row.names=F,fileEncoding = "Latin1")

TIDY6 <- merge(TIDY5,Infos_Plots) # je perds de l'info en mergeant
write.table(TIDY6 ,"output/TIDY_plot.csv",fileEncoding = "latin1",row.names=F,sep="\t",dec = ".")
