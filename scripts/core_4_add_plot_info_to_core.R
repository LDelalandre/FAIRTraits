library(tidyverse)

# This script adds info on altitude, longitude, and latitude, to the core of the database

TIDY5 <-  read.csv2("output/TIDY_MoFTraits.csv",fileEncoding = "latin1",sep="\t",dec = ".")

# Correct typos on plots
TIDY5bis <- TIDY5 %>% 
  mutate(Plot = if_else(Plot == "HGM_P7,  P9, P10", "HGM_P7, P9, P10",Plot))

# Measurements made at the level of plots
Plots <- read.csv2("data/Plots.csv",fileEncoding = "latin1",sep=";") %>% 
  # Edit plot name
  mutate(Plot = case_when(Site == "La Fage" ~ paste("FAG",Plot,sep = "_"),
                          Site == "Cazarils" ~ paste("CAZ",Plot,sep = "_"),
                          Site == "PDM" ~ paste("CRP",Plot,sep = "_"),
                          Site == "O2LA" ~ paste("CRO",Plot,sep = "_"),
                          Site == "Garraf" ~ paste("GAR",Plot,sep = "_"),
                          Site == "Hautes Garrigues" ~ paste("HGM",Plot,sep = "_"),
                          Site == "Les Agros" ~ paste("AGR",Plot,sep = "_")))



# Info to change plot and treatment names
Plots_corresp_envt <- read.csv2("data/plots-per_site_CorrespEnv.csv",fileEncoding = "latin1") %>% 
  rename(Plot = traitPlotOriginal)
# Clé de lecture:
# CRO et CRP (O2LA et PDM): ne pas y toucher = conserver noms de plots et de traitement.
# Les autres: utiliser le fichier de correspondance d'Eric. 
# Donc bien faire un left_join pour ne rien perdre des lignes de notre occurrence, et ajouter des NA dans envPlot le cas échéant.


#_______________________________
# TEMPORAIRE

# matching noms de plots dans le core et dans le fichier d'Eric ####

# noms de plots dans le core
plots_core <- TIDY5 %>% 
  select(Site,Plot) %>% 
  unique() %>%
  arrange(Site) %>% 
  rename(traitPlotOriginal = Plot) 
dim(plots_core)

plots_core %>% pull(Site) %>% unique()

A <- plots_core %>% 
  # filter(Site == "La Fage") %>% 
  mutate(origin = "core") # de quel fichier ça vient

A

B <- Plots_corresp_envt %>% 
  # filter(Site == "La Fage") %>% 
  mutate(origin2 = "fichier_corresp")
B

full_join(A,B) %>% View
MISSING_corresp <- full_join(A,B) %>% 
  filter(!(Site %in% c("PDM","O2LA"))) %>%
  filter(is.na(origin2 )) %>% 
  select(Site,traitPlotOriginal)
write.csv2(MISSING_corresp,"output/plots-per_site_CorrespEnv_MISSING.csv")


# Visiblement ok pour : Hautes Garrigues / Cazarils / Garraf
# Il en manque pour :


# Modify the Core dataframe ####
# Info (longitude, latitude) to include in the core
Infos_Plots <- Plots %>% 
  select(Site,Plot,plotLatitude,	plotLongitude,	plotAltitude) %>% 
  unique()

plots_core %>% # a remplacer par le core entier (je fais avec ça pour réduire la taille)
  full_join(Infos_Plots) %>%
  dim()

# Modify plot and treatment info, and add envPlot (to make the correspondence with abiotic measurements at the level of plots)


# Modify the Plot extension ####




write.csv2(plots_core,"output/plots_per_site.csv",row.names=F,fileEncoding = "Latin1")

TIDY6 <- merge(TIDY5,Infos_Plots) # je perds de l'info en mergeant
write.table(TIDY6 ,"output/TIDY_plot.csv",fileEncoding = "latin1",row.names=F,sep="\t",dec = ".")
