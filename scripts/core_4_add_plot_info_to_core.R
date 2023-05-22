library(tidyverse)

# This script adds info on altitude, longitude, and latitude, to the core of the database

TIDY5 <-  read.csv2("output/TIDY_MoFTraits.csv",fileEncoding = "latin1",sep="\t",dec = ".") %>% 
# Correct typos on plots
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
                          Site == "Les Agros" ~ paste("AGR",Plot,sep = "_"))) %>% 
  rename(envPlot = Plot) # to make it correspond to the core



# Info to change plot and treatment names
Plots_corresp_envt <- read.csv2("data/plots-per_site_CorrespEnv.csv",fileEncoding = "latin1") %>% 
  mutate(treatmentOriginal = paste("Treatment",treatmentOriginal,sep="_")) %>% 
  mutate(treatmentNew = paste("Treatment",treatmentNew,sep="_"))
# Clé de lecture:
# CRO et CRP (O2LA et PDM): ne pas y toucher = conserver noms de plots et de traitement.
# Les autres: utiliser le fichier de correspondance d'Eric. 
# Donc bien faire un left_join pour ne rien perdre des lignes de notre occurrence, et ajouter des NA dans envPlot le cas échéant.


#_______________________________
# TEMPORAIRE : noms de plots sans non maj avec fichier de correps

# matching noms de plots dans le core et dans le fichier d'Eric ####

# noms de plots dans le core
plots_core <- TIDY5 %>% 
  select(Site,Plot) %>% 
  unique() %>%
  arrange(Site) %>% 
  rename(traitPlotOriginal = Plot) 
dim(plots_core)
write.csv2(plots_core,"output/plots_per_site.csv",row.names=F,fileEncoding = "Latin1")

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
write.csv2(MISSING_corresp,"data/AFaire_Avril2023/plot/plots-per_site_CorrespEnv_MISSING.csv",row.names=F)


# Visiblement ok pour : Hautes Garrigues / Cazarils / Garraf
# Il en manque pour :
#_________________________________

# Modify the Core dataframe ####
## Include info longitude, latitude, altitude ####
Infos_Plots <- Plots %>% 
  select(Site,envPlot,plotLatitude,	plotLongitude,	plotAltitude) %>% 
  unique()

plots_core %>% # a remplacer par le core entier (je fais avec ça pour réduire la taille)
  full_join(Infos_Plots) %>%
  dim()

## Modify plot and treatment names ####

### CRE ####
# no modification for this site
TIDY5_CRE <- TIDY5 %>% 
  filter(Site %in% c("O2LA","PDM"))
# COMMENT ASSOCIER LES MESURES ENVIRONNEMENTALES ? (envPlot = NA pour Came Redon...)


### other sites ####
Plots_corresp_envt_others <- Plots_corresp_envt %>% 
  filter(!(Site %in% c("Camp Redon_O2LA","Camp Redon_PDM"))) 

TIDY5_others <- TIDY5 %>% 
  filter(!(Site %in% c("O2LA","PDM"))) %>% 
  rename(traitPlotOriginal = Plot) %>% 
  rename(treatmentOriginal = Treatment) %>% 
  # change plot and treatment names
  left_join(Plots_corresp_envt_others) %>% # joining by Site, traitPlotOriginal, treatmentOriginal
  select(-c(treatmentOriginal,traitPlotOriginal)) %>% 
  rename(Plot = traitPlotNew,
         Treatment = treatmentNew) %>% 
  select(all_of(colnames(TIDY5)),envPlot)

# add plot latitude, longitude, and altitude
TIDY5_others_long <- TIDY5_others %>% 
  merge(Infos_Plots,by = c("Site","envPlot"))  # merge by "Site" and "envPlot"

#_________________
# temporaire : pb dans la fusion avec les infos de latitude etc.
TIDY5_others %>% dim()
TIDY5_others_long %>% dim()

core_envPlot <- TIDY5_others %>% 
  select(Site,envPlot) %>% 
  unique() %>% 
  mutate(origin1="core")

merge(core_envPlot,Infos_Plots)

#_________________



#____________
# temporaire : plots pour lesquels traitement et plots pas maj avec fichier de correspondance vers nouveaux noms
missing_plot_info <- TIDY5_others %>% 
  filter(is.na(envPlot)) %>% 
  select(Site,treatmentOriginal,traitPlotOriginal) %>% 
  unique()
write.csv2(missing_plot_info,"output/2023_05_17_missing_plot_info.csv",row.names= F, fileEncoding = "latin1")

missing_plot_info_feuillet <- TIDY5_others %>% 
  filter(is.na(envPlot)) %>% 
  select(Site,treatmentOriginal,traitPlotOriginal,feuillet) %>% 
  unique()
write.csv2(missing_plot_info_feuillet,"output/2023_05_17_missing_plot_info_feuillet.csv",row.names= F, fileEncoding = "latin1")



#____________

# Modify plot and treatment info, and add envPlot (to make the correspondence with abiotic measurements at the level of plots)


# Modify the Plot extension ####
## Just check whether envPlot matches with the same column in the core





TIDY6 <- merge(TIDY5_plot_maj,Infos_Plots) # merge by "Site" and "envPlot"
# je perds de l'info en mergeant
# CHAMGER NOMS DE SITES
# "O2LA","PDM"
# EN
# "Camp Redon_O2LA","Camp Redon_PDM"

write.table(TIDY6 ,"output/TIDY_plot.csv",fileEncoding = "latin1",row.names=F,sep="\t",dec = ".")


