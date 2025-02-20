library(tidyverse)

# This script adds info on altitude, longitude, and latitude, to the core of the database
# It also updates names of plots, treatments, and sites

# Import data ####

## Traits ####
TIDY5 <-  data.table::fread("output/TIDY_3_MoFTraits.csv",encoding = "UTF-8") %>% 
# Correct typos on plots
  mutate(Plot = if_else(Plot == "HGM_P7,  P9, P10", "HGM_P7, P9, P10",Plot)) %>% 
# Correct typos on Treatment
  mutate(Treatment = case_when(Treatment == "Treatment_Fer_Clc_F" ~ "Treatment_Fer_Clc",
                               Treatment == "Treatment_Fer_Clc_FF" ~ "Treatment_Fer_Clc",
                               Treatment == "Treatment_Fer_Clc_FFF" ~ "Treatment_Fer_Clc",
                               
                               Treatment == "Treatment_n+" ~ "Treatment_Fer_Clc",
                               Treatment == "Treatment_n++" ~ "Treatment_Fer_Clc",
                               Treatment == "Treatment_n+++" ~ "Treatment_Fer_Clc",
                               
                               TRUE ~ Treatment)) %>% 
  mutate(Site = case_when(Site == "PDM" ~ "CRE_PDM",
                          Site == "O2LA" ~ "CRE_O2LA",
                          TRUE ~ Site))
  

## Environmental data ####

# Names of plots and values of environmental parameters
envPlots <- read.csv2("data/Plots.csv",fileEncoding = "latin1",sep=";") %>% 
  unique()

# Georeference of plots where traits were measured
traitPlots <- read.csv2("data/traitPlots_georeferences.csv",fileEncoding = "latin1")
Infos_Plots <- traitPlots %>% 
  select(traitPlot,plotLatitude,plotLongitude,plotAltitude) %>% 
  unique() %>% 
  # Duplicated lines
  filter(!(traitPlot == "FAG_C1C2Cc" & plotAltitude == 772)) %>% 
  filter(!(traitPlot == "FAG_C1C2C3Cc" & plotAltitude == 772))

Infos_Plots[which(duplicated(Infos_Plots$traitPlot)),]

# Info to update plot and treatment names 
# + correspondence between plots where traits were measured, and plots where 
# environmental parameters were measured
Plots_corresp_envt <- read.csv2("data/plots_corresp_envt.csv",fileEncoding = "latin1") %>% 
  unique()

# Modify the Core dataframe ####

## Modify plot and treatment names ####
TIDY5_plots <- TIDY5 %>% 
  rename(traitPlotOriginal = Plot) %>% 
  rename(treatmentOriginal = Treatment) %>% 
  # change plot and treatment names
  left_join(Plots_corresp_envt) %>%  # joining by Site, traitPlotOriginal, treatmentOriginal
  mutate(envPlot = case_when(Site == "CRE_PDM" ~ str_sub(traitPlotOriginal,start = 1L, end = 7L),
                             Site == "CRE_O2LA" ~ "CRO_Average",
                             traitPlotOriginal == "CRP_Glasshouse_CEFE" ~"NA",
                             TRUE ~ envPlot)) %>% 
  mutate(traitPlotNew = case_when(Site == "CRE_PDM" ~ traitPlotOriginal,
                             Site == "CRE_O2LA" ~ traitPlotOriginal,
                             TRUE ~ traitPlotNew)) %>% 
  mutate(treatmentNew = case_when(Site == "CRE_PDM" ~ treatmentOriginal,
                                  Site == "CRE_O2LA" ~ treatmentOriginal,
                                  TRUE ~ treatmentNew)) %>%
  rename(traitPlot = traitPlotNew) %>%
  rename(Treatment = treatmentNew) 



## add plot latitude, longitude, and altitude ####
TIDY5_long <- TIDY5_plots %>%
  left_join(Infos_Plots,by = c("traitPlot")) %>% 
  mutate(countryCode = if_else(Site =="Garraf","ES","FR")) %>% 
  mutate(Treatment = str_sub(Treatment,start = 11, end = -1)) # remove "Treatment



# Quality check ####

## Was plot name  updated for every plot?
TIDY5_long %>%
  filter(is.na(traitPlot))

## Is latitude documented for every plot?
TIDY5_long %>%
  filter(is.na(plotLatitude))

# Was the correspondence between plots where traits and environment were measured
# made for every record ?
TIDY5_long %>%
  filter(is.na(envPlot))

# Does row number remain identical?
dim(TIDY5)
dim(TIDY5_plots)
dim(TIDY5_long)
dim(TIDY5_long %>% unique())


# PROBLEME dans l'ajout des infos de géolocalisation des traitPlot
Coreplots <- TIDY5_plots %>% 
  pull(traitPlot) %>% 
  unique()

GPSplots <- Infos_Plots %>% 
  pull(traitPlot) %>% 
  unique()

setdiff(Coreplots, GPSplots)
setdiff(GPSplots,Coreplots) # Some names of plots in the mapping file are not present 
# in the core file (these are relicts of old plot names that we have since corrected)

# Export ####
data.table::fwrite(TIDY5_long,"output/TIDY_4_plot.csv",sep="\t")
