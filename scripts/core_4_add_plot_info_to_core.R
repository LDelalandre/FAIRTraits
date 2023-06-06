library(tidyverse)

# This script adds info on altitude, longitude, and latitude, to the core of the database

TIDY5 <-  read.csv2("output/TIDY_MoFTraits.csv",fileEncoding = "latin1",sep="\t",dec = ".") %>% 
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
  

# Measurements made at the level of plots
Plots <- read.csv2("data/Plots_vjuin2023.csv",fileEncoding = "latin1",sep=";")
  # mutate(treatmentNew = paste("Treatment",treatmentNew,sep="_"))


# Info to change plot and treatment names
Plots_corresp_envt <- read.csv2("data/plots_corresp_envt.csv",fileEncoding = "latin1")

# Modify the Core dataframe ####

## Modify plot and treatment names ####
TIDY5_plots <- TIDY5 %>% 
  # filter(!(Site %in% c("O2LA","PDM"))) %>% 
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
                                  TRUE ~ treatmentNew))

#______________
# temporaire
missing_traitPlotOriginal_treatmentOriginal_combi <- TIDY5_plots %>%
  filter(is.na(envPlot)) %>% 
  select(Site,traitPlotOriginal,treatmentOriginal) %>% 
  unique()
write.csv2(missing_traitPlotOriginal_treatmentOriginal_combi,"output/WorkingFiles/missing_traitPlotOriginal_treatmentOriginal_combi.csv",row.names=F,fileEncoding = "latin1")

TIDY5_plots %>%
  filter(is.na(envPlot)) %>% 
  select(Site,traitPlotOriginal,traitPlotNew,treatmentOriginal,treatmentNew,envPlot) %>% 
  unique()
#________________

# add plot latitude, longitude, and altitude
Infos_Plots <- Plots %>% 
  select(envPlot,plotLatitude,plotLongitude,plotAltitude)

TIDY5_long <- TIDY5_plots %>% 
  left_join(Infos_Plots,by = c("envPlot")) %>% 
  select(-c(traitPlotOriginal,treatmentOriginal)) %>%
  rename(traitPlot = traitPlotNew) %>%
  rename(Treatment = treatmentNew)

#______________
# temporaire
TIDY5_long %>%
  filter(is.na(plotLatitude )) %>% 
  select(Site,envPlot,traitPlot,Treatment,feuillet) %>% 
  unique()
# temporaire


write.table(TIDY5_long ,"output/TIDY_plot.csv",fileEncoding = "latin1",row.names=F,sep="\t",dec = ".")


