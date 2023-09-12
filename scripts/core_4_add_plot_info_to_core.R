library(tidyverse)

# This script adds info on altitude, longitude, and latitude, to the core of the database

TIDY5 <-  data.table::fread("output/TIDY_MoFTraits.csv",encoding = "UTF-8") %>% 
  # read.csv2("output/TIDY_MoFTraits.csv",fileEncoding = "latin1",sep="\t",dec = ".") %>% 
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
envPlots <- read.csv2("data/Plots.csv",fileEncoding = "latin1",sep=";") %>% 
  unique()

traitPlots <- read.csv2("data/traitPlots_georeferences.csv",fileEncoding = "latin1") %>% 
  # select(-envPlot) %>% 
  unique()

# Info to change plot and treatment names
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
  rename(Treatment = treatmentNew) %>% 
  select(-c(traitPlotOriginal,treatmentOriginal))


# add plot latitude, longitude, and altitude
Infos_Plots <- traitPlots %>% 
  select(traitPlot,plotLatitude,plotLongitude,plotAltitude) %>% 
  unique()

TIDY5_long <- TIDY5_plots %>%
  left_join(Infos_Plots,by = c("traitPlot"))  


# write.table(TIDY5_long ,"output/TIDY_plot.csv",fileEncoding = "latin1",row.names=F,sep="\t",dec = ".")
data.table::fwrite(TIDY5_long,"output/TIDY_plot.csv")



dim(TIDY5)
dim(TIDY5_plots)
dim(TIDY5_long)

TIDY5_long %>% select(verbatimTraitUnit) %>% unique() %>% View()

#_____________________________
# problems of encoding
TIDY5_long_subset <- TIDY5_long %>%
  filter(Site == "Hautes Garrigues" & verbatimTraitName == "Ldelta13C") 

TIDY5_long_subset %>% 
  write.table("output/WorkingFiles/TIDY_plot_subset.csv",fileEncoding = "latin1",row.names=F,sep="\t",dec = ".")

data.table::fwrite(TIDY5_long,"output/WorkingFiles/TIDY_plot_subset_data.table.csv")
test <- data.table::fread("output/WorkingFiles/TIDY_plot_subset_data.table.csv",encoding="UTF-8")


TIDY_plot_subset <- read.csv2("output/WorkingFiles/TIDY_plot_subset.csv",fileEncoding = "latin1",sep="\t",dec = ".")



#_________________
# TEMPORAIRE plots et géoréférencement
traitPlots <- TIDY5_long %>% 
  select(traitPlot,envPlot,plotLatitude,plotLongitude,plotAltitude) %>% 
  unique() %>%
  arrange(traitPlot)

traitPlots_to_complete <- traitPlots %>% 
  filter(!(traitPlot == envPlot))

traitPlots_ok <- traitPlots %>% 
  filter(traitPlot == envPlot)

write.csv2(traitPlots,"output/WorkingFiles/traitPlots_to_complete.csv",row.names=F,fileEncoding = "latin1")




