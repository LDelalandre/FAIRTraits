library(tidyverse)

# 1) import data ####
# 2) generate core ####
# 3) add MoFTraits ####
# 4) add plot info ####
# 5) add occurrence ####

TIDY_raw <- data.table::fread("output/TIDY.csv",encoding="UTF-8")
TIDY_raw %>% 
  filter(feuillet == "Soil data") %>% View
TIDY_raw %>% 
  pull(Site) %>% unique()
TIDY_raw %>% filter(Site == "") %>% View

# je cherche le feuillet "soil data" : dans quel site ? --> HGM
files <- read_file(sites[6])
"Soil data" %in% names(files)


TIDY <- data.table::fread("output/TIDY_corrected_typos.csv",encoding="UTF-8") %>% 
  rename(verbatimTraitName_old = verbatimTraitName) %>% 
  rename(traitEntityDataFile = traitEntity)

TIDY_traits <-  data.table::fread("output/TIDY_MoFTraits.csv",encoding = "UTF-8") %>% 
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

TIDY_traits_plots <- data.table::fread("output/TIDY_plot.csv",encoding="UTF-8")



