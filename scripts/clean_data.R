library(tidyverse)

core <-  read.csv2("output/TIDY_occurrenceID.csv",fileEncoding = "latin1",sep="\t",dec = ".")
GBIF %>% 
  group_by(Site,Species, traitName) %>% 
  summarize(n = n()) %>% 
  arrange(n) %>% 
  View()
