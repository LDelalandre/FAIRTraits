library(tidyverse)
core <-  read.csv2("output/TIDY_occurrenceID.csv",fileEncoding = "latin1",sep="\t",dec = ".")

core %>% 
  filter(verbatimTraitName == "LDMC") %>%
  ggplot(aes(x = verbatimTraitValue)) +
  geom_histogram()

