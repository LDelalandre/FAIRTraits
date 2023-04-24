library(tidyverse)

# Mean trait values per population ####
core <-  read.csv2("output/TIDY_plot.csv",fileEncoding = "latin1",sep="\t",dec = ".")

sum_core <- core  %>% 
  mutate(trait_entity = paste(verbatimTraitName,traitEntity,sep="_")) %>% 
  group_by(verbatimOccurrenceID_population,verbatimTraitName,trait_entity) %>% 
  summarize(mean_trait = mean(verbatimTraitValue))


sum_core %>% 
  filter(verbatimTraitName %in% c("SLA","LPC","LLitM_0","LLS","Ci")) %>%
  ungroup() %>% 
  select(-c(verbatimTraitName)) %>% 
  spread(key = trait_entity, value = mean_trait ) %>% View()
