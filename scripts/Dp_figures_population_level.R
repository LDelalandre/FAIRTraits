library(tidyverse)

# Mean trait values per population ####
core <-  read.csv2("output/TIDY_plot.csv",fileEncoding = "latin1",sep="\t",dec = ".")
taxon <- read.csv2("output/taxon.csv")

core2 <- core %>% 
  merge(taxon)



sum_core <- core2  %>% 
  mutate(trait_entity = paste(verbatimTraitName,traitEntity,sep="_")) %>% 
  group_by(LifeForm1, LifeCycle1,Year,verbatimOccurrenceID_population,verbatimTraitName,trait_entity) %>% 
  summarize(mean_trait = mean(verbatimTraitValue))


sum_core %>% 
  filter(verbatimTraitName %in% c("SLA","LPC","LLitM_0","LLS","Ci")) %>%
  ungroup() %>% 
  select(-c(verbatimTraitName)) %>% 
  spread(key = trait_entity, value = mean_trait ) %>% View()



# Essais Léo ####

# Regarder les variations interannuelles de valeurs de traits chez certaines annuelles et certaines pérennes
# à relier avec les variations environnementales



core2 %>% 
  pull(verbatimTraitName) %>% 
  unique()

ftrait <- "SLA"
sum_core %>% 
  filter(verbatimTraitName == ftrait) %>% 
  filter(LifeCycle1 %in% c("A","P")) %>% 
  filter(LifeForm1 %in% c("The","Hem")) %>% 
  ggplot(aes(x = Year,y=mean_trait,color = LifeCycle1)) +
  # geom_line(aes(group = verbatimOccurrenceID_population )) + # faire un identifiant par population (eg. en utilisant le plot)
  geom_point() +
  facet_wrap(~LifeForm1)


#____________________

core2 %>% 
  filter(verbatimTraitName == ftrait) %>% 
  filter(LifeCycle1 %in% c("A","P")) %>% 
  filter(LifeForm1 %in% c("The","Hem")) %>% 
  ggplot(aes(x = Year,y=verbatimTraitValue,color = LifeCycle1)) +
  geom_jitter() +
  facet_wrap(~LifeForm1)
# NB: il faut contrôler par qui a été mesuré !!

core2 %>% 
  group_by(LifeCycle1,LifeForm1) %>% 
  summarize(n = n())

core2 %>% 
  filter(is.na(LifeCycle1)) %>% 
  pull(Species) %>% 
  unique()



  
  
