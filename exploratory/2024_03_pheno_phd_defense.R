library(tidyverse)
core <-  data.table::fread("output/TIDY_occurrenceID.csv",encoding = "UTF-8") %>% 
  rename(siteName = Site)

fdata<-core %>% 
  filter(siteName == "La Fage") %>% 
  filter(Treatment %in% c("Treatment_GU_Shallow", "Treatment_GF_Clc","Treatment_GF_Dlm" )) %>% 
  filter(verbatimTraitName %in% c("Disp_rep") ) #"Flo_rep",
  # group_by(Treatment,LifeForm1) %>% summarize(n=n())


fdata %>% 
  filter(Treatment == "Treatment_GF_Clc" & LifeForm1 %in% c("The")) %>% 
  ggplot(aes(x = verbatimTraitValue)) +
  geom_density() + 
  ggtitle("Annuals in Fer") +
  xlim(c(100,300)) +
  theme_classic()

fdata %>% 
  filter(Treatment == "Treatment_GF_Dlm" & LifeForm1 %in% c("Hem","Cha","Geo")) %>% 
  ggplot(aes(x = verbatimTraitValue)) +
  geom_density() + 
  ggtitle("Perennials in Fer")+ 
  xlim(c(100,300)) +
  theme_classic()


fdata %>% 
  filter(Treatment == "Treatment_GU_Shallow" & LifeForm1 %in% c("The")) %>% 
  ggplot(aes(x = verbatimTraitValue)) +
  geom_density()+ 
  ggtitle("Annuals in Nat") +
  xlim(c(100,300)) +
  theme_classic()

fdata %>% 
  filter(Treatment == "Treatment_GU_Shallow" & LifeForm1 %in% c("Hem","Cha","Geo")) %>% 
  ggplot(aes(x = verbatimTraitValue)) +
  geom_density()+ 
  ggtitle("Perennials in Nat") +
  xlim(c(100,300)) +
  theme_classic()

