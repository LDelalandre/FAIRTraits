library(tidyverse)

tempo_evol <- read.csv2("data/La Fage/hors_data_paper/temporal_evol_Rs_all.csv") %>% 
  rename(verbatimScientificName = species)

# traits <- read.csv2("data/La Fage/processed/III) Traitdata_test.csv") %>% 
# mutate(treatment = str_sub(verbatimOccurrenceID,start = 5, end = 11)) 
# Il faut ajouter le Paddock dans l'occurrenceID quand je génère le jeu de données !

traits <- read.csv2("output/Traitdata_test_taxref_lafagetemporary.csv")
#╩ attention, traits value considered as character !

traits %>% 
  select(Treatment) %>% 
  unique()

subtraits <- traits %>% 
  mutate(trtmt = str_sub(Treatment,start = 1,end=3)) %>% 
  filter(verbatimTraitName %in% c("SLA","LNC")) %>% 
  mutate(sp_plot_trtmt = paste(Plot,trtmt,Rep,sep="_")) %>%
  # filter(Treatment %in% c("Fer_Clc","Nat_Sab")) %>% 
  unique()
  
sla_lnc <- subtraits %>%
  mutate(verbatimTraitValue = as.numeric(verbatimTraitValue)) %>% 
  group_by(verbatimScientificName,trtmt,verbatimTraitName) %>% 
  summarize(verbatimTraitValue = mean(verbatimTraitValue,na.rm = T)) %>% 
  spread(key = verbatimTraitName, value = verbatimTraitValue) %>% 
  filter(!is.na(LNC)) %>% 
  filter(!is.na(SLA)) %>% 
  merge(tempo_evol,by = "verbatimScientificName")

sla_lnc %>% 
  ggplot(aes(x=SLA,y=LNC)) +
  geom_point()

sla_lnc_photo <- sla_lnc %>% 
  mutate(LMA = 1/SLA * 100) %>% 
  tibble() %>% 
  mutate(logAmass = 0.74 * log(LNC) - 0.57*log(LMA)) 

sla_lnc_photo %>% 
  select(verbatimScientificName , trtmt, Rs, LMA) %>% 
  filter(!(trtmt == "Tem")) %>% 
  spread(key = trtmt,value = LMA) 

write.csv2(sla_lnc_photo,"data/La Fage/hors_data_paper/instrasp_var.csv",row.names=F)
  
sla_lnc_photo %>% 
  select(verbatimScientificName , trtmt, Rs, LMA) %>% 
  spread(key = trtmt,value = LMA) %>% 
  na.omit() %>% 
  mutate(difference = Fer - Nat) %>% 
  ggplot(aes(x=Rs,y=difference,label = verbatimScientificName))+
  geom_point() +
  ggrepel::geom_label_repel()
