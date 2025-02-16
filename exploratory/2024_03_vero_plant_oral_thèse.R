library(tidyverse)

core <-  data.table::fread("output/TIDY_occurrenceID.csv",encoding = "UTF-8") %>% 
  rename(siteName = Site)
taxon <- data.table::fread("output/FAIRTraits_Taxon.csv",encoding = "UTF-8")

ftaxon <- taxon %>% filter(Species %in% c("Veronica persica", "Plantago lanceolata"))
fcore <- core %>% filter(Species %in% c("Veronica persica", "Plantago lanceolata"))

fcore %>% 
  group_by(Code_Sp,verbatimTraitName,traitName,traitEntityValid) %>% 
  summarize(n = n()) %>% 
  filter(grepl("root",traitEntityValid)) %>%
  arrange(verbatimTraitName) %>% View
# Pas de traits racinaires chez le Plantain

fcore %>% 
  group_by(Code_Sp,verbatimTraitName,traitName,traitEntityValid) %>% 
  filter(verbatimTraitName == "SLA_mlf")

TRAITS <- c( "SLA_mlf","LDMC_mlf","LDM_mlf","LArea_mlf","Hveg_vsh","Hrep_rsh","Dmin_vsh")
units<- core %>% filter(verbatimTraitName %in% TRAITS) %>% select(verbatimTraitName,verbatimTraitUnit) %>% unique()
  
i<-4 # prendre SLA et LA (ou Hrep, mais ne colle pas à la photo de ma présentation)

fcore %>% 
  group_by(Code_Sp,verbatimTraitName,traitName,traitEntityValid) %>% 
  filter(verbatimTraitName == TRAITS[i]) %>% 
  group_by(Code_Sp) %>% 
  ggplot(aes(x = reorder(Species,verbatimTraitValue), y = verbatimTraitValue)) +
  geom_boxplot() +
  geom_point() +
  ggtitle(TRAITS[i]) +
  theme_classic() +
  xlab("") + ylab("") +
  scale_y_continuous(trans='log10') +
  ggtitle("Surface spécifique foliaire (m²/kg)")+
  ggtitle("Surface foliaire (cm²)")

units

# Caractérisations environnementales du site ####
envPlots<-read.csv2("data/FAIRTraits_EnvironmentalPlots.csv") %>% 
  filter(envPlotID %in% c("FAG_C1Cc","FAG_C2Cc","FAG_C1Dm","FAG_C2Dm","FAG_P1S",
                            "FAG_P8S","FAG_P10S","FAG_AvC1C2Cc","FAG_AvC1C2Dm","FAG_AvP6P8S"))
envPlots


# Checker annuelles et pérennes ####
traits<-core %>% select(verbatimTraitName) %>% unique() %>% arrange(verbatimTraitName)
traits
core %>%
  filter(LifeForm1 %in% c("The","Hem","Geo")) %>% 
  filter(verbatimTraitName %in% c("SLA_mlf","Aarea_mlf")) %>% 
  groupe_by(Code_Sp)
  select(verbatimTraitName,verbatimTraitValue,LifeForm1,verbatimOccurrenceID_sample) %>% head() 
  gather(key = verbatimTraitName,value = verbatimTraitValue,-LifeForm1) %>% head()


  