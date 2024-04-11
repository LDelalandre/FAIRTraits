library(tidyverse)

day_month<-read.table("data/day_month.txt",header=T) %>% 
  rename(Month = month, Day = day_of_month)

core <-  data.table::fread("output/TIDY_occurrenceID.csv",encoding = "UTF-8") %>% 
  merge(day_month) %>% 
  filter(inFinalFile == "yes")

# names of sites
sites<-core %>% pull(Site) %>% unique()

# number of measurements per site/treatment
nb_meas<-core %>% 
  group_by(Site, treatmentOriginal,Year) %>% 
  summarize(n = n())

fsite<-"Hautes Garrigues"



for (fsite in sites){
  fcore<-core %>% 
    group_by(Site, Treatment,Year) %>%
    filter(Site == fsite)
  
  # campaings_per_year_and_treatement
  camp<-fcore %>% 
    ggplot(aes(x = day_of_year)) +
    geom_density() +
    facet_grid(rows = vars(Treatment),cols = vars(Year)) +
    ggtitle(fsite)
  
  ggsave(paste0("output/plot/campaings_",fsite,".png"), camp) #,width = 8,height=12
}


# Par site ####

## O2LA ####

# 2012, drought : tout mesuré le même jour
core %>% 
  filter(Site == "CRE_O2LA") %>% 
  filter(Treatment == "Drought") %>% 
  filter(Year == 2012) %>% 
  pull(day_of_year) %>% 
  unique()

# 2012 Ambient : Deux campagnes de mesure dissociées : A REGARDER
core %>% 
  filter(Site == "CRE_O2LA") %>% 
  filter(Treatment == "Ambient") %>% 
  filter(Year == 2012) %>% 
  # group_by(Code_Sp) %>% summarize(n = n())
  ggplot(aes(x=day_of_year)) +
  geom_density() +
  facet_wrap(~Code_Sp)

core %>% 
  filter(Site == "CRE_O2LA") %>% 
  filter(Treatment == "Ambient") %>% 
  filter(Year == 2012) %>% 
  select(Code_Sp, day_of_year) %>% unique() %>% View

# 2013 : une seule campagne (sur deux jours)
core %>% 
  filter(Site == "CRE_O2LA") %>% 
  filter(Treatment == "Ambient") %>% 
  filter(Year == 2013) %>% 
  pull(day_of_year) %>% 
  unique()


## PDM ####

# en 2004, même échantillonnage temporel pour tout le monde
# en 2005, pas tout à fait
# voir comment on gère ça : est-ce qu'il faut considérer qu'on a plusieurs populations (statistiques) ou pas ?
core %>% 
  filter(Site == "CRE_PDM") %>% 
  filter(Treatment == "LoN") %>% 
  filter(Year == 2004) %>% 
  ggplot(aes(x=day_of_year)) +
  geom_density() +
  facet_wrap(~Code_Sp)

core %>% 
  filter(Site == "CRE_PDM") %>% 
  filter(Treatment == "LoN") %>% 
  filter(Year == 2004) %>% 
  filter(day_of_year > 115) %>% pull(verbatimTraitName) %>% unique()
  select(Code_Sp, day_of_year) %>% unique() %>% View

## Cazarils ####

# Pour chacune des années, chaque espèce est mesurée un et un seul jour
caz<-core %>% 
  filter(Site == "Cazarils") %>% 
  filter(Year == 2001) %>% 
  group_by(Code_Sp,day_of_year) %>%
  summarize(n = n())

duplicated(caz) 

## La Fage ####
fsite<-"La Fage"

fcore<-core %>% 
  group_by(Site, Treatment,Year) %>%
  filter(Site == fsite)

# campaings_per_year_and_treatement
camp<-fcore %>% 
  ggplot(aes(x = day_of_year)) +
  geom_density() +
  facet_grid(rows = vars(Treatment),cols = vars(Year)) +
  ggtitle(fsite)

ggsave(paste0("output/plot/campaings_",fsite,".png"), camp,width = 8,height=12)

# Certains moments ou deux pics de mesures pour plusieurs espèces
# Notamment en 2013, dans plusieurs traitements
fag<-core %>% 
  filter(Site == "La Fage") %>% 
  filter(Treatment == "GF_Clc") %>%
  filter(Year == 2013) %>% 
  ggplot(aes(x = day_of_year)) +
  geom_density() +
  facet_wrap(~Code_Sp)+
  ggtitle("La Fage")
fag
ggsave(paste0("output/plot/test_FAGE.png"), fag,width = 10,height=12)

core %>% 
  filter(Site == "La Fage") %>% 
  filter(Treatment == "GF_Clc") %>%
  filter(Year == 2013) %>% 
  select(Code_Sp, day_of_year) %>% unique() %>% View

core %>% 
  filter(Site == "La Fage") %>% 
  filter(Treatment == "GF_Clc") %>%
  filter(Year == 2013) %>% 
  select(day_of_year) %>% unique() %>% View

# Un petit peu en 2010 (mais je peux utiliser les critères établis ailleurs)
fag<-core %>% 
  filter(Site == "La Fage") %>% 
  filter(Treatment == "GU_Deep") %>%
  filter(Year == 2010) %>% 
  ggplot(aes(x = day_of_year)) +
  geom_density() +
  facet_wrap(~Code_Sp)+
  ggtitle("La Fage")
fag

# Pas tellement quand grands pics, genre 2006 dans GF_Dlm
fag<-core %>% 
  filter(Site == "La Fage") %>% 
  filter(Treatment == "GF_Dlm") %>%
  filter(Year == 2006) %>% 
  ggplot(aes(x = day_of_year)) +
  geom_density() +
  facet_wrap(~Code_Sp)+
  ggtitle("La Fage")
fag

## Hautes Garrigues ####
fsite<-"Hautes Garrigues"
fcore<-core %>% 
  group_by(Site, Treatment,Year) %>%
  filter(Site == fsite)
camp<-fcore %>% 
  ggplot(aes(x = day_of_year)) +
  geom_density() +
  facet_grid(rows = vars(Treatment),cols = vars(Year)) +
  ggtitle(fsite)

ggsave(paste0("output/plot/campaings_",fsite,".png"), camp,width = 8,height=12)

# Deux campagnes en 2002, pour FieldAge_42
hgm<-core %>% 
  filter(Site == "Hautes Garrigues") %>% 
  filter(Treatment == "FieldAge_12") %>%
  filter(Year == 2002) %>% 
  ggplot(aes(x = day_of_year)) +
  geom_density() +
  facet_wrap(~Code_Sp)+
  ggtitle("La Fage")
hgm

core %>% 
  filter(Site == "Hautes Garrigues") %>% 
  filter(Treatment == "FieldAge_12") %>%
  filter(Year == 2002) %>% 
  select(Code_Sp, day_of_year) %>% unique() %>% View


# Définir un identifiant de population ####

# Test sur sous-jeu de données (O2LA, où la séparation entre deux campagnes est claire)

core %>% 
  filter(Site == "CRE_O2LA") %>% 
  filter(Treatment == "Ambient") %>% 
  filter(Year == 2012) %>% 
  # group_by(Code_Sp) %>% summarize(n = n())
  ggplot(aes(x=day_of_year)) +
  geom_density() +
  facet_wrap(~Code_Sp)
