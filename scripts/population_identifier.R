library(tidyverse)

day_month<-read.table("data/day_month.txt",header=T) %>% 
  rename(Month = month, Day = day_of_month)

core <-  data.table::fread("output/TIDY_occurrenceID.csv",encoding = "UTF-8") %>% 
  filter(inFinalFile == "yes")

# Le gros problème : les mois non renseignés...
pb_month <- core %>% 
  filter(is.na(Month)) %>%
  filter(!(feuillet == "ReproPheno")) %>% # no month for pheno
  group_by(Site, Year,measurementDeterminedBy,feuillet) %>% 
  summarize(n = n()) %>% 
  arrange(Site,Year,feuillet)

write.csv2(pb_month,"output/pb_month.csv",row.names=F,fileEncoding = "Latin1")


## When day not filled in ####
# Add arbitrarily the middle of the month, except when we expect no month (e.g. for ReproPheno)
core_day <- core %>% 
  mutate(Day = case_when( !is.na(Month)  ~ 15,
                          TRUE ~ as.numeric(Day) )) %>% 
  left_join(day_month) # add the day_of_year

# Check: we still have many months not filled in...
core %>% filter(is.na(Day)) %>% 
  group_by(Site) %>% summarize(n = n())

core_day %>% filter(is.na(Day)) %>% 
  group_by(Site) %>% summarize(n = n())

core_day %>% filter(is.na(Day)) %>% 
  group_by(Site, Year,measurementDeterminedBy) %>% summarize(n = n())



#___________________
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
    geom_histogram() +
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
  geom_histogram() +
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

# en 2004, même échantillonnage temporel pour tout le monde. 3 campagnes de mesure différentes
core %>% 
  filter(Site == "CRE_PDM") %>% 
  filter(Treatment == "HiN") %>% 
  filter(Year == 2004) %>% 
  ggplot(aes(x=day_of_year)) +
  geom_histogram() +
  facet_wrap(~Code_Sp)

# en 2005
core %>% 
  filter(Site == "CRE_PDM") %>% 
  filter(Treatment == "HiN") %>% 
  filter(Year == 2005) %>% 
  filter((feuillet == "Biovolume")) %>% 
  # filter(verbatimTraitName %in% c("Dmax_vsh","Hrep_rsh")) %>% 
  ggplot(aes(x=day_of_year)) +
  geom_histogram() +
  facet_wrap(~Code_Sp)

core %>% 
  filter(Site == "CRE_PDM") %>% 
  filter(Treatment == "HiN") %>% 
  filter(Year == 2005) %>% 
  filter(!(feuillet == "Biovolume")) %>% 
  select(verbatimTraitName, day_of_year) %>% 
  unique() %>% View

core %>% 
  filter(Site == "CRE_PDM") %>% 
  filter(Treatment == "HiN") %>% 
  filter(Year == 2005) %>% 
  filter(Code_Sp == "ARENSERP") %>% 
  group_by(verbatimTraitName,day_of_year) %>% 
  summarize(n = n()) %>% View
  ggplot(aes(x=day_of_year)) +
  geom_histogram() +
  facet_wrap(~Code_Sp)
  
# Le suivi d'encombrement est fait en continu (hauteur et diamètre) (pour LoN et HiN)
  # Pour une espèce donnée, les 10 reps sont mesurées le même jour
  # Chacune des mesures = une population différente
# Pour les autres traits, deux dates, ou une date

# En 2006 : deux campagnes dans le LoN. Une seule dans le HiN.
core %>% 
  filter(Site == "CRE_PDM") %>% 
  filter(Treatment == "LoN") %>% 
  filter(Year == 2006) %>% 
  ggplot(aes(x=day_of_year)) +
  geom_histogram() +
  facet_wrap(~Code_Sp)

## Cazarils ####

# En 1998, on a deux moments de mesure pour pas mal d'espèces
core %>% 
  filter(Site == "Cazarils") %>% 
  filter(Year == 1999) %>%
  filter(!(feuillet %in% c("GasExchangeLeaf","GasExchangeChamber"))) %>% 
  ggplot(aes(x=day_of_year)) +
  geom_histogram() +
  facet_wrap(~Code_Sp)
  
  core %>% 
    filter(Site == "Cazarils") %>% 
    filter(Year == 1998) %>%
    filter(Code_Sp == "PRUNMAHA") %>% 
    ggplot(aes(x=day_of_year)) +
    geom_histogram()
  
  core %>% 
    filter(Site == "Cazarils") %>% 
    filter(Year == 1999) %>%
    filter(Code_Sp == "ARISROTU") %>% 
    pull(day_of_year) %>% unique()

## La Fage ####
fsite<-"La Fage"

fcore<-core %>% 
  group_by(Site, Treatment,Year) %>%
  filter(Site == fsite)

# campaings_per_year_and_treatement
camp<-fcore %>% 
  ggplot(aes(x = day_of_year)) +
  geom_histogram() +
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
  geom_histogram() +
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
  select(day_of_year,verbatimTraitName) %>% unique() %>% View

core %>% 
  filter(Site == "La Fage") %>% 
  # filter(Treatment == "GU_Deep") %>%
  filter(Year == 2007) %>% 
  ggplot(aes(x = day_of_year)) +
  geom_histogram(binwidth = 1) +
  ggtitle("La Fage") +
  facet_wrap(~Treatment)


core %>% 
  filter(Site == "La Fage") %>% 
  filter(Treatment == "GF_Clc") %>%
  filter(Year == 2007) %>%
  pull(day_of_year) %>% unique()

# GF_Clc : à partir du jour 172, "SdDM_msd" "Hrep_rsh" mesurés
core %>% 
  filter(Site == "La Fage") %>% 
  filter(Treatment == "GF_Clc") %>%
  filter(Year == 2006) %>% 
  filter(day_of_year %in% c( 172, 179,180, 186 ,192, 193, 201, 205, 207)) %>%
  pull(verbatimTraitName) %>% unique()


core %>% 
  filter(Site == "La Fage") %>% 
  filter(Treatment == "GU_Clc") %>%
  filter(Year == 2007) %>% 
  filter(day_of_year >199) %>%
  pull(verbatimTraitName) %>% unique()

core %>% 
  filter(Site == "La Fage") %>% 
  filter(Treatment == "GF_Clc") %>%
  filter(Year == 2006) %>% 
  pull(day_of_year) %>% unique()

# Un petit peu en 2010 (mais je peux utiliser les critères établis ailleurs)
fag<-core %>% 
  filter(Site == "La Fage") %>% 
  filter(Treatment == "GU_Deep") %>%
  filter(Year == 2010) %>% 
  ggplot(aes(x = day_of_year)) +
  geom_histogram() +
  facet_wrap(~Code_Sp)+
  ggtitle("La Fage")
fag

# Pas tellement quand grands pics, genre 2006 dans GF_Dlm
fag<-core %>% 
  filter(Site == "La Fage") %>% 
  filter(Treatment == "GF_Dlm") %>%
  filter(Year == 2006) %>% 
  ggplot(aes(x = day_of_year)) +
  geom_histogram() +
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
  geom_histogram() +
  facet_grid(rows = vars(Treatment),cols = vars(Year)) +
  ggtitle(fsite)

fcore %>% 
  ggplot(aes(x = day_of_year)) +
  geom_histogram() +
  facet_wrap(~Year) +
  ggtitle(fsite)

fcore %>% 
  filter(Year == "2000") %>% 
  ggplot(aes(x = day_of_year)) +
  geom_histogram() +
  facet_wrap(~Treatment)+
  ggtitle(fsite)

fcore %>% 
  filter(Year == "2002") %>% 
  filter(Treatment == "FieldAge_2") %>% 
  select(day_of_year,verbatimTraitName) %>% View

ggsave(paste0("output/plot/campaings_",fsite,".png"), camp,width = 8,height=12)

# Deux campagnes en 2002, pour FieldAge_42
hgm<-core %>% 
  filter(Site == "Hautes Garrigues") %>% 
  filter(Treatment == "FieldAge_11") %>%
  filter(Year == 2002) %>% 
  ggplot(aes(x = day_of_year)) +
  geom_histogram() +
  facet_wrap(~Code_Sp)+
  ggtitle("LHGM")
hgm

core %>% 
  filter(Site == "Hautes Garrigues") %>% 
  filter(Year == 2000) %>% 
  filter(day_of_year<50) %>% View

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
  geom_histogram() +
  facet_wrap(~Code_Sp)


# Pour Cazarils
core %>% 
  filter(Site == "Cazarils") %>% dim()






#_______________________
# For ALL THE SITES ####


## Specific case of Biovolume in PDM ####
# For biovolume in 2005 at PDM: one field campaign per day
core_biovolume_PDM1 <-   core_day %>% 
  filter(Site == "CRE_PDM" & Year == 2005 & feuillet=="Biovolume" ) %>%
  select(Site,traitPlot,Treatment,Year,Code_Sp,feuillet,day_of_year,verbatimOccurrenceID_population) %>%
  unique() %>%
  arrange(Code_Sp) %>% 
  group_by(Site,feuillet,Code_Sp,traitPlot,Treatment) %>% 
  mutate(numbering = row_number()) %>% 
  mutate(verbatimOccurrenceID_field_campaign = paste0(verbatimOccurrenceID_population,"_FC",numbering)) %>% 
  select(-numbering) %>% 
  ungroup() %>% 
  select(traitPlot,feuillet,verbatimOccurrenceID_population, day_of_year,verbatimOccurrenceID_field_campaign)

core_biovolume_PDM <- core_day %>% full_join(core_biovolume_PDM1) 

## Attribution to field campaigns ####
core2 <- core_biovolume_PDM %>% 
  mutate(verbatimOccurrenceID_field_campaign = 
           case_when(
             
             ### In Garraf and Les Agros ####
             # 19One field campaign per year
             Site %in% c("Garraf","Les Agros") ~ 
               paste0(verbatimOccurrenceID_population,"_FC1"),
             
             ### In Cazarils ####
             # 1997: one field campaign
             Site == "Cazarils" & Year == 1997 ~ 
               paste0(verbatimOccurrenceID_population,"_FC1"),
             
             # 1998: three field campaigns
             Site == "Cazarils" & Year == 1998 & day_of_year %in% c(15,28,29) ~ 
               paste0(verbatimOccurrenceID_population,"_FC1"),
             Site == "Cazarils" & Year == 1998 & day_of_year %in% seq(124,157, by = 1) ~ 
               paste0(verbatimOccurrenceID_population,"_FC2"),
             Site == "Cazarils" & Year == 1998 & day_of_year %in% seq(166,196, by = 1) ~ 
               paste0(verbatimOccurrenceID_population,"_FC3"),
             
             # 1999: one FC for gas exchange
             Site == "Cazarils" & Year == 1999 & feuillet %in% c("GasExchangeLeaf","GasExchangeChamber") ~ 
               paste0(verbatimOccurrenceID_population,"_FC1"),
             # One FC for the rest
             Site == "Cazarils" & Year == 1999 & !(feuillet %in% c("GasExchangeLeaf","GasExchangeChamber")) ~ 
               paste0(verbatimOccurrenceID_population,"_FC2"),
             # Except for 2 species: two FC for them
             Site == "Cazarils" & Year == 1999 & !(feuillet %in% c("GasExchangeLeaf","GasExchangeChamber") & 
                                Code_Sp == "ARISROTU" & day_of_year > 155) ~ 
               paste0(str_sub(verbatimOccurrenceID_population, end = -2),"3"),
             Site == "Cazarils" & Year == 1999 & !(feuillet %in% c("GasExchangeLeaf","GasExchangeChamber") & 
                                Code_Sp == "KICKSPUR" & day_of_year > 150) ~ 
               paste0(str_sub(verbatimOccurrenceID_population, end = -2),"3"),
             
             # 2001, 2017 and 2023
             Site == "Cazarils" & Year %in% c(2001,2017,2023)  ~ 
               paste0(verbatimOccurrenceID_population,"_FC1"),
             
             
             ### In CRE_PDM ####
             # 2004: three field campaigns, identical for every species
             Site == "CRE_PDM" & Year == 2004 & day_of_year < 100 ~ 
               paste0(verbatimOccurrenceID_population,"_FC1"),
             Site == "CRE_PDM" & Year == 2004 & day_of_year %in% seq(100,150, by = 1) ~ 
               paste0(verbatimOccurrenceID_population,"_FC2"),
             Site == "CRE_PDM" & Year == 2004 & day_of_year > 150 ~ 
               paste0(verbatimOccurrenceID_population,"_FC3"),
             
             # 2005: four field campaigns, identical for every species, plus a temporal sequence (in the biovolume spreadsheet)
             Site == "CRE_PDM" & Year == 2005 & !(feuillet=="Biovolume") & day_of_year < 100 ~ 
               paste0(verbatimOccurrenceID_population,"_FC1"),
             Site == "CRE_PDM" & Year == 2005 & !(feuillet=="Biovolume") & day_of_year %in% seq(100,150, by = 1) ~ 
               paste0(verbatimOccurrenceID_population,"_FC2"),
             Site == "CRE_PDM" & Year == 2005 & !(feuillet=="Biovolume") & day_of_year %in% seq(150,200, by = 1) ~ 
               paste0(verbatimOccurrenceID_population,"_FC3"),
             Site == "CRE_PDM" & Year == 2005 & !(feuillet=="Biovolume") & day_of_year > 200 ~ 
               paste0(verbatimOccurrenceID_population,"_FC4"),
             
             # 2005: temporal sequence (for the Biovolume sheet)
             Site == "CRE_PDM" & Year == 2005 & feuillet=="Biovolume" ~
               verbatimOccurrenceID_field_campaign,
             
             # 2006: one field campaign in the HiN, and two in the LoN
             Site == "CRE_PDM" & Year == 2006 & Treatment == "HiN" ~ 
               paste0(verbatimOccurrenceID_population,"_FC1"),
             
             Site == "CRE_PDM" & Year == 2006 & Treatment == "LoN" & day_of_year < 90 ~ 
               paste0(verbatimOccurrenceID_population,"_FC1"),
             Site == "CRE_PDM" & Year == 2006 & Treatment == "LoN" & day_of_year >= 90 ~ 
               paste0(verbatimOccurrenceID_population,"_FC2"),
             
             ### In CRE_O2LA ####
             
             # 2012: two FC
             Site == "CRE_O2LA" & Year == 2012  & day_of_year <= 157 ~ 
               paste0(verbatimOccurrenceID_population,"_FC1"),
             Site == "CRE_O2LA" & Year == 2012  & day_of_year > 158 ~ 
               paste0(verbatimOccurrenceID_population,"_FC2"),
             
             # 2013: one FC
             Site == "CRE_O2LA" & Year == 2013 ~ 
               paste0(verbatimOccurrenceID_population,"_FC1"),
             
             ### In Hautes Garrigues ####
             
             # In 2000, four FC
             Site == "Hautes Garrigues" & Year == 2000 & day_of_year < 100 ~ 
               paste0(verbatimOccurrenceID_population,"_FC1"),
             Site == "Hautes Garrigues" & Year == 2000 & day_of_year %in% seq(100,180,by = 1) ~ 
               paste0(verbatimOccurrenceID_population,"_FC2"),
             Site == "Hautes Garrigues" & Year == 2000 & day_of_year %in% seq(181,300,by = 1) ~ 
               paste0(verbatimOccurrenceID_population,"_FC3"),
             Site == "Hautes Garrigues" & Year == 2000 & day_of_year > 300 ~ 
               paste0(verbatimOccurrenceID_population,"_FC4"),
             
             # In 2001, 2003, 2004: one FC each year
             Site == "Hautes Garrigues" & Year %in% c(2001, 2003,2004) ~ 
               paste0(verbatimOccurrenceID_population,"_FC1"),
             
             # In 2002, three FC
             Site == "Hautes Garrigues" & Year == 2002 & day_of_year <= 137 ~ 
               paste0(verbatimOccurrenceID_population,"_FC1"),
             Site == "Hautes Garrigues" & Year == 2002 & day_of_year %in% seq(138,200,by = 1) ~ 
               paste0(verbatimOccurrenceID_population,"_FC2"),
             Site == "Hautes Garrigues" & Year == 2002 & day_of_year > 200 ~ 
               paste0(verbatimOccurrenceID_population,"_FC3"),
             
             
             ### In La Fage ####
             
             # In all years except 2013, one field campaign
             Site == "La Fage" & !(Year == 2013) ~
               paste0(verbatimOccurrenceID_population,"_FC1"),

             # In 2013, four field campaigns
             Site == "La Fage" & Year == 2013 & day_of_year < 50 ~
               paste0(verbatimOccurrenceID_population,"_FC1"),
             Site == "La Fage" & Year == 2013 & day_of_year %in% seq(80,127,by = 1) ~
               paste0(verbatimOccurrenceID_population,"_FC2"),
             Site == "La Fage" & Year == 2013 & day_of_year %in% seq(135,180,by = 1) ~
               paste0(verbatimOccurrenceID_population,"_FC3"),
             Site == "La Fage" & Year == 2013 & day_of_year > 250 ~ 
               paste0(verbatimOccurrenceID_population,"_FC4"),
             
             is.na(Month) & !(feuillet == "ReproPheno") ~ "no Month",
             
             TRUE ~ "problem"
           ) 
  )


# Add dates ####
# A UTILISER SI ON RAJOUTE DES DATES là ooù elles manquaient
# Voir si elles ne sont pas attribuées à des FC car hors des intervalles définis
core2 %>% 
  filter(verbatimOccurrenceID_field_campaign == "not done yet") %>% 
  group_by(Site,Year) %>% 
  summarize(n = n()) 

core2 %>% 
  filter(verbatimOccurrenceID_field_campaign == "not done yet") %>% 
  filter(Site == "Les Agros") %>% 
  select(Year,Month,Day) %>% unique()

# Check comment renseigné

core2 %>% 
  filter(verbatimOccurrenceID_field_campaign == "problem") %>% 
  group_by(Site,Year, Month, Day,day_of_year) %>% 
  summarize(n = n() )

core2 %>% 
  filter(verbatimOccurrenceID_field_campaign == "no Month") %>% 
  group_by(Site,Year) %>% 
  summarize(n = n() )

# Pbs not filled####

dim(core)
dim(core2)

# AVEC MON ALGO, quand il y a des NA dans day_of_year, on dirait qu'il ne fait rien...
core2 %>% filter(is.na(day_of_year)) %>% pull(verbatimOccurrenceID_field_campaign) %>% sort() %>%  unique()

# Problème de NA qui réapparâit dans les jours quand Month == 5
core_day %>% filter(is.na(Day)) %>% filter(Month == 5) 
core2 %>% filter(is.na(Day)) %>% filter(Month == 5) 

a<-core %>% pull(verbatimOccurrenceID) 
b<-core2 %>% pull(verbatimOccurrenceID) 

setdiff(a,b) %>% as.data.frame() %>% View

A<-core %>% filter(verbatimOccurrenceID == "AEGIGENI_Cazarils_CAZ_NA_none_1998_5_NA_Rvg1_Hveg_vsh")
B<-core_day %>% filter(verbatimOccurrenceID == "AEGIGENI_Cazarils_CAZ_NA_none_1998_5_NA_Rvg1_Hveg_vsh")
C<-core_biovolume_PDM %>% filter(verbatimOccurrenceID == "AEGIGENI_Cazarils_CAZ_NA_none_1998_5_NA_Rvg1_Hveg_vsh")
D<-core2 %>% filter(verbatimOccurrenceID == "AEGIGENI_Cazarils_CAZ_NA_none_1998_5_NA_Rvg1_Hveg_vsh") %>% select(-verbatimOccurrenceID_field_campaign)

rbind(A,B,C) %>% View
B$Day
