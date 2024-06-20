library(tidyverse)

# This script generates occurrenceIDs, defined at three levels: 
# the level of data record (verbatimOccurrenceID) ; 
# the level of the replicate on which a specific data record was taken (verbatimOccurrenceId_sample);
# the level of the population (species x site x plot x treatment x date) in which the measured individual was sampled.

TIDY_plot <- data.table::fread("output/TIDY_4_plot.csv",encoding="UTF-8")

# Generate OccurrenceIDs ####
TIDY_occurrenceID <- TIDY_plot %>%
  mutate(verbatimOccurrenceID = paste(Code_Sp,Site,traitPlot,Treatment,Year,Month,Day,Rep,verbatimTraitName,sep = "_")) %>% 
  mutate(verbatimOccurrenceID_sample = paste(Code_Sp,Site,traitPlot,Treatment,Year,Month,Day,Rep,sep = "_")) %>% 
  mutate(verbatimOccurrenceID_population = paste(Code_Sp,Site,Treatment,Year,sep = "_"))

## Quality check ####

# Is occurrenceID unique ?
which(duplicated(TIDY_occurrenceID$verbatimOccurrenceID))
dupl_occ <- TIDY_occurrenceID[which(duplicated(TIDY_occurrenceID$verbatimOccurrenceID)),]

TIDY_occurrenceID %>% pull(verbatimOccurrenceID) %>% length()
TIDY_occurrenceID %>% pull(verbatimOccurrenceID) %>% unique() %>% length()


## Export ####
data.table::fwrite(TIDY_occurrenceID,"output/TIDY_5_occurrenceID.csv",sep="\t")


# Modify population identifyer so that it includes field campaign ####

day_month<-read.table("data/day_month.txt",header=T) %>% 
  rename(Month = month, Day = day_of_month)

core <-  data.table::fread("output/TIDY_5_occurrenceID.csv",encoding = "UTF-8") %>% 
  filter(inFinalFile == "yes") %>% 
  # When day not filled in : add arbitrarily the middle of the month, except when we expect no month (e.g. for ReproPheno)
  mutate(Day = case_when( !is.na(Month) & is.na(Day)  ~ 15,
                          TRUE ~ as.numeric(Day) )) %>% 
  left_join(day_month) # add the day_of_year


# Add field campaign ID for all the sites

## Specific case of Biovolume in PDM ####
# For biovolume in 2005 at PDM: one field campaign per day
core_biovolume_PDM1 <-   core %>% 
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

core_biovolume_PDM <- core %>% full_join(core_biovolume_PDM1) 

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
             
             # is.na(Month) & !(feuillet == "ReproPheno") ~ "no Month",
             
             # TRUE ~ paste0(verbatimOccurrenceID_population,"_FC_not_defined"),
             TRUE ~ paste0(verbatimOccurrenceID_population),
           ) 
  ) 

core3 <- core2 %>% 
  rename(verbatimOccurrenceID_population_old = verbatimOccurrenceID_population) %>% 
  rename(verbatimOccurrenceID_population = verbatimOccurrenceID_field_campaign)


## Complete with manually-filled file ####
missing_id_field_campaing <- readxl::read_excel("data/TIDY_ID_field_campaign_no_field_campaign.xlsx", sheet = "WithoutRep") %>%
  rename(verbatimOccurrenceID_population_old = verbatimOccurrenceID_population) %>% 
  select(-verbatimOccurrenceID_field_campaign) %>% 
  unique()

core4 <- left_join(core3, missing_id_field_campaing)

core5 <- core4 %>% 
  mutate(verbatimOccurrenceID_population = case_when(is.na(FC) ~ verbatimOccurrenceID_population,
                                                     TRUE ~ paste(verbatimOccurrenceID_population,FC,sep = "_"))) %>% 
  select(-FC)

# Export ####
data.table::fwrite(core5 ,"output/TIDY_6_ID_field_campaign.csv",sep="\t")

dim(core)
dim(core3)
dim(core4)
dim(core5)

# A FINIR
pb_FC <- core3 %>% 
  filter(grepl("_FC_not_defined",verbatimOccurrenceID_population)) %>% 
  select(verbatimOccurrenceID_population_old) %>% 
  unique()
data.table::fwrite(pb_FC,"output/TIDY_ID_field_campaign_no_field_campaign_compacté.csv",sep="\t")



pb_FC %>% dim()


