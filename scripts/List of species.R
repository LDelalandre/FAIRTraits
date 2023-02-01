source(("scripts/1_read_files.R"))

# Per species ####
SPECIES <- NULL

for (site in sites){
  files <- read_files(site)
  
  if(class(files) == "list"){
    for (i in c(1:length(files))){
      file <- files[[i]] %>% 
        select(Species, Code_Sp, Family, LifeForm1, LifeForm2) %>% 
        unique()
      SPECIES <- rbind(SPECIES,file)
    }
  } else if (class(files) == "data.frame"){
    file <- files
    SPECIES <- rbind(SPECIES,file)
  }
  SPECIES <- unique(SPECIES)
}
SPECIES <- unique(SPECIES)
dim(SPECIES)

# correct typos in species names
SPECIES2 <- SPECIES %>% 
  mutate(Code_Sp = case_when( Species == "Carex humilis?" & Code_Sp == "CAREHUMI" ~ "CARESP",
                              TRUE ~ Code_Sp)) %>% 
  mutate(Species = case_when( Species == "Potentilla reptens" ~ "Potentilla reptans" ,
                              Species == "Vicia heteophylla" ~ "Vicia heterophylla",
                              Species == "Ampelodesmos mauritanica" ~ "Ampelodesmos mauritanicus",
                              Species == "Kickxia spruria" ~ "Kickxia spuria", # r en trop
                              Species == "Convovulus arvensis" ~ "Convolvulus arvensis", # manque un l
                              Species == "Cratægus monogyna" ~ "Crataegus monogyna",
                              Species == "Carex humilis?" ~ "Carex sp.",
                              Species == "Myosostis ramosissima subsp. ramosissima" ~ "Myosotis ramosissima subsp. ramosissima", # s en trop
                              Species == "Helichrysum stoechas ssp. stoechas" ~ "Helichrysum stoechas subsp. stoechas",
                              Species == "Viola alba ssp. scotophylla" ~ "Viola alba subsp. scotophylla",
                              Species == "Carex hallerana" ~  "Carex halleriana",
                              Species == "Plantago lanceola" ~ "Plantago lanceolata",
                              Species == "Catananche coerulea" ~ "Catananche caerulea",
                              Species == "Chamærops humilis" ~ "Chamaerops humilis",
                              Species == "Cirsium acaule" ~ "Cirsium acaulon",
                              Species == "Inula conyza" ~ "Inula conyzae",
                              TRUE ~ Species)) %>% 
  filter(!(Species == "Geranium dissectum - pétiole"))%>% 
  filter(!(Species == "Geranium dissectum - limbe"))

# espèces en plus catherine
sp_CR <- data.frame(Species = c("Pallenis maritima",
                                "Campanula rapunculus",
                                "Blackstonia perfoliata",
                                "Draba verna",
                                "Muscari comosum",
                                "Crepis pulchra")) %>% 
  mutate(Code_Sp = c("PALLMARI",
                     "CAMPRAPU",
                     "BLACPERF",
                     "DRABVERN",
                     "MUSCCOMO",
                     "CREPPULC"))

SPECIES3 <- SPECIES2 %>% 
  select(Species,Code_Sp) %>% 
  unique() %>% 
  rbind(sp_CR)

sp_dupl <- SPECIES3[which(duplicated(SPECIES3$Code_Sp)),]$Code_Sp
SPECIES3 %>% 
  filter(Code_Sp%in% sp_dupl) %>% 
  arrange(Code_Sp) %>% arrange(Species)




write.csv2(SPECIES3,"data/species/Fichier_sp_flores/Species_Code_Sp.csv",row.names = F)



#_______________________________________________________________________
# to delete?
# Species per site ###
SPECIES2 <- NULL

for (site in sites){
  files <- read_files(site)
  
  if(class(files) == "list"){
    for (i in c(1:length(files))){
      file <- files[[i]] %>% 
        select(Species, Code_Sp, Family, LifeForm1, LifeForm2,Site) %>% 
        unique()
      SPECIES2 <- rbind(SPECIES2,file)
    }
  } else if (class(files) == "data.frame"){
    file <- files %>% 
      mutate(Site = if_else(site == "HGM","HGM","PDM"))
    SPECIES2 <- rbind(SPECIES2,file)
  }
  SPECIES2 <- unique(SPECIES2)
}
SPECIES2 <- unique(SPECIES2)

write.csv2(SPECIES,"output/list of species_per_site.csv",row.names = F)
