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

# correct typos in species names
SPECIES2 <- SPECIES %>% 
  mutate(Species = case_when( Species == "Potentilla reptens" ~ "Potentilla reptans" ,
                              Species == "Vicia heteophylla" ~ "Vicia heterophylla",
                              Species == "Ampelodesmos mauritanica" ~ "Ampelodesmos mauritanicus",
                              Species == "Kickxia spruria" ~ "Kickxia spuria",
                              Species == "Convovulus arvensis" ~ "Convolvulus arvensis",
                              Species == "Cratægus monogyna" ~ "Crataegus monogyna",
                              TRUE ~ Species)) %>% 
  filter(!(Species == "Geranium dissectum - pétiole")) %>% 
  filter(!(Species == "Carex humilis?"))

# Duplicated code_Sp = pb in LifeForm, souvent !
SPECIES2 %>% 
  group_by(Code_Sp) %>% 
  filter(n()>1) %>% 
  arrange(Species) %>% 
  write.csv2("output/species_with_pb_in_lifeform.csv",row.names=F)

# Duplicated Species
SPECIES2 %>% 
  group_by(Species) %>% 
  filter(n()>1) %>% 
  arrange(Species)


# species list correct
SPECIES2 %>% 
  select(Species,Code_Sp) %>% 
  unique() %>% 
  arrange(Species) %>% 
  write.csv2("output/Species_Code_Sp.csv",row.names = F)



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
