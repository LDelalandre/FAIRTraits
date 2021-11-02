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

write.csv2(SPECIES,"output/list of species.csv",row.names = F)

# Duplicated code_Sp
SPECIES %>% 
  group_by(Code_Sp) %>% 
  filter(n()>1) %>% 
  arrange(Species) %>% 
  write.csv2("output/duplicated Code_Sp.csv",row.names=F)

# Duplicated Species
SPECIES %>% 
  group_by(Species) %>% 
  filter(n()>1) %>% 
  arrange(Species)

SPECIES %>% 
  pull(Code_Sp) %>% 
  unique()


# Per species * site ###
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

write.csv2(SPECIES,"output/list of species_site.csv",row.names = F)

####
columns_other_than_traits <- 
  c("Site", "Block" ,"Plot", "Treatment", "Year"     ,   "Day"       ,       "Species" , "Code_Sp"   ,    "Family" ,
    "LifeForm1" ,"LifeForm2" ,  "Rep",
    "nameOfProject" ,"measurementDeterminedBy")