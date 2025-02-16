source(("scripts/1_read_files.R"))

# Per species ####
SPECIES <- NULL
SPECIES_site <- NULL # avec les espèces par site

for (site in sites){
  files <- read_files(site)
  
  if(class(files) == "list"){
    for (i in c(1:length(files))){
      file <- files[[i]] %>% 
        select(Species, Code_Sp, Family, LifeForm1, LifeForm2) %>% 
        unique()
      SPECIES <- rbind(SPECIES,file)
      SPECIES_site <- rbind(SPECIES_site,file %>% mutate(site=site))
    }
  } else if (class(files) == "data.frame"){
    file <- files
    SPECIES <- rbind(SPECIES,file)
    SPECIES_site <- rbind(SPECIES_site,file %>% mutate(site=site))
  }
  SPECIES <- unique(SPECIES)
}
SPECIES <- unique(SPECIES)
dim(SPECIES)

sp_tison <- read.xlsx("data/species/Fichier_sp_flores/Species_completed_Tison.xlsx")
setdiff(sp_tison$Species,SPECIES$Species)
setdiff(SPECIES$Species,sp_tison$Species)



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
                              Species == "Festuca christiani-bernardii" ~ "Festuca christiani-bernardi",
                              # "Linum tenuifolium subsp. tenuifolium" n'existe pas dans TAXREF
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

species_leo <- read.csv2("data/species/species_leo.csv") 
sp_LD <- data.frame(Species = c("Hornungia petraea",
                                "Saxifraga tridactylites",
                                "Valerianella pumila"),
                    Code_Sp = c("HORNPETR",
                                "SAXITRID",
                                "VALEPUMI"))

SPECIES3 <- SPECIES2 %>% 
  select(Species,Code_Sp) %>% 
  unique() %>% 
  rbind(sp_CR) %>% 
  rbind(sp_LD) %>% 
  mutate(Code_Sp = case_when(Code_Sp == "AMPEMAURI" ~ "AMPEMAUR",
                             Code_Sp == "AVENBRO" ~ "AVENBROM",
                             Code_Sp == "CATACOER" ~ "CATACAER",
                             Code_Sp == "DACTGLOM" ~ "DACTGLOM-HIS",
                             Code_Sp == "HELISTOE" ~ "HELISTOE-STO",
                             Code_Sp == "RUBUSPEC" ~ "RUBUSP",
                             Code_Sp == "VIOLALBA" ~ "VIOLALBA-SCO",
                             Code_Sp == "XEREINAP" ~ "XERAINAP",
                             TRUE ~Code_Sp
  )) %>% 
  unique()

# one species duplicated: subspecies sometimes only
sp_dupl <- SPECIES3[which(duplicated(SPECIES3$Code_Sp)),]$Code_Sp
SPECIES3 %>% 
  filter(Code_Sp%in% sp_dupl) %>% 
  arrange(Code_Sp) %>% arrange(Species)

sp_dupl <- SPECIES3[which(duplicated(SPECIES3$Species)),]$Species
SPECIES3 %>% 
  filter(Species%in% sp_dupl) %>% 
  arrange(Species)

SPECIES3 

SPECIES4 <- SPECIES3 %>% 
  mutate(family = map_chr(Species,info_taxref,"FAMILLE"))


write.csv2(SPECIES4 %>% arrange(family),"data/species/Fichier_sp_flores/Species_Code_Sp.csv",row.names = F)

dim(SPECIES4)


# garder l'info sur la forme de vie quand elle y est, et compléter seulement quand besoin !
# colonnes à ajouter :

# LifeForm1 # DPh et EPh (deciduous et evergreen) for phanerophytes (evt. a posteriori)
# LifeForm2
# Woodiness (as diaz database, evt. a posteriori)
# LifeHistory (perenne, annuelle, bisannuelle), à déduire de LifeForm1 et LifeForm2 ? Mais pour bisannuelle?
# PHflora (prendre le plus haut, e.g. 10-25 : prendre 25 cm)
# Phénologie de la floraison (début et fin)
# Chorologie (chez Tison et Bernard, voir si homogène)



#___________________________________________________________________
# Espèces en commun La Fage et Cazarils ####
# Pour voir quelle flore on choisit
files <- read_files("LaFage")
SPECIES <- NULL
for (i in c(1:length(files))){
  file <- files[[i]] %>% 
    select(Species, Code_Sp) %>% 
    unique()
  SPECIES <- rbind(SPECIES,file)
}
sp_lafage<- unique(SPECIES)

files <- read_files("Cazarils")
SPECIES <- NULL
for (i in c(1:length(files))){
  file <- files[[i]] %>% 
    select(Species, Code_Sp) %>% 
    unique()
  SPECIES <- rbind(SPECIES,file)
}
sp_cazarils<- unique(SPECIES)

sp_common <- intersect(sp_cazarils$Species,sp_lafage$Species)
df <- data.frame(Species = sp_common) %>% 
  arrange(Species)
write.csv2(df,"data/species/Fichier_sp_flores/sp_common_cazarils_lafage.csv",row.names=F)

read.csv2("data/species/Fichier_sp_flores/sp_common_cazarils_lafage_completed.csv")
