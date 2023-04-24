library(tidyverse)
source("scripts/functions/regenerate_spreadsheet.R")

TIDY5 <-  read.csv2("output/TIDY_MoFTraits.csv",fileEncoding = "latin1",sep="\t",dec = ".")
taxon <- read.csv2("output/taxon.csv")

#__________________________
# Add taxon information ####
TIDY5_taxon <- TIDY5 %>% 
  merge(taxon)

# Test taxon correspondence ####
TIDY5 %>% 
  dim()

# Manque certains taxa dans l'extension
Sp_core <- TIDY5$Species %>% unique()
Sp_taxon <- taxon$Species %>% unique()

setdiff(Sp_core,Sp_taxon)
setdiff(Sp_taxon,Sp_core) # ATTENDRE QUE LA BASE SOIT COMPLETE POUR CHECKER (elle était peut-être dans les espèces dupliquées)

TIDY5 %>% 
  filter(Species == "Festuca ovina (sp.)?") %>% 
  select(Site,feuillet) %>% 
  unique()

#__________________________
# Export it into one excel file per site ####
SITES <- TIDY5_taxon %>% 
  pull(Site) %>% 
  unique()

# Supprimer les fichiers excel avant (le code ne parvient pas à les écraser)
for (fsite in SITES){
  # names of spreadsheets
  names_spreadsheet <- TIDY5_taxon %>%
    filter(Site == fsite) %>% 
    arrange(feuillet) %>% 
    pull(feuillet) %>% 
    unique()
    # mutate(site_feuillet = paste(Site,feuillet,sep = "_")) %>% 
    # mutate(site_feuillet = str_sub(site_feuillet,start = 1, end = 31)) %>%  # # maximum length of excel worksheet names
    # pull(site_feuillet)
  
  # list of spreadsheets to export
  list_tidy_spreadsheet <- TIDY5_taxon %>% 
    filter(Site == fsite) %>% 
    group_split(Site,feuillet) %>% 
    setNames(names_spreadsheet)
  
  list_spreadsheet <- lapply(list_tidy_spreadsheet,regenerate_spreadsheet)
  
  lst_data <- list_spreadsheet
  wb <- openxlsx::createWorkbook()
  purrr::imap(
    .x = lst_data,
    .f = function(df, object_name) {
      openxlsx::addWorksheet(wb = wb, sheetName = object_name)
      openxlsx::writeData(wb = wb, sheet = object_name, x = df)
    }
  )
  saveWorkbook(wb = wb, file = paste0("output/excel_files/Traits_",fsite,".xlsx"))
}

