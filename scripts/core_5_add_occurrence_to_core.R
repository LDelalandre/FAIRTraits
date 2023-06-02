library(tidyverse)

TIDY_plot <- read.csv2("output/TIDY_plot.csv",fileEncoding = "latin1",sep="\t",dec = ".")

# OccurrenceIDs ####
TIDY_occurrenceID <- TIDY_plot %>%
  mutate(verbatimOccurrenceID = paste(Code_Sp,Site,Block,traitPlot,Treatment,Year,Month,Day,Rep,verbatimTraitName,traitEntity,sep = "_")) %>% 
  mutate(verbatimOccurrenceID_echantillon = paste(Code_Sp,Site,Block,traitPlot,Treatment,Year,Month,Day,Rep,sep = "_")) %>% 
  mutate(verbatimOccurrenceID_population = paste(Code_Sp,Site,Block,traitPlot,Treatment,Year,Month,Day,sep = "_")) %>% 
  # remove columns already present in taxon
  select(-c(Code_Sp,Family,LifeForm1,LifeForm2))

#____________________________________
# TEMPORARY duplicated occurrence ####
source("scripts/functions/regenerate_spreadsheet.R")

DUPL <- TIDY4_occurrenceID[which(duplicated(TIDY4_occurrenceID$verbatimOccurrenceID)),]
DUPL %>% 
  pull(Site) %>% 
  unique()

DUPLcplet <- TIDY4_occurrenceID %>% 
  filter(verbatimOccurrenceID %in% DUPL$verbatimOccurrenceID) %>% 
  arrange(Site,feuillet)
write.csv2(DUPLcplet,"data/AFaire_Avril2023/occurrences/duplicated_occurrenceID_format_core.csv",row.names=F,fileEncoding = "latin1")

sum_DUPL <- DUPL %>% 
  group_by(Site, feuillet) %>% 
  summarize(n = n())
write.csv2(sum_DUPL,"data/AFaire_Avril2023/occurrences/sites_feuillets_with_duplicated_occurrenceID.csv",row.names=F,fileEncoding = "latin1")

names_feuillets <- DUPLcplet %>%
  select(Site,feuillet) %>% 
  unique() %>% 
  mutate(site_feuillet = paste(Site,feuillet,sep = "_")) %>% 
  mutate(site_feuillet = str_sub(site_feuillet,start = 1, end = 31)) %>%  # # maximum length of excel worksheet names
  pull(site_feuillet)

list_site_feuillet <- DUPLcplet %>% 
  select(-c(verbatimOccurrenceID,verbatimOccurrenceID_echantillon,verbatimOccurrenceID_population)) %>% 
  group_split(Site,feuillet) %>% 
  setNames(names_feuillets) %>% 
  unique()

list_feuillet <- lapply(list_site_feuillet,regenerate_spreadsheet)

# list_site_feuillet[[1]] %>% 
#   View()
# list_feuillet[[1]] %>% 
#   View()

# Export it into an excel file
lst_data <- list_feuillet
wb <- openxlsx::createWorkbook()
purrr::imap(
  .x = lst_data,
  .f = function(df, object_name) {
    openxlsx::addWorksheet(wb = wb, sheetName = object_name)
    openxlsx::writeData(wb = wb, sheet = object_name, x = df)
  }
)
saveWorkbook(wb = wb, file = "data/AFaire_Avril2023/occurrences/duplicated_occurrenceID_format_excel.xlsx")


# Export ####
TIDY4_occurrenceID_nodupl <- TIDY4_occurrenceID %>% 
  filter(!verbatimOccurrenceID %in% DUPL$verbatimOccurrenceID)

write.table(TIDY4_occurrenceID_nodupl ,"output/TIDY_occurrenceID_nodupl.csv",fileEncoding = "latin1",row.names=F,sep="\t",dec = ".")

# Info on treatments
# TIDY4_occurrenceID <- read.csv2("output/TIDY_occurrenceID.csv",sep="\t")
# treatment <- TIDY4_occurrenceID_nodupl %>% 
#   select(Site, Treatment) %>% 
#   unique() 
# write.csv2(treatment,"output/treatments_per_site.csv",row.names=F)