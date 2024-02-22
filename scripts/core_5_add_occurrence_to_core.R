library(tidyverse)

TIDY_plot <- data.table::fread("output/TIDY_plot.csv",encoding="UTF-8")


# Generate OccurrenceIDs ####
TIDY_occurrenceID <- TIDY_plot %>%
  mutate(verbatimOccurrenceID = paste(Code_Sp,Site,Block,traitPlot,Treatment,Year,Month,Day,Rep,verbatimTraitName,traitEntityValid,sep = "_")) %>% 
  mutate(verbatimOccurrenceID_sample = paste(Code_Sp,Site,Block,traitPlot,Treatment,Year,Month,Day,Rep,sep = "_")) %>% 
  mutate(verbatimOccurrenceID_population = paste(Code_Sp,Site,Block,traitPlot,Treatment,Year,Month,Day,sep = "_"))

# Quality check ####

# Is occurrenceID unique ?
which(duplicated(TIDY_occurrenceID$verbatimOccurrenceID))
dupl_occ <- TIDY_occurrenceID[which(duplicated(TIDY_occurrenceID$verbatimOccurrenceID)),]

TIDY_occurrenceID %>% pull(verbatimOccurrenceID) %>% length()
TIDY_occurrenceID %>% pull(verbatimOccurrenceID) %>% unique() %>% length()


# Export ####
data.table::fwrite(TIDY_occurrenceID,"output/TIDY_occurrenceID.csv",sep="\t")

data.table::fwrite(TIDY_occurrenceID %>% 
                     select(-c(measurementMethod)),
                   "output/TIDY_occurrenceID_no_sampling_measurement.csv",sep="\t")


#____________________________________
# TEMPORARY duplicated occurrence ####
source("scripts/functions/regenerate_spreadsheet.R")

DUPL <- TIDY_occurrenceID[which(duplicated(TIDY_occurrenceID$verbatimOccurrenceID)),]
DUPL %>% 
  pull(Site) %>% 
  unique()

DUPLcplet <- TIDY_occurrenceID %>% 
  filter(verbatimOccurrenceID %in% DUPL$verbatimOccurrenceID) %>% 
  arrange(Site,feuillet)
write.csv2(DUPLcplet,"output/WorkingFiles/2023_09_12_duplicated_occurrenceID_format_core.csv",row.names=F,fileEncoding = "latin1")

DUPLcplet %>% 
  unique() %>% 
  group_by(Site) %>%
  summarize(n = n()) %>% 
  arrange(n)

sum_DUPL <- DUPL %>% 
  group_by(Site, feuillet) %>% 
  summarize(n = n())
write.csv2(sum_DUPL,"output/WorkingFiles/sites_feuillets_with_duplicated_occurrenceID.csv",row.names=F,fileEncoding = "latin1")

names_feuillets <- DUPLcplet %>%
  select(Site,feuillet) %>% 
  unique() %>% 
  mutate(site_feuillet = paste(Site,feuillet,sep = "_")) %>% 
  mutate(site_feuillet = str_sub(site_feuillet,start = 1, end = 31)) %>%  # # maximum length of excel worksheet names
  pull(site_feuillet)

list_site_feuillet <- DUPLcplet %>% 
  select(-c(verbatimOccurrenceID,verbatimOccurrenceID_sample,verbatimOccurrenceID_population)) %>% 
  group_split(Site,feuillet) %>% 
  setNames(names_feuillets) %>% 
  unique()

list_feuillet <- lapply(list_site_feuillet,regenerate_spreadsheet)

# list_site_feuillet[[1]] %>% 
#   View()
list_feuillet[[1]] %>%
  View()

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
openxlsx::saveWorkbook(wb = wb, file = "output/WorkingFiles/2024_02_16_duplicated_occurrenceID_format_excel.xlsx")


