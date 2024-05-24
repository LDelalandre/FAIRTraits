library(tidyverse)

# This script generates occurrenceIDs, defined at three levels: 
# the level of data record (verbatimOccurrenceID) ; 
# the level of the replicate on which a specific data record was taken (verbatimOccurrenceId_sample);
# the level of the population (species x site x plot x treatment x date) in which the measured individual was sampled.

TIDY_plot <- data.table::fread("output/TIDY_plot.csv",encoding="UTF-8")

# Generate OccurrenceIDs ####
TIDY_occurrenceID <- TIDY_plot %>%
  mutate(verbatimOccurrenceID = paste(Code_Sp,Site,traitPlot,Treatment,Year,Month,Day,Rep,verbatimTraitName,sep = "_")) %>% 
  mutate(verbatimOccurrenceID_sample = paste(Code_Sp,Site,traitPlot,Treatment,Year,Month,Day,Rep,sep = "_")) %>% 
  mutate(verbatimOccurrenceID_population = paste(Code_Sp,Site,traitPlot,Treatment,Year,sep = "_"))

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




