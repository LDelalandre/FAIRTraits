# This function takes as input a TIDY version of the data
# whose columns names are :
# Site Block Plot Treatment Year Month Day Species traitEntity Rep nameOfProject measurementDeterminedBy 
# feuillet verbatimTraitName verbatimTraitvalue

# N.B. REMOVE verbatimOccurrenceID verbatimOccurrenceID_echantillon verbatimOccurrenceID_population before

# minimum column names verbatimTraitName and verbatimTraitValue

regenerate_feuillet <- function(tidy){
  feuillet <- tidy %>% 
    group_by(verbatimTraitName) %>% 
    mutate(ind = row_number()) %>% # if duplicated rows
    spread(key = "verbatimTraitName", value = "verbatimTraitValue")
  feuillet
}