# This function takes as input a TIDY version of the data
# whose columns names are :
# Site Block Plot Treatment Year Month Day Species traitEntity Rep nameOfProject measurementDeterminedBy 
# feuillet verbatimTraitName verbatimTraitvalue

# N.B. REMOVE verbatimOccurrenceID verbatimOccurrenceID_echantillon verbatimOccurrenceID_population before

# minimum column names : verbatimTraitName and verbatimTraitValue

regenerate_spreadsheet <- function(tidy){
  columns_other_than_traits <- 
    c("Site", "Block" ,"Plot", "Treatment", "Year" ,  "Month"  ,   "Day",  "Species", "Code_Sp"       ,    "Family" ,
      "LifeForm1" ,"LifeForm2" , 
      "traitEntity",    "Rep",
      "nameOfProject" ,"measurementDeterminedBy",
      "feuillet" # a supprimer à la fin, juste pour la traçabilité
      #"Published in" # for MLoss_Roots
    )
  
  spreadsheet <- tidy  %>% 
    select(any_of(columns_other_than_traits),verbatimTraitName,verbatimTraitValue) %>% 
    group_by(verbatimTraitName) %>% 
    mutate(ind = row_number()) %>% # if duplicated rows
    spread(key = "verbatimTraitName", value = "verbatimTraitValue") %>% 
    select(any_of(columns_other_than_traits),everything())
}
