# Need to charge the TAXREF data frame

# pour TAXREF :
# ORDRE == GN : genre. Abesnt de taxref !
# ORDRE == SSES : sous-espèce. Présent dans taxref
# On n'a que les rangs en-dessous de l'espèce.

get_scientificName <- function(verbatim_name,taxref){
  valid_name <- taxref %>% 
    filter(LB_NOM == verbatim_name) %>%
    pull(NOM_VALIDE)
  valid_name[1]
}

get_taxon_id <- function(verbatim_name,taxref){
  taxon_id <- taxref %>% 
    filter(LB_NOM == verbatim_name) %>%
    pull(URL)
  taxon_id[1]
}

# TOP ####
get_info_TOP <- function(trait,columnWanted,top){
  TOP %>% 
    filter(verbatimTraitName == trait) %>% 
    pull(columnWanted)
}
