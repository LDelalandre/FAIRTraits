# Need to charge the TAXREF data frame

# pour TAXREF :
# ORDRE == GN : genre
# Modifier: dans TAXREF, on trouve Carex, mais pas Carex sp. !
# et il n'y a pas d'URL, mais CD_NOM si.
# ORDRE == SSES : sous-espèce. Présent dans taxref
# On n'a que les rangs en-dessous de l'espèce.

get_scientificName <- function(verbatim_name,taxref){
  valid_name <- taxref %>% 
    filter(LB_NOM == verbatim_name %>% 
             gsub(" sp.","",.)) %>%
    pull(NOM_VALIDE)
  valid_name[1]
}

# essai pour genre
# verbatim_name <- "Carex sp."
# get_scientificName(verbatim_name,taxref)

get_taxon_id <- function(verbatim_name,taxref){
  taxon_id <- taxref %>% 
    filter(LB_NOM == verbatim_name%>% 
             gsub(" sp.","",.)) %>%
    pull(URL)
  taxon_id[1]
}

get_CD_NOM <- function(verbatim_name,taxref){
  cd_nom <- taxref %>% 
    filter(LB_NOM == verbatim_name%>% 
             gsub(" sp.","",.)) %>%
    pull(CD_NOM)
  cd_nom[1]
}

# TOP ####
get_info_TOP <- function(trait,columnWanted,top){
  TOP %>% 
    filter(verbatimTraitName == trait) %>% 
    pull(columnWanted)
}
