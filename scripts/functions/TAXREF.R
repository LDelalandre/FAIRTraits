# Need to charge the TAXREF data frame

# pour TAXREF :
# ORDRE == GN : genre
# Modifier: dans TAXREF, on trouve Carex, mais pas Carex sp. !
# et il n'y a pas d'URL, mais CD_NOM si.
# ORDRE == SSES : sous-espèce. Présent dans taxref
# On n'a que les rangs en-dessous de l'espèce.


info_taxref <- function(verbatim_name,finfo){
  # finfo can be either NOM_VALIDE, URL, CD_NOM, FAMILLE...
  valid_name <- taxref %>% 
    filter(LB_NOM == verbatim_name %>% 
             gsub(" sp\\.","",.)) %>%
    pull(finfo)
  valid_name[1]
}


get_scientificName <- function(verbatim_name,taxref){
  valid_name <- taxref %>% 
    filter(LB_NOM == verbatim_name %>% 
             gsub(" sp\\.","",.) ) %>%
    pull(NOM_VALIDE)
  valid_name[1]
}

# essai pour genre
# verbatim_name <- "Carex sp."
# get_scientificName("Kickxia spuria",taxref)
# taxref %>% filter(LB_NOM == "Kickxia spuria")

get_URL <- function(verbatim_name,taxref){
  URL <- taxref %>% 
    filter(LB_NOM == verbatim_name%>% 
             gsub(" sp\\.","",.)) %>%
    pull(URL)
  URL[1]
}

get_CD_NOM <- function(verbatim_name,taxref){
  cd_nom <- taxref %>% 
    filter(LB_NOM == verbatim_name%>% 
             gsub(" sp\\.","",.)) %>%
    pull(CD_NOM)
  cd_nom[1]
}