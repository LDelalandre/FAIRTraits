library(tidyverse)

# This script changes column names of the database to DarwinCore names

# importer TIDY_plot, mais pour l'instant pas au point
core <-  data.table::fread("output/TIDY_6_ID_field_campaign.csv",encoding = "UTF-8") %>% 
  rename(site = Site) %>% 
  rename(traitEntity = traitEntityValid) %>% 
  rename(traitPlotLatitude = plotLatitude,traitPlotLongitude = plotLongitude, traitPlotAltitude = plotAltitude)

taxon <- data.table::fread("output/FAIRTraits_Taxon.csv",encoding = "UTF-8")

mapping_occurrence <- readxl::read_excel("data/Mapping&Ordre_DwCArchives.xlsx", sheet = "Mapping_Occurence")
mapping_taxa <- readxl::read_excel("data/Mapping&Ordre_DwCArchives.xlsx", sheet = "Mapping_Taxa")

# Occurrence ####

A <- colnames(core)
B <- mapping_occurrence %>% pull(Colonne)

setdiff(A,B)
setdiff(B,A)

core_InDoRES <- core %>% 
  select(all_of(mapping_occurrence$Colonne))

drop <- mapping_occurrence %>% 
  filter(is.na(`New DwC Term`)) %>% 
  pull(Colonne)

mapping_occurrence_GBIF <- mapping_occurrence %>% 
  dplyr::filter(!(Colonne == "traitQuality")) %>% 
  dplyr::mutate(Colonne = case_when(Colonne == "traitEntity" ~ "traitEntity_traitQuality", 
                                    TRUE ~ Colonne)) %>% 
  filter(!is.na(`New DwC Term`)) 
  # filter(Colonne %in% colnames(core_for_GBIF))


core_for_GBIF <- core_InDoRES %>% 
  mutate(traitEntity_traitQuality = paste(traitEntity,traitQuality,sep="_")) %>% 
  dplyr::select(-c("traitEntity","traitQuality")) %>% 
  select(-which(names(.) %in% drop)) %>% 
  select(all_of(mapping_occurrence_GBIF$Colonne))

old <- colnames(core_for_GBIF)
new <- mapping_occurrence_GBIF$`New DwC Term`

core_GBIF <- core_for_GBIF %>% rename_with(~ new, all_of(old))



# # Taxa ####
# A <- colnames(taxon)
# B <- mapping_taxa %>% pull(Colonne)
# 
# setdiff(A,B)
# setdiff(B,A)
# 
# taxon_InDoRES <- taxon %>% select(all_of(mapping_taxa$Colonne))
# 
# mapping_taxa$Colonne[11] <- "NaturalHistoryUnstructured"
# mapping_taxa <- mapping_taxa %>% 
#   filter(!(Colonne == "Height2"))
# taxon_GBIF <- taxon %>% select(all_of(mapping_taxa$Colonne))
# colnames(taxon_GBIF) <- mapping_taxa$`DwC Term`


# Export ####
data.table::fwrite(core_InDoRES ,"output/InDoRES_occurrence.csv",sep="\t")
data.table::fwrite(core_GBIF ,"output/GBIF_occurrence.csv",sep="\t")
# data.table::fwrite(taxon_InDoRES ,"output/InDoRES_taxa.csv",sep="\t")
# data.table::fwrite(taxon_GBIF ,"output/GBIF_taxa.csv",sep="\t")

#_____________

tests <- F


if (tests == T){
  mapping_core <- readxl::read_excel("data/FAIRTraits_MappingGBIF.xlsx", sheet = "Core")
  mapping_taxon <- readxl::read_excel("data/FAIRTraits_MappingGBIF.xlsx", sheet = "Taxon")
  
  column_order_indores <- readxl::read_excel("data/OrdreColonnes_TraitValues.xlsx", sheet = "Feuil1") %>% 
    select(Colonne,Ordre,Status)
  
  
  
  # Update taxon in core ####
  intersect(colnames(core), colnames(taxon))
  
  core_upd_taxon <- core %>% 
    select(-c("CodeSp"  , "Family" ,   "LifeForm1", "LifeForm2")) %>% 
    merge(taxon %>% select("SpeciesName","CodeSp"  , "Family" ,   "LifeForm1", "LifeForm2"))
  
  A<-taxon %>% pull(SpeciesName)
  B<-core %>% pull(SpeciesName) %>% unique()
  setdiff(A,B) # in taxon, but not in core (not a problem?)
  setdiff(B,A) # in core, but not in taxon (generates a loss of rows in core)
  
  # update names of columns in the files ####
  core_upd_nm <- core_upd_taxon %>% 
    # rename(traitEntity = traitEntityValid) %>% 
    # rename(traitPlotLatitude = plotLatitude) %>% 
    # rename(traitPlotLongitude = plotLongitude) %>% 
    # rename(traitPlotAltitude = plotAltitude) 
    
    
    
    
    # Core ####
  
  # Are columns of the core described in the mapping file?
  setdiff(mapping_core$verbatimVariableName , colnames(core) )
  setdiff(colnames(core) , mapping_core$verbatimVariableName )
  
  
  ## InDoRES ####
  # col_indores <- mapping_core %>% 
  #   filter(Status_InDoRES == "keep") %>% 
  #   arrange(orderInFinalOccurenceFile) %>% 
  #   pull(verbatimVariableName)
  
  col_indores <- column_order_indores %>% 
    filter(Status == "keep") %>% 
    arrange(Ordre) %>%
    pull(Colonne)
  
  core_indores <- core_upd_nm %>% 
    select(any_of(col_indores)) 
  
  setdiff(col_indores , colnames(core_indores)) # identiques
  
  ## GBIF ####
  ### Manually ####
  core_GBIF1 <- core_indores %>% 
    mutate(measurementType = paste(traitEntity,traitQuality,sep="_")) %>% 
    mutate(measurementID = paste(termSource, localIdentifier,sep="_"))
  
  ### With mapping file ####
  
  col_GBIF_names <- mapping_core %>% 
    filter(Status_GBIF == "keep") %>% 
    filter(!(verbatimVariableName %in% c("traitEntityValid","traitQuality","termSource","locaIdentifyer"))) %>% 
    arrange(as.numeric(orderInFinalOccurenceFile)) %>% 
    select(verbatimVariableName,variableNameStandard)
  
  setdiff(col_GBIF_names$verbatimVariableName,colnames(core_GBIF1))
  setdiff(colnames(core_GBIF1),col_GBIF_names$verbatimVariableName) # OK, columns that should not be kept in GBIF
  
  core_GBIF2 <- core_GBIF1 %>% 
    select(any_of(col_GBIF_names$verbatimVariableName),measurementType,measurementID)
  
  colnames(core_GBIF2) <- c(col_GBIF_names$variableNameStandard,"measurementType","measurementID")
  
  core_GBIF2
  
  # Taxon ####
  
  # Are columns of the taxon file described in the mapping file?
  setdiff(mapping_taxon$verbatimVariableName , colnames(taxon) )
  setdiff(colnames(taxon) , mapping_taxon$verbatimVariableName )
  
  ## GBIF ####
  var_taxon_GBIF <- mapping_taxon %>% 
    filter(Status_GBIF == "keep") %>% 
    arrange(orderInFinalTaxonFile) %>% 
    select(verbatimVariableName,variableNameStandard)
  
  taxon_GBIF <- taxon %>% 
    select(all_of(var_taxon_GBIF$verbatimVariableName))
  
  colnames(taxon_GBIF) <- var_taxon_GBIF$variableNameStandard
  
  
  
  
  
  # Export ####
  data.table::fwrite(core_upd_nm,"output/core_raw.csv",sep="\t")
  
  data.table::fwrite(core_indores,"output/core_InDoRES.csv",sep="\t")
  
  data.table::fwrite(core_GBIF2,"output/core_GBIF.csv",sep="\t")
  
  data.table::fwrite(taxon_GBIF,"output/taxon_GBIF.csv",sep="\t")
}
