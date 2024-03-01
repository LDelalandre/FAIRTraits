# The FAIRTrait database

This repository contains the code used for the article "FAIRTraits: a semantically enriched data base of plant traits from Mediterranean populations of 242 species".

You can clone the repository with git by pasting in your terminal:

	git clone https://src.koda.cnrs.fr/cefe/fairtraits.git
    
or 
just download the repository:
[FAIRTraits](https://src.koda.cnrs.fr/cefe/fairtraits/archive/master.zip).

If you have [Rstudio](https://www.rstudio.com/) installed on your computer, you can then open `FAIRTraits.Rproj` with Rstudio.

You can contact me at  <leo.delalandre@protonmail.com>

# Structure of the project repository

## data
Contains raw data (in excel and csv format). If you are from CNRS and have the required access, raw data can be dowloaded with your Janus identifier from [MyCore](https://mycore.core-cloud.net/index.php/apps/files/?dir=/DataPaper_ECOPAR/data/raw&fileid=3059273178). They correspond to "native" data, i.e. formatted as used in research projects for which they were acquired.

## output
Contains the processed data.

## scripts

| **Name of script**                 | **Actions performed**                                                                                                                                                                                                                                                                                                                                             |
| ---------------------------------- | ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| core_1_import_data.R               | This script imports raw trait data from multiple excel files and writes a single csv file with one row corresponding to one record (i.e., one trait measured on one individual at one time)                                                                                                                                                                       |
| core_2_generate_core.R             | This script imports the previously generated csv file (containing one record per row), updates the values taken in some columns (Plot, Treatment), and corrects typos                                                                                                                                                                                             |
| core_3_add_MoFTraits_to_core.R     | This script updates trait names and adds information on trait measurement and sampling method to the core of the database, using an intermediate file constructed manually. This file is called MoFTraits, MoF being the abbreviation of Measurement or Fact, which is defined in the Ecological Trait-data Standard (Schneider et al., 2019).[[LD1]](#_msocom_1) |
| core_4_add_plot_info_to_core.R     | This script adds information on altitude, longitude, and latitude, to the core of the database. It uses manually-constructed files containing environmental information, and mapping information from native names of plots, treatments, and sites, to the names used in the present database.                                                                    |
| core_5_add_occurrence_to_core.R    | This script generates occurrenceIDs, defined at three levels: the level of data record (verbatimOccurrenceID); the level of the replicate on which a specific data record was taken (verbatimOccurrenceId_sample); the level of the population (species x site x plot x treatment x date) in which the measured individual was sampled.                           |
| core_6_DarwinCore_mapping.R        | This script changes column names of the database to DarwinCore names, as described in Table 4, section IV.B.1.                                                                                                                                                                                                                                                    |
| quality_check_non_numeric_fields.R | Quality control of non-numeric fields. Quality control for numeric fields (i.e., trait data) is presented in section V.B. This script verifies the class of each column, identifies empty fields, and checks the correspondence between multiple fields.                                                                                                          |
| taxon.R                            | This script imports data on taxon names, with information manually extracted from a Mediterranean flora (Tison et al., 2014). It then adds information from the TaxRef referential, version 16 (see section IV.B.3), namely scientific name, identifiant number and uniform resource identifyer of the species.                                                   |
