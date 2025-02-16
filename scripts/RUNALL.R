library(tidyverse)

# This script runs all the scripts that process data from "native" format to the database

source("scripts/core_1_import_data.R")
source("scripts/core_2_generate_core.R")
source("scripts/core_3_add_MoFTraits_to_core.R")
source("scripts/core_4_add_plot_info_to_core.R")
source("scripts/core_5_add_occurrence_to_core.R")
source("scripts/core_6_DarwinCore_mapping.R")
