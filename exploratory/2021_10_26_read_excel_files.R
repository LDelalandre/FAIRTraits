# library("xlsx")
# test <- read.xlsx2("data/La Fage/LaFage_PlantTraits.xls",sheetName = "LeafMorpho_traits",colClasses=NA)
# # /!\ NA are replaced by NaN within numeric columns. Il faudra que je les retire dans le fichier final 
# test_karim <- read.xlsx2("data/La Fage/LaFage_PlantTraits_Karim.xlsx",sheetName = "LeafMorpho_traits",colClasses=NA)
# 
# 
# 
# 
# test2 <- read.xlsx2( "data/LaFage_PlantTraitsDP_vp.xlsx" ,sheetName = "LeafMorpho_traits",colClasses=NA)

# LE MIEUX:
library("openxlsx")
mydf <- read.xlsx("data/LaFage_PlantTraitsDP_vp.xlsx", sheet = "LeafMorpho_traits", startRow = 1, colNames = TRUE)




# library(readxl) # c'est la cata, tout plante !
# read_x("data/La Fage/LaFage_PlantTraits_Karim.xlsx")
