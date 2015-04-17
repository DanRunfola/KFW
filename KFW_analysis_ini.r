library(devtools)
devtools::install_github("itpir/SAT")
library(SAT)

#load SAT libraries
loadLibs()

#View all functions
lsf.str("package:SAT")

shpfile = "Processed_Data/Matched_Indigenous_Lands_DemResults.shp"
dta_Shp = readShapePoly(shpfile)

#Calculate NDVI pre-trend
dta_Shp@data["NDVI_95_92"] <- 