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
dta_Shp@data["NDVI_95_82"] <- dta_Shp@data["NDVI1995"] - dta_Shp@data["NDVI1982"] 
dta_Shp@data["NDVI_10_95"] <- dta_Shp@data["NDVI2010"] - dta_Shp@data["NDVI1995"]

#Calculate average temperature and precip for pre- and post- periods

timeRangeAvg <- function(dta,prefix,startyr,endyr)
{
  searchS = paste("^",prefix,startyr,sep="")
  searchE = paste("^",prefix,endyr,sep="")
  strt_id <- grep(searchS,colnames(dta))
  end_id <- grep(searchE,colnames(dta))
  rmean <- rowMeans(dta[strt_id:end_id])
  return(rmean)
}

dta_Shp@data["meanT_95_82"] <- timeRangeAvg(dta_Shp@data,"MeanT_",1982,1995)
dta_Shp@data["maxT_95_82"] <- timeRangeAvg(dta_Shp@data,"MaxT_",1982,1995)
dta_Shp@data["minT_95_82"] <- timeRangeAvg(dta_Shp@data,"MinT_",1982,1995)

dta_Shp@data["meanP_95_82"] <- timeRangeAvg(dta_Shp@data,"MeanP_",1982,1995)
dta_Shp@data["maxP_95_82"] <- timeRangeAvg(dta_Shp@data,"MaxP_",1982,1995)
dta_Shp@data["minP_95_82"] <- timeRangeAvg(dta_Shp@data,"MinP_",1982,1995)

dta_Shp@data["meanT_10_95"] <- timeRangeAvg(dta_Shp@data,"MeanT_",1995,2010)
dta_Shp@data["maxT_10_95"] <- timeRangeAvg(dta_Shp@data,"MaxT_",1995,2010)
dta_Shp@data["minT_10_95"] <- timeRangeAvg(dta_Shp@data,"MinT_",1995,2010)

dta_Shp@data["meanP_10_95"] <- timeRangeAvg(dta_Shp@data,"MeanP_",1995,2010)
dta_Shp@data["maxP_10_95"] <- timeRangeAvg(dta_Shp@data,"MaxP_",1995,2010)
dta_Shp@data["minP_10_95"] <- timeRangeAvg(dta_Shp@data,"MinP_",1995,2010)

#Make a binary to test treatment..
dta_Shp@data["TrtBin"] <- 0
dta_Shp@data$TrtBin[dta_Shp@data$stagenum == 6] <- 1
dta_Shp@data$TrtBin[dta_Shp@data$stagenum == 7] <- 1
dta_Shp@data$TrtBin[dta_Shp@data$stagenum == 8] <- 1

#Fix size..
dta_Shp@data["terrai_are"] <- lapply(dta_Shp@data["terrai_are"], function(x) as.numeric(gsub("Ha","",x)))

#Define the first stage PSM model
psmModel <- "TrtBin ~ terrai_are + factor(UF) + Pop_1990 + Pop_2000 + meanT_95_82 + 
meanP_95_82 + meanT_10_95 + meanP_10_95 + Slope + Elevation + NDVI_95_82 + UrbTravTim"

#Define the second-stage model
analyticModel <- "NDVI2010 ~ TrtBin + terrai_are + factor(UF) + Pop_1990 + Pop_2000 + meanT_95_82 + 
meanP_95_82 + meanT_10_95 + meanP_10_95 + Slope + Elevation + NDVI_95_82 + UrbTravTim"

psmRes <- SAT::SpatialCausalPSM(dta_Shp,mtd="logit",psmModel,drop="overlap",visual=TRUE)

psm_Pairs <- SAT::SpatialCausalDist(dta = psmRes, mtd = "fastNN", vars = psmModel, ids = "id", drop_unmatched = TRUE, drop_method = "SD", drop_thresh=0.25, visual="TRUE")

summary(lm(analyticModel,psm_Pairs))
