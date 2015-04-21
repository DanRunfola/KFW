library(devtools)
devtools::install_github("itpir/SAT@Alpha")
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

dta_Shp@data["NDVI_10_95_Percent"] <- dta_Shp@data["NDVI2010"] / dta_Shp@data["NDVI1995"]
dta_Shp@data["NDVI_95_82_Percent"] <- dta_Shp@data["NDVI1995"] / dta_Shp@data["NDVI1982"]

#Calculate average temperature and precip for pre- and post- periods
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
 #dta_Shp@data$TrtBin[dta_Shp@data$stagenum == 6] <- 1
 #dta_Shp@data$TrtBin[dta_Shp@data$stagenum == 7] <- 1
 #dta_Shp@data$TrtBin[dta_Shp@data$stagenum == 8] <- 1

dta_Shp@data$TrtBin[dta_Shp@data$demend_y <= 2001] <- 1

dta_Shp@data$NA_check <- 0

dta_Shp@data$NA_check[is.na(dta_Shp@data$demend_y)] <- 1

int_Shp <- dta_Shp[dta_Shp@data$NA_check != 1,]

dta_Shp <- int_Shp

analyticModel <- "NDVI_10_95 ~ TrtBin + terrai_are + Pop_1995 + meanT_95_82 + 
meanP_95_82 + meanT_10_95 + meanP_10_95 + Slope + Elevation + NDVI_95_82 + UrbTravTim + NDVI1995 + factor(PSM_match_ID)"

#Levels equation - Raw NDVI 2010 as outcome
psmModel <- "TrtBin ~ terrai_are + Pop_1990 + Pop_2000 + meanT_95_82 + MeanT_1995 + meanP_95_82 + MeanP_1995 +
meanT_10_95 + MeanT_2010 + MeanP_2010 + meanP_10_95 + UrbTravTim + Slope + Elevation + NDVI1995 + NDVI_95_82"
analyticModel <- "NDVI2010 ~ TrtBin + terrai_are + Pop_1990 + Pop_2000 + meanT_95_82 + MeanT_1995 + meanP_95_82 + MeanP_1995 +
meanT_10_95 + MeanT_2010 + MeanP_2010 + meanP_10_95 + UrbTravTim + Slope + Elevation + NDVI1995 + NDVI_95_82 + factor(UF)"

#Trend equation - NDVI 2010 as a percentage of NDVI 1995 as outcome.
psmModel <- "TrtBin ~ terrai_are + Pop_1990 + Pop_2000 + meanT_95_82 + MeanT_1995 + meanP_95_82 + MeanP_1995 +
meanT_10_95 + MeanT_2010 + MeanP_2010 + meanP_10_95 + UrbTravTim + Slope + Elevation + NDVI1995 + NDVI_95_82_Percent"
analyticModel <- "NDVI_10_95_Percent ~ TrtBin + terrai_are + Pop_1990 + Pop_2000 + meanT_95_82 + MeanT_1995 + meanP_95_82 + MeanP_1995 +
meanT_10_95 + MeanT_2010 + MeanP_2010 + meanP_10_95 + UrbTravTim + Slope + Elevation + NDVI1995 + NDVI_95_82_Percent + factor(UF)"

#Trend equations



psmRes <- SAT::SpatialCausalPSM(dta_Shp,mtd="lm",psmModel,drop="overlap",visual=FALSE)

#Add in records for PFE
drop_set<- c(drop_unmatched=TRUE,drop_method="None",drop_thresh=0.25)
psm_Pairs <- SAT::SpatialCausalDist_Binary(dta = psmRes, mtd = "fastNN",constraints=NULL,psm_eq = psmModel, ids = "id", drop_opts = drop_set, visual="TRUE", TrtBinColName="TrtBin")
#constraints option: c(groups="UF")
m_fit <- lm(analyticModel,psm_Pairs)
summary(m_fit)
lm.beta(m_fit)

#writePolyShape(psm_Pairs,'/home/aiddata/Desktop/quickPairs.shp')
