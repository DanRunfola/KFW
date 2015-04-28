library(devtools)
devtools::install_github("itpir/SAT@alpha3")
library(SAT)
library(RColorBrewer)

#ttest

#load SAT libraries
loadLibs()

#View all functions
lsf.str("package:SAT")

shpfile = "Processed_Data/Matched_Indigenous_Lands_DemResults.shp"
dta_Shp = readShapePoly(shpfile)

#Drop out a few units
#These units are being dropped due to issues with the historic data
#Specifically, effects of large water bodies and large areas of deforestation (pre-existing)
dta_Shp <- dta_Shp[dta_Shp@data$SP_ID != 62,]
dta_Shp <- dta_Shp[dta_Shp@data$SP_ID != 127,]
dta_Shp <- dta_Shp[dta_Shp@data$SP_ID != 92,]
dta_Shp <- dta_Shp[dta_Shp@data$SP_ID != 16,]
dta_Shp <- dta_Shp[dta_Shp@data$SP_ID != 143,]
dta_Shp <- dta_Shp[dta_Shp@data$SP_ID != 69,]
dta_Shp <- dta_Shp[dta_Shp@data$SP_ID != 139,]

#Calculate NDVI Trends
dta_Shp$pre_trend <- timeRangeTrend(dta_Shp,"NDVI[0-9][0-9][0-9][0-9]",1982,1995,"SP_ID")
dta_Shp$post_trend <- timeRangeTrend(dta_Shp,"NDVI[0-9][0-9][0-9][0-9]",2001,2010,"SP_ID")
dta_Shp@data["NDVIslopeChange"] <- dta_Shp@data["post_trend"] - dta_Shp@data["pre_trend"]

dta_Shp@data["NDVI_14_94_Percent"] <- dta_Shp@data["NDVI2014"] / dta_Shp@data["NDVI1994"]
dta_Shp@data["NDVI_94_82_Percent"] <- dta_Shp@data["NDVI1994"] / dta_Shp@data["NDVI1982"]

#Calculate average temperature and precip for pre- and post- periods
dta_Shp@data["meanT_94_82"] <- timeRangeAvg(dta_Shp@data,"MeanT_",1982,1994)
dta_Shp@data["maxT_94_82"] <- timeRangeAvg(dta_Shp@data,"MaxT_",1982,1994)
dta_Shp@data["minT_94_82"] <- timeRangeAvg(dta_Shp@data,"MinT_",1982,1994)

dta_Shp@data["meanP_94_82"] <- timeRangeAvg(dta_Shp@data,"MeanP_",1982,1994)
dta_Shp@data["maxP_94_82"] <- timeRangeAvg(dta_Shp@data,"MaxP_",1982,1994)
dta_Shp@data["minP_94_82"] <- timeRangeAvg(dta_Shp@data,"MinP_",1982,1994)

dta_Shp@data["meanT_10_94"] <- timeRangeAvg(dta_Shp@data,"MeanT_",1994,2010)
dta_Shp@data["maxT_10_94"] <- timeRangeAvg(dta_Shp@data,"MaxT_",1994,2010)
dta_Shp@data["minT_10_94"] <- timeRangeAvg(dta_Shp@data,"MinT_",1994,2010)

dta_Shp@data["meanP_10_94"] <- timeRangeAvg(dta_Shp@data,"MeanP_",1994,2010)
dta_Shp@data["maxP_10_94"] <- timeRangeAvg(dta_Shp@data,"MaxP_",1994,2010)
dta_Shp@data["minP_10_94"] <- timeRangeAvg(dta_Shp@data,"MinP_",1994,2010)

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




psmModel <- "TrtBin ~ terrai_are + Pop_1990 + MeanT_1995 + MeanP_1995 + pre_trend +
Slope + Elevation +  NDVI1995 + Riv_Dist + Road_dist"
analyticModel <- "NDVIslopeChange ~ TrtBin + terrai_are + Pop_1990 + Pop_2000 + MeanT_1995 + MeanP_1995 + pre_trend +
MeanT_2010 + MeanP_2010 + Slope + Elevation + factor(PSM_match_ID) + NDVI1995 + Riv_Dist + Road_dist + factor(UF)"


psmRes <- SAT::SpatialCausalPSM(dta_Shp,mtd="logit",psmModel,drop="overlap",visual=TRUE)

#Add in records for PFE
drop_set<- c(drop_unmatched=TRUE,drop_method="None",drop_thresh=0.25)
psm_Pairs <- SAT::SpatialCausalDist_Binary(dta = psmRes, mtd = "fastNN",constraints=c(groups="UF"),psm_eq = psmModel, ids = "id", drop_opts = drop_set, visual="TRUE", TrtBinColName="TrtBin")
#
m_fit <- lm(analyticModel,psm_Pairs)
summary(m_fit)
texreg::plotreg(m_fit,omit.coef="(match)|(Intercept)",custom.model.names="Unstandardized Model")

#Scale all of the data to get standardized coefficients...
psm_PairsB <- psm_Pairs
ind <- sapply(psm_PairsB@data, is.numeric)
psm_PairsB@data[ind] <- lapply(psm_PairsB@data[ind],scale)
m_fit <- lm(analyticModel,psm_PairsB)
summary(m_fit)
texreg::plotreg(m_fit,omit.coef="(match)|(Intercept)",custom.model.names="Standardized Model")

SAT::ViewTimeSeries(dta=psm_Pairs,IDfield="reu_id",TrtField="TrtBin",idPre="NDVI[0-9][0-9][0-9][0-9]")
SAT::ViewTimeSeries(dta=psm_Pairs,IDfield="reu_id",TrtField="TrtBin",idPre="MeanP_19[8-9][0-9]|MeanP_20[0-9][0-9]")
SAT::ViewTimeSeries(dta=psm_Pairs,IDfield="reu_id",TrtField="TrtBin",idPre="MeanT_19[8-9][0-9]|MeanT_20[0-9][0-9]")
