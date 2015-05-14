library(devtools)
devtools::install_github("itpir/SAT@spatialBranch3")
library(SAT)
library(RColorBrewer)

#load SAT libraries
loadLibs()

shpfile = "Processed_Data/Matched_Indigenous_Lands_DemResults.shp"
dta_Shp = readShapePoly(shpfile)

#Drop out a few units
#These units are being dropped due to issues with the historic data
#Specifically, effects of large water bodies and large areas of deforestation (pre-existing)
drop_rows <- c(62,127,92,16,143,69,139)
dta_Shp <- removeRow(dta=dta_Shp,columnID="SP_ID",matchIDs=drop_rows)

#Calculate NDVI Trends
dta_Shp$pre_trend_NDVI <- timeRangeTrend(dta_Shp,"NDVI[0-9][0-9][0-9][0-9]",1982,1995,"SP_ID")
#NDVI Trends for 1995-2001
dta_Shp$post_trend_NDVI_01 <- timeRangeTrend(dta_Shp,"NDVI[0-9][0-9][0-9][0-9]",1995,2001,"SP_ID")
dta_Shp@data["NDVIslopeChange_01"] <- dta_Shp@data["post_trend_NDVI_01"] - dta_Shp@data["pre_trend_NDVI"]
#NDVI Trends for 2001-2010
dta_Shp$post_trend_NDVI_10 <- timeRangeTrend(dta_Shp,"NDVI[0-9][0-9][0-9][0-9]",2001,2010,"SP_ID")
dta_Shp@data["NDVIslopeChange_10"] <- dta_Shp@data["post_trend_NDVI_10"] - dta_Shp@data["pre_trend_NDVI"]

dta_Shp@data["NDVI_14_94_Percent"] <- dta_Shp@data["NDVI2014"] / dta_Shp@data["NDVI1994"]
dta_Shp@data["NDVI_94_82_Percent"] <- dta_Shp@data["NDVI1994"] / dta_Shp@data["NDVI1982"]

#Calculate Temp and Precip Pre and Post Trends
dta_Shp$pre_trend_temp <- timeRangeTrend(dta_Shp,"MeaT_[0-9][0-9][0-9][0-9]",1982,1995,"SP_ID")
dta_Shp$post_trend_temp_01 <- timeRangeTrend(dta_Shp,"MeanT_[0-9][0-9][0-9][0-9]",1995,2001,"SP_ID")
dta_Shp$post_trend_temp_10 <- timeRangeTrend(dta_Shp,"MeanT_[0-9][0-9][0-9][0-9]",2001,2010,"SP_ID")

dta_Shp$pre_trend_precip <- timeRangeTrend(dta_Shp,"MeanP_[0-9][0-9][0-9][0-9]",1982,1995,"SP_ID")
dta_Shp$post_trend_precip_01 <- timeRangeTrend(dta_Shp,"MeanP_[0-9][0-9][0-9][0-9]",1995,2001,"SP_ID")
dta_Shp$post_trend_precip_10 <- timeRangeTrend(dta_Shp,"MeanP_[0-9][0-9][0-9][0-9]",2001,2010,"SP_ID")

dta_Shp$pre_avg_temp <- timeRangeAvg(dta_Shp@data,"MeanT_",1982,1995)
dta_Shp$post_avg_temp_01 <- timeRangeAvg(dta_Shp@data,"MeanT_",1995,2001)
dta_Shp$post_avg_pre_01 <- timeRangeAvg(dta_Shp@data,"MeanP_",1995,2001)


#Make a binary to test treatment..
dta_Shp@data["TrtBin"] <- 0
dta_Shp@data$TrtBin[dta_Shp@data$demend_y <= 2001] <- 1
dta_Shp@data$TrtBin[(dta_Shp@data$demend_m > 4) & (dta_Shp@data$demend_y==2001)] <- 0

dta_Shp@data$NA_check <- 0
dta_Shp@data$NA_check[is.na(dta_Shp@data$demend_y)] <- 1
int_Shp <- dta_Shp[dta_Shp@data$NA_check != 1,]
dta_Shp <- int_Shp

psmModel <- "TrtBin ~ terrai_are + Pop_1990 + MeanT_1995 + pre_trend_temp + MeanP_1995 + pre_trend_precip + 
pre_trend_NDVI + Slope + Elevation +  NDVI1995 + Riv_Dist + Road_dist"

psmRes <- SAT::SpatialCausalPSM(dta_Shp,mtd="logit",psmModel,drop="None",visual=TRUE)

pre_Moran <- SAT::PSMdistDecay(dta=psmRes,psm_col="PSM_trtProb",start=1,end=500,h=10)

drop_set<- c(drop_unmatched=TRUE,drop_method="SD",drop_thresh=0.25)
psm_Pairs <- SAT(dta = psmRes, mtd = "fastNN",constraints=c(groups=c("UF"),distance=250),psm_eq = psmModel, ids = "id", drop_opts = drop_set, visual="TRUE", TrtBinColName="TrtBin")
#c(groups=c("UF"),distance=NULL)
post_Moran <- SAT::PSMdistDecay(dta=psm_Pairs,psm_col="PSM_trtProb",start=1,end=500,h=10)

analyticModelLate <- "NDVIslopeChange_10 ~ TrtBin+ terrai_are + Pop_1990 + pre_trend_NDVI + MeanT_1995  + post_avg_temp_01 +
MeanP_1995 + post_avg_pre_01 + Slope + Elevation + factor(PSM_match_ID) + NDVI1995 + Riv_Dist + Road_dist + factor(UF)"

analyticModelEarly3 <- "NDVIslopeChange_01 ~ TrtBin+ terrai_are + Pop_1990 + pre_trend_NDVI + MeanT_1995  + post_avg_temp_01 +
MeanP_1995 + post_avg_pre_01 + Slope + Elevation + factor(PSM_match_ID) + NDVI1995 + Riv_Dist + Road_dist + factor(UF)"

test <- Stage2PSM(analyticModelEarly3,psm_Pairs,type="lm", table_out=TRUE)

analyticModelPanel<- "NDVIslopeChange_01 ~ TrtBin+ terrai_are + Pop_1990 + pre_trend_NDVI + MeanT_1995  + post_avg_temp_01 +
MeanP_1995 + post_avg_pre_01 + Slope + Elevation + factor(PSM_match_ID) + NDVI1995 + Riv_Dist + Road_dist + factor(UF)"

#Variables to include in the time series
varList = c("NDVI","MeanT_","MeanP_","TrtMnt","MaxT_","MaxP_","MinP_","MinT_")
psm_Long <- BuildTimeSeries(dta=dta_Shp,idField="reu_id",varList_pre=varList,1983,2010,colYears="regend_y",interpYears=c("Slope","Road_dist","Riv_Dist","UF","Elevation","terrai_are","Pop_"))

#Panel Models
pModelA <- "NDVI ~ TrtMnt + MeanT_ + MeanP_ + Pop_ + MaxT_ + MaxP_ + MinT_ + MinP_ + factor(reu_id)"

#pModelA <- "NDVI ~ TrtMnt + MeanT_ + MeanP_"


Stage2PSM(pModelA,psm_Long,type="cmreg", table_out=TRUE, opts=c("reu_id","Year"))

#SAT::ViewTimeSeries(dta=psm_Pairs,IDfield="reu_id",TrtField="TrtBin",idPre="NDVI[0-9][0-9][0-9][0-9]")
#stargazer(test["Standardized"],test["Unstandardized"], title="Regression Results", type="html",align=TRUE)

