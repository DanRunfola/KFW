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
drop_rows <- c(94,84,137,143,115,135,112)
dta_Shp <- removeRow(dta=dta_Shp,columnID="reu_id",matchIDs=drop_rows)

#Calculate NDVI Trends
dta_Shp$pre_trend_NDVI <- timeRangeTrend(dta_Shp,"MeanL_[0-9][0-9][0-9][0-9]",1982,1995,"SP_ID")
dta_Shp$NDVIslope_95_10 <- timeRangeTrend(dta_Shp,"MeanL_[0-9][0-9][0-9][0-9]",1995,2010,"SP_ID")
dta_Shp$NDVIslopeChange <- dta_Shp$NDVIslope_95_10 - dta_Shp$pre_trend_NDVI

#Calculate Temp and Precip Pre and Post Trends
dta_Shp$pre_trend_temp_mean <- timeRangeTrend(dta_Shp,"MeanT_[0-9][0-9][0-9][0-9]",1982,1995,"SP_ID")
dta_Shp$pre_trend_temp_max <- timeRangeTrend(dta_Shp,"MaxT_[0-9][0-9][0-9][0-9]",1982,1995,"SP_ID")
dta_Shp$pre_trend_temp_min <- timeRangeTrend(dta_Shp,"MinT_[0-9][0-9][0-9][0-9]",1982,1995,"SP_ID")

dta_Shp$pre_trend_precip_mean <- timeRangeTrend(dta_Shp,"MeanP_[0-9][0-9][0-9][0-9]",1982,1995,"SP_ID")
dta_Shp$pre_trend_precip_max <- timeRangeTrend(dta_Shp,"MaxP_[0-9][0-9][0-9][0-9]",1982,1995,"SP_ID")
dta_Shp$pre_trend_precip_min <- timeRangeTrend(dta_Shp,"MinP_[0-9][0-9][0-9][0-9]",1982,1995,"SP_ID")

#Make a binary to test treatment..
dta_Shp@data["TrtBin"] <- 0
dta_Shp@data$TrtBin[dta_Shp@data$demend_y <= 2001] <- 1
dta_Shp@data$TrtBin[(dta_Shp@data$demend_m > 4) & (dta_Shp@data$demend_y==2001)] <- 0

#Look at the full-set NDVI record for later comparison.
ViewTimeSeries(dta=dta_Shp,IDfield="reu_id",TrtField="TrtBin",idPre="NDVI[0-9][0-9][0-9][0-9]")

#Remove units that did not ever receive any treatment (within-sample test)
dta_Shp@data$NA_check <- 0
dta_Shp@data$NA_check[is.na(dta_Shp@data$demend_y)] <- 1
int_Shp <- dta_Shp[dta_Shp@data$NA_check != 1,]
dta_Shp <- int_Shp

#View the updated time series
ViewTimeSeries(dta=dta_Shp,IDfield="reu_id",TrtField="TrtBin",idPre="NDVI[0-9][0-9][0-9][0-9]")

psmModel <- "TrtBin ~ terrai_are + Pop_1990 + pre_trend_precip_mean + pre_trend_precip_max + 
pre_trend_precip_min + pre_trend_temp_mean + pre_trend_temp_max + pre_trend_temp_min +
pre_trend_NDVI + Slope + Elevation +  MeanP_1995 + MeanT_1995 + MaxN_1995 + MeanN_1995 + Riv_Dist + Road_dist"

psmRes <- SAT::SpatialCausalPSM(dta_Shp,mtd="logit",psmModel,drop="support",visual=TRUE)

pre_Moran <- SAT::PSMdistDecay(dta=psmRes,psm_col="PSM_trtProb",start=1,end=500,h=10)

drop_set<- c(drop_unmatched=TRUE,drop_method="None",drop_thresh=0.25)
psm_Pairs <- SAT(dta = psmRes, mtd = "fastNN",constraints=NULL,psm_eq = psmModel, ids = "id", drop_opts = drop_set, visual="TRUE", TrtBinColName="TrtBin")
#c(groups=c("UF"),distance=NULL)
#post_Moran <- SAT::PSMdistDecay(dta=psm_Pairs,psm_col="PSM_trtProb",start=1,end=500,h=10)

# mTest <- "NDVIslopeChange ~ TrtBin + terrai_are + Pop_1990 + pre_trend_precip_mean + pre_trend_precip_max + 
# pre_trend_precip_min + pre_trend_temp_mean + pre_trend_temp_max + pre_trend_temp_min +
# pre_trend_NDVI + Slope + Elevation +  MeanP_1995 + MeanT_1995 + MeanL_1995 + MaxL_1995 + Riv_Dist + Road_dist"
# 
# Stage2PSM(mTest,psm_Pairs,type="lm", table_out=TRUE)

# 
# analyticModelLate <- "NDVIslopeChange_10 ~ TrtBin+ terrai_are + Pop_1990 + pre_trend_NDVI + MeanT_1995  + post_avg_temp_01 +
# MeanP_1995 + post_avg_pre_01 + Slope + Elevation + factor(PSM_match_ID) + NDVI1995 + Riv_Dist + Road_dist + factor(UF)"
# 
# analyticModelEarly3 <- "NDVIslopeChange_01 ~ TrtBin+ terrai_are + Pop_1990 + pre_trend_NDVI + MeanT  + post_avg_temp_01 +
# MeanP_1995 + post_avg_pre_01 + Slope + Elevation + factor(PSM_match_ID) + NDVI1995 + Riv_Dist + Road_dist + factor(UF)"
# 
# Model1_data <- psm_Pairs
# 
# analyticModelPanel<- "NDVIslopeChange_01 ~ TrtBin+ terrai_are + Pop_1990 + pre_trend_NDVI + MeanT_1995  + post_avg_temp_01 +
# MeanP_1995 + post_avg_pre_01 + Slope + Elevation + factor(PSM_match_ID) + NDVI1995 + Riv_Dist + Road_dist + factor(UF)"
# 
# #Variables to include in the time series
varList = c("MeanN_","MaxN_","MeanL_","MaxL_")
psm_Long <- BuildTimeSeries(dta=psm_Pairs,idField="reu_id",varList_pre=varList,1982,2010,colYears="demend_y",interpYears=c("Slope","Road_dist","Riv_Dist","UF","Elevation","terrai_are","Pop_","MeanT_","MeanP_","TrtMnt","MaxT_","MaxP_","MinP_","MinT_"))


#Panel Models
#psm_Long$TrtPair <- as.numeric(psm_Long$PSM_match_ID) * psm_Long$TrtMnt
pModelA <- "MeanL_ ~ TrtMnt + MeanT_ + MeanP_ + Pop_ + MaxT_ + MaxP_ + MinT_ + MinP_  + factor(reu_id)"

Stage2PSM(pModelA,psm_Long,type="cmreg", table_out=TRUE, opts=c("reu_id","Year"))

ViewTimeSeries(dta=dta_Shp,IDfield="reu_id",TrtField="TrtBin",idPre="MeanL_[0-9][0-9][0-9][0-9]")



#Stage2PSM(pModelA,psm_Long,type="cmreg", table_out=TRUE, opts=c("reu_id","Year"))

#SAT::ViewTimeSeries(dta=psm_Pairs,IDfield="reu_id",TrtField="TrtBin",idPre="NDVI[0-9][0-9][0-9][0-9]")
#SAT::ViewTimeSeries(dta=dta_Shp,IDfield="reu_id",TrtField="TrtBin",idPre="NDVI[0-9][0-9][0-9][0-9]")
#stargazer(test["Standardized"],test["Unstandardized"], title="Regression Results", type="html",align=TRUE)

