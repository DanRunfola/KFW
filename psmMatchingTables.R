library(devtools)
devtools::install_github("itpir/SAT@master")
library(SAT)
library(RColorBrewer)

#load SAT libraries
loadLibs()


shpfile = "Processed_Data/Matched_Indigenous_Lands_ProcessedResults.shp"

dta_Shp = readShapePoly(shpfile)

#Drop out a few units
#These units are being dropped due to issues with the historic data
#Specifically, effects of large water bodies and large areas of deforestation (pre-existing)
#drop_rows <- c(94,84,137,143,115,135,112)
#dta_Shp <- removeRow(dta=dta_Shp,columnID="reu_id",matchIDs=drop_rows)

#Calculate NDVI Trends
dta_Shp$pre_trend_NDVI_mean <- timeRangeTrend(dta_Shp,"MeanL_[0-9][0-9][0-9][0-9]",1982,1995,"SP_ID")
dta_Shp$pre_trend_NDVI_max <- timeRangeTrend(dta_Shp,"MaxL_[0-9][0-9][0-9][0-9]",1982,1995,"SP_ID")
dta_Shp$NDVIslope_95_10 <- timeRangeTrend(dta_Shp,"MeanL_[0-9][0-9][0-9][0-9]",1995,2010,"SP_ID")
dta_Shp$NDVIslopeChange <- dta_Shp$NDVIslope_95_10 - dta_Shp$pre_trend_NDVI_mean

dta_Shp$level <- dta_Shp$MeanL_2010 - dta_Shp$MeanL_1995

#Calculate Temp and Precip Pre and Post Trends
dta_Shp$pre_trend_temp_mean <- timeRangeTrend(dta_Shp,"MeanT_[0-9][0-9][0-9][0-9]",1982,1995,"SP_ID")
dta_Shp$pre_trend_temp_max <- timeRangeTrend(dta_Shp,"MaxT_[0-9][0-9][0-9][0-9]",1982,1995,"SP_ID")
dta_Shp$pre_trend_temp_min <- timeRangeTrend(dta_Shp,"MinT_[0-9][0-9][0-9][0-9]",1982,1995,"SP_ID")

dta_Shp$pre_trend_precip_mean <- timeRangeTrend(dta_Shp,"MeanP_[0-9][0-9][0-9][0-9]",1982,1995,"SP_ID")
dta_Shp$pre_trend_precip_max <- timeRangeTrend(dta_Shp,"MaxP_[0-9][0-9][0-9][0-9]",1982,1995,"SP_ID")
dta_Shp$pre_trend_precip_min <- timeRangeTrend(dta_Shp,"MinP_[0-9][0-9][0-9][0-9]",1982,1995,"SP_ID")

saveTheYears <- dta_Shp@data$demend_y
saveTheMonths <- dta_Shp@data$demend_m
d_index <- sapply(dta_Shp@data, is.numeric)
dta_Shp@data[d_index] <- lapply(dta_Shp@data[d_index],scale)
dta_Shp@data$demend_y <- saveTheYears
dta_Shp@data$demend_m <- saveTheMonths

dta_ShpC <- dta_Shp

#Make a binary to test treatment..
dta_ShpC@data["TrtBin"] <- 0
dta_ShpC@data$TrtBin[dta_ShpC@data$demend_y <= 2001] <- 1
dta_ShpC@data$TrtBin[(dta_ShpC@data$demend_m > 4) & (dta_ShpC@data$demend_y==2001)] <- 0

dta_ShpC@data$NA_check <- 0
dta_ShpC@data$NA_check[is.na(dta_ShpC@data$demend_y)] <- 1
int_Shp <- dta_ShpC[dta_ShpC@data$NA_check != 1,]
dta_ShpC <- int_Shp

psmModel <- "TrtBin ~ terrai_are + Pop_1990 + pre_trend_precip_mean + pre_trend_precip_max + 
pre_trend_precip_min + pre_trend_temp_mean + pre_trend_temp_max + pre_trend_temp_min +
pre_trend_NDVI_mean + pre_trend_NDVI_max + Slope + Elevation +  MeanP_1995 + MeanT_1995 + MaxL_1995 + 
MeanL_1995 + Riv_Dist + Road_dist"

psmResA <- SAT::SpatialCausalPSM(dta_ShpC,mtd="logit",psmModel,drop="support",visual=TRUE)

dta_Shp@data["TrtBinB"] <- 0
dta_Shp@data$NA_check <- 0
dta_Shp@data$NA_check[is.na(dta_Shp@data$demend_y)] <- 1
dta_Shp@data$TrtBinB[dta_Shp@data$NA_check != 1] <- 1

psmModel <- "TrtBinB ~ terrai_are + Pop_1990 + pre_trend_precip_mean + pre_trend_precip_max + 
pre_trend_precip_min + pre_trend_temp_mean + pre_trend_temp_max + pre_trend_temp_min +
pre_trend_NDVI_mean + pre_trend_NDVI_max + Slope + Elevation +  MeanP_1995 + MeanT_1995 + MaxL_1995 + 
MeanL_1995 + Riv_Dist + Road_dist"

psmResB <- SAT::SpatialCausalPSM(dta_Shp,mtd="logit",psmModel,drop="support",visual=TRUE)

stargazer(psmResA$model,psmResB$model,
          covariate.labels=c("Area (hectacres)","Population Baseline","Pre-trend Mean Precipitation","Pre-trend Max Precipitation",
                             "Pre-trend Min Precipitation","Pre-trend Mean Temperature","Pre-trend Max Temperature","Pre-trend Min Temperature",
                             "Pre-trend NDVI Mean", "Pre-trend NDVI Max", "Slope","Elevation","Mean Precipitation Baseline","Mean Temperature Baseline","Max NDVI Baseline",
                             "Mean NDVI Baseline", "Distance to Rivers", "Distance to Roads"
                             ),
          dep.var.labels=c("Early vs. Late","Ever Demarcated"),
          title="PSM First Stage Results", type="html", omit.stat=c("f","ser"), align=TRUE)