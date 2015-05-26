library(devtools)
devtools::install_github("itpir/SAT@spatialBranch3")
library(SAT)
library(RColorBrewer)

#load SAT libraries
loadLibs()

shpfile = "/Users/rbtrichler/Desktop/Matched_Indigenous_Lands_DemResults_LTDR.shp"
dta_Shp = readShapePoly(shpfile)
View(dta_Shp)
#Drop out a few units
#These units are being dropped due to issues with the historic data
#Specifically, effects of large water bodies and large areas of deforestation (pre-existing)
#drop_rows <- c(94,84,137,143,115,135,112)
#dta_Shp <- removeRow(dta=dta_Shp,columnID="reu_id",matchIDs=drop_rows)

#Calculate NDVI Trends
dta_Shp$pre_trend_NDVI_mean <- timeRangeTrend(dta_Shp,"MeanL_[0-9][0-9][0-9][0-9]",1982,1995,"SP_ID")
dta_Shp$pre_trend_NDVI_max <- timeRangeTrend(dta_Shp,"MaxL_[0-9][0-9][0-9][0-9]",1982,1995,"SP_ID")
dta_Shp$NDVIslope_95_10 <- timeRangeTrend(dta_Shp,"MeanL_[0-9][0-9][0-9][0-9]",1995,2010,"SP_ID")
dta_Shp@data["NDVIslopeChange_95_10"] <- dta_Shp@data["NDVIslope_95_10"] - dta_Shp@data["pre_trend_NDVI_mean"]

#NDVI Trends for 1995-2001
dta_Shp$post_trend_NDVI_95_01 <- timeRangeTrend(dta_Shp,"MeanL_[0-9][0-9][0-9][0-9]",1995,2001,"SP_ID")
dta_Shp@data["NDVIslopeChange_95_01"] <- dta_Shp@data["post_trend_NDVI_95_01"] - dta_Shp@data["pre_trend_NDVI_mean"]
#NDVI Trends for 2001-2010
dta_Shp$post_trend_NDVI_01_10 <- timeRangeTrend(dta_Shp,"MeanL_[0-9][0-9][0-9][0-9]",2001,2010,"SP_ID")
dta_Shp@data["NDVIslopeChange_01_10"] <- dta_Shp@data["post_trend_NDVI_01_10"] - dta_Shp@data["pre_trend_NDVI_mean"]

#Calculate Temp and Precip Pre and Post Trends
dta_Shp$pre_trend_temp_mean <- timeRangeTrend(dta_Shp,"MeanT_[0-9][0-9][0-9][0-9]",1982,1995,"SP_ID")
dta_Shp$pre_trend_temp_max <- timeRangeTrend(dta_Shp,"MaxT_[0-9][0-9][0-9][0-9]",1982,1995,"SP_ID")
dta_Shp$pre_trend_temp_min <- timeRangeTrend(dta_Shp,"MinT_[0-9][0-9][0-9][0-9]",1982,1995,"SP_ID")

dta_Shp$post_trend_temp_mean <- timeRangeTrend(dta_Shp,"MeanT_[0-9][0-9][0-9][0-9]",1995,2010,"SP_ID")
dta_Shp$post_trend_temp_max <- timeRangeTrend(dta_Shp,"MaxT_[0-9][0-9][0-9][0-9]",1995,2010,"SP_ID")
dta_Shp$post_trend_temp_min <- timeRangeTrend(dta_Shp,"MinT_[0-9][0-9][0-9][0-9]",1995,2010,"SP_ID")
dta_Shp$post_trend_temp_95_01 <- timeRangeTrend(dta_Shp,"MeanT_[0-9][0-9][0-9][0-9]",1995,2001,"SP_ID")
dta_Shp$post_trend_temp_01_10 <- timeRangeTrend(dta_Shp,"MeanT_[0-9][0-9][0-9][0-9]",2001,2010,"SP_ID")

dta_Shp$pre_trend_precip_mean <- timeRangeTrend(dta_Shp,"MeanP_[0-9][0-9][0-9][0-9]",1982,1995,"SP_ID")
dta_Shp$pre_trend_precip_max <- timeRangeTrend(dta_Shp,"MaxP_[0-9][0-9][0-9][0-9]",1982,1995,"SP_ID")
dta_Shp$pre_trend_precip_min <- timeRangeTrend(dta_Shp,"MinP_[0-9][0-9][0-9][0-9]",1982,1995,"SP_ID")

dta_Shp$post_trend_precip_mean <- timeRangeTrend(dta_Shp,"MeanP_[0-9][0-9][0-9][0-9]",1995,2010,"SP_ID")
dta_Shp$post_trend_precip_max <- timeRangeTrend(dta_Shp,"MaxP_[0-9][0-9][0-9][0-9]",1995,2010,"SP_ID")
dta_Shp$post_trend_precip_min <- timeRangeTrend(dta_Shp,"MinP_[0-9][0-9][0-9][0-9]",1995,2010,"SP_ID")
dta_Shp$post_trend_precip_95_01 <- timeRangeTrend(dta_Shp,"MeanP_[0-9][0-9][0-9][0-9]",1995,2001,"SP_ID")
dta_Shp$post_trend_precip_01_10 <- timeRangeTrend(dta_Shp,"MeanP_[0-9][0-9][0-9][0-9]",2001,2010,"SP_ID")

#Make a binary for ever vs. never
dta_Shp@data["TrtBin"] <- 0
dta_Shp@data$NA_check <- 0
dta_Shp@data$NA_check[is.na(dta_Shp@data$demend_y)] <- 1
dta_Shp@data$TrtBin[dta_Shp@data$NA_check != 1] <- 1
demtable <- table(dta_Shp@data$TrtBin)
View(demtable)

#Measure treatment as duration for each individual unit
#dta_Shp@data["TrtBin"] <- 0
#dta_Shp@data$TrtBin <- (2010 - dta_Shp@data["demend_y"])

#Make a binary to test treatment..
#dta_Shp@data["TrtBin"] <- 0
#dta_Shp@data$TrtBin[dta_Shp@data$demend_y <= 2001] <- 1
#dta_Shp@data$TrtBin[(dta_Shp@data$demend_m > 4) & (dta_Shp@data$demend_y==2001)] <- 0

#Remove units that did not ever receive any treatment (within-sample test)
#dta_Shp@data$NA_check <- 0
#dta_Shp@data$NA_check[is.na(dta_Shp@data$demend_y)] <- 1
#int_Shp <- dta_Shp[dta_Shp@data$NA_check != 1,]
#dta_Shp <- int_Shp

psmModel <-  "TrtBin ~ terrai_are + Pop_1990 + MeanT_1995 + pre_trend_temp_mean + pre_trend_temp_min + 
pre_trend_temp_max + MeanP_1995 + pre_trend_precip_min + 
pre_trend_NDVI_mean + pre_trend_NDVI_max + Slope + Elevation +  MeanL_1995 + MaxL_1995 + Riv_Dist + Road_dist +
pre_trend_precip_mean + pre_trend_precip_max"

# terrai_are + Pop_1990 + pre_trend_precip_mean + pre_trend_precip_max + 
# pre_trend_precip_min + pre_trend_temp_mean + pre_trend_temp_max + pre_trend_temp_min +
# pre_trend_NDVI + Slope + Elevation +  MeanP_1995 + MeanT_1995 + MaxN_1995 + MeanN_1995 + Riv_Dist + Road_dist

psmRes <- SAT::SpatialCausalPSM(dta_Shp,mtd="logit",psmModel,drop="support",visual=TRUE)

drop_set<- c(drop_unmatched=TRUE,drop_method="None",drop_thresh=0.5)
psm_Pairs <- SAT(dta = psmRes, mtd = "fastNN",constraints=c(groups="UF"),psm_eq = psmModel, ids = "id", drop_opts = drop_set, visual="TRUE", TrtBinColName="TrtBin")
#c(groups=c("UF"),distance=NULL)
trttable <- table (psm_Pairs@data$TrtBin)
View(trttable)

#Scale all of the data to get standardized coefficients, create psm_PairsB
psm_PairsB <- psm_Pairs
ind <- sapply(psm_PairsB@data, is.numeric)
psm_PairsB@data[ind] <- lapply(psm_PairsB@data[ind],scale)

# #Variables to include in the time series
varList = c("MeanN_","MaxN_","MeanL_","MaxL_")
psm_Long <- BuildTimeSeries(dta=dta_Shp,idField="reu_id",varList_pre=varList,1982,2010,colYears="demend_y",interpYears=c("Slope","Road_dist","Riv_Dist","UF","Elevation","terrai_are","Pop_","MeanT_","MeanP_","TrtMnt","MaxT_","MaxP_","MinP_","MinT_"))


#Panel Models
#psm_Long$TrtPair <- as.numeric(psm_Long$PSM_match_ID) * psm_Long$TrtMnt
pModelA <- "MaxL_ ~ TrtMnt + MeanT_ + MeanP_ + Pop_ + MaxT_ + MaxP_ + MinT_ + MinP_  + factor(reu_id)"

OutputPanel=Stage2PSM(pModelA,psm_Long,type="cmreg", table_out=TRUE, opts=c("reu_id","Year"))

ViewTimeSeries(dta=dta_Shp,IDfield="reu_id",TrtField="TrtBin",idPre="MaxL_[0-9][0-9][0-9][0-9]")

#Cross-section Models

## Early vs. Late

#analyticModelEarly1, no pair FE, no covars, 1995-2001
summary(analyticModelEarly1 <- lm(NDVIslopeChange_95_01 ~ TrtBin, data=psm_Pairs))
#Standardized Betas
summary(analyticModelEarly1B <- lm(NDVIslopeChange_95_01 ~ TrtBin, data=psm_PairsB))

#analyticModelEarly2, treatment effect + pair fixed effects, 1995-2001
analyticModelEarly2 <- "NDVIslopeChange_95_01 ~ TrtBin + factor(PSM_match_ID)"

OutputEarly2=Stage2PSM(analyticModelEarly2,psm_Pairs,type="lm",table_out=TRUE)

#analyticModelEarly3, treatment effect + pair fixed effects + covars 1995-2001

#create new dataset and rename column names in new dataset to enable multiple columns in stargazer
Data_Early3 <- psm_Pairs
colnames(Data_Early3@data)[(colnames(Data_Early3@data)=="Pop_1990")] <- "Pop_B"
colnames(Data_Early3@data)[(colnames(Data_Early3@data)=="MeanT_1995")] <- "MeanT_B"
colnames(Data_Early3@data)[(colnames(Data_Early3@data)=="MeanP_1995")] <- "MeanP_B"
colnames(Data_Early3@data)[(colnames(Data_Early3@data)=="post_trend_temp_95_01")] <- "post_trend_temp"
colnames(Data_Early3@data)[(colnames(Data_Early3@data)=="post_trend_precip_95_01")] <- "post_trend_precip"
#colnames(Data_Early3@data)

analyticModelEarly3 <- "NDVIslopeChange_95_01 ~ TrtBin+ pre_trend_NDVI_mean + MeanL_1995 + terrai_are + Pop_B + MeanT_B  + post_trend_temp +
MeanP_B + post_trend_precip + Slope + Elevation + Riv_Dist + Road_dist + factor(PSM_match_ID)"
OutputEarly3=Stage2PSM(analyticModelEarly3,Data_Early3,type="lm",table_out=TRUE)

#analyticModelLate, treatment effect + pair fixed effects + covars 2001-2010
#create new dataset and rename column names in new dataset to enable multiple columns in stargazer
Data_Late <- psm_Pairs
colnames(Data_Late@data)[(colnames(Data_Late@data)=="Pop_2000")] <- "Pop_B"
colnames(Data_Late@data)[(colnames(Data_Late@data)=="MeanT_2001")] <- "MeanT_B"
colnames(Data_Late@data)[(colnames(Data_Late@data)=="MeanP_2001")] <- "MeanP_B"
colnames(Data_Late@data)[(colnames(Data_Late@data)=="post_trend_temp_01_10")] <- "post_trend_temp"
colnames(Data_Late@data)[(colnames(Data_Late@data)=="post_trend_precip_01_10")] <- "post_trend_precip"
#colnames(Data_Late@data)

analyticModelLate <- "NDVIslopeChange_01_10 ~ TrtBin + pre_trend_NDVI_mean + MeanL_1995 + terrai_are + Pop_B + MeanT_B + post_trend_temp + 
MeanP_B + post_trend_precip + Slope + Elevation + Riv_Dist + Road_dist + factor(PSM_match_ID)"
OutputLate=Stage2PSM(analyticModelLate,Data_Late,type="lm",table_out=TRUE)

stargazer(analyticModelEarly1B,OutputEarly2$standardized,OutputEarly3$standardized,OutputLate$standardized,
          keep=c("TrtBin", "pre_trend_NDVI_mean","MeanL_1995", "terrai_are","Pop_B", "MeanT_B","post_trend_temp","MeanP_B",
                 "post_trend_precip", "Slope","Elevation","Riv_Dist","Road_dist"),
          covariate.labels=c("Treatment", "Pre-Trend NDVI", "Baseline NDVI","Area (hectares)", "Baseline Population Density",
                             "Baseline Temperature", "Temperature Trends", "Baseline Precipitation", "Precipitation Trends",
                             "Slope", "Elevation", "Distance to River", "Distance to Road"),
          title="Regression Results", type="html", omit.stat=c("f","ser"), align=TRUE)

#Ever vs. Never

#OLS, no pair FEs, no covars, 1995-2010
summary(analyticModelEver1 <- lm(NDVIslopeChange_95_10 ~ TrtBin, data=psm_Pairs))
summary(analyticModelEver1B <- lm(NDVIslopeChange_95_10 ~ TrtBin, data=psm_PairsB))

#analyticModelEver2, pair FEs, no covars, 1995-2010

analyticModelEver2 <- "NDVIslopeChange_95_10 ~ TrtBin + factor(PSM_match_ID)"

OutputEver2=Stage2PSM(analyticModelEver2,psm_Pairs,type="lm",table_out=TRUE)

#analyticModelEver3, pair FEs, covars, 1995-2010

#create new dataset and rename column names in new dataset to enable multiple columns in stargazer
Data_Ever3 <- psm_Pairs
colnames(Data_Ever3@data)[(colnames(Data_Ever3@data)=="Pop_1990")] <- "Pop_B"
colnames(Data_Ever3@data)[(colnames(Data_Ever3@data)=="MeanT_1995")] <- "MeanT_B"
colnames(Data_Ever3@data)[(colnames(Data_Ever3@data)=="MeanP_1995")] <- "MeanP_B"
colnames(Data_Ever3@data)[(colnames(Data_Ever3@data)=="post_trend_temp_mean")] <- "post_trend_temp"
colnames(Data_Ever3@data)[(colnames(Data_Ever3@data)=="post_trend_precip_mean")] <- "post_trend_precip"
#colnames(Data_Ever3@data)

analyticModelEver3 <- "NDVIslopeChange_95_10 ~ TrtBin + pre_trend_NDVI_mean + MeanL_1995 + + terrai_are + Pop_B + MeanT_B + post_trend_temp +
MeanP_B + post_trend_precip + Slope + Elevation  + Riv_Dist + Road_dist + factor(PSM_match_ID)"
OutputEver3=Stage2PSM(analyticModelEver3,Data_Ever3,type="lm",table_out=TRUE)

# Results Tables

stargazer(analyticModelEver1B, OutputEver2$standardized, OutputEver3$standardized,
          keep=c("TrtBin", "terrai_are","Pop_B","pre_trend_NDVI_mean","MeanL_1995","MeanT_B","post_trend_temp","MeanP_B",
                 "post_trend_precip","Slope","Elevation","Riv_Dist","Road_dist"),
          covariate.labels=c("Treatment","Area (hectares)", "Baseline Population Density", "Pre-Trend NDVI", "Baseline NDVI",
                             "Baseline Temperature", "Temperature Trends", "Baseline Precipitation", "Precipitation Trends",
                             "Slope", "Elevation", "Distance to River", "Distance to Road"),
          title="Regression Results", type="html", omit.stat=c("f","ser"), align=TRUE)
