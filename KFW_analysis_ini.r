library(devtools)
devtools::install_github("itpir/SAT@spatialBranch3")
library(SAT)
library(RColorBrewer)

#load SAT libraries
loadLibs()

#View all functions
lsf.str("package:SAT")

#Set working directory
shpfile = "Processed_Data/Matched_Indigenous_Lands_DemResults.shp"
dta_Shp = readShapePoly(shpfile)
#View(dta_Shp)

#Drop out a few units
#These units are being dropped due to issues with the historic data
#Specifically, effects of large water bodies and large areas of deforestation (pre-existing)
dta_Shp <- dta_Shp[dta_Shp@data$reu_id != 94,]
dta_Shp <- dta_Shp[dta_Shp@data$reu_id != 84,]
dta_Shp <- dta_Shp[dta_Shp@data$reu_id != 137,]
dta_Shp <- dta_Shp[dta_Shp@data$reu_id != 143,]
dta_Shp <- dta_Shp[dta_Shp@data$reu_id != 115,]
dta_Shp <- dta_Shp[dta_Shp@data$reu_id != 135,]
dta_Shp <- dta_Shp[dta_Shp@data$reu_id != 112,]

#Calculate NDVI Trends
dta_Shp$pre_trend_NDVI <- timeRangeTrend(dta_Shp,"NDVI[0-9][0-9][0-9][0-9]",1982,1995,"SP_ID")
#NDVI Trends for 1995-2001
dta_Shp$post_trend_NDVI_01 <- timeRangeTrend(dta_Shp,"NDVI[0-9][0-9][0-9][0-9]",1995,2001,"SP_ID")
dta_Shp@data["NDVIslopeChange_01"] <- dta_Shp@data["post_trend_NDVI_01"] - dta_Shp@data["pre_trend_NDVI"]
#NDVI Trends for 2001-2010
dta_Shp$post_trend_NDVI_10 <- timeRangeTrend(dta_Shp,"NDVI[0-9][0-9][0-9][0-9]",2001,2010,"SP_ID")
dta_Shp@data["NDVIslopeChange_10"] <- dta_Shp@data["post_trend_NDVI_10"] - dta_Shp@data["pre_trend_NDVI"]
#NDVI Trends for 1995-2010
dta_Shp$post_trend_NDVI_95_10 <- timeRangeTrend(dta_Shp,"NDVI[0-9][0-9][0-9][0-9]",1995,2010,"SP_ID")
dta_Shp@data["NDVIslopeChange_95_10"] <- dta_Shp@data["post_trend_NDVI_95_10"] - dta_Shp@data["pre_trend_NDVI"]


dta_Shp@data["NDVI_14_94_Percent"] <- dta_Shp@data["NDVI2014"] / dta_Shp@data["NDVI1994"]
dta_Shp@data["NDVI_94_82_Percent"] <- dta_Shp@data["NDVI1994"] / dta_Shp@data["NDVI1982"]

#Calculate Temp and Precip Pre and Post Trends
dta_Shp$pre_trend_temp <- timeRangeTrend(dta_Shp,"MeanT_[0-9][0-9][0-9][0-9]",1982,1995,"SP_ID")
dta_Shp$post_trend_temp_95_01 <- timeRangeTrend(dta_Shp,"MeanT_[0-9][0-9][0-9][0-9]",1995,2001,"SP_ID")
dta_Shp$post_trend_temp_01_10 <- timeRangeTrend(dta_Shp,"MeanT_[0-9][0-9][0-9][0-9]",2001,2010,"SP_ID")
dta_Shp$post_trend_temp_95_10 <- timeRangeTrend(dta_Shp, "MeanT_[0-9][0-9][0-9][0-9]",1995,2010,"SP_ID")

dta_Shp$pre_trend_precip <- timeRangeTrend(dta_Shp,"MeanP_[0-9][0-9][0-9][0-9]",1982,1995,"SP_ID")
dta_Shp$post_trend_precip_95_01 <- timeRangeTrend(dta_Shp,"MeanP_[0-9][0-9][0-9][0-9]",1995,2001,"SP_ID")
dta_Shp$post_trend_precip_01_10 <- timeRangeTrend(dta_Shp,"MeanP_[0-9][0-9][0-9][0-9]",2001,2010,"SP_ID")
dta_Shp$post_trend_precip_95_10 <- timeRangeTrend(dta_Shp, "MeanP_[0-9][0-9][0-9][0-9]",1995,2010,"SP_ID")


#Calculate average temperature and precip for pre- and post- periods
#dta_Shp@data["meanT_95_82"] <- timeRangeAvg(dta_Shp@data,"MeanT_",1982,1995)
#dta_Shp@data["maxT_94_82"] <- timeRangeAvg(dta_Shp@data,"MaxT_",1982,1994)
#dta_Shp@data["minT_94_82"] <- timeRangeAvg(dta_Shp@data,"MinT_",1982,1994)

#dta_Shp@data["meanP_94_82"] <- timeRangeAvg(dta_Shp@data,"MeanP_",1982,1994)
#dta_Shp@data["maxP_94_82"] <- timeRangeAvg(dta_Shp@data,"MaxP_",1982,1994)
#dta_Shp@data["minP_94_82"] <- timeRangeAvg(dta_Shp@data,"MinP_",1982,1994)

#dta_Shp@data["meanT_01_95"] <- timeRangeAvg(dta_Shp@data,"MeanT_",1995,2001)
#dta_Shp@data["meanT_10_01"] <- timeRangeAvg(dta_Shp@data,"MeanT_",2001,2010)

#dta_Shp@data["maxT_10_94"] <- timeRangeAvg(dta_Shp@data,"MaxT_",1994,2010)
#dta_Shp@data["minT_10_94"] <- timeRangeAvg(dta_Shp@data,"MinT_",1994,2010)

#dta_Shp@data["meanP_01_95"] <- timeRangeAvg(dta_Shp@data,"MeanP_",1995,2001)
#dta_Shp@data["meanP_10_01"] <- timeRangeAvg(dta_Shp@data,"MeanP_",2001,2010)

#dta_Shp@data["maxP_10_94"] <- timeRangeAvg(dta_Shp@data,"MaxP_",1994,2010)
#dta_Shp@data["minP_10_94"] <- timeRangeAvg(dta_Shp@data,"MinP_",1994,2010)

#Make a binary to test treatment..
dta_Shp@data["TrtBin"] <- 0
#dta_Shp@data$TrtBin[dta_Shp@data$stagenum == 6] <- 1
#dta_Shp@data$TrtBin[dta_Shp@data$stagenum == 7] <- 1
#dta_Shp@data$TrtBin[dta_Shp@data$stagenum == 8] <- 1

dta_Shp@data$TrtBin[dta_Shp@data$demend_y <= 2001] <- 1
dta_Shp@data$TrtBin[(dta_Shp@data$demend_m > 4) & (dta_Shp@data$demend_y==2001)] <- 0
#summary(dta_Shp@data$TrtBin)
demtable <- table(dta_Shp@data$TrtBin)
View(demtable)

dta_Shp@data$NA_check <- 0
dta_Shp@data$NA_check[is.na(dta_Shp@data$demend_y)] <- 1
int_Shp <- dta_Shp[dta_Shp@data$NA_check != 1,]
dta_Shp <- int_Shp

## \\ Matching //
psmModel <- "TrtBin ~ terrai_are + Pop_1990 + MeanT_1995 + pre_trend_temp + MeanP_1995 + pre_trend_precip + 
pre_trend_NDVI + Slope + Elevation +  NDVI1995 + Riv_Dist + Road_dist"

psmRes <- SAT::SpatialCausalPSM(dta_Shp,mtd="logit",psmModel,drop="overlap",visual=TRUE)

#Add in records for Pair FE, create psm_Pairs
drop_set<- c(drop_unmatched=TRUE,drop_method="None",drop_thresh=0.25)
psm_Pairs <- SAT(dta = psmRes, mtd = "fastNN",constraints=NULL,psm_eq = psmModel, 
                 ids = "id", drop_opts = drop_set, visual="TRUE", TrtBinColName="TrtBin")
trttable <- table (psm_Pairs@data$TrtBin)
View(trttable)

## \\ Run Analytic Models //

## Early vs. Late

#analyticModelEarly1, no pair FE, no covars, 1995-2001
summary(analyticModelEarly1 <- lm(NDVIslopeChange_01 ~ TrtBin, data=psm_Pairs))
#Standardized Betas
summary(analyticModelEarly1B <- lm(NDVIslopeChange_01 ~ TrtBin, data=psm_PairsB))


#analyticModelEarly2, treatment effect + pair fixed effects, 1995-2001
analyticModelEarly2 <- "NDVIslopeChange_01 ~ TrtBin + factor(PSM_match_ID)"

OutputEarly2=Stage2PSM(analyticModelEarly2,psm_Pairs,type="lm",table_out=TRUE)

#mfit <- lm(analyticModelEarly2,psm_Pairs)
#summary(m_fit)
#texreg::plotreg(m_fit,omit.coef="(match)|(Intercept)",custom.model.names="Unstandardized Model")
#Standardized Betas
#m_fit <- lm(analyticModelEarly2,psm_PairsB)
#summary(m_fit)
#texreg::plotreg(m_fit,omit.coef="(match)|(Intercept)",custom.model.names="Standardized Model")


#analyticModelEarly3, treatment effect + pair fixed effects + covars 1995-2001

#create new dataset and rename column names in new dataset to enable multiple columns in stargazer
Data_Early3 <- psm_Pairs
colnames(Data_Early3@data)[(colnames(Data_Early3@data)=="Pop_1990")] <- "Pop_B"
colnames(Data_Early3@data)[(colnames(Data_Early3@data)=="MeanT_1995")] <- "MeanT_B"
colnames(Data_Early3@data)[(colnames(Data_Early3@data)=="MeanP_1995")] <- "MeanP_B"
colnames(Data_Early3@data)[(colnames(Data_Early3@data)=="post_trend_temp_95_01")] <- "post_trend_temp"
colnames(Data_Early3@data)[(colnames(Data_Early3@data)=="post_trend_precip_95_01")] <- "post_trend_precip"
#colnames(Data_Early3@data)

analyticModelEarly3 <- "NDVIslopeChange_01 ~ TrtBin+ pre_trend_NDVI + NDVI1995 + terrai_are + Pop_B + MeanT_B  + post_trend_temp +
MeanP_B + post_trend_precip + Slope + Elevation + Riv_Dist + Road_dist + factor(PSM_match_ID)"
OutputEarly3=Stage2PSM(analyticModelEarly3,Data_Early3,type="lm",table_out=TRUE)

#m_fit <- lm(analyticModelEarly3,psm_Pairs)
#summary(m_fit)
#texreg::plotreg(m_fit,omit.coef="(match)|(Intercept)",custom.model.names="Unstandardized Model")
#Standardized Betas
#m_fit <- lm(analyticModelEarly3,psm_PairsB)
#summary(m_fit)
#texreg::plotreg(m_fit,omit.coef="(match)|(Intercept)",custom.model.names="Standardized Model")


#analyticModelLate, treatment effect + pair fixed effects + covars 2001-2010
#create new dataset and rename column names in new dataset to enable multiple columns in stargazer
Data_Late <- psm_Pairs
colnames(Data_Late@data)[(colnames(Data_Late@data)=="Pop_2000")] <- "Pop_B"
colnames(Data_Late@data)[(colnames(Data_Late@data)=="MeanT_2001")] <- "MeanT_B"
colnames(Data_Late@data)[(colnames(Data_Late@data)=="MeanP_2001")] <- "MeanP_B"
colnames(Data_Late@data)[(colnames(Data_Late@data)=="post_trend_temp_01_10")] <- "post_trend_temp"
colnames(Data_Late@data)[(colnames(Data_Late@data)=="post_trend_precip_01_10")] <- "post_trend_precip"
#colnames(Data_Late@data)

analyticModelLate <- "NDVIslopeChange_10 ~ TrtBin + pre_trend_NDVI + NDVI1995 + terrai_are + Pop_B + MeanT_B + post_trend_temp + 
MeanP_B + post_trend_precip + Slope + Elevation + Riv_Dist + Road_dist + factor(PSM_match_ID)"
OutputLate=Stage2PSM(analyticModelLate,Data_Late,type="lm",table_out=TRUE)

# m_fit <- lm(analyticModelLate,psm_Pairs)
# summary(m_fit)
# texreg::plotreg(m_fit,omit.coef="(match)|(Intercept)",custom.model.names="Unstandardized Model")
# #Standardized Betas
# m_fit <- lm(analyticModelLate,psm_PairsB)
# summary(m_fit)
# texreg::plotreg(m_fit,omit.coef="(match)|(Intercept)",custom.model.names="Standardized Model")

## Never vs. Ever

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
colnames(Data_Ever3@data)[(colnames(Data_Ever3@data)=="post_trend_temp_95_10")] <- "post_trend_temp"
colnames(Data_Ever3@data)[(colnames(Data_Ever3@data)=="post_trend_precip_95_10")] <- "post_trend_precip"
colnames(Data_Ever3@data)

analyticModelEver3 <- "NDVIslopeChange_95_10 ~ TrtBin + pre_trend_NDVI + NDVI1995 + + terrai_are + Pop_B + MeanT_B + post_trend_temp +
MeanP_B + post_trend_precip + Slope + Elevation  + Riv_Dist + Road_dist + factor(PSM_match_ID)"
OutputEver3=Stage2PSM(analyticModelEver3,Data_Ever3,type="lm",table_out=TRUE)

# Results Tables

library(stargazer)
stargazer(analyticModelEarly1B,OutputEarly2$standardized,OutputEarly3$standardized,OutputLate$standardized,
          keep=c("TrtBin", "pre_trend_NDVI","NDVI1995", "terrai_are","Pop_B", "MeanT_B","post_trend_temp","MeanP_B",
                 "post_trend_precip", "Slope","Elevation","Riv_Dist","Road_dist"),
          covariate.labels=c("Treatment", "Pre-Trend NDVI", "Baseline NDVI","Area (hectares)", "Baseline Population Density",
                             "Baseline Temperature", "Temperature Trends", "Baseline Precipitation", "Precipitation Trends",
                             "Slope", "Elevation", "Distance to River", "Distance to Road"),
          title="Regression Results", type="html", omit.stat=c("f","ser"), align=TRUE)
stargazer(analyticModelEver1B, OutputEver2$standardized, OutputEver3$standardized,
          keep=c("TrtBin", "terrai_are","Pop_1990","pre_trend_NDVI","NDVI1995","MeanT_1995","post_trend_temp_95_10","MeanP_1995",
                 "post_trend_precip_95_10","Slope","Elevation","Riv_Dist","Road_dist"),
          covariate.labels=c("Treatment","Area (hectares)", "Baseline Population Density", "Pre-Trend NDVI", "Baseline NDVI",
                             "Baseline Temperature", "Temperature Trends", "Baseline Precipitation", "Precipitation Trends",
                             "Slope", "Elevation", "Distance to River", "Distance to Road"),
          title="Regression Results", type="html", omit.stat=c("f","ser"), align=TRUE)

##NDVI and CoVar Visualizations

SAT::ViewTimeSeries(dta=psm_Pairs,IDfield="reu_id",TrtField="TrtBin",idPre="NDVI[0-9][0-9][0-9][0-9]")
SAT::ViewTimeSeries(dta=psm_Pairs,IDfield="reu_id",TrtField="TrtBin",idPre="NDVI199[5-9]|NDVI200[0-9]|NDVI2010")
SAT::ViewTimeSeries(dta=psm_Pairs,IDfield="reu_id",TrtField="TrtBin",idPre="NDVI199[5-9]|NDVI200[0-1]")
SAT::ViewTimeSeries(dta=psm_Pairs,IDfield="reu_id",TrtField="TrtBin",idPre="NDVI200[1-9]|NDVI2010")

SAT::ViewTimeSeries(dta=psm_Pairs,IDfield="reu_id",TrtField="TrtBin",idPre="MeanP_19[8-9][0-9]|MeanP_20[0-9][0-9]")
SAT::ViewTimeSeries(dta=psm_Pairs,IDfield="reu_id",TrtField="TrtBin",idPre="MeanT_19[8-9][0-9]|MeanT_20[0-9][0-9]")
