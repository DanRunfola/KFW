library(devtools)
devtools::install_github("itpir/SAT@Alpha")
library(SAT)
library(RColorBrewer)

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



#Calculate NDVI Differences
dta_Shp@data["NDVI_94_82"] <- dta_Shp@data["NDVI1994"] - dta_Shp@data["NDVI1982"] 
dta_Shp@data["NDVI_14_94"] <- dta_Shp@data["NDVI2012"] - dta_Shp@data["NDVI1994"]
#dta_Shp@data["NDVIslpChg"] <- (dta_Shp@data["NDVI_10_95"] / 16) - (dta_Shp@data["NDVI_95_82"]/14)

#Model the "pre-" and "post-" trend for each unit of analysis
NDVI_only <- dta_Shp@data[c(1,14:45)]
melted_NDVI <- melt(NDVI_only, id="SP_ID")
melted_NDVI["Year"] <-  lapply(melted_NDVI["variable"], FUN = function(x) as.numeric(gsub("NDVI", "", x)))
melted_NDVI["Year"] <- (melted_NDVI["Year"])
pre_NDVI <- melted_NDVI[melted_NDVI["Year"] < 1995,]
post_NDVI <- melted_NDVI[melted_NDVI["Year"] >= 1995,]
post_NDVI <- post_NDVI[post_NDVI["Year"] <= 2014,]
dta_Shp@data["preNDVISlope"] <- 0
dta_Shp@data["postNDVISlope"] <- 0
for(i in 1:length(dta_Shp))
{
  ID_NDVI <- as.character(dta_Shp@data["SP_ID"][i,])
  ID_NDVIframe_pre <- pre_NDVI[pre_NDVI["SP_ID"] == ID_NDVI,]
  ID_NDVIframe_post <- post_NDVI[post_NDVI["SP_ID"] == ID_NDVI,]
  
  #Fit each model and save the slope  
  pre_model <- lm(value ~ Year,data=ID_NDVIframe_pre)
  plot(value~Year, data=ID_NDVIframe_pre, main = ID_NDVI)
  abline(pre_model,col="Red")
  
  dta_Shp@data["preNDVISlope"][i,] <- summary(pre_model)$coefficients[2]
  
  #Fit each model and save the slope  
  post_model <- lm(value ~ Year,data=ID_NDVIframe_post)
  plot(value~Year, data=ID_NDVIframe_post, main = ID_NDVI)
  abline(post_model,col="Red")
  
  dta_Shp@data["postNDVISlope"][i,] <- summary(post_model)$coefficients[2]
}

dta_Shp@data["NDVIslopeChange"] <- dta_Shp@data["postNDVISlope"] - dta_Shp@data["preNDVISlope"]

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




psmModel <- "TrtBin ~ terrai_are + Pop_1990 + Pop_2000 + MeanT_1995 + MeanP_1995 + preNDVISlope +
MeanT_2010 + MeanP_2010 + Slope + Elevation +  NDVI1995 + Riv_Dist + Road_dist"
analyticModel <- "NDVIslopeChange ~ TrtBin + terrai_are + Pop_1990 + Pop_2000 + MeanT_1995 + MeanP_1995 + preNDVISlope +
MeanT_2010 + MeanP_2010 + Slope + Elevation + factor(PSM_match_ID) + NDVI1995 + Riv_Dist + Road_dist"

analyticModel <- "postNDVISlope ~ TrtBin + terrai_are + Pop_1990 + Pop_2000 + MeanT_1995 + MeanP_1995 + preNDVISlope +
MeanT_2010 + MeanP_2010 + Slope + Elevation + factor(PSM_match_ID) + NDVI1995 + Riv_Dist + Road_dist"


psmRes <- SAT::SpatialCausalPSM(dta_Shp,mtd="logit",psmModel,drop="none",visual=TRUE)

#Add in records for PFE
drop_set<- c(drop_unmatched=TRUE,drop_method="None",drop_thresh=0.25)
psm_Pairs <- SAT::SpatialCausalDist_Binary(dta = psmRes, mtd = "fastNN",constraints=c(groups="UF"),psm_eq = psmModel, ids = "id", drop_opts = drop_set, visual="FALSE", TrtBinColName="TrtBin")
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
#writePolyShape(psm_Pairs,'/home/aiddata/Desktop/quickPairs.shp')

#Making a heatmap of the results
psm_PairsC <- psm_Pairs
psm_PairsC@data["subID"] <- do.call(paste,c(psm_PairsC@data["SP_ID"],psm_PairsC@data["terrai_nom"],sep="_"))

row.names(psm_PairsC@data) <- psm_PairsC@data$subID

#Subset only the NDVI record
psm_PairsC <- psm_PairsC[14:45]

#Test for negative year-on-year change:
psm_PairsD <- psm_PairsC
for(i in 2:length(psm_PairsD@data))
{
  year <- as.numeric(sub("NDVI","",colnames(psm_PairsD@data)[i]))
  str <- sub("NDVI","Chg_",colnames(psm_PairsD@data)[i])
  str_loss <- sub("NDVI","Loss_",colnames(psm_PairsD@data)[i])
  str_gain <- sub("NDVI","Gain_",colnames(psm_PairsD@data)[i])
  str_bin <- sub("NDVI","BinMod_",colnames(psm_PairsD@data)[i])
  last_year <- paste("NDVI",round(year-1,0),sep="")
  cur_year <- paste("NDVI",round(year),sep="")
  psm_PairsD@data[str] <- (psm_PairsD@data[cur_year] - psm_PairsD@data[last_year])
  psm_PairsD@data[str] <- (psm_PairsD@data[cur_year] - psm_PairsD@data["NDVI1995"])
  psm_PairsD@data[str_loss] <- 0
  psm_PairsD@data[str_gain] <- 0
  psm_PairsD@data[str_loss][psm_PairsD@data[str] < -0.025,] <- -1
  psm_PairsD@data[str_gain][psm_PairsD@data[str] > 0.025,] <- 1
  psm_PairsD@data[str_bin] <- psm_PairsD@data[str_loss] + psm_PairsD@data[str_gain]
}

psm_PairsD@data <- psm_PairsD@data[grepl("NDVI",names(psm_PairsD@data))]

Pairs_matrix <- data.matrix(psm_PairsD@data)

hmcol <- brewer.pal(10,"RdYlGn")

#test_heat <- heatmap(Pairs_matrix, Rowv=NA, Colv=NA, col=hmcol, scale="none", margins=c(5,10),add.expr=abline(v=13,col="blue",lty=1,lwd=3))

