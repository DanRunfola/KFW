library(GISTools)
library(reshape)
library(splitstackshape)
library(ggplot2)

#Session - Set Working Directory - To Source File Location
#File for the KFW analysis
shpfile = "Input_Data/Matched_Indigenous_Lands_DemDates.shp"
src_Shp = readShapePoly(shpfile)


#Clean the source Shapefile to remove extra columns of data.
cln_Shp <- src_Shp[,c("terrai_nom","terrai_are","reu_id","id","UF", "pop","demend_y","stagenum")]

#Population -------------------------------------------
GPW_pop <- "/mnt/sciclone-aiddata/REU/projects/kfw/extracts/gpw/extract_merge.csv"
GPW_pop <- read.csv(GPW_pop)
#Rename the columns for easier interpretation later..
colnames(GPW_pop)[2] <- "Pop_1990"
colnames(GPW_pop)[3] <- "Pop_1995"
colnames(GPW_pop)[4] <- "Pop_2000"
#Merge it in
kfw.SPDF <- merge(cln_Shp, GPW_pop, by.x="id", by.y="id")

#NDVI -------------------------------------------------
#Historic GIMMS NDVI
GIMMS_hist <- NA
GIMMS_hist <- "/mnt/sciclone-aiddata/REU/projects/kfw/extracts/historic_ndvi/historic_ndvi_SIMULATED_yearly.csv"
GIMMS_hist <- read.csv(GIMMS_hist)
#Rename columns...
for (i in 2:length(GIMMS_hist))
{
  colnames(GIMMS_hist)[i] <- sub("_SIM_X","",colnames(GIMMS_hist)[i])
}
#Drop extra ID column
GIMMS_hist <- GIMMS_hist[-c(1)]

#Merge
kfw.SPDF <- merge(kfw.SPDF, GIMMS_hist, by.x="id", by.y="id")

#Contemporary GIMMS NDVI
GIMMS_cont <- "/mnt/sciclone-aiddata/REU/projects/kfw/extracts/ndvi/merge_year_max.csv"
GIMMS_cont <- read.csv(GIMMS_cont)
#Rename columns...
for (i in 2:length(GIMMS_cont))
{
  colnames(GIMMS_cont)[i] <- sub("X","NDVI",colnames(GIMMS_cont)[i])
}
#Merge
kfw.SPDF <- merge(kfw.SPDF, GIMMS_cont, by.x="id", by.y="id")

NDVI_sub = kfw.SPDF@data[c(1,12:44)]
names(NDVI_sub) <- sub("NDVI","",names(NDVI_sub))
NDVI_long <- melt(NDVI_sub, id.vars=c("id"))

ggplot() + geom_point(data=NDVI_long, aes(x=value, y=variable, colour=factor(id))) + scale_fill_manual(values=c("blue","cyan4"))

#Slope ----------------------------------------------------
SRTM_slope <- "/mnt/sciclone-aiddata/REU/projects/kfw/extracts/srtm_slope/SRTM_500m_slope.shp"
SRTM_slope <- readShapePoly(SRTM_slope)
SRTM_slope <- SRTM_slope@data
#Keep only the relevant columns
SRTM_slope <- SRTM_slope[c("id","SRTM_500m_")]

#Rename
colnames(SRTM_slope)[2] <- "Slope"
#Merge it in
kfw.SPDF <- merge(kfw.SPDF, SRTM_slope, by.x="id", by.y="id")

#Elevation -----------------------------------------------
SRTM_elev <- "/mnt/sciclone-aiddata/REU/projects/kfw/extracts/srtm/SRTM_500m.shp"
SRTM_elev <- readShapePoly(SRTM_elev)
SRTM_elev <- SRTM_elev@data
#Keep only the relevant columns
SRTM_elev <- SRTM_elev[c("id","SRTM_500m")]

#Rename
colnames(SRTM_elev)[2] <- "Elevation"
#Merge it in
kfw.SPDF <- merge(kfw.SPDF, SRTM_elev, by.x="id", by.y="id")

#Urban travel time ---------------------------------------
urb_trv <- "/mnt/sciclone-aiddata/REU/projects/kfw/extracts/accessibility_map/access_50k.shp"
urb_trv <- readShapePoly(urb_trv)
urb_trv <- urb_trv@data
#Keep only the relevant columns
urb_trv <- urb_trv[c("id","access_50k")]

#Rename
colnames(urb_trv)[2] <- "UrbTravTime"
#Merge it in
kfw.SPDF <- merge(kfw.SPDF, urb_trv, by.x="id", by.y="id")

#Air Temperature----------------------------------------------
air_temp <- "/mnt/sciclone-aiddata/REU/projects/kfw/extracts/terrestrial_air_temperature/extract_merge.csv"
air_temp <- read.csv(air_temp)

for (i in 2:length(air_temp))
{
  splt <- strsplit(colnames(air_temp)[i],"_")
  splt[[1]][1] <- sub("X","",splt[[1]][1])
  month = splt[[1]][2]
  year = splt[[1]][1]
  dt = paste(year,"-",month,sep="")
  colnames(air_temp)[i] <- dt
}

air_temp_ts <- melt(air_temp,id="id")
air_temp_ts <- cSplit(air_temp_ts, "variable", "-")
air_temp_ts_mean <- aggregate(value ~ variable_1 + id, air_temp_ts, FUN=mean)
air_temp_ts_max <- aggregate(value ~ variable_1 + id, air_temp_ts, FUN=max)
air_temp_ts_min <- aggregate(value ~ variable_1 + id, air_temp_ts, FUN=min)
air_temp_mean <- reshape(air_temp_ts_mean, idvar=c("id"), direction="wide", timevar="variable_1")
air_temp_max <- reshape(air_temp_ts_max, idvar=c("id"), direction="wide", timevar="variable_1")
air_temp_min <- reshape(air_temp_ts_min, idvar=c("id"), direction="wide", timevar="variable_1")

#Rename vars
for (i in 2:length(air_temp_mean))
{
  colnames(air_temp_mean)[i] <- sub("value.","MeanT_",colnames(air_temp_mean)[i])
  colnames(air_temp_max)[i] <- sub("value.","MaxT_",colnames(air_temp_max)[i])
  colnames(air_temp_min)[i] <- sub("value.","MinT_",colnames(air_temp_min)[i])
}

#ggplot() + geom_point(data=air_temp_ts_mean, aes(x=value, y=variable_1, colour=factor(id))) + scale_fill_manual(values=c("blue","cyan4"))
#Merge it in
kfw.SPDF <- merge(kfw.SPDF, air_temp_mean, by.x="id", by.y="id")
kfw.SPDF <- merge(kfw.SPDF, air_temp_max, by.x="id", by.y="id")
kfw.SPDF <- merge(kfw.SPDF, air_temp_min, by.x="id", by.y="id")


#Precipitation----------------------------------------------
precip <- "/mnt/sciclone-aiddata/REU/projects/kfw/extracts/terrestrial_precipitation/extract_merge.csv"
precip <- read.csv(precip)

for (i in 2:length(precip))
{
  splt <- strsplit(colnames(precip)[i],"_")
  splt[[1]][1] <- sub("X","",splt[[1]][1])
  month = splt[[1]][2]
  year = splt[[1]][1]
  dt = paste(year,"-",month,sep="")
  colnames(precip)[i] <- dt
}

precip_ts <- melt(precip,id="id")
precip_ts <- cSplit(precip_ts, "variable", "-")
precip_ts_mean <- aggregate(value ~ variable_1 + id, precip_ts, FUN=mean)
precip_ts_max <- aggregate(value ~ variable_1 + id, precip_ts, FUN=max)
precip_ts_min <- aggregate(value ~ variable_1 + id, precip_ts, FUN=min)
precip_mean <- reshape(precip_ts_mean, idvar=c("id"), direction="wide", timevar="variable_1")
precip_max <- reshape(precip_ts_max, idvar=c("id"), direction="wide", timevar="variable_1")
precip_min <- reshape(precip_ts_min, idvar=c("id"), direction="wide", timevar="variable_1")

#Rename vars
for (i in 2:length(air_temp_mean))
{
  colnames(precip_mean)[i] <- sub("value.","MeanP_",colnames(precip_mean)[i])
  colnames(precip_max)[i] <- sub("value.","MaxP_",colnames(precip_max)[i])
  colnames(precip_min)[i] <- sub("value.","MinP_",colnames(precip_min)[i])
}

#ggplot() + geom_point(data=air_temp_ts_mean, aes(x=value, y=variable_1, colour=factor(id))) + scale_fill_manual(values=c("blue","cyan4"))
#Merge it in
kfw.SPDF <- merge(kfw.SPDF, precip_mean, by.x="id", by.y="id")
kfw.SPDF <- merge(kfw.SPDF, precip_max, by.x="id", by.y="id")
kfw.SPDF <- merge(kfw.SPDF, precip_min, by.x="id", by.y="id")

#Fix size..
kfw.SPDF@data["terrai_are"] <- lapply(kfw.SPDF@data["terrai_are"], function(x) as.numeric(gsub("Ha","",x)))


#Distance to Rivers
Riv_dist <- "/mnt/sciclone-aiddata/REU/projects/kfw/extracts/rivers_dist/rivers_dist_sa.shp"
Riv_dist <- readShapePoly(Riv_dist)
Riv_dist <- Riv_dist@data

Riv_dist <- Riv_dist[c("id","dist")]

#Rename the columns for easier interpretation later..
colnames(Riv_dist)[2] <- "Riv_Dist"
#Merge it in
kfw.SPDF <- merge(kfw.SPDF, Riv_dist, by.x="id", by.y="id")

#Distance to Roads
Road_dist <- "/mnt/sciclone-aiddata/REU/projects/kfw/extracts/roads_dist/roads_dist_sa.shp"
Road_dist <- readShapePoly(Road_dist)
Road_dist<- Road_dist@data

Road_dist <- Road_dist[c("id","dist")]

#Rename the columns for easier interpretation later..
colnames(Road_dist)[2] <- "Road_dist"
#Merge it in
kfw.SPDF <- merge(kfw.SPDF, Road_dist, by.x="id", by.y="id")

writePolyShape(kfw.SPDF,"Processed_Data/Matched_Indigenous_Lands_DemResults.shp")
