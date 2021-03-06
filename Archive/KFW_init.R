#Load required functions from SAT
source('Dependencies/dep.R', chdir=T)
source('Tools/descriptives.R', chdir=T)
source('Tools/SpatialCausalPSM.R', chdir=T)
source('functions.R', chdir=T)

#File for the KFW analysis
shpfile = "Input_Data/KFW/Matched_Indigenous_Lands_id.shp"
src_Shp = readShapePoly(shpfile)

#Clean the source Shapefile to remove extra columns of data.
cln_Shp <- src_Shp[,c("terrai_nom","terrai_are","reu_id","id")]

#Population -------------------------------------------
GPW_pop <- "/mnt/sciclone-aiddata/REU/projects/kfw/extracts/gpw/gpw_extract_merge.csv"
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
GIMMS_cont <- "/mnt/sciclone-aiddata/REU/projects/kfw/extracts/ndvi/ndvi_extract_year_max.csv"
GIMMS_cont <- read.csv(GIMMS_cont)
#Rename columns...
for (i in 2:length(GIMMS_cont))
{
  colnames(GIMMS_cont)[i] <- sub("X","NDVI",colnames(GIMMS_cont)[i])
}
#Merge
kfw.SPDF <- merge(kfw.SPDF, GIMMS_cont, by.x="id", by.y="id")

NDVI_sub = kfw.SPDF@data[c(1,8:40)]
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

#Air Temperature
air_temp <- "/mnt/sciclone-aiddata/REU/projects/kfw/extracts/terrestrial_air_temperature/air_temp_extract_merge.csv"
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

