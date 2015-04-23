#Script to identify a reasonable mask for NDVI using an input 
#raster and set of points known to be forest cover.
library(sp)
library(maptools)
library(raster)

#load points
NDVI_pts <- "/home/aiddata/Desktop/R_Repo/KFW/Input_Data/Dense_forest_points.shp"
NDVI_pts = readShapePoints(NDVI_pts)



shpfile = "Processed_Data/Matched_Indigenous_Lands_DemResults.shp"
dta_Shp = readShapePoly(shpfile)
plot(dta_Shp)

#Quickly look at point distributions
points(NDVI_pts, col="red")

#Raster year to use as baseline, folder:
folder = "/mnt/sciclone-aiddata/REU/data/gimms.gsfc.nasa.gov/MODIS/std/GMOD09Q1/tif/NDVI/2001"
days <- list.dirs(folder)
NDVI_vals <- NDVI_pts@data
for (i in 2:length(days))
{
  path = paste(days[i],"/",list.files(days[i]),sep="")
  MODIS_scene <- raster(path)
  NDVI_vals[i] <- extract(MODIS_scene,NDVI_pts,method="simple",df=TRUE)[2]
  print(days[i])
}


tot = 0
cnt = 0
mean_list = vector()
min_list = vector()
for (i in 2:length(colMeans(NDVI_vals,na.rm=TRUE)))
{
  tot = tot + colMeans(NDVI_vals,na.rm=TRUE)[[i]]
  cnt = cnt + 1
  min_list[i] <- min(NDVI_vals[i],na.rm=TRUE)
  mean_list[i] <- colMeans(NDVI_vals,na.rm=TRUE)[[i]]
}

avg = tot/cnt
print(avg)
print(min(min_list,na.rm=TRUE))
print(mean(min_list,na.rm=TRUE))


