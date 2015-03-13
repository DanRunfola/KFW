#Data Generation Script for KFW

#This script will eventually load all data into a primary database for analysis
#In the near term, this script will generate random data for use in analysis.

#Libraries
library(sp) #spatial data handling
library(GISTools)


#First, load the shapefile with all communities that were at least approved.
KFW_shp = "/home/aiddata/Desktop/R_Repo/KFW/Input_Data/Matched_Indigenous_Lands.shp"
KFW_poly = readShapePoly(KFW_shp)

#Remove the "Ha" from the area field, so we can interpret it as a numeric value.
KFW_poly@data$terrai_are <- sub('Ha', '',KFW_poly@data$terrai_are)

