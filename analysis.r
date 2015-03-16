#Load in the shapefile generated via the data generation script.
KFW_shp = "/home/aiddata/Desktop/R_Repo/KFW/Outputs/KFW_poly.shp"
KFW_poly = readShapePoly(KFW_shp)

#-----------------------------------------
#-----------------------------------------
#PSM
#-----------------------------------------
#-----------------------------------------
#Calculate the NDVI trend pre-treatments:
KFW_poly@data["NDVI_trend"] <- KFW_poly@data["NDVI_1995"] - KFW_poly@data["NDVI_1981"]

#The PSM is based on the anticipated year of treatment.
#In a crossectional, it is defined as:
PSM_model = lm(Accepted_Y ~ Entry_Year + NDVI_trend + NDVI_1995 + CommunityA + factor(State), data=KFW_poly@data)

#View the model results:
summary(PSM_model)

#Run the model on our dataframe and record the PSM results (probability of receiving treatment)
KFW_poly@data$PSM <- predict(PSM_model,KFW_poly@data)