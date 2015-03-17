library(sp) #spatial data handling
library(GISTools)
library(FNN) #For Knn calculations

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

#Make copies of the dataframe for use in the PSM..
f.SPDF <- KFW_poly

#Run the model on our dataframe and record the PSM results (estimated year of treatment)
f.SPDF@data$PSM <- predict(PSM_model,KFW_poly@data)

#Create a new version of the data frame which records only matched pairs.
#Further, add a column to keep track of the matches.
m.SPDF = f.SPDF
m.SPDF$match = 0

#add an ID column to track matches in the m.SPDF frame, and to remove from f.SPDF  
m.SPDF$PSM_ID <- seq_len(nrow(m.SPDF))
f.SPDF$PSM_ID <- seq_len(nrow(f.SPDF))

#add a distance column to track the PSM distance for later analysis
m.SPDF$PSM_distance <- -1

#add a match pair so we can view what was matched with what.
m.SPDF$PSM_match_ID <- -1

#Count the total number of areas we're matching (/2, as each match excludes two areas)
cnt = length(m.SPDF) / 2

#Loop through all treatment cases to find a match.
for (j in 1:cnt)
{
  
  #Run the KNN for all neighbors.  We want to optimize the total distance between all units PSM scores
  #to be as low as possible.  Thus, we run the full set, choose the lowest distance, drop both pairs, repeat.
  #Note we calculate both the 1st closest neighbor (always yourself) and the second closest to make referencing
  #easier...
  k <- get.knnx(f.SPDF@data$PSM, f.SPDF@data$PSM, 2)

  #Add the matched treatment and control values to the m.SPDF data frame
  best_m = as.matrix(apply(k$nn.dist, 2, which.min))[2]
  
  #Control PSM ID
  Control_ID = f.SPDF@data$PSM_ID[as.matrix(apply(k$nn.dist, 2, which.min))[2]]
  
  
  #Treatment PSM ID
  k_match_id = k[[1]][best_m,][2]
  Treatment_ID = f.SPDF@data$PSM_ID[k_match_id]
  
  
  #Add the Treatment ID to the Control Row 
  m.SPDF@data$match[which(m.SPDF@data$PSM_ID == Control_ID)] = Treatment_ID
  m.SPDF@data$PSM_distance[which(m.SPDF@data$PSM_ID == Control_ID)] = k$nn.dist[,2][k_match_id]
  m.SPDF@data$PSM_match_ID[which(m.SPDF@data$PSM_ID == Control_ID)] = j
  
  #Add the Control ID to the Treatment Row
  m.SPDF@data$match[which(m.SPDF@data$PSM_ID == Treatment_ID)] = Control_ID
  m.SPDF@data$PSM_distance[which(m.SPDF@data$PSM_ID == Treatment_ID)] = k$nn.dist[,2][k_match_id]
  m.SPDF@data$PSM_match_ID[which(m.SPDF@data$PSM_ID == Treatment_ID)] = j
  
  #Drop the paired match out of the f.SPDF matrix 
  f.SPDF@data <- f.SPDF@data[f.SPDF@data$PSM_ID != Treatment_ID ,]
  f.SPDF@data <- f.SPDF@data[f.SPDF@data$PSM_ID != Control_ID ,]
  
}

#Create a copy of the data to drop observations out of...
KFW_df = m.SPDF

#Drop any un-paired observations
KFW_df@data <- KFW_df@data[KFW_df@data$PSM_match_ID != -1 ,]

#Estimate the balance equations, and check if IVs of interest are significant...
IVA = lm(Entry_Year ~ Accepted_Y + factor(PSM_match_ID), KFW_df)
IVA_pVal = summary(IVA)$coefficients[,4][[2]]

IVB = lm(NDVI_trend ~ Accepted_Y + factor(PSM_match_ID), KFW_df)
IVB_pVal = summary(IVB)$coefficients[,4][[2]]

IVC = lm(NDVI_1995 ~ Accepted_Y + factor(PSM_match_ID), KFW_df)
IVC_pVal = summary(IVC)$coefficients[,4][[2]]

IVD = lm(CommunityA ~ Accepted_Y + factor(PSM_match_ID), KFW_df)
IVD_pVal = summary(IVD)$coefficients[,4][[2]]

print("Significance Tests for Each Balance Test:")
print("IVA:")
print(IVA_pVal)
print("IVB:")
print(IVB_pVal)
print("IVC:")
print(IVC_pVal)
print("IVD:")
print(IVD_pVal)

#Finally, estimate the final stage equation
KFW_df@data$NDVI_Outcome = (KFW_df@data$NDVI_2014 - KFW_df@data$NDVI_1995)

Bmodel = lm(NDVI_Outcome ~ Accepted_Y + Entry_Year + CommunityA + factor(State) + factor(PSM_match_ID), KFW_df@data)

summary(Bmodel)
