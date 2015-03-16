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

#Change it to a numeric value (it still technically thinks it's a string)
KFW_poly@data$terrai_are <- as.double(KFW_poly@data$terrai_are)

#Rename the field so we can reference it easier, later.
names(KFW_poly@data)[names(KFW_poly@data) == 'terrai_are'] <- "CommunityArea_Ha" 

#Set a seed so that the data can be re-created..
set.seed("427", kind=NULL, normal.kind=NULL)

#For now, create an imaginary NDVI value for each year between 1981 and 2013.
#First, initialize a counter to track the "year" we are creating data for.
year = 1981

#Now, make a loop that creates "dummy" data for each year.
#Note, to make this at least vaguely real we will be correlating
#each year with the previous year's information, rather than simply
#generating random data for each year.

#Generate year 1 randomly (because there isn't anything before it to go on):
#First, we create the variable name.  This means the column will end up "NDVI_1981"
col_name = paste("NDVI_",year, sep="")

#Now, we populate the column with a random value between 0 and 1- there are many ways to do this,
#The below gives you the most explicit control (runif(1) does a much simpler version of this):
#First, we seed it with random value between 0 and 10,000:
KFW_poly@data[col_name] = sample(10000, size=nrow(KFW_poly@data), replace=TRUE)
#Then we divide by the total maximum value (in our case, 10,000):
KFW_poly@data[col_name] = KFW_poly@data[col_name] / 10000

#We generated the first year, so we go ahead and move to the next one:
year = year + 1

#From here out, we are going to generate the next year based on the previous one,
#based on a truncated normal distribution.
#Note NDVI can never exceed 1 or decrease below 0 in our true dataset
#so those restrictions are enforced here.

while (year <= 2014)
{
  col_name = paste("NDVI_",year, sep="")
  last_year = year - 1
  last_year_name = paste("NDVI_",last_year, sep="")
  
  #Declare the new column to be similar to last year, based on an entirely arbitrary
  #normal distribution centered on 0 to at least allow for *some* possible shocks.
  KFW_poly@data[col_name] = KFW_poly@data[last_year_name] + rnorm(nrow(KFW_poly@data),mean=0,sd=0.1)
  
  #Replace values greater than 1 with 1, less than 0 with 0
  KFW_poly@data[col_name][KFW_poly@data[col_name]<0] <- 0
  KFW_poly@data[col_name][KFW_poly@data[col_name]>1] <- 1
  
  #Go to the next year and do it again!
  year = year + 1
  
}

#Assign every unit of observation a random "Entry Year" and "Accepted" year.
#Note the slightly different use of sample here, as we want an integer (not a double [i.e., a decimal])
KFW_poly@data["Entry_Year"] = sample(1995:2005, size=nrow(KFW_poly@data), replace=TRUE)
KFW_poly@data["Accepted_Year"] = KFW_poly@data["Entry_Year"] + sample(1:9, size=nrow(KFW_poly@data), replace=TRUE)

#Here we make up a fake dummy value for each state.  
#This is so we can include it as a factor later (i.e., a Fixed Effect)
#5 states are defined, entirely arbitrarily.
KFW_poly@data["State"] = sample(1:5, size=nrow(KFW_poly@data), replace=TRUE)


#We now have a dataset with the following relevant attributes:
#Entry_Year - the year the treatment began
#Accepted_Year - the year the community was accepted formally
#NDVI_1981 to NDVI_2014 - mock NDVI data for each year
#CommunityArea_Ha - the physical size of each community
#State - an entirely made up state dummy, ranging from 1-5.

#Save the file to a CSV and shapefile for analysis in other programs.
write.table(KFW_poly@data, file="/home/aiddata/Desktop/R_Repo/KFW/Outputs/KFW_poly.csv", sep=",")
writePolyShape(KFW_poly,"/home/aiddata/Desktop/R_Repo/KFW/Outputs/KFW_poly")

#-----------------------------------------
#-----------------------------------------
#PSM
#-----------------------------------------
#-----------------------------------------
#Calculate the NDVI trend pre-treatments:
KFW_poly@data["NDVI_trend"] <- KFW_poly@data["NDVI_1995"] - KFW_poly@data["NDVI_1981"]

#The PSM is based on the anticipated year of treatment.
#In a crossectional, it is defined as:
PSM_model = lm(Accepted_Year ~ Entry_Year + NDVI_trend + NDVI_1995 + CommunityArea_Ha + factor(State), data=KFW_poly@data)

#View the model results:
summary(PSM_model)

#Run the model on our dataframe and record the PSM results (probability of receiving treatment)
KFW_poly@data$PSM <- predict(PSM_model,KFW_poly@data)





