#Making a heatmap of the results
psm_PairsC <- psm_Pairs
psm_PairsC@data["subID"] <- do.call(paste,c(psm_PairsC@data["SP_ID"],psm_PairsC@data["terrai_nom"],sep="_"))

row.names(psm_PairsC@data) <- psm_PairsC@data$subID

#Subset only the NDVI record
psm_PairsC <- psm_PairsC[14:44]


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
