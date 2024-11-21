library(tidyverse) # for manipulating the data frame
require(scales) # for legend scaling
library(ggmap) # for making map's plots (install using devtools::install_github("dkahle/ggmap"))
library(corrplot) # for the correlation plot


plotHousing = function(region,housingDf,whatPlot,contrast){
  
  # This function creates a plot of the median house value for the given region 
  # Args:
  #       region: region for which we want the plot (whole, bayArea, or else redding)
  #       housingDf: the dataframe of the specified region 
  #       whatPlot: choose between the plot of the predictions or the actual values
  #       contrast: a coefficient from 0 to 1 to specify the contrast of the dots within the plot
  # Return: a list with rmse,smape scores and the predicted values
  
  statesUS = map_data("state")
  calDf = statesUS %>% filter(region == "california")
  
  if(region=="whole"){
    calBasePlot = ggplot(data = calDf, mapping = aes(x = long, y = lat)) + 
      coord_quickmap() +  geom_polygon(color = "black", fill = "white")
  }
  else if(region=="bayArea"){
    calBasePlot = ggplot(data = calDf, mapping = aes(x = long, y = lat)) + 
      coord_quickmap(xlim=c(-123.56,-121.2),ylim=c(36.85,38.87)) +  geom_polygon(color = "black", fill = "white")
  }
  else{
    calBasePlot = ggplot(data = calDf, mapping = aes(x = long, y = lat)) + 
      coord_quickmap(xlim=c(-124.45,-119.99),ylim=c(39.59,42.01)) +  geom_polygon(color = "black", fill = "white")
  }
  
  if(whatPlot=="actual"){
    HousingPlot = calBasePlot + geom_point(data =  housingDf , aes(x = longitude, y = latitude, color = exp(logMedianHouseValue), size = population),alpha = contrast)+
      scale_color_distiller(palette = "Spectral",labels=comma)+
      xlab("Longitude") +
      ylab("Latitude") +
      labs(color = "Median House Value (in $USD)", size = "Population")  
  }
  else{
    HousingPlot = calBasePlot + geom_point(data =  housingDf , aes(x = longitude, y = latitude, color = prediction, size = population),alpha = contrast)+
      scale_color_distiller(palette = "Spectral",labels=comma)+
      xlab("Longitude") +
      ylab("Latitude") +
      labs(color = "Median House Value (in $USD)", size = "Population")
  }
  
  HousingPlot
}


correlationPlot = function(housingDf){
  
  # this function creates a correlation plot of the non-spatial covariates
  
  col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
  cPlot = corrplot(cor(housingDf[4:9]), method="color", col=col(200),  
                   type="upper", order="hclust", 
                   addCoef.col = "black", 
                   tl.col="black", tl.srt=45, 
                   diag=FALSE
  )
  
  cPlot
  
}


plotHousing("whole",calHousing,"actual",0.5)
correlationPlot(calHousing)
plotHousing("bayArea",bayArea,"actual",0.5)
correlationPlot(bayArea)
plotHousing("redding",redding,"actual",0.8)
correlationPlot(redding)


# Plot the predictions

dfCaliforniaPredictions = calHousing[!splitTrainTestCalifornia,]
dfCaliforniaPredictions$prediction = bestCalifornia[[2]]
plotHousing("whole",dfCaliforniaPredictions,"prediction",0.5)

dfBayAreaPredictions = bayArea[!splitTrainTestBayArea,]
dfBayAreaPredictions$prediction = bestBayArea[[2]]
plotHousing("bayArea",dfBayAreaPredictions,"prediction",0.5)

dfReddingPredictions = redding[!splitTrainTestRedding,]
dfReddingPredictions$prediction = bestRedding[[2]]
plotHousing("redding",dfReddingPredictions,"prediction",0.5)

