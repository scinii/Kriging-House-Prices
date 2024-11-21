library(glmnet) # Lasso Regression
library(readr) # read csv file
library(dplyr) # data manipulation
library(gstat) # for kriging
library(Metrics) # for performance scores
library(tidyverse) # for manipulating the data frame
library(dismo) # for k-fold cross validation
library(sf) # for converting coordinates
library(pls) # for pcr
set.seed(42) # for reproducibility 
setwd('C:\\Users\\rober\\Desktop\\Stats\\Project') # set working directory

calHousing = read.csv("calHousing.csv")
calHousing = as_data_frame( calHousing %>% 
                              group_by(longitude,latitude) %>%  # there are duplicate locations so we group them
                                                                # and take the average or median of the (in)dependent variables.
                              summarise(
                                housingMedianAge = median(housingMedianAge),
                                totalRooms = mean(totalRooms),
                                totalBedrooms = mean(totalBedrooms),
                                population = mean(population),
                                households = mean(households),
                                medianIncome = median(medianIncome),
                                medianHouseValue = median(medianHouseValue),
                                .groups = "drop"
                              ) )
calHousing['medianIncome'] = calHousing$medianIncome*10000 # we multiply by 10000 the house price to have it in dollars as the house
calHousing['logMedianHouseValue'] = log(calHousing$medianHouseValue) # log-scale the price 
calHousing = subset(calHousing,select = -c(medianHouseValue))
calHousing = calHousing %>% relocate(logMedianHouseValue) 
calHousing = calHousing %>% relocate(latitude) %>% relocate(longitude)



getDataRegion=function(californiaData,specificRegion){ 
  
  #Slice the California dataframe to obtain the data of a specific region (either BayArea or Redding)
  
  if(specificRegion=="BayArea"){
    regionalData = californiaData[ californiaData$longitude > -123.56 &  californiaData$longitude< -121.2 & californiaData$latitude > 36.85 &  californiaData$latitude< 38.87,]
  }
  else if(specificRegion=="Redding"){
    regionalData = californiaData[ californiaData$longitude > -124.45 &  californiaData$longitude< -119.99 & californiaData$latitude > 39.59 &  californiaData$latitude< 42.01,]
    
  }
  return(regionalData)
}


pcrKrigingPrediction = function(train,test,components,covariance){
  
  # performs Kriging with external drift provided by principal component regression (pcr)
  # Args:
  #       train: training sf dataframe
  #       test: testing sf dataframe
  #       components: number of components for pcr (if 6 then we get the ordinary least squares )
  #       covariance: covariance/variogram type of the gstat package
  # Return: a list with rmse,smape scores and the predicted values
  
  results = array(1:2)
  modelPCR = pcr( logMedianHouseValue ~. ,scale=TRUE,center=TRUE,data = as.data.frame(train)[1:7])
  train$resid = residuals(modelPCR)[,,components]
  fittedVariogram = fit.variogram(variogram(resid~1,train),model = vgm(covariance))
  residKriged = krige(resid ~ 1,locations = train,newdata = test,model = fittedVariogram,beta = 0) # predict residuals value 
                                                                                                   # beta is used to set the value of the mean (mean(residuals)=0)
  test$predi = exp(residKriged$var1.pred+predict(modelPCR, as.data.frame(test)[1:7], ncomp = components))
  results[1] = rmse(test$predi, exp(test$logMedianHouseValue )) #compute rmse
  results[2] = smape(test$predi, exp(test$logMedianHouseValue)) # compute smape
  return(list(results,test$predi))
}



lassoKrigingPrediction = function(train,test,covariance){
  
  # performs Kriging with external drift provided by lasso regression
  # Args:
  #       train: training sf dataframe
  #       test: testing sf dataframe
  #       covariance: covariance/variogram type of the gstat package
  # Return: a list with rmse,smape scores and the predicted values
  
  results = array(1:2)
  grid = 10^seq(9, -2, length = 100) # grid for selecting the best lambda for lasso
  matrixCovariates = model.matrix(logMedianHouseValue~.,as.data.frame(train)[,1:7] )[,-1] # adjust format for glmnet
  matrixTarget = train$logMedianHouseValue 
  testCovariates = model.matrix(logMedianHouseValue~.,as.data.frame(test)[,1:7] )[,-1] # adjust format for glmnet
  cv = cv.glmnet(matrixCovariates, matrixTarget, alpha = 0) # perform cross validation to get best lambda
  bestLambda = cv$lambda.min
  bestModel = glmnet(matrixCovariates, matrixTarget, alpha = 0, lambda = bestLambda,standardize = TRUE) # implement lasso (alpha=0) for the best lambda
  residuals = matrixTarget - predict(bestModel, s=bestLambda, newx =matrixCovariates ) # calculate the residuals from lasso 
  train$resid = residuals
  fittedVariogram = fit.variogram(variogram(resid~1,train),model = vgm(covariance))
  residKriged = krige(resid ~ 1,locations = train,newdata = test,model = fittedVariogram,beta = 0)
  test$predi = exp(predict(bestModel,s=bestLambda, newx =testCovariates ) + residKriged$var1.pred)
  results[1] = rmse( test$predi,exp(test$logMedianHouseValue)) 
  results[2] = smape(test$predi, exp(test$logMedianHouseValue)) 
  return(list(results,test$predi))
}


validation = function(train,components,nfolds,method){
  
  # performs k fold cross validation
  # Args:
  #       train: training sf dataframe
  #       components: number of principal components to use for pcr. If you want to use lasso then put none
  #       nfolds: number of folds to use 
  #       method: lasso or pcr
  # Return: a matrix of size 2*2*3 where [,,j] corresponds to the mean and variance of the scores for variogram/covariance j 
  
  
  results = array(1:8, c(2,2,3))
  covariances = c("Exp","Mat","Sph")
  score1 = c(1:nfolds)
  score2 = c(1:nfolds)
  for(i in 1:3){
    covariance = covariances[i]
    for(j in 1:nfolds){
      
      foldIndex = kfold(train,nfolds)
      trainFolds = train[!foldIndex==j,]
      valFold = train[foldIndex==j,]
      if(method=="pcr"){
        scores = pcrKrigingPrediction(trainFolds,valFold,components,covariance)
      }
      else if(method=="lasso"){
        scores = lassoKrigingPrediction(trainFolds,valFold,covariance)
      }
      score1[j] = scores[[1]][1]
      score2[j] = scores[[1]][2]
    }
    results[1,1,i] = round(mean(score1),2)
    results[1,2,i] = round(sd(score1),2)
    results[2,1,i] = round(mean(score2)*100,2)
    results[2,2,i] = round(sd(score2)*100,2)
  }
  return(results)
}



baselinePrediction = function(train,test,components,type){
  
  # performs lasso and pcr regression (NO KRIGING)
  # Args:
  #       train: training dataframe
  #       test: testing dataframe
  #       components: number of components for PCR
  #       type: "lasso" or else "pcr"
  # Return: an array with rmse,smape scores 
  
  
  results = c(1:2)
  
  if(type=="lasso"){
    grid = 10^seq(9, -2, length = 100) # grid for selecting the best lambda for lasso
    matrixCovariates = model.matrix(logMedianHouseValue~.,as.data.frame(train)[,1:7] )[,-1] # adjust format form glmnet
    matrixTarget = train$logMedianHouseValue 
    testCovariates = model.matrix(logMedianHouseValue~.,as.data.frame(test)[,1:7] )[,-1] # adjust format form glmnet
    cv = cv.glmnet(matrixCovariates, matrixTarget, alpha = 0) # perform cross validation to get best lambda
    bestLambda = cv$lambda.min
    bestModel = glmnet(matrixCovariates, matrixTarget, alpha = 0, lambda = bestLambda,standardize = TRUE) # implement lasso (alpha=0) for the best lambda
    prediction =predict(bestModel,s=bestLambda, newx =testCovariates )
   
    results[1] = rmse( exp(prediction),exp(test$logMedianHouseValue)) 
    results[2] = smape(exp(prediction), exp(test$logMedianHouseValue)) 
  }
  else{
    
    modelPCR = pcr(logMedianHouseValue ~. ,scale=TRUE,center=TRUE,data = as.data.frame(train)[1:7])
    prediction = predict(modelPCR, as.data.frame(test)[1:7], ncomp = components)
    results[1] = rmse( exp(prediction),exp(test$logMedianHouseValue)) 
    results[2] = smape(exp(prediction), exp(test$logMedianHouseValue))
    
  }
  return(results)
}




########################## WHOLE CALIFORNIA #############################################

calHousingsf = st_as_sf(calHousing, coords=c("longitude","latitude"))
st_crs(calHousingsf) = 3309 # set coordinate reference system 
splitTrainTestCalifornia = sample(c(TRUE, FALSE), nrow(calHousingsf), replace=TRUE, prob=c(0.7,0.3))
trainCalifornia = calHousingsf[splitTrainTestCalifornia,]
testCalifornia = calHousingsf[!splitTrainTestCalifornia,]
californiaM1 = validation(trainCalifornia,6,5,"pcr")
californiaM2 = validation(trainCalifornia,3,5,"pcr")
californiaM3 = validation(trainCalifornia,2,5,"pcr")
californiaM4 =  validation(trainCalifornia,none,5,"lasso")

#summary( pcr( logMedianHouseValue ~. ,scale=TRUE,center=TRUE,data = as.data.frame(trainCalifornia)[1:7]))
bestCalifornia = pcrKrigingPrediction(trainCalifornia,testCalifornia,2,"Sph")
baseLineCalifornia = baselinePrediction(trainCalifornia,testCalifornia,2,"pcr")
########################################################################################


########################## BAY AREA #############################################

bayArea = getDataRegion(calHousing,specificRegion="BayArea")
bayAreasf = st_as_sf(bayArea, coords=c("longitude","latitude"))
st_crs(bayAreasf) = 7132 
splitTrainTestBayArea = sample(c(TRUE, FALSE), nrow(bayAreasf), replace=TRUE, prob=c(0.7,0.3))
trainBayArea = bayAreasf[splitTrainTestBayArea,]
testBayArea = bayAreasf[!splitTrainTestBayArea,]

bayAreaM1 = validation(trainBayArea,6,7,"pcr")
bayAreaM2 = validation(trainBayArea,3,7,"pcr")
bayAreaM3 = validation(trainBayArea,2,7,"pcr")
bayAreaM4 = validation(trainBayArea,none,7,"lasso") 

# summary( pcr( logMedianHouseValue ~. ,scale=TRUE,center=TRUE,data = as.data.frame(trainBayArea)[1:7]))
bestBayArea = pcrKrigingPrediction(trainBayArea,testBayArea,2,"Sph")
baseLineBayArea = baselinePrediction(trainBayArea,testBayArea,2,"pcr")
##########################################################################################


####################### REDDING ########################################################

redding = getDataRegion(calHousing,specificRegion="Redding")
reddingsf = st_as_sf(redding, coords=c("longitude","latitude"))
st_crs(reddingsf) = 2225
splitTrainTestRedding = sample(c(TRUE, FALSE), nrow(reddingsf), replace=TRUE, prob=c(0.7,0.3))
trainRedding = reddingsf[splitTrainTestRedding,]
testRedding = reddingsf[!splitTrainTestRedding,]

reddingM1 = validation(trainRedding,6,10,"pcr")
reddingM2 = validation(trainRedding,3,10,"pcr")
reddingM3 = validation(trainRedding,2,10,"pcr")
reddingM4 = validation(trainRedding,none,10,"lasso")

#summary( pcr( logMedianHouseValue ~. ,scale=TRUE,center=TRUE,data = as.data.frame(trainRedding)[1:7]))
bestRedding = lassoKrigingPrediction(trainRedding,testRedding,"Mat")
baseLineRedding = baselinePrediction(trainRedding,testRedding,none,"lasso")
########################################################################################

