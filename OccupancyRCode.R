library(ggplot2)
library(GGally)
library(caret)
library(randomForest)
#Please import datatest2,datatest, OccupancyTrainingSet, OccupancyTestingSet, OccupancyValidationSet
office <- datatest2
office$Occupancy <- factor(office$Occupancy)
#Plot a Matrix ScatterPlot
ggpairs(office[,c(2:7)])
#Make sure to add names to the variables in the datasets
names(OccupancyTrainingSet) <- c("Temperature","Humidity","Light","CO2","Occupancy")
names(OccupancyTestingSet)<- c("Temperature","Humidity","Light","CO2","Occupancy")
names(OccupancyValidationSet)<- c("Temperature","Humidity","Light","CO2","Occupancy")
names(datatest)<- c("Temperature","Humidity","Light","CO2","Occupancy")
#Make sure to change occupancy to a factor
datatest$Occupancy <- factor(datatest$Occupancy)
OccupancyTrainingSet$Occupancy <- factor(OccupancyTrainingSet$Occupancy)
OccupancyTestingSet$Occupancy <- factor(OccupancyTestingSet$Occupancy)
OccupancyValidationSet$Occupancy <- factor(OccupancyValidationSet$Occupancy)
#KNN requires scaling and centring the predictors
ctrl <- trainControl(method = "repeatedcv", number = 10,repeats = 3)
Knn_fit <- train(Occupancy~Temp+CO2+Humidity+Light, data =OccupancyTrainingSet,
                  method = "knn",
                  preProcess = c("scale","center"),
                  trControl = ctrl)
result = predict(Knn_fit,OccupancyTestingSet)
confusionMatrix(OccupancyTestingSet$Occupancy,result,positive="1")
result = predict(Knn_fit,OccupancyValidationSet)
confusionMatrix(OccupancyValidationSet$Occupancy,result,positive="1")
result = predict(Knn_fit,datatest)
confusionMatrix(datatest$Occupancy,result,positive="1")
#RandomForest 
ctrl <- trainControl(method = "repeatedcv", number = 10,repeats = 3)
rf_fit <- train(Occupancy~Temp+CO2+Humidity+Light, data =OccupancyTrainingSet,
                 method = "rf",
                 ntree = 50,
                 trControl = ctrl)
result = predict(rf_fit,OccupancyTestingSet)
confusionMatrix(OccupancyTestingSet$Occupancy,result,positive="1")
result = predict(rf_fit,OccupancyValidationSet)
confusionMatrix(OccupancyValidationSet$Occupancy,result,positive="1")
result = predict(rf_fit,datatest)
confusionMatrix(datatest$Occupancy,result,positive="1")
