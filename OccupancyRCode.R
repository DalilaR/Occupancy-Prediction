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
                  tuneGrid = data.frame(k = 10),
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
#Trying with the datatraing in the Occupancy Article
#When I found the dataset, I didn't have access to this one, which the authors used 
#to build their model.  I wanted to know why my performance was much better?  Did it have to do
# with his sample.  In here I am using his sample with down sampling, but withing the training
#
names(datatraining)<- c("date","Temp","Humidity","Light","CO2","HumidityRatio","Occupancy")
datatraining$Occupancy <- factor(datatraining$Occupancy)
#In this approach I will use datatest, and split it into training, testing, and evaluating.
#We are assuming that's all we have 
split1<- createDataPartition(datatraining$Occupancy,p=0.7)[[1]]
other <- datatraining[-split1,]
training <- datatraining[split1,]

summary(training[,2:7])
set.seed(934)
split2<- createDataPartition(other$Occupancy,p=1/3)[[1]]
evaluating <- other[split2,]
testing <- other[-split2,]
library("DMwR")
set.seed(1130)
ctrl <- trainControl(method = "repeatedcv", number = 10,repeats = 3)
ctrl$sampling <- "down"
rf_fit <- train(Occupancy~Temp+CO2+Humidity+Light, data =training,
                method = "rf",
                ntree = 50,
                trControl = ctrl)
result = predict(rf_fit,testing)
confusionMatrix(testing$Occupancy,result,positive="1")
result = predict(rf_fit,evaluating)
confusionMatrix(evaluating$Occupancy,result,positive="1")
result = predict(rf_fit,OccupancyTestingSet)
confusionMatrix(OccupancyTestingSet$Occupancy,result,positive="1")
result = predict(rf_fit,OccupancyValidationSet)
confusionMatrix(OccupancyValidationSet$Occupancy,result,positive="1")
# Accuracy 97.86 ( more work but worth it)
result = predict(rf_fit,datatest)
confusionMatrix(datatest$Occupancy,result,positive="1")

# Now we are told that datatraining is the training dataset
# I will still sample down
set.seed(1130)
ctrl <- trainControl(method = "repeatedcv", number = 10,repeats = 3)
ctrl$sampling <- "down"
rf_fit <- train(Occupancy~Temp+CO2+Humidity+Light, data =datatraining,
                method = "rf",
                ntree = 50,
                trControl = ctrl)

result = predict(rf_fit,OccupancyTestingSet)
confusionMatrix(OccupancyTestingSet$Occupancy,result,positive="1")
result = predict(rf_fit,OccupancyValidationSet)
confusionMatrix(OccupancyValidationSet$Occupancy,result,positive="1")
#Performance 94.52%.  In short, it always pays to 
result = predict(rf_fit,datatest)
confusionMatrix(datatest$Occupancy,result,positive="1")