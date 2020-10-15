## To load the dataset
Bike <- read.csv("hour.csv")

##View the dataset
Bike 

#numerical summary of each of our variables
summary(Bike)

#structure of the data
str(Bike)
head(Bike)

#count of missing values(NA)
sum(is.na(Bike))

#data preparation
#converting char to dates
Bike$dteday=as.Date(Bike$dteday)
class(Bike$dteday)
View(Bike)

#as.factor, convert season and weathersit to factor
#converting int to factor w/ level
Bike$season=factor(Bike$season, labels = c("Spring", "Summer", "Fall", "Winter"))
table(Bike$season)

Bike$weathersit=factor(Bike$weather, labels = c("Good", "Normal", "Bad", "Very Bad"))
table(Bike$weathersit)

#structure of the data
str(Bike)


#Modelling - Splitting data before analysis
#Creating training set for 2011
bike_Train= subset(Bike,yr==0)
#Combined Testing and Validation
bike_TestandValid= subset(Bike,yr==1)
#Creating validation set of year 2012
bike_Valid = bike_TestandValid[sample(which(bike_TestandValid$yr==1),0.6*length(which(bike_TestandValid$yr==1))),]
#Creating testing set of year 2012
bike_Test = setdiff((bike_TestandValid),bike_Valid)

dim(bike_Train)

#Using Linear regression

#1st model - cnt variable includes both casual and registered 
#dteday is a dependent variable but consists of other overlapping variable  
#Hence we dont consider those variables for our model
model_lm1 = lm(cnt~.-instant-casual-registered-yr-dteday, data=bike_Train)
summary(model_lm1)

#2nd Model - many insignificant variables
#so removing the weekday variables as the p value is the highest, 0.843007
model_lm2 = lm(cnt~.-instant-casual-registered-yr-dteday-weekday, data=bike_Train)
summary(model_lm2)

#3rd Model - remove the atemp variables as the p value is the high, 0.475687
model_lm3 = lm(cnt~.-instant-casual-registered-yr-dteday-weekday-atemp, data=bike_Train)
summary(model_lm3)


#After removing weekday, the Adjusted R-squared remains the same, 
#but the Multiple R-square reduces slightly.
#bias between them becomes smaller. It means the model is improved.
#remove the workingday variables as the p value is the high, 0.23899
model_lm4 = lm(cnt~.-instant-casual-registered-yr-dteday-weekday-atemp-workingday, data=bike_Train)
summary(model_lm4)


#the Adjusted R-squared stops increasing and reduce slightly,
#removing workingday does not improve the model. However, workingday is not significant. 
model_lm5 = lm(cnt~.-instant-casual-registered-yr-dteday-weekday-atemp-workingday-windspeed, data=bike_Train)
summary(model_lm5)


#using the Pearson's Chi-squared test to check correlation between month and seasons variables
chisq.test(bike_Train$mnth, bike_Train$season, correct = FALSE)

#since the p-value less than 0.05(cutoff value),
#the variables, mnth and season are dependent to each other.mnth or season should be removed. 
#Season removed as one of its level is not significant.

model_lm6 = lm(cnt~.-instant-casual-registered-yr-dteday-weekday-atemp-workingday-windspeed-season, data=bike_Train)
summary(model_lm6)

chisq.test(bike_Train$mnth, bike_Train$holiday, correct = FALSE)
chisq.test(bike_Train$mnth, bike_Train$temperature, correct = FALSE)

#Multiple R-squared and the Adjusted R-squared stops increasing and reduce a little
#Informing us that weathersit does not improve the model. 
#therefore we stick with the 3rd model after which the decrease started

#Model3 - prediction check its error level
test.pred.lin=predict(model_lm3,bike_Test)-1
valid.pred.lin=predict(model_lm3,bike_Valid)-1

#Assessing better fit using errors
#Mean Squared Error
MSE.lin.reg=mean(test.pred.lin-bike_Test$cnt)^2
MSE.lin.reg.valid = mean(valid.pred.lin-bike_Valid$cnt)^2
MSE.lin.reg
MSE.lin.reg.valid

#Root Mean Squared Error
RMSE.lin.reg=sqrt(mean(test.pred.lin-bike_Test$cnt)^2)
RMSE.lin.reg.valid=sqrt(mean(valid.pred.lin-bike_Valid$cnt)^2)
RMSE.lin.reg
RMSE.lin.reg.valid

#Mean Absolute Error
MAE.lin.reg=mean(abs(test.pred.lin-bike_Test$cnt))
MAE.lin.reg.valid=mean(abs(valid.pred.lin-bike_Valid$cnt))
MAE.lin.reg 
MAE.lin.reg.valid

#Sum of Squared Prediction Error
SSE.lin.reg = sum((test.pred.lin - bike_Test$cnt)^2)
SSE.lin.reg.valid = sum((valid.pred.lin - bike_Valid$cnt)^2)
SSE.lin.reg
SSE.lin.reg.valid 

#Regression tree 
library(rpart)
library(rpart.plot)

#Prediction using Model 3 
#cnt -  function of month, hour, holiday, weather situation, temperature and humidity
Bike_tree=rpart(cnt~.-instant-casual-registered-yr-dteday-weekday-atemp, data=bike_Train)
Bike_tree

#Regression tree
rpart.plot(Bike_tree,type=3,digits = 3,fallen.leaves = TRUE)
#Decision tree shows month hour and temperature

#using tree to predict
tree.pred = predict(Bike_tree, newdata=bike_Test)
valid.pred = predict(Bike_tree, newdata=bike_Valid)

#Checking accuracy using errors
##Mean Squared Error
MSE.CART=(mean((tree.pred-bike_Test$cnt)^2))
MSE.CART.valid=(mean((valid.pred-bike_Valid$cnt)^2))
MSE.CART 
MSE.CART.valid

#Root Mean Squared Error
RMSE.CART=sqrt(mean((tree.pred-bike_Test$cnt)^2))
RMSE.CART.valid=sqrt(mean((valid.pred-bike_Valid$cnt)^2))
RMSE.CART 
RMSE.CART.valid

#Mean Absolute Error
MAE.CART=mean(abs(tree.pred-bike_Test$cnt))
MAE.CART.valid=mean(abs(valid.pred-bike_Valid$cnt))
MAE.CART 
MAE.CART.valid

#Sum of Squared Prediction Error
tree.sse = sum((tree.pred - bike_Test$cnt)^2)
tree.sse.valid = sum((valid.pred - bike_Valid$cnt)^2)
tree.sse
tree.sse.valid

#The tree.sse is 202215226 which is lower than we received in linear model i.e  SSE= 320061531
#We also notice the MAE and MSE error values for regression trees which are lower than Linear regression
#So we consider regression trees better than linear regression 


#Cross Validation
library(caret)
library(e1071)

#define train control of 10 folds
train_control = trainControl(method = "cv", number = 10)

#cross validation fitting

#changing factor variables to numerical variables
Bike$season=as.numeric(Bike$season)
bike_Train$season=as.numeric(bike_Train$season)
bike_Test$season=as.numeric(bike_Test$season)
bike_Valid$season=as.numeric(bike_Valid$season)
Bike$weathersit=as.numeric(Bike$weathersit)
bike_Train$weathersit=as.numeric(bike_Train$weathersit)
bike_Test$weathersit=as.numeric(bike_Test$weathersit)
bike_Valid$weathersit=as.numeric(bike_Valid$weathersit)
str(bike_Train)
str(bike_Test)
str(bike_Valid)
Bike_Tree.2=train(cnt~+season+mnth+hr+holiday+workingday+weathersit+temp+hum+windspeed, 
  data=bike_Train, trControl=train_control, method="rpart")
Bike_Tree.2   

Tree2 = Bike_Tree.2$finalModel

#plot
rpart.plot(Tree2)

#prediction using Tree2
Tree2_pred = predict(Tree2, newdata=bike_Test)  
Tree2_pred.valid = predict(Tree2, newdata=bike_Valid)  

#Checking accuracy using errors
#Sum of Squared Prediction Error
Tree_sse2 = sum((Tree2_pred - bike_Test$cnt)^2)
Tree_sse2.valid = sum((Tree2_pred.valid - bike_Valid$cnt)^2)
Tree_sse2
Tree_sse2.valid

#Root Mean Squared Error
RMSE_CV1=sqrt(mean((Tree2_pred-bike_Test$cnt)^2))
RMSE_CV1.valid=sqrt(mean((Tree2_pred.valid-bike_Valid$cnt)^2))
RMSE_CV1 
RMSE_CV1.valid

##Mean Squared Error
MSE_CV1=(mean((Tree2_pred-bike_Test$cnt)^2))
MSE_CV1.valid=(mean((Tree2_pred.valid-bike_Valid$cnt)^2))
MSE_CV1 
MSE_CV1.valid

#Mean Absolute Error
MAE_CV1=mean(abs(Tree2_pred-bike_Test$cnt))
MAE_CV1.valid=mean(abs(Tree2_pred.valid-bike_Valid$cnt))
MAE_CV1 
MAE_CV1.valid

#Random forest 
library(dplyr)
library(ggplot2)
library(randomForest)

#changing factor variables to numerical variables
str(bike_Train)
str(bike_Test)
set.seed(123)
tree_rf=randomForest(cnt~+season+mnth+hr+holiday+workingday+weathersit+temp+hum+windspeed, data=bike_Train,importance=TRUE,ntree=500)
tree_rf

varImpPlot(tree_rf)
which.min(tree_rf$mse)
plot(tree_rf)

##Mean Squared Error
Imp <- as.data.frame(sort(importance(tree_rf)[,1],decreasing = TRUE),optional = T)
names(Imp)="%Inc MSE"
Imp

#Predict
predict_rf=predict(tree_rf,bike_Test)
predict_rf_valid=predict(tree_rf,bike_Valid)

#Checking accuracy using errors

##Root Mean Squared Error
RMSE_forest=sqrt(mean((predict_rf-bike_Test$cnt)^2))
RMSE_forest_valid=sqrt(mean((predict_rf_valid-bike_Valid$cnt)^2))
RMSE_forest
RMSE_forest_valid

##Mean Squared Error
MSE_forest=mean((predict_rf-bike_Test$cnt)^2)
MSE_forest_valid=mean((predict_rf_valid-bike_Valid$cnt)^2)
MSE_forest
MSE_forest_valid

#Mean Absolute Error
MAE_forest=mean(abs(predict_rf- bike_Test$cnt))
MAE_forest_valid=mean(abs(predict_rf_valid- bike_Valid$cnt))
MAE_forest
MAE_forest_valid

#Sum of Squared Error
tree.sserf = sum((predict_rf - bike_Test$cnt)^2)
tree.sserf_valid = sum((predict_rf_valid - bike_Valid$cnt)^2)
tree.sserf
tree.sserf_valid

#Evaluation for the methods performed

Accuracy_test=data.frame(Method=c("Linear Regression","Regression Tree","Cross Validation","Random Forest"),
                    RMSE=c(RMSE.lin.reg,RMSE.CART,RMSE_CV1,RMSE_forest),
                    MAE=c(MAE.lin.reg,MAE.CART,MAE_CV1,MAE_forest),
                    MSE=c(MSE.lin.reg,MSE.CART,MSE_CV1,MSE_forest),
                    SSE=c(SSE.lin.reg,tree.sse,Tree_sse2,tree.sserf))

Accuracy_valid=data.frame(Method=c("Linear Regression","Regression Tree","Cross Validation","Random Forest"),
                    RMSE=c(RMSE.lin.reg.valid,RMSE.CART.valid,RMSE_CV1.valid,RMSE_forest_valid),
                    MAE=c(MAE.lin.reg.valid,MAE.CART.valid,MAE_CV1.valid,MAE_forest_valid),
                    MSE=c(MSE.lin.reg.valid,MSE.CART.valid,MSE_CV1.valid,MSE_forest_valid),
                    SSE=c(SSE.lin.reg.valid,tree.sse.valid,Tree_sse2.valid,tree.sserf_valid))

Accuracy_test
Accuracy_valid


Accuracy_valid$RMSE = round(Accuracy_valid$RMSE,2)
Accuracy_valid$MAE = round(Accuracy_valid$MAE,2)
Accuracy_valid$MSE = round(Accuracy_valid$MSE,2)
Accuracy_valid$SSE = round(Accuracy_valid$SSE,2)
Accuracy_valid

Accuracy_test$RMSE = round(Accuracy_test$RMSE,2)
Accuracy_test$MAE = round(Accuracy_test$MAE,2)
Accuracy_test$MSE = round(Accuracy_test$MSE,2)
Accuracy_test$SSE = round(Accuracy_test$SSE,2)
Accuracy_test


