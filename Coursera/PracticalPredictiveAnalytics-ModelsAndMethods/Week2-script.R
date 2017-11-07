# Coursera - Data science and analytics
# working with data from the SeaFlow environmental flow cytometry instrument.

#import required libs
library(caret)
library(rpart)
library(randomForest) 
library(e1071)

#######################################
# Step 1: Read and summarize the data #
#######################################
inputdata <- read.csv(file="~/Documents/Tutorials/PredictiveAnalytics/datasci_course/assignment5/seaflow_21min.csv",head=TRUE,sep=",")
attributes(inputdata)
summary(inputdata)
dim(inputdata)

######################################################
# Step 2: Split the data into test and training sets #
######################################################
trainIndex <- createDataPartition(inputdata$pe, times = 1, p = 0.5, list = FALSE)
head(trainIndex)
trainingData <- inputdata[trainIndex, ]
testData <- inputdata[-trainIndex, ]
dim(trainingData)
summary(trainingData)

##############################################################
# Step 3: Plot the data (pe = f(chl_small) and color by pop) #
##############################################################
ggplot(inputdata, aes(x=inputdata$chl_small, y=inputdata$pe, fill=inputdata$pop)) + geom_point(aes(colour=inputdata$pop))

#################################
# Step 4: Train a decision tree #
#################################
# Create a function of measurements of interest (i.e. fsc_small, fsc_perp, chl_small, pe, chl_big, chl_small as they are related to opticall measurements)
#fol <- formula(inputdata$pop ~ inputdata$fsc_small + inputdata$fsc_perp + inputdata$chl_small + inputdata$pe + inputdata$chl_big + inputdata$chl_small)
fol <- formula(pop ~ fsc_small + fsc_perp + chl_small + pe + chl_big + chl_small)
print(fol)

# Create the model based on recursive partition trees
model <- rpart(formula=fol, data=trainingData, method="class")
print(model)
# Examine the model results using cross-validation results
plotcp(model) 
summary(model)
# plot tree
plot(model, uniform=TRUE,main="Classification Tree for SeaFlow environmental flow cytometry instrument")
text(model, use.n=TRUE, all=TRUE, cex=.6)

#############################################################
# Step 5: Evaluate the decision tree model on the test data #
#############################################################
prediction <- predict(model, testData,type="class") 
# Compare prediction results with the real outputs of test data
prediction == testData$pop
summary(prediction == testData$pop)
# Compute accuracy
accuracy <- ( sum(prediction == testData$pop) / length(testData$pop) )
print(accuracy)

####################################################
# Step 6: Build and evaluate a random forest model #
####################################################
# Compute the model based on random forest
rf_model <- randomForest(formula =fol, data=trainingData)
# Evaluate the model on test data
rf_prediction <- predict(rf_model, testData,type="class") 
# Compare prediction results with the real outputs of test data
rf_prediction == testData$pop
summary(rf_prediction == testData$pop)
# Compute accuracy
rf_accuracy <- ( sum(rf_prediction == testData$pop) / length(testData$pop) )
print(rf_accuracy)

# Compute importance of the model (i.e. Gini impurty)
print(importance(rf_model))
# plot giny impurty of tested  factors
varImpPlot(rf_model)

####################################################################
# Step 7: Train a support vector machine model and compare results #
####################################################################
# Compute the model based on SVM
svm_model <- svm(formula=fol, data=trainingData)
# Evaluate the svm_ model on test data
svm_prediction <- predict(svm_model, testData,type="class") 
# Compare svm prediction results with the real outputs of test data
svm_prediction == testData$pop
summary(svm_prediction == testData$pop)
# Compute accuracy
svm_accuracy <- ( sum(svm_prediction == testData$pop) / length(testData$pop) )
print(svm_accuracy)


########################################
# Step 8: Construct confusion matrices #
########################################
# compute confusion matrix for decision tree method
print( confusionMatrix(prediction, testData$pop) )

# compute confusion matrix for random forest method
print( confusionMatrix(rf_prediction, testData$pop) )

# compute confusion matrix for svm method
print( confusionMatrix(svm_prediction, testData$pop) )

#
table(pred = predictions, true = testingdata$pop)

#################################
# Step 9: Sanity check the data #
#################################
print(summary(inputdata$fsc_small))
print(summary(inputdata$fsc_big))
print(summary(inputdata$fsc_perp))
print(summary(inputdata$pe))
print(summary(inputdata$chl_small))
print(summary(inputdata$chl_big))

ggplot(inputdata, aes(x=inputdata$chl_big, y=inputdata$time)) + geom_point(aes(colour=inputdata$pop))

#################################################################
# Step 10: remove incorrect data and reprocess the cleaned data #
#################################################################
# Remove data associated with file_id 208 and recomputethe three models and their accuracy
filteredInputData <- inputdata[which(inputdata$file_id!="208"),]
ggplot(filteredInputData, aes(x=filteredInputData$chl_big, y=filteredInputData$time)) + geom_point(aes(colour=filteredInputData$pop))

######################################################
# A: Split the data into test and training sets #
######################################################
f_trainIndex <- createDataPartition(filteredInputData$pe, times = 1, p = 0.5, list = FALSE)
head(f_trainIndex)
f_trainingData <- filteredInputData[f_trainIndex, ]
f_testData <- filteredInputData[-f_trainIndex, ]
dim(f_trainingData)
summary(f_trainingData)

##############################################################
# Step 3: Plot the data (pe = f(chl_small) and color by pop) #
##############################################################
ggplot(filteredInputData, aes(x=filteredInputData$chl_small, y=filteredInputData$pe, fill=filteredInputData$pop)) + geom_point(aes(colour=filteredInputData$pop))

#################################
# Step 4: Train a decision tree #
#################################
# Create a function of measurements of interest (i.e. fsc_small, fsc_perp, chl_small, pe, chl_big, chl_small as they are related to opticall measurements)
#fol <- formula(inputdata$pop ~ inputdata$fsc_small + inputdata$fsc_perp + inputdata$chl_small + inputdata$pe + inputdata$chl_big + inputdata$chl_small)
f_fol <- formula(pop ~ fsc_small + fsc_perp + chl_small + pe + chl_big + chl_small)
print(f_fol)

# Create the model based on recursive partition trees
f_model <- rpart(formula=f_fol, data=f_trainingData, method="class")
print(f_model)
# Examine the model results using cross-validation results
plotcp(f_model) 
summary(f_model)
# plot tree
plot(f_model, uniform=TRUE,main="Classification Tree for SeaFlow environmental flow cytometry instrument")
text(f_model, use.n=TRUE, all=TRUE, cex=.6)

#############################################################
# Step 5: Evaluate the decision tree model on the test data #
#############################################################
f_prediction <- predict(f_model, f_testData,type="class") 
# Compare prediction results with the real outputs of test data
f_prediction == f_testData$pop
summary(f_prediction == f_testData$pop)
# Compute accuracy
f_accuracy <- ( sum(f_prediction == f_testData$pop) / length(f_testData$pop) )
print(f_accuracy)

####################################################
# Step 6: Build and evaluate a random forest model #
####################################################
# Compute the model based on random forest
f_rf_model <- randomForest(formula =f_fol, data=f_trainingData)
# Evaluate the model on test data
f_rf_prediction <- predict(f_rf_model, f_testData,type="class") 
# Compare prediction results with the real outputs of test data
f_rf_prediction == f_testData$pop
summary(f_rf_prediction == f_testData$pop)
# Compute accuracy
f_rf_accuracy <- ( sum(f_rf_prediction == f_testData$pop) / length(f_testData$pop) )
print(f_rf_accuracy)

# Compute importance of the model (i.e. Gini impurty)
print(importance(f_rf_model))
# plot giny impurty of tested  factors
varImpPlot(f_rf_model)

####################################################################
# Step 7: Train a support vector machine model and compare results #
####################################################################
# Compute the model based on SVM
f_svm_model <- svm(formula=f_fol, data=f_trainingData)
# Evaluate the svm_ model on test data
f_svm_prediction <- predict(f_svm_model, f_testData,type="class") 
# Compare svm prediction results with the real outputs of test data
f_svm_prediction == f_testData$pop
summary(f_svm_prediction == f_testData$pop)
# Compute accuracy
f_svm_accuracy <- ( sum(f_svm_prediction == f_testData$pop) / length(f_testData$pop) )
print(f_svm_accuracy)


########################################
# Step 8: Construct confusion matrices #
########################################
# compute confusion matrix for decision tree method
print( confusionMatrix(f_prediction, f_testData$pop) )

# compute confusion matrix for random forest method
print( confusionMatrix(f_rf_prediction, f_testData$pop) )

# compute confusion matrix for svm method
print( confusionMatrix(f_svm_prediction, f_testData$pop) )

#
table(pred = predictions, true = f_testingdata$pop)




