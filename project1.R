## Machine Learning Project
## Author: M. Chitnis
## Date: July 24 , 2015
## Filename: project1.R

#Loading required packages
library(caret)
library(randomForest)
library(e1071)

setwd("/Users/milindchitnis/Documents/My Docs/Data Analytics/RDir/mlearning")

## Get and load data
trainUrl <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv"
testUrl <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv"

## Check if file exists, else download the data
if(!file.exists("./Data/pml-training.csv")){
  download.file(url = trainUrl, destfile = "./Data/pml-training.csv", method="curl")
}
if(!file.exists("./ProjData/pml-testing.csv")){
  download.file(url = testUrl, destfile = "./Data/pml-testing.csv", method="curl")
}

origTraining <- read.csv("./Data/pml-training.csv")
origTesting <- read.csv("./Data/pml-testing.csv")

## Review Data
names(origTraining)
str(origTraining)

## Renmove unwanted columns
uIndex <- grep("X|timestamp|user_name|new_window", names(origTraining));
origTraining <- origTraining[,-uIndex];
origTesting <- origTesting[,-uIndex];

## Remove ZeroVariance data
zIndex <- nearZeroVar(origTraining);
origTraining <- origTraining[,-zIndex];
origTesting <- origTesting[,-zIndex];

## Remove columns with more than 25% NA data
origTraining <- origTraining[,colSums(is.na(origTraining)) < nrow(origTraining)*0.75]
origTesting <- origTesting[,colSums(is.na(origTesting)) < nrow(origTesting)*0.75]

## Set seed and divide the training use createDataPartition and use 75% data for training
set.seed(12345)
partTrain = createDataPartition(origTraining$classe, p=0.75, list=FALSE)
trainData = origTraining[ partTrain,]
testData =  origTraining[-partTrain,]

## Use randomForest to fit a model for the split trainData 
modelFit <- randomForest(classe ~., data=trainData, p=0.60, list = FALSE)

## Fit the model for the split testData for predicting
modelPred <- predict(modelFit,testData)
confusionMatrix(modelPred, testData$classe);

## Predict values for original test values of 20 cases
finalPred <- predict(modelFit,origTesting);
finalPred

## Create output file for submission
answers <- as.vector(finalPred)

pml_write_files = function(x){
  n = length(x)
  for(i in 1:n){
    filename = paste0("./Output/out_",i,".txt")
    write.table(x[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
  }
}

## Run functon to create the output
pml_write_files(answers)


