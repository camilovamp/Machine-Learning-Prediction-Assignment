library(caret)
library(rpart)
library(rpart.plot)
library(RColorBrewer)
library(rattle)
library(randomForest)

#set the seed
set.seed(12345)

# set url variables with the respective url
# the read.csv, do not acept https so I use only http
trainUrl <- "http://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv"
testUrl <- "http://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv"

#read the files for the training and the testing
trainingcsv <- read.csv(url(trainUrl), na.strings=c("NA","#DIV/0!",""))
testingcsv <- read.csv(url(testUrl), na.strings=c("NA","#DIV/0!",""))

#Partition set for Training (60%) and Testing (40%)
inTrain <- createDataPartition(y=training$classe, p=0.6, list=FALSE)
training <- trainingcsv[inTrain, ]
testing <- testingcsv[-inTrain, ]

#clean the data
myDataNZV <- nearZeroVar(training, saveMetrics=TRUE)
#create another subset without any NZV variables
myNZVvars <- names(training) %in% c("new_window", "kurtosis_roll_belt", "kurtosis_picth_belt",
                                      "kurtosis_yaw_belt", "skewness_roll_belt", "skewness_roll_belt.1", "skewness_yaw_belt",
                                      "max_yaw_belt", "min_yaw_belt", "amplitude_yaw_belt", "avg_roll_arm", "stddev_roll_arm",
                                      "var_roll_arm", "avg_pitch_arm", "stddev_pitch_arm", "var_pitch_arm", "avg_yaw_arm",
                                      "stddev_yaw_arm", "var_yaw_arm", "kurtosis_roll_arm", "kurtosis_picth_arm",
                                      "kurtosis_yaw_arm", "skewness_roll_arm", "skewness_pitch_arm", "skewness_yaw_arm",
                                      "max_roll_arm", "min_roll_arm", "min_pitch_arm", "amplitude_roll_arm", "amplitude_pitch_arm",
                                      "kurtosis_roll_dumbbell", "kurtosis_picth_dumbbell", "kurtosis_yaw_dumbbell", "skewness_roll_dumbbell",
                                      "skewness_pitch_dumbbell", "skewness_yaw_dumbbell", "max_yaw_dumbbell", "min_yaw_dumbbell",
                                      "amplitude_yaw_dumbbell", "kurtosis_roll_forearm", "kurtosis_picth_forearm", "kurtosis_yaw_forearm",
                                      "skewness_roll_forearm", "skewness_pitch_forearm", "skewness_yaw_forearm", "max_roll_forearm",
                                      "max_yaw_forearm", "min_roll_forearm", "min_yaw_forearm", "amplitude_roll_forearm",
                                      "amplitude_yaw_forearm", "avg_roll_forearm", "stddev_roll_forearm", "var_roll_forearm",
                                      "avg_pitch_forearm", "stddev_pitch_forearm", "var_pitch_forearm", "avg_yaw_forearm",
                                      "stddev_yaw_forearm", "var_yaw_forearm")

training <- training[!myNZVvars]
#dim(training)

#Remove the first column (the ID)
training <- training[c(-1)]

#Cleaning Variables with too many NAs, variables with more than 60% NA, should be ignore (60% is the training)
trainingV3 <- training #variable just for the loop
for(i in 1:length(training)) { #for every column in  training 
  if( sum( is.na( training[, i] ) ) /nrow(training) >= .6 ) { #if NAs > 60% of  observations
    for(j in 1:length(trainingV3)) {
      if( length( grep(names(training[i]), names(trainingV3)[j]) ) ==1)  { #columns are the same???
        trainingV3 <- trainingV3[ , -j] #Remove that column
      }   
    } 
  }
}
training <- trainingV3
rm(trainingV3)
clean1 <- colnames(training)
clean2 <- colnames(training[, -58]) #remove classe
testing <- testing[clean1]
testing <- testing[clean2]
for (i in 1:length(testing) ) {
  for(j in 1:length(training)) {
    if( length( grep(names(training[i]), names(testing)[j]) ) ==1)  {
      class(testing[j]) <- class(training[i])
    }      
  }      
}
testing <- rbind(training[2, -58] , testing) #row 2 does not have any meaning:
testing <- testing[-1,]
modFitA1 <- rpart(classe ~ ., data=training, method="class")
predictionsA1 <- predict(modFitA1, testing, type = "class")
confusionMatrix(predictionsA1, testing$classe)
modFitB1 <- randomForest(classe ~. , data=training)
predictionsB1 <- predict(modFitB1, testing, type = "class")
confusionMatrix(predictionsB1, testing$classe)
predictionsB2 <- predict(modFitB1, testing, type = "class")

pml_write_files = function(x){
  n = length(x)
  for(i in 1:n){
    filename = paste0("problem_id_",i,".txt")
    write.table(x[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
  }
}

pml_write_files(predictionsB2)
