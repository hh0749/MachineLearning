##Final assignment for Practical Machine Learning

##Read data into R
setwd("~/Documents/coursera/8. Practical Machine Learning")
training = read.csv("pml-training.csv", na.strings = c("NA", ""))
testing = read.csv("pml-testing.csv", na.strings = c("NA", ""))

dim(training)
dim(testing)

##Cleaning Data
##Remove nearzerovariance variables
nzv <- nearZeroVar(training, saveMetrics=TRUE)
training <- training[,nzv$nzv==FALSE]

##Remove the first column
training <- training[c(-1)]

##Remove variables with full of NA and make testing dataset consistent with training dataset in terms of available columns
features <- names(training[,colSums(is.na(training)) == 0])[1:57]
training<-training[,c(features,"classe")]
testing<-testing[,c(features)]

for (i in 1:length(testing) ) {
  for(j in 1:length(training)) {
    if( length( grep(names(training[i]), names(testing)[j]) ) ==1)  {
      class(testing[j]) <- class(training[i])
    }      
  }      
}

testing <- rbind(training[2, -58] , testing) 
testing <- testing[-1,]

##Partitioning the training sets 60% for mytraining and 40% for mytesting
inTrain <- createDataPartition(y=training$classe, p=0.6, list=FALSE)
mytraining <- training[inTrain, ]; mytesting <- training[-inTrain, ]
dim(mytraining)
dim(mytesting)

##Decision Tree
DT <- rpart(classe ~ ., data=mytraining, method="class")
fancyRpartPlot(DT)
DT2 <- predict(DT, mytesting, type = "class")
confusionMatrix(DT2, mytesting$classe)

##Ramdom Forests
RF <- randomForest(classe ~. , data=mytraining)
RF2 <- predict(RF, mytesting, type = "class")
confusionMatrix(RF2, mytesting$classe)

##Out of sample error
predictions <- predict(RF, testing, type = "class")
pml_write_files = function(x){
  n = length(x)
  for(i in 1:n){
    filename = paste0("problem_id_",i,".txt")
    write.table(x[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
  }
}

pml_write_files(predictions)




