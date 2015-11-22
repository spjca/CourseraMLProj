#Practical Machine Learning Project
#Classification of Motion Data by Activity


#load packages
library(caret)
library(rpart.plot)
library(rpart)


#load data
train.data <- read.csv("pml-training.csv", na.strings=c("NA",""), stringsAsFactors = FALSE)
test.data <- read.csv("pml-testing.csv", na.strings=c("NA","","#DIV/0"), stringsAsFactors = FALSE)


#clean data

#remove variables that don't make intuitive sense for prediction
train.data <- train.data[, -(1:7)]

#randomly subset the data into sample set and validation set to estimate out of sample error
set.seed(92648)
train.partition <- createDataPartition(y=train.data$classe, p=0.7, list=FALSE)
train.sample <- train.data[train.partition, ]
train.validation <- train.data[-train.partition, ]


# remove variables with nearly zero variance
train.nearzero <- nearZeroVar(train.sample)
train.sample <- train.sample[, -train.nearzero]
train.validation <- train.validation[, -train.nearzero]

# remove variables that are almost always NA
train.majorityNA <- sapply(train.sample, function(x) mean(is.na(x))) > 0.95
train.sample <- train.sample[, train.majorityNA==FALSE]
train.validation <- train.validation[, train.majorityNA==FALSE]



#Random Forest Analysis
train.control <- trainControl(method="cv", number = 3, verboseIter = FALSE)
train.fit <- train(classe ~ ., data=train.sample, method="rf",trControl=train.control)