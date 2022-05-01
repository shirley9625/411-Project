#install.packages("caret")
#install.packages("tidyverse")
#install.packages("kknn")
library(caret)
library(dplyr)

setwd("/Users/starr/Documents/411/project")
training <- read.csv("training.csv",header =FALSE)
testing <- read.csv("testing.csv",header =FALSE)

training <- training %>% distinct
testing <- testing %>% distinct
training <- training[,-1]
testing <- testing[,-1]
training[is.na(training)]<-0
testing[is.na(testing)]<-0
data=rbind(training,testing)
data <- data[,-319]

index<-sample(1:nrow(data),round(0.1*nrow(data)))

aircraft_type <- as.factor(data[index,1])
truth <- as.factor(data[-index,1])

train_data <- data[index,-1]
test_data <- data[-index,-1]
model <- train(train_data,
               aircraft_type,
               method = "kknn",
               preProcess = c("center","scale"),
               trControl = trainControl(method="cv",number=5),
               tuneLength = 5)

model

pred <- predict(model,newdata = test_data)
test_confusion<-confusionMatrix(as.factor(pred),as.factor(truth))

pred <- predict(model,newdata = train_data)
train_confusion<-confusionMatrix(as.factor(pred),as.factor(truth))