#install.packages("caret")
#install.packages("tidyverse")
#install.packages("kknn")
#library(kknn)
library(caret)
library(tidyverse)
setwd("/Users/starr/Documents/411/75")
training <- read.csv("trainingset75.csv",header =FALSE)
testing <- read.csv("testingset75.csv",header =FALSE)

training <- training %>% distinct
testing <- testing %>% distinct
training <- training[,-1]
testing <- testing[,-1]
data=rbind(training,testing)
data<-data[-1,]
data[is.na(data)]<-0

for(i in seq(2,76))
{
  if(!is.numeric(data[,i]))
  {
    data[,i]<-as.numeric(data[,i])
  }
}
data[is.na(data)]<-0

index<-sample(1:nrow(data),round(0.8*nrow(data)))

aircraft_type <- as.factor(data[index,1])

train_data <- data[index,-1]
test_data <- data[-index,-1]
model <- train(train_data,
               aircraft_type,
               method = "kknn",
               preProcess = c("center","scale"),
               trControl = trainControl(method="cv",number=5),
               tuneLength = 5
               )

model

truth <- as.factor(data[-index,1])
pred <- predict(model,newdata = test_data)
test_confusion<-confusionMatrix(as.factor(truth),as.factor(pred))

truth <- as.factor(data[index,1])
pred <- predict(model,newdata = train_data)
train_confusion<-confusionMatrix(as.factor(truth),as.factor(pred))
