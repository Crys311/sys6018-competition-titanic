# SYS 6018 
# Wenxi Zhao / wz8nx
# Kaggle Competition

library(readr)  # Provides "read_csv" function
library(dplyr)  # Allows for nicer display of data frame

train1 = read_csv("train.csv")
gender1 = read_csv("gender_submission.csv")
test1 = read_csv("test.csv")

train1.lg <- glm(Survived~., data=train1, family = "binomial") # make binomial model and get summary
summary(train1.lg)

n <- nrow(test1)
train2.lg <- glm(Survived~.-Ticket-Fare-Embarked, data=train1, family = "binomial") # make a binomial model excluded all insignificant variables
probs<-as.vector(predict(train2.lg,newdata=test1, type="response"))
preds <- rep(0,n)  # Initialize prediction vector
preds[probs>0.5] <- 1 # p>0.5 -> 1
t1 <- cbind(preds,test1$PassengerId) 
t2 <- data.frame(t1)
colnames(t2)[c(1,2)] <- c("Survived","PassengerID")
t3 <- t2[c("PassengerID","Survived")]

write.csv(t3,file="wz8nx-mypredictions.csv") # write out this data into a new csv file
