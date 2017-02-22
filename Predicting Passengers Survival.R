#installing packages 


install.packages("caret")

library(caret)

install.packages("randomForest")

library(randomForest)

install.packages("klaR")

library(klaR)

install.packages("plyr")

library(plyr)

install.packages("reshape2")

library(reshape2)

install.packages("ggplot2")
library(ggplot2)



#Loading the data
passengers <- read.csv("titanicdata.csv")



#Making necessary conversions
passengers$Name=as.character(passengers$Name)

passengers$Survived = as.factor(passengers$Survived)

passengers$Pclass = as.factor(passengers$Pclass)

passengers$SibSp = as.factor(passengers$SibSp)

passengers$SibSp = as.factor(passengers$Parch)

str(passengers) #rechecking the data after necessary conversions are made



#Cleaning data


passengers$Age[is.na(passengers$Age)] = mean(passengers$Age, na.rm=TRUE) # Assigning the mean of the Ages to NA values
passengers$Age <- round(passengers$Age) # Rounding off Age to the nearest decimal



#Exploratory Data Analysis
ggplot(passengers, aes(x=Age, fill=Sex)) + geom_histogram(binwidth=7) 

ggplot(passengers, aes(x=Age, color=Sex)) + geom_density() 

ggplot(passengers, aes(x=Pclass, y=Fare)) + geom_boxplot()
ggplot(passengers, aes(x=Survived, fill=Sex)) + geom_histogram()

ggplot(passengers, aes(x=Survived, fill=Pclass)) + geom_histogram()
ggplot(passengers, aes(x=factor(Survived), y=Age)) + geom_boxplot()

ggplot(passengers, aes(x=Age, y=Fare)) + geom_point()



#Normalization function


normalize <- function(x) 
{
  
num <- x - min(x)
  
denom <- max(x) - min(x)
  
return (num/denom)

}



#Standardizing the fare


nFare <- normalize(passengers$Fare)

nAge <- normalize(passengers$Age)



#Creating a cleaner set for analysis


attach(passengers)
passengersnew <-data.frame(PassengerId,Survived,Pclass,Sex,nAge,SibSp,Parch,nFare,Embarked)

head(passengersnew)

str(passengersnew)




#Creating a Training and Test dataset

set.seed(1)
trainIndex <- createDataPartition(passengersnew$Survived, p=0.70, list=FALSE)

data_train <- passengersnew[trainIndex,]
data_test <- passengersnew[-trainIndex,]




#Building a model


m1 <- randomForest(Survived~ nFare+Pclass+Sex+nAge+SibSp+Parch+nFare+Embarked, data = data_train, importance=TRUE,
 keep.forest=TRUE
)
m2 <- NaiveBayes(Survived~., data=data_train)
m3 <- glm(Survived ~ Sex * Pclass, data = data_train, family = binomial(link="logit"))

m4 <- glm(Survived ~ Pclass+nAge+nFare+SibSp+Parch, data = data_train, family="binomial")



#Predicting values


p1 <- predict(m1, data_test[,-2])

p2 <- predict(m2, data_test[,-2])


p3 <- predict(m3, passengersnew = data_test[-2], type="response")

p3_cut <- cut(p3,breaks=2) 

#converting numeric type to factors so that it can compare to the factors of Survived variable in test 
class
levels(p3_cut) = c(0,1) #setting levels to be 0 and 1



p4 <-  predict(m4, passengersnew = data_test[-2], type="response")

p4_cut <- cut(p4,breaks=2)
levels(p4_cut) = c(0,1)




#Creating the actual confusion Matrix


confusionMatrix(p1, data_test$Survived)

confusionMatrix(p2$class, data_test$Survived)

confusionMatrix(p3_cut, data_test$Survived)

confusionMatrix(p4_cut, data_test$Survived)