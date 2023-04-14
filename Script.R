library(ggplot2)
library(cowplot)
library(tidyverse)
library(caret)

DF = read.csv('./Data/insurance.csv')

#EDA of the dataset
#Plan so far 

str(DF)
plot(DF)

#check for missing values
sum(is.na(DF))

#Fix the outliers first
p1 <- ggplot(data=DF,aes(x=age))+geom_histogram()
p2 <- ggplot(data=DF, aes(x=bmi))+geom_histogram()
p3 <- ggplot(data=DF, aes(x=children))+geom_histogram()
p <- plot_grid(p1,p2,p3, ncol=2)
p

#boxplot of bmi
ggplot(data=DF, aes(y=bmi))+geom_boxplot()

#handle the outliers
# First Quantile:
Q1 = quantile(DF[,3])[2] 
# Third Quantile:
Q2 = quantile(DF[,3])[4]
# Inner Quantile distance:
IQR = Q2-Q1

# Lower bound Quantile Range:
lo = Q1-1.5*IQR
# Upper bound Quantile Range:
up = Q2+1.5*IQR
# Put outliers greater than upper quantile equal to upper quantile range:  
if(sum(DF[3]>up)){
  indexu <- which(DF[3]>up)
  DF[[3]][indexu] = up
} else if(sum(DF[3]<lo)){ 
  indexl <- which(DF[3]<lo)
  DF[[3]][indexl] = lo}


#Checking For outliers again
ggplot(data=DF, aes(y=bmi))+geom_boxplot()

#EDA for smoker
ggplot(data=DF, mapping = aes(x=smoker,y=charges,fill=smoker))+geom_boxplot()

#Charges and children
ggplot(data=DF, mapping = aes(x=as.factor(children), y=charges))+geom_boxplot()

#Charges and sex
ggplot(data=DF, mapping = aes(x=sex,y=charges,fill=sex))+geom_boxplot()

#Charges and region
ggplot(data=DF, mapping = aes(x=region,y=charges))+geom_boxplot()

#Charges with sex and status of smoker
ggplot(data=DF, mapping = aes(x=sex,y=charges,fill=smoker))+geom_boxplot()

#Charges and BMI
ggplot(data=DF, mapping = aes(x=bmi,y=charges))+geom_point()

#Correlation plot




## Models

str(DF)

#Linear Model
set.seed(7)

random_sample <- createDataPartition(DF$charges,
                                     p=0.8,list=FALSE)
train.set <- DF[random_sample,]
test.set <- DF[-random_sample,]


LinearM <- lm(charges ~.,data = train.set)
summary(LinearM) #R2 0.75

plot(LinearM)

predictions <- predict(LinearM, test.set)
data.frame(R2 = R2(predictions,test.set$charges),
           RMSE = RMSE(predictions,test.set$charges))

mean.charges = mean(DF[,7])
RMSE = RMSE(predictions,test.set$charges)
Error.rate = RMSE/mean.charges
print(Error.rate)

#10 fold cross validation and check the predictions with testset

train_control = trainControl(method="cv",number=10)
lm.cv <- train(charges~.,data=train.set,
               method="lm",
               trControl=train_control)
print(lm.cv)


predictions2 <- predict(lm.cv, test.set)
data.frame(R2 = R2(predictions2,test.set$charges),
           RMSE = RMSE(predictions2,test.set$charges))


### Pick up, figure out if you need to change the cat to as.factor and decide what to do with the diagnostic plot of LM 
### how to check the assumptions of lasso and ridge.

#Step model


