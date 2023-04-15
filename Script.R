library(ggplot2)
library(cowplot)
library(tidyverse)
library(caret)
library(dplyr)
library(tidyverse)
library(glmnet)
library(neuralnet)
library(MASS)

DF = read.csv('./Data/insurance.csv')

#### Observe dataset and handle missing values ####

str(DF) 
plot(DF)

#check for missing values
sum(is.na(DF))

#Check the outliers first
p1 <- ggplot(data=DF,aes(x=age))+geom_histogram()
p2 <- ggplot(data=DF, aes(x=bmi))+geom_histogram()
p3 <- ggplot(data=DF, aes(x=children))+geom_histogram()
p <- plot_grid(p1,p2,p3, ncol=2, labels="auto")
p



#boxplot of bmi
ggplot(data=DF, aes(y=bmi))+geom_boxplot()

#handle the outliers of bmi
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


#Checking BMI outlier again
ggplot(data=DF, aes(y=bmi))+geom_boxplot()

#### ####

#### EDA of the dataset ####
#EDA for smoker
ggplot(data=DF, mapping = aes(x=smoker,y=charges,fill=smoker))+geom_boxplot()

#Charges and children
ggplot(data=DF, mapping = aes(x=as.factor(children), y=charges,))+geom_boxplot()

#Charges and sex
ggplot(data=DF, mapping = aes(x=sex,y=charges,fill=sex))+geom_boxplot()

#Charges and region
ggplot(data=DF, mapping = aes(x=region,y=charges))+geom_boxplot()

#Charges with sex and status of smoker
ggplot(data=DF, mapping = aes(x=sex,y=charges,fill=smoker))+geom_boxplot()

#Charges and BMI
ggplot(data=DF, mapping = aes(x=bmi,y=charges,color=bmi))+geom_point(size=5)

#### ####

##Models

##Scale the numeric variables 
SDF <- DF
SDF$age <- (DF$age - mean(DF$age) ) / sd(DF$age)
SDF$bmi <- (DF$bmi - mean(DF$bmi) ) / sd(DF$bmi)
SDF$children <- (DF$children - mean(DF$children) ) / sd(DF$children)
SDF$charges <- (DF$charges - mean(DF$charges) ) / sd(DF$charges)

max(DF[,7])
min(DF[,7])
mean(DF[,7])

#### Linear Model ####
set.seed(7)

random_sample <- createDataPartition(SDF$charges,
                                     p=0.8,list=FALSE)
train.set <- SDF[random_sample,]
test.set <- SDF[-random_sample,]


LinearM <- lm(charges ~.,data = train.set)
summary(LinearM) #R2 0.75


plot(LinearM)

predictions <- predict(LinearM, test.set)
predictiona = (predictions*sd(DF[,7])+mean(DF[,7]))
test.a = (test.set$charges*sd(DF[,7])+mean(DF[,7]))
data.frame(R2 = R2(predictions,test.set$charges),
           RMSE = RMSE(predictiona,test.a))

#### ####


#### 10 fold cross validation and check the predictions with testset ####

train_control = trainControl(method="cv",number=10)
lm.cv <- train(charges~.,data=train.set,
               method="lm",
               trControl=train_control)
print(lm.cv)


predictions2 <- predict(lm.cv, test.set)

data.frame(R2 = R2(predictions2,test.set$charges),
           RMSE = RMSE(predictions2,test.set$charges))

#### ####

## Step model ####

step.model = train(charges~.,data=train.set,
                   method="leapBackward",
                   tuneGrid=data.frame(nvmax=1:6),
                   trControl=train_control)

step.model$results
step.model$bestTune

summary(step.model$finalModel)

predictions = predict(step.model,test.set)
predictiona = (predictions*sd(DF[,7])+mean(DF[,7]))
test.a = (test.set$charges*sd(DF[,7])+mean(DF[,7]))
data.frame(R2 = R2(predictiona,test.set$charges),
           RMSE = RMSE(predictiona,test.b))

## ####

## Ridge ####
set.seed(7)

x <- model.matrix(charges~.,train.set)[,-1]
y <- train.set$charges

#Best lambda using cv
cv <- cv.glmnet(x,y,alpha = 0)
cv$lambda.min

#Fit the final model on the train set
ridge <- glmnet(x=as.data.frame(x),y,alpha = 0,lambda = cv$lambda.min)
coef(ridge)

#Make predictions on test set
x.test <- model.matrix(charges~.,test.set)[,-1]
predictions <- ridge %>% predict(x.test) %>% as.vector()
predictionsa <- (predictions*sd(DF[,7])+mean(DF[,7]))
test.a = (test.set$charges*sd(DF[,7])+mean(DF[,7]))
data.frame(
  R2 = R2(predictionsa,test.set$charges),
  RMSE = RMSE(predictionsd,test.a)
)

## ####

## Lasso ####
set.seed(7)
cv <- cv.glmnet(x,y,alpha=1)
cv$lambda.min

lasso <- glmnet(x,y,alpha=1,lambda=cv$lambda.min)
coef(lasso)
plot(cv)
#Make predictions on the test data
x.test <- model.matrix(charges~.,test.set)[,-1]
predictions <- lasso %>% predict(x.test) %>% as.vector()
predictionsa <- (predictions*sd(DF[,7])+mean(DF[,7]))
test.a = (test.set$charges*sd(DF[,7])+mean(DF[,7]))
data.frame(
  R2 = R2(predictionsa,test.set$charges),
  RMSE = RMSE(predictionsd,test.a)
)

## ####

## ElasticNet ####
set.seed(7)
cv <- cv.glmnet(x,y,alpha=0.5)
cv$lambda.min

elastic <- glmnet(x,y,alpha=0.5,lambda=cv$lambda.min)
coef(elastic)

#Make predictions on the test data
x.test <- model.matrix(charges~.,test.set)[,-1]
predictions <- elastic %>% predict(x.test) %>% as.vector()
predictionsa <- (predictions*sd(DF[,7])+mean(DF[,7]))
test.a = (test.set$charges*sd(DF[,7])+mean(DF[,7]))
data.frame(
  R2 = R2(predictionsa,test.set$charges),
  RMSE = RMSE(predictionsd,test.a)
)

## ####

#Neural Network
dataset1 <- model.matrix(
  ~age+sex+bmi+children+smoker+region+charges,
  data=DF
)
dataset <- model.matrix(
  ~age+sex+bmi+children+smoker+region+charges,
  data=SDF
)

train1 <- model.matrix(
  ~age+sex+bmi+children+smoker+region+charges,
  data=train.set
)
test1 <-model.matrix(
  ~age+sex+bmi+children+smoker+region+charges,
  data=test.set
)

#Train NN
nn <- neuralnet(charges~age+sexmale+bmi+children+smokeryes+regionnorthwest+regionsoutheast+regionsouthwest,
                data=train1,hidden = c(6),
                linear.output = TRUE, threshold = 0.02,stepmax = 100000)

plot(nn)
#predict on test data
predict.nn <- compute(nn,test1[,-10])

#Caculate MSE
predict.nn2 <- predict.nn$net.result * sd(dataset1[,10]) +mean(dataset1[,10])
test.r <- (test1[,10])* sd(dataset1[,10]) +mean(dataset1[,10])
MSE.nn <- sum((test.r - predict.nn2)^2)/nrow(test1)
print(MSE.nn)
RMSE <- sqrt(MSE.nn)
print(RMSE)

## ####