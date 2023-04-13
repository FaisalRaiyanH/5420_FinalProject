library(ggplot2)
library(cowplot)

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
q1 = quantile(DF[,3])[2] 
# Third Quantile:
q2 = quantile(DF[,3])[4]
# Inner Quantile distance:
iqr = q2-q1

# Lower bound Quantile Range:
l = q1-1.5*iqr
# Upper bound Quantile Range:
u = q2+1.5*iqr
# Put outliers greater than upper quantile equal to upper quantile range:  
if(sum(DF[3]>u)){
  indexu <- which(DF[3]>u)
  DF[[3]][indexu] = u
} else if(sum(DF[3]<l)){ 
  indexl <- which(DF[3]<l)
  DF[[3]][indexl] = l}
DF[,3]

#Checking For outliers again
ggplot(data=DF, aes(y=bmi))+geom_boxplot()

#EDA for smoker
boxplot(charges~smoker, data=DF)
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

#Charges and BMI as range