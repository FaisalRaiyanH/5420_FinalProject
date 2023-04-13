library(ggplot2)

DF = read.csv('./Data/insurance.csv')

#EDA of the dataset
#Plan so far 

str(DF)
plot(DF)

#Fix the outliers first



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


