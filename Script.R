library(ggplot2)

DF = read.csv('./Data/insurance.csv')

#EDA of the dataset
#Plan so far 

str(DF)
plot(DF)

#EDA for smoker
boxplot(charges~smoker, data=DF)
ggplot(data=DF, mapping = aes(x=smoker,y=charges,fill=smoker))+geom_boxplot()
