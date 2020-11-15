rm(list=ls())
getwd()
setwd("D:/RCode/challenge")
data<-read.csv("data.csv")
m<-data=="?"
is.na(data)<-m
unique(data$make)
unique(data$body.style)
#treating missing values
#normalized losses
mean(data$normalized.losses,na.rm=TRUE)
data$normalized.losses<-as.numeric(data$normalized.losses)
sum(is.na(data$normalized.losses))
data$normalized.losses[is.na(data$normalized.losses)]="122"
#price
sum(is.na(data$price))
data$price<-as.numeric(data$price)
mean(data$price,na.rm=TRUE)
data$price[is.na(data$price)]<-mean(data$price,na.rm=TRUE)
#horsepower
sum(is.na(data$horsepower))
data$horsepower<-as.numeric(data$horsepower)
mean(data$horsepower,na.rm=TRUE)
data$horsepower[is.na(data$horsepower)]<-mean(data$horsepower,na.rm=TRUE)
#peak-rpm
sum(is.na(data$peak.rpm))
data$peak.rpm<-as.numeric(data$peak.rpm)
mean(data$peak.rpm,na.rm=TRUE)
data$peak.rpm[is.na(data$peak.rpm)]<-mean(data$peak.rpm,na.rm=TRUE)
#bore
sum(is.na(data$bore))
data$bore<-as.numeric(data$bore)
mean(data$bore,na.rm=TRUE)
data$bore[is.na(data$bore)]<-mean(data$bore,na.rm=TRUE)
#stroke
sum(is.na(data$stroke))
data$stroke<-as.numeric(data$stroke)
mean(data$stroke,na.rm=TRUE)
data$stroke[is.na(data$stroke)]<-mean(data$stroke,na.rm=TRUE)

summary(data)
plot(data[c(24,22,17,14,26)])

#Univariate analysis
library(ggplot2)
ggplot(data) + geom_histogram(aes(data$price), binwidth = 6000, fill = "darkgreen") +
  xlab("Price")
p0 = ggplot(data) + geom_histogram(aes(price), binwidth = 6000, fill = "blue")
p0
p1 = ggplot(data) + geom_histogram(aes(engine.size), binwidth = 55, fill = "blue")
p1
min(data$curb.w8)

p2 = ggplot(data) + geom_histogram(aes(curb.w8), binwidth = 800, fill = "blue")
p2
p3 = ggplot(data) + geom_histogram(aes(horsepower), binwidth = 60, fill = "blue")
p3
p4<-ggplot(data) + geom_histogram(aes(peak.rpm), binwidth = 400, fill = "blue")
p4
library(cowplot)
plot_grid(p0, p1, p2, p3, p4,nrow = 1)

unique(data$engine.type)
library(dplyr)
p5<-ggplot(data %>% group_by(engine.type) %>% summarise(Count = n())) + 
  geom_bar(aes(engine.type, Count), stat = "identity", fill = "coral1") +
  xlab("Engine Type") +
  geom_label(aes(engine.type, Count, label = Count), vjust = 0.5) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  ggtitle("Engine Type")
p5
p6<-ggplot(data %>% group_by(num.of.doors) %>% summarise(Count = n())) + 
  geom_bar(aes(num.of.doors, Count), stat = "identity", fill = "coral1") +
  xlab("Num of doors") +
  geom_label(aes(num.of.doors, Count, label = Count), vjust = 0.5) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  ggtitle("Num of doors")
p6
p7<-ggplot(data %>% group_by(fuel.type) %>% summarise(Count = n())) + 
  geom_bar(aes(fuel.type, Count), stat = "identity", fill = "coral1") +
  xlab("Fuel Type") +
  geom_label(aes(fuel.type, Count, label = Count), vjust = 0.5) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  ggtitle("Fuel Type")
p7
p8<-ggplot(data %>% group_by(body.style) %>% summarise(Count = n())) + 
  geom_bar(aes(body.style, Count), stat = "identity", fill = "coral1") +
  xlab("Body Style") +
  geom_label(aes(body.style, Count, label = Count), vjust = 0.5) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  ggtitle("Body Style")
p8
plot_grid(p5,p6,p7,p8, ncol = 1)

## Correlation Plot
cor_train = cor(data[,c(3,4,5,6,7,8,9,15,18)])
library(corrplot)
corrplot(cor_train, method = "pie", type = "lower", tl.cex = 0.9)

#bivariate analysis

ggplot(data,aes(x=make,y=price,fill=make))+geom_boxplot(alpha=0.3)
ggplot(data,aes(x=body.style,y=price,fill=body.style))+geom_boxplot(alpha=0.3)
ggplot(data) + geom_violin(aes(body.style, price), fill = "magenta") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.text = element_text(size = 6),
        axis.title = element_text(size = 8.5))
ggplot(data,aes(x=drive.wheels,y=price,fill=drive.wheels))+geom_boxplot(alpha=0.3)
ggplot(data) + geom_histogram(aes(x=num.of.cylinders,y= horsepower)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.text = element_text(size = 6),
        axis.title = element_text(size = 8.5))


ggplot(data)+geom_bar(aes(x=engine.type,fill=factor(fuel.type)))
ggplot(data)+geom_bar(aes(x=body.style,fill=factor(engine.type)))
ggplot(data)+geom_bar(aes(x=body.style,fill=factor(fuel.type)))
ggplot(data)+geom_point(aes(x=num.of.cylinders,y=horsepower))
ggplot(data)+geom_point(aes(x=symboling,y=normalized.losses))
ggplot(data)+geom_bar(aes(x=symboling,fill=factor(body.style)))


#kmeans on body type and consistent sorting
#https://stackoverflow.com/questions/39906180/consistent-cluster-order-with-kmeans-in-r

#https://www.kaggle.com/shubhamsinghgharsele/exploratory-data-analysis-on-automobile-dataset