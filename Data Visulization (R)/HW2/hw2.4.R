######################Problem 4 (mine) #########################################################################
library(readxl)
library(ggplot2)
library(reshape)
library(scales)

intel = read_csv('/Users/appobs/Desktop/hw/465/week3/Datasets/Intel1990-2000.csv')
head(intel)

intel$Date = as.Date(intel$Date, format="%M/%D/%Y")
ggplot(intel, aes(x=Date, y =`Adj Close`, color='red')) + geom_line(size=1.5, alpha=.5)





######################Problem 4  #########################################################################
setwd('/Users/appobs/Desktop/hw/465/week4/hw2')
getwd()

library(readxl)
Messier= read_excel("/Users/appobs/Desktop/hw/465/week3/Datasets/MessierDataCleanHeaders.xlsx",sheet = 1)
names(Messier) <- c('Messier.no','NGC.no','Kind', 'Type','Distance_LY', 'Size', 'Apparent_Magnitude','R.A.','Dec.','Constellation','Season','Discoverer','year','Remarks')


## Problem 6.a completed using Tableau 

## Problem 6.b

ggplot(data=subset(Messier, !is.na(Kind)), aes(x = reorder(Kind,Distance_LY,median), y=Distance_LY, color= Kind)) +
  geom_boxplot(width=0.6,size=0.2,color="black")+
  geom_point(size=1)+
  theme(text = element_text(size=10),axis.text.x = element_text(angle=30, hjust=1)) +
  labs(title = "The Distance of Messier Object by Kind")+
  theme(legend.position='none',plot.title = element_text(hjust = 0.5,color="#666666", face="bold", size=15))+
  geom_jitter(position=position_jitter(width=.1, height=0),color="blue", size=0.7,alpha=.3)+
  xlab("Kind") +
  ylab("Distance_LY[Log10]") +
  scale_y_log10(labels = comma)


## Problem 6.c
ggplot(data=subset(Messier, !is.na(Kind)), aes(x = Apparent_Magnitude, y=Distance_LY ,color= Apparent_Magnitude)) +
  geom_point(aes(size= Size)) +
  scale_color_gradient(low="blue", high="gray") +
  labs(title = "The Apparent Magnitude of Messier Object")+
  theme(plot.title = element_text(hjust = 0.5,color="#666666", face="bold", size=15)) +
  xlab("Apparent Magnitude") +
  ylab("Distance_LY [Log10]")+
  scale_y_log10(labels = comma)



## Problem 6.d
ggplot(data=subset(Messier, !is.na(Kind)), aes(x = Apparent_Magnitude, y=Distance_LY,color= Apparent_Magnitude)) +
  geom_point(aes(size= Size, shape=Kind)) +
  scale_color_gradient(low="blue", high="gray") +
  labs(title = "The Apparent Magnitude of Messier Object")+
  theme(plot.title = element_text(hjust = 0.5,color="#666666", face="bold", size=15)) +
  xlab("Apparent Magnitude") +
  ylab("Distance_LY [Log10]") +
  scale_y_log10(labels = comma) +
  scale_shape_manual(values=c(0,1,2,3,7,10,13))












