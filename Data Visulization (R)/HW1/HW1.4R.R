library(foreign)  # Allows us to read spss files!
library(corrplot)
library(QuantPsyc)
library(leaps)
library(car)
library(RColorBrewer)
library(Hmisc)
library(psych)
library(readxl)
library(ggplot2)

infant = read_excel("/Users/appobs/Desktop/hw/465/week1/hw1.2/Datasets/InfantData.xlsx", sheet = 1, )
head(infant)
str(infant)

ggplot(data = infant, aes(x=`Height in`, y=`Weight lbs`, color = Sex)) + geom_point() +ylab("Weight lbs") + xlab("Height in") + ggtitle("scatter plot of Infant")
ggplot(data = infant, aes(x=`Height in`, y=`Weight lbs`, color = Sex, shape = Sex)) + geom_point() +ylab("Weight lbs") + xlab("Height in") + ggtitle("scatter plot of Infant") + geom_smooth(method = "lm", se=F)