library(foreign)  # Allows us to read spss files!
library(corrplot)
library(leaps)
library(QuantPsyc)
library(car)
library(RColorBrewer)
library(Hmisc)
library(psych)
library(readxl)

PCA_Plot = function(pcaData)
{
  library(ggplot2)
  
  theta = seq(0,2*pi,length.out = 100)
  circle = data.frame(x = cos(theta), y = sin(theta))
  p = ggplot(circle,aes(x,y)) + geom_path()
  
  loadings = data.frame(pcaData$rotation, .names = row.names(pcaData$rotation))
  p + geom_text(data=loadings, mapping=aes(x = PC1, y = PC2, label = .names, colour = .names, fontface="bold")) +
    coord_fixed(ratio=1) + labs(x = "PC1", y = "PC2")
}

PCA_Plot_Secondary = function(pcaData)
{
  library(ggplot2)
  
  theta = seq(0,2*pi,length.out = 100)
  circle = data.frame(x = cos(theta), y = sin(theta))
  p = ggplot(circle,aes(x,y)) + geom_path()
  
  loadings = data.frame(pcaData$rotation, .names = row.names(pcaData$rotation))
  p + geom_text(data=loadings, mapping=aes(x = PC3, y = PC4, label = .names, colour = .names, fontface="bold")) +
    coord_fixed(ratio=1) + labs(x = "PC3", y = "PC4")
}

PCA_Plot_Psyc = function(pcaData)
{
  library(ggplot2)
  
  theta = seq(0,2*pi,length.out = 100)
  circle = data.frame(x = cos(theta), y = sin(theta))
  p = ggplot(circle,aes(x,y)) + geom_path()
  
  loadings = as.data.frame(unclass(pcaData$loadings))
  s = rep(0, ncol(loadings))
  for (i in 1:ncol(loadings))
  {
    s[i] = 0
    for (j in 1:nrow(loadings))
      s[i] = s[i] + loadings[j, i]^2
    s[i] = sqrt(s[i])
  }
  
  for (i in 1:ncol(loadings))
    loadings[, i] = loadings[, i] / s[i]
  
  loadings$.names = row.names(loadings)
  
  p + geom_text(data=loadings, mapping=aes(x = PC1, y = PC2, label = .names, colour = .names, fontface="bold")) +
    coord_fixed(ratio=1) + labs(x = "PC1", y = "PC2")
}

PCA_Plot_Psyc_Secondary = function(pcaData)
{
  library(ggplot2)
  
  theta = seq(0,2*pi,length.out = 100)
  circle = data.frame(x = cos(theta), y = sin(theta))
  p = ggplot(circle,aes(x,y)) + geom_path()
  
  loadings = as.data.frame(unclass(pcaData$loadings))
  s = rep(0, ncol(loadings))
  for (i in 1:ncol(loadings))
  {
    s[i] = 0
    for (j in 1:nrow(loadings))
      s[i] = s[i] + loadings[j, i]^2
    s[i] = sqrt(s[i])
  }
  
  for (i in 1:ncol(loadings))
    loadings[, i] = loadings[, i] / s[i]
  
  loadings$.names = row.names(loadings)
  
  print(loadings)
  p + geom_text(data=loadings, mapping=aes(x = PC3, y = PC4, label = .names, colour = .names, fontface="bold")) +
    coord_fixed(ratio=1) + labs(x = "PC3", y = "PC4")
}

# Set the working director
work_dir <- "/Users/appobs/Desktop/hw/424/hw2"
setwd(work_dir)
# Read in the file
cen2<- read.csv("/Users/appobs/Desktop/hw/424/hw2/Census2.csv")
View(cen2)

#basic info
str(cen2)
#1st var info
head(cen2)
#desceiptive table
describe(cen2)
#Checking missing value, result 0 meaning No missing
sum(is.na(cen2))
#Handle missing value
cen2 <- na.omit(cen2)
plot(cen2)
# Compute the correlation matrix and visualize it
cor.cen2 = cor(cen2)
cor.cen2
corrplot(cor.cen2, method = "ellipse")
corrplot(cor.cen2, method="number")
corrplot(cor.cen2, method="circle",col=c("yellow", "red","blue","green"))
# Look at the size of the numeric data
dim(cen2)
# Run a correlation test to see how correlated the variables are.  
# Which correlations are significant
library(psych)
options("scipen" = 100, "digits" = 5)
round(cor(cen2[, 1:5]), 2)
MCorrTest=corr.test(cen2[, 1:5], adjust = "none")
MCorrTest

M = MCorrTest$p
M
# Now, for each element, 
# see if it is < .01 or whatever significance) and set the entry to 
# true = significant or false
MTest = ifelse(M<0.01, T, F)
MTest
# Now lets see how many significant correlations there are for each variable.
# We can do
# this by summing the columns of the matrix
colSums(MTest) - 1  # We have to subtract 1 for the diagonal elements (self-correlation)

#start to run the PCA;center meaning the different units;
#def center=T:a logical value indicating whether the variables should be shifted to be zero centered.
#Alternately, a vector of length equal the number of columns of x can be supplied.
#The value is passed to scale.
#def scale=T: determing the variance whether is 0;
p = prcomp(cen2)
plot(p)
#set the cut off value for variance;
abline(1, 0)
summary(p)
#dividing the MedianHomeValue field by 100,000
cen2$MedianHomeVal <- (cen2$MedianHomeVal)/100000
cen2
p = prcomp(cen2)
plot(p)
abline(1,0)
summary(p)
#for Q (c)
p = prcomp(cen2, center = TRUE, scale = TRUE )
plot(p)
abline(1, 0)
summary(p)

options("scipen" = 100, "digits" = 5)
round(cor(cen2[, 1:5]), 2)
MCorrTest=corr.test(cen2[, 1:5], adjust = "none")
MCorrTest
#calculate the correlation matrix
M = MCorrTest$p
M
# Now, for each element, 
# see if it is < .01 or whatever significance) and set the entry to 
# true = significant or false
MTest = ifelse(M<0.01, T, F)
MTest
# Now lets see how many significant correlations there are for each variable.
# We can do
# this by summing the columns of the matrix
colSums(MTest) - 1  # We have to subtract 1 for the diagonal elements (self-correlation)
#for P6(D), deleting the variable Population;
p2 = subset(cen2, select = -c(Population))
p2
p2 = prcomp(p2, center = TRUE, scale = TRUE )
plot(p2)
abline(1, 0)
summary(p2)
