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
tr <- read.file("/Users/appobs/Desktop/hw/424/hw2/trackRecord.txt")
View(tr)

#basic info
str(tr)
#1st var info
head(tr)
#desceiptive table
describe(tr)
#Checking missing value, result 0 meaning No missing
sum(is.na(tr))
#Handle missing value
tr <- na.omit(tr)
plot(tr)
# Look at the size of the numeric data
dim(tr)
#before running PCA, should centralize the measure in dataset;
tr$m100 <- (tr$m100)/60
tr$m200 <- (tr$m200)/60
tr$m400 <- (tr$m400)/60
tr
# Run a correlation test to see how correlated the variables are.  
# Which correlations are significant
library(psych)
options("scipen" = 100, "digits" = 5)
round(cor(tr[, 2:9]), 2)
MCorrTest=corr.test(tr[, 2:9], adjust = "none")
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
#Drop the category var
p2 = subset(tr, select = -c(Country))
#run the PCA without center and scale;
p2 = prcomp(p2)
plot(p2)
#set the cut off value for variance;
abline(1, 0)
summary(p2)
#Drop the category var
p2 = subset(tr, select = -c(Country))
#run the PCA withIN center and scale
p3 = prcomp(p2, center = TRUE, scale = TRUE)
plot(p3)
#set the cut off value for variance;
abline(1, 0)
summary(p3)
print(p3)
par(mar=c(2, 2, 2, 2))
plot(p3)
PCA_Plot(p3)
PCA_Plot_Secondary(p3)
biplot(p3)
#loading and varimax ratation;
rawLoadings = p3$rotation %*% diag(p3$sdev, nrow(p3$rotation), nrow(p3$rotation))
print(rawLoadings)
v = varimax(rawLoadings)
ls(v)
v

# The Psych package has a wonderful PCA function that allows many more options
# including build-in factor rotation, specifiying a number of factors to include 
# and automatic "score" generation
p4 = psych::principal(p2, rotate="varimax", nfactors=(4), scores=TRUE)
p4

#print out the loading cut off point, in this case cut off = .4;
#get total 6 components since your cut off value set as .4;
print(p4$loadings, cutoff=.4, sort=T)
print(p4$loadings, cutoff=.6, sort=T)
p4$loadings
p4$values
p4$communality
p4$rot.mat

scores <-p4$scores
#find the minimum socre among with each components;
min_pca1Score <- min(scores[,1])
min_pca1Score
min_pca2Score <- min(scores[,2])
min_pca2Score
min_pca3Score <- min(scores[,3])
min_pca3Score
min_pca4Score <- min(scores[,4])
min_pca4Score

#find the maximum socre among with each components;
max_pca1Score <- max(scores[,1])
max_pca1Score
max_pca2Score <- max(scores[,2])
max_pca2Score
max_pca3Score <- max(scores[,3])
max_pca3Score
max_pca4Score <- max(scores[,4])
max_pca4Score
#part B
fit = factanal(p2, 4)
print(fit$loadings, cutoff=.4, sort=T)
summary(fit)
#different number of factory;
fit = factanal(p2, 3)
print(fit$loadings, cutoff=.4, sort=T)
print(fit$loadings, cutoff=.6, sort=T)
summary(fit)
