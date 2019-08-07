library(foreign)  # Allows us to read spss files!
library(corrplot)
library(QuantPsyc)
library(leaps)
library(car)
library(RColorBrewer)
library(Hmisc)
library(psych)
library(readxl)
library(xlsx)
library(aplpack)

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
work_dir <- "/Users/appobs/Desktop/hw/424/project"
setwd(work_dir)
# Read in the file
LF <- read.csv("/Users/appobs/Desktop/hw/424/project/Life Expectancy Data.csv")
View(LF)

#basic info
str(LF)
#1st var info
head(LF)
#desceiptive table
describe(LF)
#Checking missing value, result 0 meaning No missing
sum(is.na(LF))
#Handle missing value
#LF2 <- na.omit(LF)
#plot(LF2)

#Handle missing value by using the multiple imputation with mice package;
library(mice)
#cheeck the missing value problem pattern
md.pattern(LF)
install.packages("VIM")
library(VIM)
LF_plot <- aggr(LF, col=c('navyblue','yellow'),
                  numbers=TRUE, sortVars=TRUE,
                  labels=names(LF), cex.axis=.7,
                  gap=3, ylab=c("Missing data","Pattern"))

#since the method = 'pmm' does not work due to the collinearity,
#but still suggest to use it in other dataset;

imputed_Data = mice(LF, m=5, maxit = 5, method = 'cart', seed = 500)
summary(imputed_Data)
#check imputed values
imputed_Data$imp$Population
imputed_Data$imp$GDP
#get complete data ( 2nd out of 5)
completeData <- complete(imputed_Data,2)
#check the missing value problem & export dataset as .cvs(change it to your directory)
sum(is.na(completeData))
#write.csv(completeData, "/Users/appobs/Desktop/hw/424/project/completeData.csv")

# multiple imputation by using the Hmise package
# install.packages("Hmisc")
# library(Hmisc)
# impute_arg <- aregImpute(~ Population + GDP + Alcohol + Hepatitis.B +
#                            Total.expenditure + Income.composition.of.resources + 
#                            Schooling, data = LF, n.impute = 5)
# impute_arg
# sum(is.na(LF))


#We remove first columns
LFReduced = completeData[,c(2:22)]
head(LFReduced)

#Construct dummy vairalbr for status
d.status = c(1,0)
names(d.status) = c("Developing", "Developed")
LFReduced$Status.f <- d.status[LFReduced$Status]

#Construct dummy vairalbr for years
#2,2,2,2,2,3,3,3,3,3
d.year = c(1,1,1,1,1,1,2,2,2,2,2,3,3,3,3,3)
names(d.year) = c("2000", "2001","2002","2003","2004","2005","2006","2007","2008","2009","2010","2011","2012","2013","2014","2015")
LFReduced$Year.f <- d.year[LFReduced$Year]
#write.csv(LFReduced, "/Users/appobs/Desktop/hw/424/project/LFReduced.csv")

#Drop the category var
LFReduced2 = subset(LFReduced, select = -c(Status))
# boxPLOT for var thinness..1.19.years and thinness.5.9.years
boxplot(thinness..1.19.years, data = LFReduced2, xlab = "thinness..1.19.years",
        ylab = "thinness.5.9.years", main = "thinness Data")

boxplot(LFReduced2$thinness..1.19.years, xlab = "thinness..1.19.years",
        ylab = "thinness.5.9.years", main = "thinness Data")

# Compute the correlation matrix and visualize it
cor.LF= cor(LFReduced2)
cor.LF
corrplot(cor.LF, method="ellipse")
# Look at the size of the numeric data
dim(LF)

# Create boxplot for observing the relation between TWo var;
# Boxplot of MPG by Car Cylinders 
#boxplot(data = LFReduced, las = 2, name = c("thinness..1.19.years", "thinness.5.9.years"))

# Run a correlation test to see how correlated the variables are.  Which correlations are significant
library(psych)
options("scipen"=100, "digits"=5)
round(cor(LFReduced2[, 1:21]), 2)
MCorrTest = corr.test(LFReduced2[, 1:21], adjust="none")
MCorrTest

M = MCorrTest$p
M
# Now, for each element, see if it is < .05 (or whatever significance) and set the entry to 
# true = significant or false
MTest = ifelse(M < .01, T, F)
MTest
# Now lets see how many significant correlations there are for each variable.  We can do
# this by summing the columns of the matrix
colSums(MTest) - 1  # We have to subtract 1 for the diagonal elements (self-correlation)

# need to check the histogram, outliers, VIF;
