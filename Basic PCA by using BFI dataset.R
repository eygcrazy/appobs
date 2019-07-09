library(foreign)  # Allows us to read spss files!
library(corrplot)
library(QuantPsyc)
library(leaps)
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
bfi2 <- read.csv("/Users/appobs/Desktop/hw/424/hw2/bfi(clear).csv")
View(bfi2)

#basic info
str(bfi2)
#1st var info
head(bfi2)
#desceiptive table
describe(bfi2)
#Checking missing value, result 0 meaning No missing
sum(is.na(bfi2$x6))
#Handle missing value
bfi2 <- na.omit(bfi2)
plot(bfi2)
# Compute the correlation matrix and visualize it
cor.bfi= cor(bfi2)
cor.bfi
corrplot(cor.bfi, method="ellipse")
# Look at the size of the numeric data
dim(bfi2)
# Run a correlation test to see how correlated the variables are.  Which correlations are significant
library(psych)
options("scipen"=100, "digits"=5)
round(cor(bfi2[, 1:29]), 2)
MCorrTest = corr.test(bfi2[, 1:29], adjust="none")
MCorrTest

M = MCorrTest$p
M
# Now, for each element, see if it is < .05 (or whatever significance) and set the entry to 
# true = significant or false
MTest = ifelse(M < .05, T, F)
MTest
# Now lets see how many significant correlations there are for each variable.  We can do
# this by summing the columns of the matrix
colSums(MTest) - 1  # We have to subtract 1 for the diagonal elements (self-correlation)

# We remove Three columns, the last three (x27 - x29) are not used at this assginment;
bfiReduced = bfi2[, -c(1,27:29)]
head(bfiReduced)
cor(bfiReduced)
#basic info for bfiReduced
str(bfiReduced)
#check missing value at dataset bfiReduced,result 0 meaning No missing
sum(is.na(bfiReduced))
#Handle missing value
bfiReduced2 <- na.omit(bfiReduced)
#start to run the PCA;
p = prcomp(bfiReduced2, center=T, scale=T) 
plot(p)
abline(1, 0)
summary(p)
print(p)
par(mar=c(2, 2, 2, 2))
plot(p)
PCA_Plot(p)
PCA_Plot_Secondary(p)
biplot(p)
#loading and varimax ratation;
rawLoadings = p$rotation %*% diag(p$sdev, nrow(p$rotation), nrow(p$rotation))
print(rawLoadings)
v = varimax(rawLoadings)
ls(v)
v

# The Psych package has a wonderful PCA function that allows many more options
# including build-in factor rotation, specifiying a number of factors to include 
# and automatic "score" generation
p2 = psych::principal(bfiReduced2, rotate="varimax", nfactors=(8), scores=TRUE)
p2
#print out the loading cut off point, in this case cut off = .4;
#get total 6 components since your cut off value set as .4;
print(p2$loadings, cutoff=.4, sort=T)
p2$loadings
p2$values
p2$communality
p2$rot.mat

v$loadings
#finde the socres among with each components;
scores <-p2$scores
#find the minimum socre among with each components;
min_pca1Score <- min(scores[,1])
min_pca1Score
min_pca2Score <- min(scores[,2])
min_pca2Score
min_pca3Score <- min(scores[,3])
min_pca3Score
min_pca4Score <- min(scores[,4])
min_pca4Score
min_pca5Score <- min(scores[,5])
min_pca5Score
min_pca6Score <- min(scores[,6])
min_pca6Score
min_pca7Score <- min(scores[,7])
min_pca7Score
min_pca8Score <- min(scores[,8])
min_pca8Score
#find the maximum socre among with each components;
max_pca1Score <- max(scores[,1])
max_pca1Score
max_pca2Score <- max(scores[,2])
max_pca2Score
max_pca3Score <- max(scores[,3])
max_pca3Score
max_pca4Score <- max(scores[,4])
max_pca4Score
max_pca5Score <- max(scores[,5])
max_pca5Score
max_pca6Score <- max(scores[,6])
max_pca6Score
max_pca7Score <- max(scores[,7])
max_pca7Score
max_pca8Score <- max(scores[,8])
max_pca8Score
# Alternatively, we could use sort function; this function will give you
# the indexof each score;
scores <- sort(p2$scores[,1])
scores <- sort(p2$scores[,2])
scores <- sort(p2$scores[,3])
scores <- sort(p2$scores[,4])
scores <- sort(p2$scores[,5])
scores <- sort(p2$scores[,6])
scores <- sort(p2$scores[,7])
scores <- sort(p2$scores[,8])



fit = factanal(bfiReduced2, 8)
print(fit$loadings, cutoff=.4, sort=T)
summary(fit)

#######################################################################################
#Using Factoextra
library(factoextra)
#if does not work and get error: invalid graphics state,enter dev.off() 
#before running the fviz;
p3 <- prcomp(bfiReduced2, scale = TRUE) 
fviz_eig(p3, choice = "eigenvalue")
#PCA Individuals
fviz_pca_ind(p3,
             col.ind = "cos2", # Color by the quality of representation
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     # Avoid text overlapping
)

#PCA Variables
fviz_pca_var(p3,
             col.var = "contrib", # Color by contributions to the PC
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     # Avoid text overlapping
)

#Biplot
fviz_pca_biplot(p3, repel = TRUE,
                col.var = "#2E9FDF", # Variables color
                col.ind = "#696969"  # Individuals color
)

library("FactoMineR")
p4 <- PCA(bfiReduced2, graph = FALSE)
#IF graph is set to true, it will provide the individual and variable maps

#Shows all the objects or functions available in PCA
print(p4)

#Options for providing screeplot
fviz_eig(p4, addlabels = TRUE, ylim = c(0, 35))
fviz_screeplot(p4, addlabels = TRUE, ylim = c(0, 35))

variables <- get_pca_var(p4)

#Which variables contibute the most to the PCs?
#there are ll variables
head(variables$contrib, 11)

library("corrplot")
corrplot(variables$contrib, is.corr=FALSE)    

# Contributions of variables to PC1
fviz_contrib(p4, choice = "var", axes = 1, top = 10)
# Contributions of variables to PC2
fviz_contrib(p4, choice = "var", axes = 2, top = 10)


library(ade4)
p5 <- dudi.pca(bfiReduced2,
               scannf = FALSE,   # Hide scree plot
               nf = 4           # Number of components kept in the results
)
fviz_screeplot(p5, addlabels = TRUE, ylim = c(0, 35))

variables2 <- get_pca_var(p5)

#Which variables contibute the most to the PCs?
#there are ll variables
head(variables2$contrib, 11)

library("corrplot")
corrplot(variables2$contrib, is.corr=FALSE)    