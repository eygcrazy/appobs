library(CCA)
library(yacca)
library(MASS)
library(foreign)  # Allows us to read spss files!
library(corrplot)
library(psych)
work_dir <- "/Users/appobs/Desktop/hw/424/hw3"
setwd(work_dir)
# Read in the file
ml <- read.csv("/Users/appobs/Desktop/would you?/424/hw3/data_marsh_cleaned.csv", skip =1)
names(ml) = c('STATION', 'MEHGSWB' ,'TURB', 'DOCSWD' ,'SRPRSWFB', 'THGFSFC', 'THGSDFC', 'TCSDFB','TPRSDFB')
head(ml) 

# delete first info row
#ml <- ml[2:nrow(ml), ]

#basic info
str(ml)
#1st var info
head(ml)
#desceiptive table
describe(ml)
#Checking missing value, result 0 meaning No missing
sum(is.na(ml))

###################################################################
# Exploring correlations between sepal and petal
###################################################################
#Investigating Correlations

round(cor(ml[, 1:9]), 2)

###################################################################
# This is a nice function for computing the Wilks lambdas for 
# CCA data from the CCA library's method
# It computes the wilkes lambas the degrees of freedom and te 
# p-values
###################################################################

ccaWilks = function(set1, set2, cca)
{
  ev = ((1 - cca$cor^2))
  ev
  
  n = dim(set1)[1]
  p = length(set1)
  q = length(set2)
  k = min(p, q)
  m = n - 3/2 - (p + q)/2
  m
  
  w = rev(cumprod(rev(ev)))
  
  # initialize
  d1 = d2 = f = vector("numeric", k)
  
  for (i in 1:k) 
  {
    s = sqrt((p^2 * q^2 - 4)/(p^2 + q^2 - 5))
    si = 1/s
    d1[i] = p * q
    d2[i] = m * s - p * q/2 + 1
    r = (1 - w[i]^si)/w[i]^si
    f[i] = r * d2[i]/d1[i]
    p = p - 1
    q = q - 1
  }
  
  pv = pf(f, d1, d2, lower.tail = FALSE)
  dmat = cbind(WilksL = w, F = f, df1 = d1, df2 = d2, p = pv)
}
###################################################################
# Now, lets do some computation
###################################################################

water = ml[, 2:6]
soil = ml[, 7:9]

# This gives us the cannonical correlates, but no significance tests
c = cancor(water, soil)
c
# The CCA library has more extensive functionality
library(CCA)

#Breakdown of the Correlations(same result as c = cancor(water, soil))
matcor(water, soil)

#Correlations between water & soil (X)
#Correlations between soil & water (Y)

ccLm = cc(water, soil)
ccLm$cor
#Funcrions for CCA
ls(ccLm)

#XCoef Correlations
ccLm$xcoef

#YCoef Correlations
ccLm$ycoef

#Calculate Scores
loadingsMl = comput(water, soil, ccLm)
ls(loadingsMl)

#Correlation X Scores
loadingsMl$corr.X.xscores

#Correlation Y Scores
loadingsMl$corr.Y.yscores

#Wilk's Lambda Test
wilksml = ccaWilks(water, soil, ccLm)
round(wilksml, 2)

# Now, let's calcualte the standardized coefficients(important variables)
s1 = diag(sqrt(diXag(cov(water))))
s1 %*% ccLm$xcoef

s2 = diag(sqrt(diag(cov(soil))))
s2 %*% ccLm$ycoef

# A basic visualization of the cannonical correlatioxn
plt.cc(ccLm)

###################################################################
#Extra credit
###################################################################
work_dir <- "/Users/appobs/Desktop/hw/424/hw3"
setwd(work_dir)
sp <- read.csv("/Users/appobs/Desktop/hw/424/hw3/sports.csv")
View(sp)

head(sp)
str(sp)

library(ca)
# first read the original data file as vector;
# Then create 5x5 matrix;
# mapping the variable into the matrix;
data=as.numeric(unlist(sp))
data=matrix(data,ncol=5,nrow = 5)
rownames(data)=c('Agree_strongly','Agree','Neither_nor','Disagree','Disagree_strongly')
colnames(data)=c('UK','USA','Russia','Spain','France')
data

library(vcd)
mosaic(data, shade=TRUE, legend=TRUE)

fit = ca(data)
summary(fit)
fit

plot(fit)

plot(fit, mass=T, contrib="absolute", 
     map="rowgreen", arrows=c(F, T))

# Now lets look at women
prop.table(Women, 1)  # Row Percentages
prop.table(Women, 2)  # Col Percentages

fit = ca(Women)
summary(fit)
fit

plot(fit)
Women

plot(fit, mass=T, contrib="absolute", 
     map="rowgreen", arrows=c(F, T))

