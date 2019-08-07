library(foreign)  # Allows us to read spss files!
library(corrplot)
library(car)
library(QuantPsyc)
library(leaps)
library(RColorBrewer)
library(Hmisc)
library(psych)
library(readxl)

# Set the working director
work_dir <- "/Users/appobs/Desktop/hw/424/hw1/"
setwd(work_dir)
# Read in the HousePrice.xlsx
hp2 <- read_excel("/Users/appobs/Desktop/hw/424/hw1/housing_prices2.xlsx")
View(hp2)

#basic info
str(hp2)
#1st var info
head(hp2)
#desceiptive table
describe(hp2)
#Checking missing value, result 0 meaning No missing
sum(is.na(hp2$x6))
#Handle missing value
#!hp2.1 <- na.omit(hp2)

#Construct dummy vairalbr for Fuel.Type
fuel = c(1,0,0,0,0,0,0)
names(fuel) = c("Electric", "Gas", "None", "Oil", "Solar", "Unknown/Other", "Wood")
hp2$Fuel.Type.f <- fuel[hp2$Fuel.Type]

#Construct dummy vairalbr for Heat.Type
heat = c(1,0,0,0)
names(heat) = c("Electric", "Hot Water", "Hot Air", "None")
hp2$Heat.Type.f <- heat[hp2$Heat.Type]

#Construct dummy vairalbr for Heat.Type
sewer = c(0,1,0)
names(sewer) = c("None/Unknown", "Private", "Public")
hp2$Sewer.Type.f <- sewer[hp2$Sewer.Type]

hp2$Fuel.Type.c <- factor(hp2$Fuel.Type, labels = c("Electric", "Gas", "None", "Oil", "Solar", "Unknown/Other", "Wood"))
tapply(hp2$Price, hp2$Fuel.Type.c, mean)
#the contrast matrix for categorical variable with levels
contr.treatment(7)
#assigning the treatment contrasts to Fuel.Type.f
contrasts(hp2$Fuel.Type.c) = contr.treatment(7)
#sample the regression
summary(lm(Price ~ Fuel.Type.c, data=hp2))


#Alternative way
#!Fuel.Type <- as.factor(c("Electric", "Gas", "None", "Oil", "Solar", "Unknown/Other", "Wood"))
#!Fuel.Type
#!unclass(Fuel.Type)

#Change Variable Names
hpNumeric = hp2[, c(1, 2:7, 11:19)]
head(hpNumeric)
#!figure margins too large
plot(hpNumeric)
#Check data types
str(hpNumeric)
# Compute the correlation matrix and visualize it
cor.hp2 = cor(hpNumeric)
cor.hp2
corrplot(cor.hp2, method="ellipse")
corrplot(cor.hp2, method="number")
corrplot(cor.hp2, method="circle",col=c("yellow", "red","blue","green"))
corrplot(cor.hp2, method="color", col=brewer.pal(n=8, name="RdYlBu"))
#Show full color palette
display.brewer.all()

#Fitting regression 
fullmodel = (lm(Price ~ ., hpNumeric))
summary(fullmodel)
#Info about beta
lm.beta.fullmodel <- lm.beta(fullmodel)
lm.beta.fullmodel
# And compute the vif scores to get an idea of the multicolinearities here!
vif(fullmodel)
#Refer to Correlation Plot for Correlations of Fuel.Type.f and Heat.Type.f
corrplot(cor.hp2, method="number")
fullmodel1 = lm(Price ~ . - Fuel.Type.f - Heat.Type.f, data=hpNumeric)
vif(fullmodel1)
# Try adding another parameter (this one has the most correlation with the residuals of the last fit)
fit1 = lm(Price ~ Living.Area + Rooms, data=hpNumeric)
summary(fit1)

###################################################################
# Automated fitting
###################################################################

hpSubsets = regsubsets(Price ~ Lot.Size + Waterfront + Age + Land.Value + New.Construct
                       + Central.Air + Living.Area + Pct.College + Bedrooms + Fireplaces
                       + Bathrooms + Rooms + Fuel.Type.f + Heat.Type.f + Sewer.Type.f, data=hpNumeric, nbest=10)
hpSubsets
plot(hpSubsets, scale="adjr2")

bestR2Fit = lm(Price ~ Waterfront + Land.Value + Living.Area + Bathrooms, data=hpNumeric)
summary(bestR2Fit)

# The R function "step" can perform stepwise regression, but to get going, we need to feed 
# it the two "bounding" models so that it knows what to work with
null = lm(Price ~ 1, data=hpNumeric)
null

full = lm(Price ~ ., data=hpNumeric)
full

## First we do a forward search - Forward Stepwise
hpForward = step(null, scope = list(lower=null, upper=full), direction="forward")
summary(hpForward)
# Compare the results to the full search above

#decide to start with backfard selection lowe & upper meaning Y and X at last code
# Next do a backward search - Backward Stepwise
hpBackward = step(full, direction="backward")
summary(hpBackward)

# Finally we do a "stepwise" search combining the two - Both Forward and Backward Stepwise
hpStep = step(null, scope = list(upper=full), direction="both")
summary(hpStep)
summary(hpStep)
summary(hpStep)

# Things are really nice if they all agree!
dim(hpNumeric)

#corr test
library(psych)
options("scipen"=100, "digits"=2)
cor(hpNumeric[, 2:16])
MCorrTest = corr.test(hpNumeric[, 2:16], adjust="none")
MCorrTest

M = MCorrTest$p
M

# The probability matrix uses a different test above the diagonal, we are just interested in 
# the entries below the diagonal, so we make it symmetric
for (i in 2:nrow(M))
  for (j in 1:(i-1))  # Only grab elements below the diagonal
    M[j, i] = M[i,j]  # Copy into the corresponding element above the diagonal
M

# Now, for each element, see if it is < .01 (or whatever significance) and set the entry to 
# true = significant or false
MTest = ifelse(M < .01, T, F)
MTest
# Now lets see how many significant correlations there are for each variable.  We can do
# this by summing the columns of the matrix
colSums(MTest) - 1  # We have to subtract 1 for the diagonal elements (self-correlation)
