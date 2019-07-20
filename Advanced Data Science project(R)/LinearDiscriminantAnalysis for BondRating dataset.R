library(CCA)
library(yacca)
library(MASS)
library(GGally)
library(RGCCA)
library(candisc)
library(readxl)  #Rear xls file
library(caret)


work_dir <- "/Users/appobs/Desktop/hw/424/hw4"
setwd(work_dir)
# Read in the file
BondRating <- read_excel("BondRating.xls",sheet = 'training',skip=2) 

## data cleaning;

new_BondRating=BondRating[,c(2:13)]
new_BondRating$ RATING
# The dependent variable : rating --categorical
## need to remove the CODERTG, since it messes up the RATING;
new_BondRating = new_BondRating[-c(2)]
BondRatingLDA = lda(RATING ~ ., data=new_BondRating)
BondRatingLDA

plot(BondRatingLDA)
# Try to predict the class from the original data
# Note ... this is JUST a test to see how this works
# In practice you will want to use cross-validation!

p = predict(BondRatingLDA, newdata=new_BondRating[,2:11])$class
p

# Compare the results of the prediction
table(p, BondRating$RATING)

pred.accuracy = round(mean(p == BondRating$RATING)*100,2)
pred.accuracy

# Setting "CV = T" will have the lda function perform
# "Leave-one-out" cross-validation
BondRatingLDA2 = lda(RATING ~ ., data=new_BondRating, CV=T)
BondRatingLDA2

table(BondRatingLDA2$class, BondRating$RATING)
#coef(BondRatingLDA)

pred.accuracy = round(mean(BondRatingLDA2$class == BondRating$RATING)*100,2)
pred.accuracy
###########################################################

validation<- read_excel("BondRating.xls",sheet = 'validation',skip=2) 
new_validation=validation[,c(2,4,5,6,7,8,9,10,11,12,13)] 

# The dependent variable : rating --categorical
p2 = predict(BondRatingLDA, newdata=new_validation[2:11])$class
table(p2, new_validation$RATING)
pred.accuracy2 = round(mean(p2 == new_validation$RATING)*100,2)
pred.accuracy2

