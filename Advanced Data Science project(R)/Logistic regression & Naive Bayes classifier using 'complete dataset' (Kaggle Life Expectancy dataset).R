library(CCA)
library(yacca)
library(MASS)
library(foreign)  # Allows us to read spss files!
library(corrplot)
library(psych)
library(ca)
library(car)
library(aod)
library(ggplot2)
library(caret)
library(leaps)
library(graphics)
library(rms)
library(reshape2)
library(ggplot2)
work_dir <- "/Users/appobs/Desktop/hw/424/project"
setwd(work_dir)
# Read in the file
LF <- read.csv("/Users/appobs/Desktop/hw/424/project/completeData.csv")

#basic info
str(LF)
#1st var info
head(LF)
#desceiptive table
describe(LF)
#Checking missing value, result 0 meaning No missing
sum(is.na(LF))

LFReduced = LF[,c(2:23)]
head(LFReduced)
str(LFReduced)


#Change Variable Names in order
LFReduced = LFReduced[, c(3, 1:2, 4:22)]
head(LFReduced)

## two-way contingency table of categorical outcome and predictors we want
## to make sure there are not 0 cells
xtabs(~Status + Year, data = LFReduced)

#Construct dummy vairalbr for status
d.status = c(1,0)
names(d.status) = c("Developed", "Developing")
LFReduced$Status.f <- d.status[LFReduced$Status]

# Sort it again: drop original Status, and put the Status.f in first column;
LFReduced = LFReduced[,c(2:23)]
LFReduced = LFReduced[, c(22, 1:21)]

# Histogram, makes sense since we could see the frequency
hist(LFReduced$Status.f, col="blue", xlab = "distribution of Status", main = "histogram")

#########################################################
# this method does not work, but good for tring!
# The R function "step" can perform stepwise regression, but to get going, we need to feed 
# it the two "bounding" models so that it knows what to work with
# null = lm(Status.f ~ 1, data=train)
# null

# full = lm(Status.f ~ ., data=train)
# full

## Apply the backward selection
# lfBackward = step(null, scope = list(lower=null, upper=full), direction="backward")
# summary(lfBackward)

## Apply the fardward selection
# lfforward = step(null, scope = list(lower=null, upper=full), direction="forward")
# summary(lfforward)
#########################################################

# We convert rank to a factor to indicate that rank should be treated as a 
# categorical variable.

LFReduced$Year <- factor(LFReduced$Year)
mylogit <- glm(Status.f ~ Year+ Life.expectancy+ Adult.Mortality+ infant.deaths+
                 Alcohol+ percentage.expenditure+ Hepatitis.B+ Measles+  BMI+ 
                 under.five.deaths+ Polio+Total.expenditure+ Diphtheria+ HIV.AIDS+
                 GDP+ Population+ thinness..1.19.years+thinness.5.9.years+ 
                 Income.composition.of.resources+ Schooling,
               data = LFReduced, family = "binomial")

summary(mylogit)

# then calculate McFadden's R squared using the fitted model log likelihood values
# The pseudo R2 of this is based on the McFadden's R squared:R2 = 1-M~full/M~inter

nulllogit = glm(Status.f ~ 1, data = LFReduced, family = "binomial")
1-logLik(mylogit)/logLik(nulllogit) #R = 0.69 or 0.7


# We can use the confint function to obtain confidence 
# intervals for the coefficient estimates.
# And compute the vif scores to get an idea of the multicolinearities here!
vif(mylogit)

## need to remove multicoolinearity var:under.five.deaths from full model;
mylogit2 <- glm(Status.f ~ Year+ Life.expectancy+ Adult.Mortality+ infant.deaths+
                 Alcohol+ percentage.expenditure+ Hepatitis.B+ Measles+  BMI+ 
                 Polio+Total.expenditure+ Diphtheria+ HIV.AIDS+
                 GDP+ Population+ thinness..1.19.years+thinness.5.9.years+ 
                 Income.composition.of.resources+ Schooling,
               data = LFReduced, family = "binomial")

summary(mylogit2)

# to see the pseudo R^2 from above model2;
nulllogit2 = glm(Status.f ~ 1, data = LFReduced, family = "binomial")
1-logLik(mylogit2)/logLik(nulllogit2) #R = 0.69 or 0.7
vif(mylogit2)
###################################################################
# Mullticollinearity problem has been handled by removing the var:under.five.deaths
###################################################################

## CIs using profiled log-likelihood
confint(mylogit2)
## CIs using standard errors
confint.default(mylogit2)

# Test for an overall effect of year using the wald.test function of the aod library.
# terms 5:16 represents that levels for the Year;
wald.test(b = coef(mylogit2), Sigma = vcov(mylogit2), Terms = 5:16)

## Exponentiate the coefficients and interpret them as odds-ratios;
## odds ratios only

###############################################################
# What does the slope β1Alcohol = 1.3836 mean?
# Log odds log(p/(1-p)) of Failure increase by 1.3836, for when Alcohol are present (i.e. =1)
# Using the anti-log function exp(1.3836) = 6.27. The odds p/(1-p) of Failure increases by 527%, when sediments = 1  i.e. [ (6.27-1)*100]

###############################################################
exp(coef(mylogit2))
## odds ratios and 95% CI
exp(cbind(OR = coef(mylogit2), confint(mylogit2)))

# Split the dataset as train and test, to use the test to run the model selection;
# 75% of train;
set.seed(500)

sample = sample.int(n = nrow(LFReduced), size = floor(.75 * nrow(LFReduced)), replace = F)

train = LFReduced[sample, ]
test = LFReduced[-sample, ]

# Define train control for k fold cross validation
train_control <- trainControl(method="cv", number=5)

## need to convert Status.f as factor, since the bayes model would not recognize
## the Status.f as numeric 
LFReduced$Status.f <- factor(LFReduced$Status.f)

# Fit Naive Bayes Model
model <- train(Status.f~., data=LFReduced, trControl=train_control, method="nb")
# Summarise Results
print(model)


###################################################################
# fitting model
###################################################################

mylogit3 <- glm(Status.f ~ Year+ Life.expectancy+ Adult.Mortality+ infant.deaths+
                 Alcohol+ percentage.expenditure+ Hepatitis.B+ Measles+  BMI+ 
                 Polio+Total.expenditure+ Diphtheria+ HIV.AIDS+
                 GDP+ Population+ thinness..1.19.years+thinness.5.9.years+ 
                 Income.composition.of.resources+ Schooling,
               data = train, family = "binomial")

summary(mylogit3)

# Write the dataset without under5 in train;

tran_new = train[, -c(12)]
# to see the pseudo R^2 from above model2;
nulllogit3 = glm(Status.f ~ 1, data = tran_new, family = "binomial")
1-logLik(mylogit3)/logLik(nulllogit3) #R = 0.69 or 0.7
vif(mylogit3)

## odds ratios and 95% CI
exp(cbind(OR = coef(mylogit3), confint(mylogit3)))


newdata1 <- with(tran_new, data.frame(Life.expectancy = mean(Life.expectancy), Adult.Mortality = mean(Adult.Mortality),
                                   Adult.Mortality = mean(Adult.Mortality), infant.deaths = mean(infant.deaths),
                                   Alcohol = mean(Alcohol), percentage.expenditure = mean(percentage.expenditure),
                                   Hepatitis.B = mean(Hepatitis.B), Measles = mean(Measles), 
                                   BMI = mean(BMI), Polio = mean(Polio), Total.expenditure = mean(Total.expenditure),
                                   Diphtheria = mean(Diphtheria), HIV.AIDS = mean(HIV.AIDS), 
                                   GDP = mean(GDP), Population = mean(Population), thinness..1.19.years = mean(thinness..1.19.years),
                                   thinness.5.9.years = mean(thinness.5.9.years), Income.composition.of.resources = mean(Income.composition.of.resources),
                                   Schooling = mean(Schooling), Year = factor(2005:2015)))
                 
newdata1

#########################################################
#levels(droplevels(tran_new$Year))
newdata1$YearP <- predict(mylogit3, newdata = newdata1, type = "response")
newdata1

# Interpretion:For example, year2005 the predicted probability of being develped country
# is 2.22(222%)  holding other variables at their means
#########################################################

#########################################################
#The code to generate the predicted probabilities (the first line below) is the same 
#as before, except we are also going to ask for standard errors so we can plot a confidence interval.
tran_new$Life.expectancy = rownames(tran_new)
melt(tran_new)

newdata2 <- with(tran_new, data.frame(Life.expectancy = rep(seq(from = 0, to = 200, length.out = 100), 11), Adult.Mortality = mean(Adult.Mortality),
                                      Adult.Mortality = mean(Adult.Mortality), infant.deaths = mean(infant.deaths),
                                      Alcohol = mean(Alcohol), percentage.expenditure = mean(percentage.expenditure),
                                      Hepatitis.B = mean(Hepatitis.B), Measles = mean(Measles), 
                                      BMI = mean(BMI), Polio = mean(Polio), Total.expenditure = mean(Total.expenditure),
                                      Diphtheria = mean(Diphtheria), HIV.AIDS = mean(HIV.AIDS), 
                                      GDP = mean(GDP), Population = mean(Population), thinness..1.19.years = mean(thinness..1.19.years),
                                      thinness.5.9.years = mean(thinness.5.9.years), Income.composition.of.resources = mean(Income.composition.of.resources),
                                      Schooling = mean(Schooling), Year = factor(rep(2005:2015, each = 100))))

newdata2

#########################################################

#########################################################
# The code to generate the predicted probabilities (the first line below) 
# is the same as before, except we are also going to ask for standard errors 
# so we can plot a confidence interval
newdata3 <- cbind(newdata2, predict(mylogit3, newdata = newdata2, type = "link",
                                    se = TRUE))
newdata3 <- within(newdata3, {
  PredictedProb <- plogis(fit)
  LL <- plogis(fit - (1.96 * se.fit))
  UL <- plogis(fit + (1.96 * se.fit))
})

## view first few rows of final dataset
head(newdata3)
#########################################################

#########################################################
# It can also be helpful to use graphs of predicted probabilities to understand 
# and/or present the model. We will use the ggplot2 package for graphing. 
# Below we make a plot with the predicted probabilities, and 95% confidence intervals.
ggplot(newdata3, aes(x = Life.expectancy, y = PredictedProb)) + geom_ribbon(aes(ymin = LL,
        ymax = UL, fill = rank), alpha = 0.2) + geom_line(aes(colour = Year),
        size = 1)




#########################################################