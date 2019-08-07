###################################################################
#Extra credit (correspondence analysis on the countries and sports liking data in Sports.csv)
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

