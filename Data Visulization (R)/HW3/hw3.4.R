library(ggplot2)
library(zoo)
library(lubridate)
library(stringr)
library(treemap)

company = read.csv("/Users/appobs/Desktop/hw/465/week7/hw3/datasets/company.csv")
head(company)
treemap(company,
        index = c("company", "division", "office"),
        vSize = "budget",
        vColor = "company"
        )

# options
treemap(company,
        index = c("company", "division", "office"),
        vSize = "budget",
        vColor = "office",
        palette=terrain.colors(10)
          
        )