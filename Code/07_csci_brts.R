### CSCI brts

setwd("/Users/katieirving/Documents/git/SOC_tier2/SOC_tier_2")

library(gbm)
install.packages("dismo")
library(dismo)


## upload data

data <- read.csv("output_data/00_csci_delta_formatted_median.csv")
head(data)
## remove peak timing
names(data)
data <- data[,-c(26:28)]

## gbm functions in brt.functions script
?gbm.step
## CSCI
gbm.step(data, gbm.x = 12:33, gbm.y = 6)
         
tbrt <- gbm.step(data, gbm.x = 12:33, gbm.y = 6, 
                 family ="bernoulli", tree.complexity = 5, learning.rate = 0.0001, bag.fraction = 0.5) 
