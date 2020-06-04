### CSCI brts

setwd("/Users/katieirving/Documents/git/SOC_tier2/SOC_tier_2")

library(gbm)
install.packages("dismo")
library(dismo)


## upload data

data <- read.csv("output_data/00_csci_delta_formatted_median_updated.csv")
head(data)
## remove peak timing
names(data)
data <- data[,-c(24:26)]

## gbm functions in brt.functions script
?gbm.step
## CSCI
gbm.step(data, gbm.x = 10:31, gbm.y = 7)
         
tbrt <- gbm.step(data, gbm.x = 10:31, gbm.y = 7, 
                 family ="gaussian", tree.complexity = 5, learning.rate = 0.0001, bag.fraction = 0.5) 


tbrt2 <- gbm.step(data, gbm.x = 10:31, gbm.y = 7, 
                 family ="gaussian", tree.complexity = 5, learning.rate = 0.001, bag.fraction = 0.5) 

tbrt2$contributions

# Peak_2                 Peak_2 17.2523157
# Wet_BFL_Dur       Wet_BFL_Dur 13.2986249
# SP_Tim                 SP_Tim 12.6436707
# SP_ROC                 SP_ROC  9.6853021
# SP_Dur                 SP_Dur  5.8145340

# Peak Flow: Magnitude (2-year flood)
# Wet-Season Base Flow: Duration
# Spring Recession Flow: Timing
# Spring Recession Flow: Rate of Change
# Spring Recession Flow: Duration


### figures for only top 5 most important vars

## upload s curve data 
## negative delta h 
neg_csci <- read.csv("output_data/01_csci_neg_logR_metrics_figures.csv")

## positive delta h 
pos_csci <- read.csv("output_data/01_csci_pos_logR_metrics_figures.csv")

head(pos_csci)
head(neg_csci)

## list of metrics

metrics <- c("Peak_2", "Wet_BFL_Dur", "SP_Tim", "SP_ROC", "SP_Dur")
## subset to only important metrics

pos_csci_sub <- subset(pos_csci, hydro.endpoints %in% metrics)
head(pos_csci_sub)

neg_csci_sub <- subset(neg_csci, hydro.endpoints %in% metrics)
head(neg_csci_sub)

## figures

## postive delta H - CSCI
ggplot(pos_csci_sub, aes(x=hydro.threshold, y=PredictedProbability, color=biol.endpoints))+
  # geom_point(size=1)+
  geom_path()+#stat_summary(fun.y="mean", geom="line")+
  facet_wrap(~hydro.endpoints, scales="free_x", nrow=4)+scale_y_continuous(limits=c(0,1))+
  geom_vline(xintercept=0, linetype="dashed")+
  geom_point(aes(y=Condition, x=hydro), colour = 'black', size = 1)


## negative delta H - CSCI
ggplot(neg_csci_sub, aes(x=hydro.threshold, y=PredictedProbability, color=biol.endpoints))+
  # geom_point(size=1)+
  geom_path()+#stat_summary(fun.y="mean", geom="line")+
  facet_wrap(~hydro.endpoints, scales="free_x", nrow=4)+scale_y_continuous(limits=c(0,1))+
  geom_vline(xintercept=0, linetype="dashed")+
  geom_point(aes(y=Condition, x=hydro), colour = 'black', size = 1)

