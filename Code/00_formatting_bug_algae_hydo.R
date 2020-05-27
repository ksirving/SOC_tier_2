## formatting hydro and extracting csci/asci sites

setwd("/Users/katieirving/Documents/git/SOC_tier2/SOC_tier_2")

library(CSCI)
library(reshape)
library(reshape2)
# install.packages("reshape")
library(ggplot2)

## SOC sites

soc <- read.csv("Data/CSCI_ASCI_SitesAll_for_SOC_FlowEcologyStudy.csv")
head(soc)
dim(soc) ## 841
unique(soc$BugID)

## bug sites - needs to be updated

csci<-read.csv("Data/DeltaH_CSCI_MMI.CSV")
head(csci) ## 524
unique(csci$Bug_StationCode)


## match sites

csci_sites <- unique(csci$Bug_StationCode)
soc_sites <- unique(soc$BugID)

soc_sites %in% csci_sites
sum(soc_sites %in% csci_sites) ## 524

## all original sites match, not 800+ sites from Kris

## kris data

dh_data <- read.csv("/Users/katieirving/Documents/git/SOC_tier2/SOC_tier_2/Data/deltaH_summary_all_KTQ.csv")
head(dh_data)
unique(dh_data$site)
## match sites to csci sites

csci_sites <- unique(csci$Bug_StationCode)
delta_sites <- unique(dh_data$site)

delta_sites %in% csci_sites
sum(delta_sites %in% csci_sites) # 273

csci_sites %in% delta_sites
sum(csci_sites %in% delta_sites) #273

### extract only csci sites in delta sites

delta_csci <- subset(csci, Bug_StationCode %in% delta_sites)
dim(delta_csci)
head(delta_csci)

## format delta to columns
## subset hydro data to only median for testing

dh_median <- subset(dh_data, summary.statistic =="median")
dh_mean <- subset(dh_data, summary.statistic =="mean")
dh_min <- subset(dh_data, summary.statistic =="min")
dh_max <- subset(dh_data, summary.statistic =="max")
head(dh_median)
dh_median$deltaH ## lots of zeros
dh_mean$deltaH
dh_min$deltaH
dh_max$deltaH

## median
dh_medianx <- dh_median[,c(1,4,7)]
head(dh_medianx)
dim(dh_medianx) ## 9897
dh_medianx <- dcast(dh_medianx, site~flow_metric) # 443 ( waiting for additional bio sites)

## mean
dh_meanx <- dh_mean[,c(1,4,7)]
head(dh_meanx)
dim(dh_meanx) ## 9897
dh_meanx <- dcast(dh_meanx, site~flow_metric) # 443 ( waiting for additional bio sites)

## min

dh_minx <- dh_min[,c(1,4,7)]
head(dh_minx)
dim(dh_minx) ## 9897
dh_minx <- dcast(dh_minx, site~flow_metric) # 443 ( waiting for additional bio sites)

## max
dh_maxx <- dh_max[,c(1,4,7)]
head(dh_maxx)
dim(dh_maxx) ## 9897
dh_maxx <- dcast(dh_maxx, site~flow_metric) # 443 ( waiting for additional bio sites)


## remove replicates
head(csci_sub)

csci_sub <- csci[, c(1:10)]
dim(csci_sub) ## 660 - replicate samples - take the most recent
## use duplicate to remove all duplicates except for last
csci_sub<- csci_sub[ !duplicated(csci_sub[, c("Bug_StationCode")], fromLast=T),]
dim(csci_sub) ## 524

unique(csci_sub$Bug_StationCode)

## join with hydro data 

names(dh_medianx)
names(csci_sub)

## each type of metric
all_dat_med <- merge(csci_sub, dh_medianx, by.x="Bug_StationCode", by.y="site")
dim(all_dat_med) ## 273 (correct number from match above, waiting for updated csci values)

all_dat_mean <- merge(csci_sub, dh_meanx, by.x="Bug_StationCode", by.y="site")
dim(all_dat_mean) ## 273 (correct number from match above, waiting for updated csci values)

all_dat_min <- merge(csci_sub, dh_minx, by.x="Bug_StationCode", by.y="site")
dim(all_dat_min) ## 273 (correct number from match above, waiting for updated csci values)

all_dat_max <- merge(csci_sub, dh_maxx, by.x="Bug_StationCode", by.y="site")
dim(all_dat_max) ## 273 (correct number from match above, waiting for updated csci values)


head(all_dat_med)
## save all 
write.csv(all_dat_med, "output_data/00_csci_delta_formatted_median.csv")
write.csv(all_dat_mean, "output_data/00_csci_delta_formatted_mean.csv")
write.csv(all_dat_min, "output_data/00_csci_delta_formatted_min.csv")
write.csv(all_dat_max, "output_data/00_csci_delta_formatted_max.csv")

names(all_dat_max)
