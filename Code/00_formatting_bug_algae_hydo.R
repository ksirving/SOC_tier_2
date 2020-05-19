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
head(dh_median)
dh_median$deltaH ## lots of zeros

dh_medianx <- dh_median[,c(1,4,7)]
head(dh_medianx)
dim(dh_medianx) ## 9897

dh_medianx <- dcast(dh_medianx, site~flow_metric) # 443 ( waiting for additional bio sites)


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

all_dat <- merge(csci_sub, dh_medianx, by.x="Bug_StationCode", by.y="site")
dim(all_dat) ## 273 (correct number from match above, waiting for updated csci values)

head(all_dat)

write.csv(all_dat, "output_data/00_csci_delta_formatted.csv")



