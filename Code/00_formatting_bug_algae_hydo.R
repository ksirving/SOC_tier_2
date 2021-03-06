## formatting hydro and extracting csci/asci sites

setwd("/Users/katieirving/Documents/git/SOC_tier2/SOC_tier_2")

library(CSCI)
library(reshape)
library(reshape2)
# install.packages("reshape")
library(ggplot2)
library(lubridate)
library(tidyr)

## SOC sites

soc <- read.csv("Data/CSCI_ASCI_SitesAll_for_SOC_FlowEcologyStudy.csv")
head(soc)
dim(soc) ## 841
unique(soc$BugID)

## bug sites - needs to be updated
csci<-read.csv("Data/Liesl_CSCI_deltaHsites_soCA_regionalcurves_060420.csv")
# csci<-read.csv("Data/DeltaH_CSCI_MMI.CSV")
head(csci) ## 524
unique(csci$stationcode) ## 709


## match sites

csci_sites <- unique(csci$stationcode)
soc_sites <- unique(soc$BugID)

soc_sites %in% csci_sites
sum(soc_sites %in% csci_sites) ## 409

## all original sites match, not 800+ sites from Kris

## kris data

dh_data <- read.csv("/Users/katieirving/Documents/git/SOC_tier2/SOC_tier_2/Data/deltaH_summary_all_KTQ.csv")
head(dh_data)
unique(dh_data$site)
## match sites to csci sites

csci_sites <- unique(csci$stationcode)
delta_sites <- unique(dh_data$site)

delta_sites %in% csci_sites
sum(delta_sites %in% csci_sites) # 409

csci_sites %in% delta_sites
sum(csci_sites %in% delta_sites) # 409

### extract only csci sites in delta sites

delta_csci <- subset(csci, stationcode %in% delta_sites)
dim(delta_csci)
head(delta_csci)
names(delta_csci)
delta_csci <- delta_csci[,c(1,3,4,18,20,22,43,44)]

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

# csci_sub <- csci[, c(1:10)]
# dim(csci_sub) ## 660 - replicate samples - take the most recent
# ## use duplicate to remove all duplicates except for last
# csci_sub<- csci_sub[ !duplicated(csci_sub[, c("Bug_StationCode")], fromLast=T),]
# dim(csci_sub) ## 524
# 
# unique(csci_sub$Bug_StationCode)

## join with hydro data 

names(dh_medianx)
names(csci_sub)
names(delta_csci)

csci_sub <- delta_csci

## each type of metric
all_dat_med <- merge(csci_sub, dh_medianx, by.x="stationcode", by.y="site")
dim(all_dat_med) ## 409 

all_dat_mean <- merge(csci_sub, dh_meanx, by.x="stationcode", by.y="site")
dim(all_dat_mean) ## 409 

all_dat_min <- merge(csci_sub, dh_minx, by.x="stationcode", by.y="site")
dim(all_dat_min) ## 409

all_dat_max <- merge(csci_sub, dh_maxx, by.x="stationcode", by.y="site")
dim(all_dat_max) ## 409 


head(all_dat_med)
## save all 
write.csv(all_dat_med, "output_data/00_csci_delta_formatted_median_updated.csv")
write.csv(all_dat_mean, "output_data/00_csci_delta_formatted_mean_updated.csv")
write.csv(all_dat_min, "output_data/00_csci_delta_formatted_min_updated.csv")
write.csv(all_dat_max, "output_data/00_csci_delta_formatted_max_updated.csv")

names(all_dat_max)


#### algae
## upload asci data

diat <- read.csv("Data/asci.d.csv")
hybrid <- read.csv("Data/asci.hybrid.csv")
sba <- read.csv("Data/asci.sba.csv")

## extract columns needed
diat <- diat[,c(1, 15)]
colnames(diat) <- c("sample_id", "asci_d")
hybrid <- hybrid[,c(1, 15)]
colnames(hybrid) <- c("sample_id", "asci_h")
sba <- sba[,c(1, 15)]
colnames(sba) <- c("sample_id", "asci_s")

## combine dfs

all_asci <- merge(diat, hybrid, by="sample_id", all=T)
all_asci <- merge(all_asci, sba, by="sample_id", all=T)
head(all_asci)

algae <- separate(all_asci, col = sample_id , into=c("StationID", "MM", "DD", "YY", "Rep", "Rep2"), remove = F) 

which(!is.na(algae))
nas <- which(!is.na(algae$Rep2))

#  all station IDs with the extra element
weird_station_IDs <- unique(algae[nas, 2])
weird_station_IDs
# vector with index positions
wsid <- algae$StationID %in% weird_station_IDs
# sum(wsid) #26
#  change values using index
algae[wsid,2] <- paste(algae$StationID[wsid], "_", algae$MM[wsid], sep="")
algae[wsid,3] <- paste(algae$DD[wsid])
algae[wsid,4] <- paste(algae$YY[wsid])
algae[wsid,5] <- paste(algae$Rep[wsid])
algae[wsid,6] <- paste(algae$Rep2[wsid])

head(algae)

unique(algae$StationID)

algae$sampledate = ymd(paste0(algae$YY,"-", algae$MM, "-",algae$DD))

#  remove Rep column 

algae$Rep2 <- NULL
algae$Rep <- NULL
sum(is.na(algae)) # 437

## use duplicate to remove all duplicates except for last
asci_sub<- algae[ !duplicated(algae[, c("StationID")], fromLast=T),]
head(asci_sub)
sum(is.na(asci_sub$sampledate)) #0
sum(is.na(asci_sub)) ## 368

## check nas and remove
ns <- which(is.na(asci_sub))
asci_sub[ns,] ## all whole rows - can delete
asci_sub <- na.omit(asci_sub)
# asci_sub["312PS0099", ]

## delta H  data

dh_data <- read.csv("/Users/katieirving/Documents/git/SOC_tier2/SOC_tier_2/Data/deltaH_summary_all_KTQ.csv")
head(dh_data)
unique(dh_data$site)
## match sites to csci sites

asci_sites <- unique(asci_sub$StationID)
delta_sites <- unique(dh_data$site)

delta_sites %in% asci_sites
sum(delta_sites %in% asci_sites) # 261

asci_sites %in% delta_sites
sum(asci_sites %in% delta_sites) #261

### extract only asci sites in delta sites

delta_asci <- subset(asci_sub, StationID %in% delta_sites)
dim(delta_asci)
head(delta_asci)

## remove date calumns

delta_asci <- delta_asci[,-c(3:5)]


## only median delta
dh_median <- subset(dh_data, summary.statistic =="median")

head(dh_median)
dh_median$deltaH ## lots of zeros


## median
dh_medianx <- dh_median[,c(1,4,7)]
head(dh_medianx)
dim(dh_medianx) ## 9897
dh_medianx <- dcast(dh_medianx, site~flow_metric) # 443 ( waiting for additional bio sites)


head(asci_sub)
head(dh_medianx)

## merge asci with delta H
all_dat_med <- merge(delta_asci, dh_medianx, by.x="StationID", by.y="site")
dim(all_dat_med) ## 261 (correct number from match above, waiting for updated asci values)
head(all_dat_med)

## save 

write.csv(all_dat_med, "output_data/00_asci_delta_formatted_median.csv")
  
