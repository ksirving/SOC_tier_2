## updated analysis
setwd("/Users/katieirving/Documents/git/SOC_tier2/SOC_tier_2")

csci<-read.csv("Data/DeltaH_CSCI_MMI.CSV")
head(csci)
library(CSCI)
library(reshape)
# install.packages("reshape")
library(ggplot2)

## work flow
## redo figures with S curves
## median values only
## BRTs

## below defines thresholds from all csci
head(csci)
ref<-loadRefData()
endpoints<-c("CSCI","MMI","OoverE","Clinger_PercentTaxa_scored","Coleoptera_PercentTaxa_scored","EPT_PercentTaxa_scored",
             "Intolerant_Percent_scored","Shredder_Taxa_scored","Taxonomic_Richness_scored")

thresholds<-data.frame(Index=endpoints)
head(thresholds)
thresholds$Mean<-  sapply(endpoints, function(x) mean(ref[,x]))
thresholds$Mean[thresholds$Index %in% c("CSCI","OoverE")]<-1
thresholds$SD<-  sapply(endpoints, function(x) sd(ref[,x]))
thresholds$Thirtieth<-  sapply(1:length(thresholds$Mean), function(i)
  qnorm(p=.3, mean=thresholds[i,"Mean"], sd=thresholds[i,"SD"], lower.tail=T))
thresholds$Tenth<-  sapply(1:length(thresholds$Mean), function(i)
  qnorm(p=.1, mean=thresholds[i,"Mean"], sd=thresholds[i,"SD"], lower.tail=T))
thresholds$Index2<-  gsub("_scored","_score",    thresholds$Index)
thresholds$ObservedQuartile<-sapply(thresholds$Index2, function(ind)
{
  quantile(na.omit(csci[,ind]),.25, na.rm=T)
})

head(thresholds)
## from here add new formatted data
## median only so far
new_csci <- read.csv("output_data/00_csci_delta_formatted_median.csv")
head(csci)
head(new_csci)
biol.endpoints<-c("CSCI")#,
# "Clinger_PercentTaxa_score","Coleoptera_PercentTaxa_score","Taxonomic_Richness_score",
# "EPT_PercentTaxa_score","Shredder_Taxa_score","Intolerant_Percent_score")
## will change to FFM and deltaH
hydro.endpoints<- colnames(new_csci)[12:36]

#c("BFR","FracYearsNoFlow","HighDur","HighNum","Hydroperiod","LowDur", "LowNum","MaxMonth","MaxMonthQ","MedianNoFlowDays","MinMonth","MinMonthQ","NoDisturb","PDC50","Q01","Q05","Q10","Q25","Q50","Q75","Q90", "Q95","Q99","Qmax","QmaxIDR","Qmean","QmeanIDR","QmeanMEDIAN","Qmed","Qmin", "QminIDR","SFR")


bio_h_summary<-  expand.grid(biol.endpoints=biol.endpoints,hydro.endpoints=hydro.endpoints, stringsAsFactors = F)

# glm(CSCI.10.pf_factor ~ HighDur, family=binomial(link="logit"), data=subset(csci, HighDur<=0))
neg.glm<-lapply(1:nrow(bio_h_summary), function(i)
{
  
  hmet<-as.character(bio_h_summary[i,"hydro.endpoints"])
  bmet<-as.character(bio_h_summary[i,"biol.endpoints"])
  bmet.thresh<-thresholds[which(thresholds$Index2==bmet),"Tenth"]
  
  mydat<-na.omit(new_csci[,c(hmet, bmet)])
  names(mydat)<-c("hydro","bio")
  
  mydat[which(mydat$hydro<=0 ),]
  
  #   mydat$Condition<-ifelse(mydat$bio< bmet.thresh,"Poor","Healthy")
  #   mydat$Condition<-factor(mydat$Condition, levels=c("Poor","Healthy")) 
  #   mydat$Condition<-factor(mydat$Condition, levels=c("Poor","Healthy"))
  mydat$Condition<-ifelse(mydat$bio< bmet.thresh,0,1)
  mydat<-mydat[order(mydat$bio),]
  # print(dim(mydat))
  
  glm(Condition~hydro, family=binomial(link="logit"), data=mydat)
  write.csv(mydat, paste("output_data/csci_glm_data/06_glm_", bmet, hmet, ".csv"), sep="")
})
neg.glm
i=1

par(mfrow=c(4,5))
pos.glm<-lapply(1:nrow(bio_h_summary), function(i)
{
  hmet<-as.character(bio_h_summary[i,"hydro.endpoints"])
  bmet<-as.character(bio_h_summary[i,"biol.endpoints"])
  bmet.thresh<-thresholds[which(thresholds$Index2==bmet),"Tenth"]
  
  mydat<-na.omit(new_csci[,c(hmet, bmet)])
  names(mydat)<-c("hydro","bio")
  
  mydat[which(mydat$hydro>=0 ),]
  
  #   mydat$Condition<-ifelse(mydat$bio< bmet.thresh,"Poor","Healthy")
  #   mydat$Condition<-factor(mydat$Condition, levels=c("Poor","Healthy")) 
  #   mydat$Condition<-factor(mydat$Condition, levels=c("Poor","Healthy"))
  mydat$Condition<-ifelse(mydat$bio< bmet.thresh,0,1)
  mydat<-mydat[order(mydat$bio),]
  
  test <- glm(Condition~hydro, family=binomial(link="logit"), data=mydat)
  xval <- seq(min(mydat$hydro), max(mydat$hydro), 1)
  yval <- predict(test, list(hydro=xval), type="response")
 
  
  plot(mydat$hydro, mydat$Condition, ylab="probability", xlab=hmet)
  lines(xval, yval)
})


head(mydat)
xval <- seq(min(mydat$hydro), max(mydat$hydro), 1)
yval <- predict(test, list(hydro=xval), type="response")
yval

plot(mydat$hydro, mydat$Condition, ylab="probability", xlab=hmet)
lines(xval, yval)

# warnings()
head(new_csci)
hydro.m<-na.omit(unique(melt(new_csci[,hydro.endpoints])))
hydro.m<-hydro.m[order(hydro.m$variable,hydro.m$value),]
names(hydro.m)<-c("hydro.endpoints","hydro.threshold")
# head(hydro.m)
# i=6124
dim(bio_h_summary3)
length(unique(bio_h_summary3$NegPredicted))
unique(bio_h_summary3$Percentile)
## need dataframe with good/bad csci, predicted prob and pos/neg delta H values

## models dfs
neg.glm ## 75 models - 3 x bio endpoints, 25 x hydro endpoints

biol.endpoints
hydro.endpoints
bio_h_summary ## which models by number/order
## find only csci models
# csci_models <- which(bio_h_summary$biol.endpoints =="CSCI" )
# csci_glms <- neg.glm[csci_models]
# 
# csci_glms[1]
summary(neg.glm[1])
str(neg.glm)
test_mod <- neg.glm[1]
str(test_mod)
test_mod$
?glm


