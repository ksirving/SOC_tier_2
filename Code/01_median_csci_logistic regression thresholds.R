# setwd("/Users/katieirving/Documents/Projects/San_Juan/tier_2/forKris")
setwd("/Users/katieirving/Documents/git/SOC_tier2/SOC_tier_2")

csci<-read.csv("Data/DeltaH_CSCI_MMI.CSV")
head(csci)
# library(CSCI)
# install.packages("CSCI")
library(reshape)
# install.packages("devtools")
# library(devtools)
# install_github("SCCWRP/CSCI")
library(CSCI)
# install.packages("reshape")
library(ggplot2)

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
## 
new_csci <- read.csv("output_data/00_csci_delta_formatted_median_updated.csv")
head(new_csci)
## remove peak timeing vars
names(new_csci)
new_csci <- new_csci[, -c(24:26)]
names(new_csci)[5:7] <- c("OoverE", "MMI", "CSCI")
## add condition here so can add it to the figures later
# thresholds
# csci.thresh<-thresholds[which(thresholds$Index2=="CSCI"),"Tenth"]
# OoverE.thresh<-thresholds[which(thresholds$Index2=="OoverE"),"Tenth"]
# mmi.thresh<-thresholds[which(thresholds$Index2=="MMI"),"Tenth"]
# 
# new_csci$CSCI_Condition <-ifelse(new_csci$CSCI< csci.thresh,0,1)
# new_csci$MMI_Condition <-ifelse(new_csci$MMI< mmi.thresh,0,1)
# new_csci$OoverE_Condition <-ifelse(new_csci$OoverE< csci.thresh,0,1)




biol.endpoints<-c("CSCI","OoverE","MMI")#,
# "Clinger_PercentTaxa_score","Coleoptera_PercentTaxa_score","Taxonomic_Richness_score",
# "EPT_PercentTaxa_score","Shredder_Taxa_score","Intolerant_Percent_score")
## will change to FFM and deltaH

hydro.endpoints<- colnames(new_csci)[10:31]
  #c("BFR","FracYearsNoFlow","HighDur","HighNum","Hydroperiod","LowDur", "LowNum","MaxMonth","MaxMonthQ","MedianNoFlowDays","MinMonth","MinMonthQ","NoDisturb","PDC50","Q01","Q05","Q10","Q25","Q50","Q75","Q90", "Q95","Q99","Qmax","QmaxIDR","Qmean","QmeanIDR","QmeanMEDIAN","Qmed","Qmin", "QminIDR","SFR")


bio_h_summary<-  expand.grid(biol.endpoints=biol.endpoints,hydro.endpoints=hydro.endpoints, stringsAsFactors = F)
neg.glm
pos.glm
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
  
  

    glm(Condition~hydro, family=binomial(link="logit"), data=mydat)
  
    
})

data <- NULL
data$site_num <- 1
data$hydro <- 1
data$hydro_code <- paste("hmet")
data$bio <- paste("bmet")
data$Condition <- 1
names(data)

bio_h_summary$comb_code <- paste(bio_h_summary$biol.endpoints, "_", bio_h_summary$hydro.endpoints, sep="")
length(bio_h_summary)

code <- bio_h_summary$comb_code
code
i=1

  for(i in 1: length(code)) {
  
  hmet<-as.character(bio_h_summary[i,"hydro.endpoints"])
  bmet<-as.character(bio_h_summary[i,"biol.endpoints"])
  bmet.thresh<-thresholds[which(thresholds$Index2==bmet),"Tenth"]
  
  mydat<-na.omit(new_csci[,c(hmet, bmet)])
  names(mydat)<-c("hydro","bio")
  mydat
  mydat <- mydat[which(mydat$hydro<=0 ),]
 
  #   mydat$Condition<-ifelse(mydat$bio< bmet.thresh,"Poor","Healthy")
  #   mydat$Condition<-factor(mydat$Condition, levels=c("Poor","Healthy")) 
  #   mydat$Condition<-factor(mydat$Condition, levels=c("Poor","Healthy"))
  mydat$Condition<-ifelse(mydat$bio< bmet.thresh,0,1)
  mydat<-mydat[order(mydat$bio),]
  mydat$site_num <- rownames(mydat)
  # write.csv(mydat, paste("output_data/glm_data/06_", bmet,"_neg_", hmet,  "_glm.csv", sep=""))
 
  # glm(Condition~hydro, family=binomial(link="logit"), data=mydat)
  mydat
  mydat$hydro_code <- hmet
  mydat$bio <- bmet
  names(data)
  names(mydat)
  mydat <- mydat[c(4,1,5,2,3)]
  data <- rbind(data, mydat)


}

data_neg <- data[-1,]

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
 # write.csv(mydat, paste("output_data/glm_data/06_", bmet,"_pos_", hmet,  "_glm.csv", sep=""))
 
  glm(Condition~hydro, family=binomial(link="logit"), data=mydat)
 
})

data <- NULL
data$site_num <- 1
data$hydro <- 1
data$hydro_code <- paste("hmet")
data$bio <- paste("bmet")
data$Condition <- 1
names(data)

bio_h_summary$comb_code <- paste(bio_h_summary$biol.endpoints, "_", bio_h_summary$hydro.endpoints, sep="")
length(bio_h_summary)

code <- bio_h_summary$comb_code
code

for(i in 1: length(code)) {
  
  hmet<-as.character(bio_h_summary[i,"hydro.endpoints"])
  bmet<-as.character(bio_h_summary[i,"biol.endpoints"])
  bmet.thresh<-thresholds[which(thresholds$Index2==bmet),"Tenth"]
  
  mydat<-na.omit(new_csci[,c(hmet, bmet)])
  names(mydat)<-c("hydro","bio")
  
  mydat <- mydat[which(mydat$hydro>=0  ),]
  mydat
  
  #   mydat$Condition<-ifelse(mydat$bio< bmet.thresh,"Poor","Healthy")
  #   mydat$Condition<-factor(mydat$Condition, levels=c("Poor","Healthy")) 
  #   mydat$Condition<-factor(mydat$Condition, levels=c("Poor","Healthy"))
  mydat$Condition<-ifelse(mydat$bio< bmet.thresh,0,1)
  mydat<-mydat[order(mydat$bio),]
  mydat$site_num <- rownames(mydat)
  # write.csv(mydat, paste("output_data/glm_data/06_", bmet,"_neg_", hmet,  "_glm.csv", sep=""))
  
  # glm(Condition~hydro, family=binomial(link="logit"), data=mydat)
  
  mydat
  mydat$hydro_code <- hmet
  mydat$bio <- bmet
  names(data)
  names(mydat)
  mydat <- mydat[c(4,1,5,2,3)]
  data <- rbind(data, mydat)
  
  
}

data_pos <- data[-1,]
data_pos



# warnings()
head(new_csci)
hydro.m<-na.omit(unique(melt(new_csci[,hydro.endpoints])))
hydro.m<-hydro.m[order(hydro.m$variable,hydro.m$value),]
names(hydro.m)<-c("hydro.endpoints","hydro.threshold")
# head(hydro.m)
# i=6124

## hydro threshold here - just used to see whether delta h is negative or positive
bio_h_summary2<-merge(bio_h_summary, hydro.m)
head(bio_h_summary2)
i=2
neg.glm
bio_h_summary2$PredictedProbability<-
  sapply(1:nrow(bio_h_summary2), function(i)
  {
  hmet<-bio_h_summary2[i,"hydro.endpoints"]
  bmet<-bio_h_summary2[i,"biol.endpoints"]
  thresh<-bio_h_summary2[i,"hydro.threshold"]
  thresh
  # print(paste(hmet,bmet))
  modnum<-  which(bio_h_summary$hydro.endpoints==hmet & bio_h_summary$biol.endpoints==bmet)
  modnum
  if(thresh<0)
    mymod<-neg.glm[[modnum]] else
      mymod<-pos.glm[[modnum]]
  neg.glm
  mydata<-data.frame(hydro=thresh)
  predict(mymod, newdata=mydata, type="response")
  
})
head(mydata)
bio_h_summary2$Type<-ifelse(bio_h_summary2$hydro.threshold<0,"Negative","Positive")



head(bio_h_summary2)
tail(bio_h_summary2)

## negative delta H
## subset only negative numbers
neg_pred <- subset(bio_h_summary2, Type!="Positive")
## merge dataframes
head(data_neg) ## condition 1/0s
head(neg_pred) ## negative pred probs 
# dim(neg_pred)
data_neg$comb_code <- paste(data_neg$bio, "_", data_neg$hydro_code, sep="")
# dim(data_neg)
all_data <- merge(neg_pred, data_neg, by="comb_code", all=T)
head(all_data)
# unique(all_data$Type)

all_data <- all_data[,c(1:7,8,11)]
write.csv(all_data, "output_data/01_all_data_neg_logR_metrics_figures.csv")
### CSCI endpoint
all_data_csci <- subset(all_data,biol.endpoints=="CSCI")
head(all_data_csci)
write.csv(all_data_csci, "output_data/01_csci_neg_logR_metrics_figures.csv")

### OoverE endpoint
all_data_oe <- subset(all_data,biol.endpoints=="OoverE")
head(all_data_oe)
write.csv(all_data_oe, "output_data/01_OoverE_neg_logR_metrics_figures.csv")

### MMI endpoint
all_data_mmi <- subset(all_data,biol.endpoints=="MMI")
head(all_data_mmi)
write.csv(all_data_mmi, "output_data/01_mmi_neg_logR_metrics_figures.csv")

# all_data[which(all_data$hydro==-194),]
## all endpoints - each endpoint has different number of 1s and 0s
ggplot(all_data, aes(x=hydro.threshold, y=PredictedProbability, color=biol.endpoints))+
  # geom_point(size=1)+
  geom_path()+#stat_summary(fun.y="mean", geom="line")+
  facet_wrap(~hydro.endpoints, scales="free_x", nrow=4)+scale_y_continuous(limits=c(0,1))+
  geom_vline(xintercept=0, linetype="dashed")+
  geom_point(aes(y=Condition, x=hydro), colour = 'black', size = 1)
  
## CSCI
ggplot(all_data_csci, aes(x=hydro.threshold, y=PredictedProbability, color=biol.endpoints))+
  # geom_point(size=1)+
  geom_path()+#stat_summary(fun.y="mean", geom="line")+
  facet_wrap(~hydro.endpoints, scales="free_x", nrow=4)+scale_y_continuous(limits=c(0,1))+
  geom_vline(xintercept=0, linetype="dashed")+
  geom_point(aes(y=Condition, x=hydro), colour = 'black', size = 1)

## OoverE
ggplot(all_data_oe, aes(x=hydro.threshold, y=PredictedProbability, color=biol.endpoints))+
  # geom_point(size=1)+
  # geom_line(aes(group=1), colour="green")+
  geom_path()+#stat_summary(fun.y="mean", geom="line")+
  facet_wrap(~hydro.endpoints, scales="free_x", nrow=4)+scale_y_continuous(limits=c(0,1))+
  geom_vline(xintercept=0, linetype="dashed")+
  geom_point(aes(y=Condition, x=hydro), colour = 'black', size = 1)

  ## MMI
ggplot(all_data_mmi, aes(x=hydro.threshold, y=PredictedProbability, color=biol.endpoints))+
  # geom_point(size=1)+
  geom_path()+#stat_summary(fun.y="mean", geom="line")+
  facet_wrap(~hydro.endpoints, scales="free_x", nrow=4)+scale_y_continuous(limits=c(0,1))+
  geom_vline(xintercept=0, linetype="dashed")+
  geom_point(aes(y=Condition, x=hydro), colour = 'black', size = 1)


# all_data[which(all_data$hydro==-194),]

## positive

## subset only negative numbers
pos_pred <- subset(bio_h_summary2, Type!="Negative")
## merge dataframes
head(data_pos) ## condition 1/0s
head(pos_pred) ## negative pred probs 
# dim(neg_pred)
data_pos$comb_code <- paste(data_pos$bio, "_", data_pos$hydro_code, sep="")
# dim(data_neg)
all_data <- merge(pos_pred, data_pos, by="comb_code", all=T)
head(all_data)
# unique(all_data$Type)

all_data <- all_data[,c(1:7,8,11)]
write.csv(all_data, "output_data/01_all_data_pos_logR_metrics_figures.csv")
### CSCI endpoint
all_data_csci <- subset(all_data,biol.endpoints=="CSCI")
head(all_data_csci)
write.csv(all_data_csci, "output_data/01_csci_pos_logR_metrics_figures.csv")

### OoverE endpoint
all_data_oe <- subset(all_data,biol.endpoints=="OoverE")
head(all_data_oe)
write.csv(all_data_oe, "output_data/01_OoverE_pos_logR_metrics_figures.csv")

### MMI endpoint
all_data_mmi <- subset(all_data,biol.endpoints=="MMI")
head(all_data_mmi)
write.csv(all_data_mmi, "output_data/01_mmi_pos_logR_metrics_figures.csv")

# all_data[which(all_data$hydro==-194),]
## all endpoints - each endpoint has different number of 1s and 0s
ggplot(all_data, aes(x=hydro.threshold, y=PredictedProbability, color=biol.endpoints))+
  # geom_point(size=1)+
  geom_path()+#stat_summary(fun.y="mean", geom="line")+
  facet_wrap(~hydro.endpoints, scales="free_x", nrow=4)+scale_y_continuous(limits=c(0,1))+
  geom_vline(xintercept=0, linetype="dashed")+
  geom_point(aes(y=Condition, x=hydro), colour = 'black', size = 1)

## CSCI
ggplot(all_data_csci, aes(x=hydro.threshold, y=PredictedProbability, color=biol.endpoints))+
  # geom_point(size=1)+
  geom_path()+#stat_summary(fun.y="mean", geom="line")+
  facet_wrap(~hydro.endpoints, scales="free_x", nrow=4)+scale_y_continuous(limits=c(0,1))+
  geom_vline(xintercept=0, linetype="dashed")+
  geom_point(aes(y=Condition, x=hydro), colour = 'black', size = 1)

## OoverE
ggplot(all_data_oe, aes(x=hydro.threshold, y=PredictedProbability, color=biol.endpoints))+
  # geom_point(size=1)+
  # geom_line(aes(group=1), colour="green")+
  geom_path()+#stat_summary(fun.y="mean", geom="line")+
  facet_wrap(~hydro.endpoints, scales="free_x", nrow=4)+scale_y_continuous(limits=c(0,1))+
  geom_vline(xintercept=0, linetype="dashed")+
  geom_point(aes(y=Condition, x=hydro), colour = 'black', size = 1)

## MMI
ggplot(all_data_mmi, aes(x=hydro.threshold, y=PredictedProbability, color=biol.endpoints))+
  # geom_point(size=1)+
  geom_path()+#stat_summary(fun.y="mean", geom="line")+
  facet_wrap(~hydro.endpoints, scales="free_x", nrow=4)+scale_y_continuous(limits=c(0,1))+
  geom_vline(xintercept=0, linetype="dashed")+
  geom_point(aes(y=Condition, x=hydro), colour = 'black', size = 1)



####### other 



bio_h_summary3<-expand.grid(biol.endpoints=biol.endpoints,hydro.endpoints=hydro.endpoints, 
                                Percentile=seq(from=0.05, to=.95, by=.05),
                                stringsAsFactors = F)

bio_h_summary3$NegValue<-sapply(1:nrow(bio_h_summary3), function(i)
  {
  hmet<-bio_h_summary3[i,"hydro.endpoints"]
  per<-bio_h_summary3[i,"Percentile"]
  mydat<-data.frame(hydro=new_csci[,hmet])
  mydat<-mydat$hydro[which(mydat$hydro<0)]
  quantile(mydat, per, na.rm=T)
})


bio_h_summary3$PosValue<-sapply(1:nrow(bio_h_summary3), function(i)
{
  hmet<-bio_h_summary3[i,"hydro.endpoints"]
  per<-bio_h_summary3[i,"Percentile"]
  mydat<-data.frame(hydro=new_csci[,hmet])
  mydat<-mydat$hydro[which(mydat$hydro>0)]
  quantile(mydat, per, na.rm=T)
})

bio_h_summary3<-na.omit(bio_h_summary3)

bio_h_summary3$NegPredicted<-
  sapply(1:nrow(bio_h_summary3), function(i)
  {
    hmet<-bio_h_summary3[i,"hydro.endpoints"]
    bmet<-bio_h_summary3[i,"biol.endpoints"]
    thresh<-bio_h_summary3[i,"NegValue"]
    modnum<-  which(bio_h_summary$hydro.endpoints==hmet & bio_h_summary$biol.endpoints==bmet)
    mymod<-neg.glm[[modnum]] 
    mydata<-data.frame("hydro"=thresh)
    predict(mymod, newdata=mydata, type="response")
  })

bio_h_summary3$PosPredicted<-
  sapply(1:nrow(bio_h_summary3), function(i)
  {
    hmet<-bio_h_summary3[i,"hydro.endpoints"]
    bmet<-bio_h_summary3[i,"biol.endpoints"]
    thresh<-bio_h_summary3[i,"PosValue"]
    modnum<-  which(bio_h_summary$hydro.endpoints==hmet & bio_h_summary$biol.endpoints==bmet)
    mymod<-pos.glm[[modnum]] 
    mydata<-data.frame("hydro"=thresh)
    predict(mymod, newdata=mydata, type="response")
  })

head(bio_h_summary3)
bio_h_summary3


ggplot(data=bio_h_summary3, aes(x=NegValue, y=NegPredicted, color=biol.endpoints))+
  geom_path()+
  facet_wrap(~hydro.endpoints, scales="free_x")

ggplot(data=bio_h_summary3, aes(x=PosValue, y=PosPredicted, color=biol.endpoints))+
  geom_path()+
  facet_wrap(~hydro.endpoints, scales="free_x")
# 
# ### looking at specific variables
# ggplot(data=subset(bio_h_summary3, hydro.endpoints=="Q01" ), aes(x=NegValue, y=NegPredicted, color=biol.endpoints))+
#   geom_path()+
#   facet_wrap(~hydro.endpoints, scales="free_x")+theme_classic(base_size=20)+
#   ylab("Probability of good condition")+xlab("Depletion")
# 
# ggplot(data=subset(bio_h_summary3, hydro.endpoints=="Q99" ), aes(x=PosValue, y=PosPredicted, color=biol.endpoints))+
#   geom_path()+
#   facet_wrap(~hydro.endpoints, scales="free_x")+theme_classic(base_size=20)+
#   ylab("Probability of good condition")+xlab("Augmentation")
# 
bio_h_summary3

ggplot(data=bio_h_summary3, aes(x=Percentile, y=NegPredicted, color=biol.endpoints))+
  geom_path()+
  facet_wrap(~hydro.endpoints, scales="free_x")

### get df of condition for each metric


glm_cond <- list.files(path="output_data/glm_data", pattern=c("CSCI_neg"))
glm_cond

## make dataframe with numbers 1-273 and each prediction, and 1/0s
i=1
data <- NULL
data$site_num <- 1:273

for(i in 1: length(glm_cond)) {

  test <- read.csv(paste("output_data/glm_data/", glm_cond[i], sep=""))
  colnames(test)
  test <- test[,c(1,3,4)]
  nms <- strsplit(glm_cond[i], c("_"))
  colnames(test)[1] <- "site_num"
  met_name <- paste(nms[[1]][4],"_", nms[[1]][5], "_", nms[[1]][6], sep="")
  colnames(test)[2] <- met_name

  data <- merge(data, test, by="site_num", all=T)

}
# 
# head(data)
# tail(data)
# data

## use info from model for plots

biol.endpoints ## df of numbered models
## sequence of number to extract csci models
ind <- seq(1,75,3)
## extract models
csci_glm <- neg.glm[ind]
csci_glm

par("mar")
par(mar=c(3,1,1,1))
par(mfrow=c(5,5))

for(i in 1:length(csci_glm)) {
  
  mod <- csci_glm[[i]]
  yplot <- mod$model$Condition
  xplot <- mod$model$hydro
  xval <- seq(min(mod$model[2]), max(mod$model[2]), 1)
  yval <- predict(mod, list(hydro=xval), type="response")
  plot(xplot, yplot)
  lines(xval, yval)
  
  
}

?axis



## each bio endpoint has a different number of 1/0s so can not go on same figure
# par(new=TRUE)
# mod <- neg.glm[[2]]
# yplot <- mod$model$Condition
# xplot <- mod$model$hydro
# xval <- seq(min(mod$model[2]), max(mod$model[2]), 1)
# yval <- predict(mod, list(hydro=xval), type="response")
# plot(xplot, yplot, axes=F, ylab="", xlab="")
# lines(xval, yval)


head(new_csci)
head(bio_h_summary3)
junka<-bio_h_summary3[,c("biol.endpoints","hydro.endpoints","Percentile","NegValue","NegPredicted")]
junka<-plyr::rename(junka, replace=c("NegValue"="Value","NegPredicted"="Predicted"))
junka$Type<-"Negative"
junkb<-bio_h_summary3[,c("biol.endpoints","hydro.endpoints","Percentile","PosValue","PosPredicted")]
junkb<-plyr::rename(junkb, replace=c("PosValue"="Value","PosPredicted"="Predicted"))
junkb$Type<-"Positive"
bio_h_summary4<-rbind(junka, junkb)

head(hydro.m)
head(csci.m)
csci.m<-na.omit(melt(new_csci[,c("SampleID", "CSCI_Percentile",hydro.endpoints)], id.var=c("SampleID","CSCI_Percentile")))
names(csci.m)[3:4]<-c("hydrol.endpoints","Value")
csci.m$Predicted<-0
hydro.endpoints
plot.dat<-bio_h_summary4[which(bio_h_summary4$biol.endpoints=="CSCI" & bio_h_summary4$hydro.endpoints %in% c("DS_Dur_WS", "DS_Mag_50","DS_Mag_90","DS_Tim","Peak_Fre_10")),]
head(plot.dat)

ggplot(data=plot.dat, aes(x=Value, y=Predicted))+
  geom_path()+
  # geom_rug(data=subset(csci.m, hydro.endpoints %in% plot.dat$hydro.endpoints & CSCI_Percentile < .1), sides="b", color="#e41a1c")+
  # geom_rug(data=subset(csci.m, hydro.endpoints %in% plot.dat$hydro.endpoints & CSCI_Percentile >= .1), sides="t", color="#377eb8")+
    # geom_point(data=csci, aes(x=Qmean, y=CSCI))+
  facet_wrap(hydro.endpoints~Type, scales="free_x", ncol=2)+
  theme_classic(base_size = 18)#+scale_y_continuous(limits=c(0,1))
# 
# ggplot(data=bio_h_summary4, aes(x=Value, y=Predicted,color=biol.endpoints))+
#   geom_path()+
#   facet_wrap(~hydro.endpoints, scales="free_x")
# 
