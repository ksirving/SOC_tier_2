# setwd("/Users/katieirving/Documents/Projects/San_Juan/tier_2/forKris")
setwd("/Users/katieirving/Documents/git/SOC_tier2/SOC_tier_2")

csci<-read.csv("Data/DeltaH_CSCI_MMI.CSV")
head(csci)
library(CSCI)
library(reshape)
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
## median only so far
new_csci <- read.csv("output_data/00_csci_delta_formatted_min.csv")

biol.endpoints<-c("CSCI","OoverE","MMI")#,
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
  
  
  glm(Condition~hydro, family=binomial(link="logit"), data=mydat)
})

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
  
  glm(Condition~hydro, family=binomial(link="logit"), data=mydat)
})
# warnings()
head(new_csci)
hydro.m<-na.omit(unique(melt(new_csci[,hydro.endpoints])))
hydro.m<-hydro.m[order(hydro.m$variable,hydro.m$value),]
names(hydro.m)<-c("hydro.endpoints","hydro.threshold")
# head(hydro.m)
# i=6124

## hydro threshold here - just used to see whether delta h is negative or positive
bio_h_summary2<-merge(bio_h_summary, hydro.m)
# head(bio_h_summary2)
bio_h_summary2$PredictedProbability<-
  sapply(1:nrow(bio_h_summary2), function(i)
  {
    hmet<-bio_h_summary2[i,"hydro.endpoints"]
    bmet<-bio_h_summary2[i,"biol.endpoints"]
    thresh<-bio_h_summary2[i,"hydro.threshold"]
    print(paste(hmet,bmet))
    modnum<-  which(bio_h_summary$hydro.endpoints==hmet & bio_h_summary$biol.endpoints==bmet)
    if(thresh<0)
      mymod<-neg.glm[[modnum]] else
        mymod<-pos.glm[[modnum]]
    mydata<-data.frame(hydro=thresh)
    predict(mymod, newdata=mydata, type="response")
  })
head(mydata)
bio_h_summary2$Type<-ifelse(bio_h_summary2$hydro.threshold<0,"Negative","Positive")
head(bio_h_summary2)
tail(bio_h_summary2)

ggplot(data=subset(bio_h_summary2, Type!="Negative" & biol.endpoints=="CSCI"), aes(x=hydro.threshold, y=PredictedProbability, color=biol.endpoints))+
  # geom_point(size=1)+
  geom_path()+#stat_summary(fun.y="mean", geom="line")+
  facet_wrap(~hydro.endpoints, scales="free_x", nrow=4)+scale_y_continuous(limits=c(0,1))+
  geom_vline(xintercept=0, linetype="dashed")


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



ggplot(data=bio_h_summary3, aes(x=NegValue, y=NegPredicted, color=biol.endpoints))+
  geom_path()+
  facet_wrap(~hydro.endpoints, scales="free_x")

ggplot(data=bio_h_summary3, aes(x=PosValue, y=PosPredicted, color=biol.endpoints))+
  geom_path()+
  facet_wrap(~hydro.endpoints, scales="free_x")
# 
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


ggplot(data=bio_h_summary3, aes(x=Percentile, y=NegPredicted, color=biol.endpoints))+
  geom_path()+
  facet_wrap(~hydro.endpoints, scales="free_x")
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
