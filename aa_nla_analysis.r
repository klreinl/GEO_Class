## load different archetypes
load("archetypes_20210813_7.Rdata")
(archetypes)

##simple plot (obviously needs to be scaled)
matplot(t(archetypes),type="b")

## read in data frame with NLA data and archetype weights
nla.lag=read.csv("nla_lagos_aa_7.csv")

## chla vs tp
plot(log(nla.lag$chla),log(nla.lag$tp_ug),col=nla.lag$max.arch)

## removing NA values
data.with.no.na=nla.lag[which(nla.lag$chla_ug>0),]


colnames(data.with.no.na)
data<-data.with.no.na
data$TP_uM<-data$tp_ug/30.97
data$TN_uM<-data$tn_ug/14.007
data$N.P<-c(data$TN_uM/data$TP_uM)

hist(data$max.arch)


##MODELS

#TP vs chla

hist(data$tp_ug)
hist(log(data$tp_ug))

hist(data$chla_ug)
hist(log(data$chla_ug))

## fit correlation model between tp and chla_ug with archetype weights as interactions
fit.aa=lm(log(tp_ug)~0+w.arch1+w.arch2+w.arch3+w.arch4+w.arch5+w.arch6+w.arch7+log(chla_ug):w.arch1+log(chla_ug):w.arch2+log(chla_ug):w.arch3+log(chla_ug):w.arch4+log(chla_ug):w.arch5+log(chla_ug):w.arch6+log(chla_ug):w.arch7,data=data)
summary(fit.aa)


## fit correlation model between tp and chla_ug with max archetype as interactions
fit.aa.cat=lm(log(tp_ug)~log(chla_ug)*max.arch,data=data)
summary(fit.aa.cat)

## fit correlation model between tp and chla_ug with eco-regions as interactions
fit.ecoreg=lm(log(tp_ug)~log(chla_ug)*ag_eco9,data=data)
summary(fit.ecoreg)

## see which has a better fit to the data
AIC(fit.aa)
AIC(fit.aa.cat)
AIC(fit.ecoreg)



##
#TN vs chl-a


hist(data$tn_ug)
hist(log(data$tn_ug))

hist(data$chla_ug)
hist(log(data$chla_ug))

## fit correlation model between tn and chla_ug with archetype weights as interactions
fit.aa=lm(log(tn_ug)~0+w.arch1+w.arch2+w.arch3+w.arch4+w.arch5+w.arch6+w.arch7+log(chla_ug):w.arch1+log(chla_ug):w.arch2+log(chla_ug):w.arch3+log(chla_ug):w.arch4+log(chla_ug):w.arch5+log(chla_ug):w.arch6+log(chla_ug):w.arch7,data=data)
summary(fit.aa)


## fit correlation model between tn and chla_ug with max archetype as interactions
fit.aa.cat=lm(log(tn_ug)~log(chla_ug)*max.arch,data=data)
summary(fit.aa.cat)

## fit correlation model between tn and chla_ug with eco-regions as interactions
fit.ecoreg=lm(log(tn_ug)~log(chla_ug)*ag_eco9,data=data)
summary(fit.ecoreg)

## see which has a better fit to the data
AIC(fit.aa)
AIC(fit.aa.cat)
AIC(fit.ecoreg)


#TP vs chla

hist(data$TP_uM)
hist(log(data$TP_uM))

hist(data$chla_ug)
hist(log(data$chla_ug))

## fit correlation model between tp and chla_ug with archetype weights as interactions
fit.aa=lm(log(TP_uM)~0+w.arch1+w.arch2+w.arch3+w.arch4+w.arch5+w.arch6+w.arch7+log(chla_ug):w.arch1+log(chla_ug):w.arch2+log(chla_ug):w.arch3+log(chla_ug):w.arch4+log(chla_ug):w.arch5+log(chla_ug):w.arch6+log(chla_ug):w.arch7,data=data)
summary(fit.aa)


## fit correlation model between tp and chla_ug with max archetype as interactions
fit.aa.cat=lm(log(TP_uM)~log(chla_ug)*max.arch,data=data)
summary(fit.aa.cat)

## fit correlation model between tp and chla_ug with eco-regions as interactions
fit.ecoreg=lm(log(TP_uM)~log(chla_ug)*ag_eco9,data=data)
summary(fit.ecoreg)

## see which has a better fit to the data
AIC(fit.aa)
AIC(fit.aa.cat)
AIC(fit.ecoreg)


##AA model best



#TN vs chla

hist(data$TN_uM)
hist(log(data$TN_uM))

hist(data$chla_ug)
hist(log(data$chla_ug))

## fit correlation model between TN and chla_ug with archetype weights as interactions
fit.aa=lm(log(TN_uM)~0+w.arch1+w.arch2+w.arch3+w.arch4+w.arch5+w.arch6+w.arch7+log(chla_ug):w.arch1+log(chla_ug):w.arch2+log(chla_ug):w.arch3+log(chla_ug):w.arch4+log(chla_ug):w.arch5+log(chla_ug):w.arch6+log(chla_ug):w.arch7,data=data)
summary(fit.aa)


## fit correlation model between TN and chla_ug with max archetype as interactions
fit.aa.cat=lm(log(TN_uM)~log(chla_ug)*max.arch,data=data)
summary(fit.aa.cat)

## fit correlation model between TN and chla_ug with eco-regions as interactions
fit.ecoreg=lm(log(TN_uM)~log(chla_ug)*ag_eco9,data=data)
summary(fit.ecoreg)

## see which has a better fit to the data
AIC(fit.aa)
AIC(fit.aa.cat)
AIC(fit.ecoreg)


##AA model best

#######
#Secchi vs chla

hist(data$secchi_m)
hist(log(data$secchi_m))

hist(data$chla_ug)
hist(log(data$chla_ug))


data$log.secchi_m<-c(log(data$secchi_m))
data<-data[data$log.secchi_m >(-5),]


## fit correlation model between tp and chla_ug with archetype weights as interactions
fit.aa=lm(log(secchi_m)~0+w.arch1+w.arch2+w.arch3+w.arch4+w.arch5+w.arch6+w.arch7+log(chla_ug):w.arch1+log(chla_ug):w.arch2+log(chla_ug):w.arch3+log(chla_ug):w.arch4+log(chla_ug):w.arch5+log(chla_ug):w.arch6+log(chla_ug):w.arch7,data=data)
summary(fit.aa)


## fit correlation model between tp and chla_ug with max archetype
fit.aa.cat=lm(log(secchi_m)~log(chla_ug)*max.arch,data=data)
summary(fit.aa.cat)

## fit correlation model between tp and chla_ug with eco-regions as interactions
fit.ecoreg=lm(log(secchi_m)~log(chla_ug)*ag_eco9,data=data)
summary(fit.ecoreg)

## see which has a better fit to the data
AIC(fit.aa)
AIC(fit.aa.cat)
AIC(fit.ecoreg)

##Ecoregion model best

####

data<-data.with.no.na
data$TP_uM<-data$tp_ug/30.97
data$TN_uM<-data$tn_ug/14.007
data$N.P<-c(data$TN_uM/data$TP_uM)

#N:P vs Chla

hist(data$N.P)
hist(log(data$N.P))

hist(data$chla_ug)
hist(log(data$chla_ug))

## fit correlation model between tp and chla_ug with archetype weights as interactions
fit.aa=lm(log(N.P)~0+w.arch1+w.arch2+w.arch3+w.arch4+w.arch5+w.arch6+w.arch7+log(chla_ug):w.arch1+log(chla_ug):w.arch2+log(chla_ug):w.arch3+log(chla_ug):w.arch4+log(chla_ug):w.arch5+log(chla_ug):w.arch6+log(chla_ug):w.arch7,data=data)
summary(fit.aa)


## fit correlation model between tp and chla_ug with max archetype
fit.aa.cat=lm(log(N.P)~log(chla_ug)*max.arch,data=data)
summary(fit.aa.cat)

## fit correlation model between tp and chla_ug with eco-regions as interactions
fit.ecoreg=lm(log(N.P)~log(chla_ug)*ag_eco9,data=data)
summary(fit.ecoreg)

## see which has a better fit to the data
AIC(fit.aa)
AIC(fit.aa.cat)
AIC(fit.ecoreg)

##ECOregion model best

#NH3 vs TN


hist(data$ammonia_mg)
hist(log(data$ammonia_mg))

hist(data$tn_ug)
hist(log(data$tn_ug))

data$log.NH3<-c(log(data$ammonia_mg))
data<-data[data$log.NH3 >(-9),]

## fit correlation model between tp and chla_ug with archetype weights as interactions
fit.aa=lm(log(ammonia_mg)~0+w.arch1+w.arch2+w.arch3+w.arch4+w.arch5+w.arch6+w.arch7+
            log(tn_ug):w.arch1+log(tn_ug):w.arch2+log(tn_ug):w.arch3+log(tn_ug):w.arch4+
            log(tn_ug):w.arch5+log(tn_ug):w.arch6+log(tn_ug):w.arch7,data=data, na.action=na.exclude)
summary(fit.aa)


## fit correlation model between tp and tn_ug with max archetype
fit.aa.cat=lm(log(ammonia_mg)~log(tn_ug)*max.arch,data=data)
summary(fit.aa.cat)



## fit correlation model between tp and tn_ug with eco-regions as interactions
fit.ecoreg=lm(log(ammonia_mg)~log(tn_ug)*ag_eco9,data=data)
summary(fit.ecoreg)

## see which has a better fit to the data
AIC(fit.aa)
AIC(fit.aa.cat)
AIC(fit.ecoreg)

##AA model best


#NO3 vs TN

hist(data$nitrate_mg)
hist(log(data$nitrate_mg))

hist(data$tn_ug)
hist(log(data$tn_ug))


data<-data.with.no.na
data$log.NO3<-c(log(data$nitrate_mg))
data<-data[data$log.NO3 >(-10),]

## fit correlation model between tp and chla_ug with archetype weights as interactions
fit.aa=lm(log(nitrate_mg)~0+w.arch1+w.arch2+w.arch3+w.arch4+w.arch5+w.arch6+w.arch7+
            log(tn_ug):w.arch1+log(tn_ug):w.arch2+log(tn_ug):w.arch3+log(tn_ug):w.arch4+
            log(tn_ug):w.arch5+log(tn_ug):w.arch6+log(tn_ug):w.arch7,data=data, na.action=na.exclude)
summary(fit.aa)


## fit correlation model between tp and tn_ug with max archetype
fit.aa.cat=lm(log(nitrate_mg)~log(tn_ug)*max.arch,data=data)
summary(fit.aa.cat)



## fit correlation model between tp and tn_ug with eco-regions as interactions
fit.ecoreg=lm(log(nitrate_mg)~log(tn_ug)*ag_eco9,data=data)
summary(fit.ecoreg)

## see which has a better fit to the data
AIC(fit.aa)
AIC(fit.aa.cat)
AIC(fit.ecoreg)


## Ecoregion model best

#doc vs Chla

hist(data$doc_mg)
hist(log(data$doc_mg))

hist(data$chla_ug)
hist(log(data$chla_ug))


## fit correlation model between tp and chla_ug with archetype weights as interactions
fit.aa=lm(log(doc_mg)~0+w.arch1+w.arch2+w.arch3+w.arch4+w.arch5+w.arch6+w.arch7+log(chla_ug):w.arch1+log(chla_ug):w.arch2+log(chla_ug):w.arch3+log(chla_ug):w.arch4+log(chla_ug):w.arch5+log(chla_ug):w.arch6+log(chla_ug):w.arch7,data=data)
summary(fit.aa)


## fit correlation model between tn and chla_ug with max archetype as interactions
fit.aa.cat=lm(log(doc_mg)~log(chla_ug)*max.arch,data=data)
summary(fit.aa.cat)

## fit correlation model between tn and chla_ug with eco-regions as interactions
fit.ecoreg=lm(log(doc_mg)~log(chla_ug)*ag_eco9,data=data)
summary(fit.ecoreg)

## see which has a better fit to the data
AIC(fit.aa)
AIC(fit.aa.cat)
AIC(fit.ecoreg)


#doc vs toc

hist(data$doc_mg)
hist(log(data$doc_mg))

hist(data$toc_mg)
hist(log(data$toc_mg))


## fit correlation model between tp and toc_mg with archetype weights as interactions
fit.aa=lm(log(doc_mg)~0+w.arch1+w.arch2+w.arch3+w.arch4+w.arch5+w.arch6+w.arch7+log(toc_mg):w.arch1+log(toc_mg):w.arch2+log(toc_mg):w.arch3+log(toc_mg):w.arch4+log(toc_mg):w.arch5+log(toc_mg):w.arch6+log(toc_mg):w.arch7,data=data)
summary(fit.aa)


## fit correlation model between tn and toc_mg with max archetype as interactions
fit.aa.cat=lm(log(doc_mg)~log(toc_mg)*max.arch,data=data)
summary(fit.aa.cat)

## fit correlation model between tn and toc_mg with eco-regions as interactions
fit.ecoreg=lm(log(doc_mg)~log(toc_mg)*ag_eco9,data=data)
summary(fit.ecoreg)

## see which has a better fit to the data
AIC(fit.aa)
AIC(fit.aa.cat)
AIC(fit.ecoreg)


##
## some plots
##

plot(log(data.with.no.na$chla_ug),log(data.with.no.na$tp_ug),col=data.with.no.na$max.arch+1,pch=data.with.no.na$max.arch)
abline(mean(fit.aa$coef[1:7]),mean(fit.aa$coef[8:14]),col=grey(.50),lwd=10)
points(log(data.with.no.na$chla_ug),log(data.with.no.na$tp_ug),col=data.with.no.na$max.arch+1,pch=data.with.no.na$max.arch)



plot(log(data.with.no.na$chla_ug),log(data.with.no.na$tp_ug),col=data.with.no.na$max.arch+1,pch=data.with.no.na$max.arch)
abline(mean(fit.aa$coef[1:7]),mean(fit.aa$coef[8:14]),col=grey(.50),lwd=10)
points(log(data.with.no.na$chla_ug),log(data.with.no.na$tp_ug),col=data.with.no.na$max.arch+1,pch=data.with.no.na$max.arch)
for(k in 1:7){
    abline(fit.aa$coef[k],fit.aa$coef[k+7],col=k+1,lty=1,lwd=2)
}

par(mfrow=c(2,4))
for(k in 1:7){
    idx.k=which(data.with.no.na$max.arch==k)
    if(length(idx.k)>0){
        plot(log(data.with.no.na$chla_ug),log(data.with.no.na$tp_ug),type="p",pch=".",col=grey(.6),main=paste("Arch = ",k,sep=""))
        points(log(data.with.no.na$chla_ug[idx.k]),log(data.with.no.na$tp_ug[idx.k]),col=k+1,main=paste("Arch = ",k,sep=""))
        abline(fit.aa$coef[k],fit.aa$coef[k+7],col=k+1,lty=1,lwd=2)
    }
}

##########################
#Kait Additions
colnames(data.with.no.na)
data<-data.with.no.na
data$TP_uM<-data$tp_ug/30.97
data$TN_uM<-data$tn_ug/14.007
data$N.P<-c(data$TN_uM/data$TP_uM)

library(ggplot2)
library(ggpmisc)

fit.aa.=lm(log(tp_ug)~0+w.arch1+w.arch2+w.arch3+w.arch4+w.arch5+w.arch6+w.arch7+log(chla_ug):w.arch1+log(chla_ug):w.arch2+log(chla_ug):w.arch3+log(chla_ug):w.arch4+log(chla_ug):w.arch5+log(chla_ug):w.arch6+log(chla_ug):w.arch7,data=data.with.no.na)
summary(fit.aa)


my.formula <- y ~ x

#ggplot(data=data, aes(x=log(chla_ug),y=log(tp_ug), col=as.factor(max.arch)))+
 #   geom_point()+
  #  geom_smooth(aes(group=as.factor(max.arch)),method="lm")+theme_bw()+
   # scale_colour_discrete(name="Max Archteype")+xlab("Log(Chl-a, ug)")+ylab("Log(TP, ug)")+
    #stat_poly_eq(formula = my.formula, 
     #            aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
      #           parse = TRUE) 


############################
ggplot(data=data, aes(x=log(chla_ug),y=log(tp_ug), col=as.factor(max.arch)))+
    geom_point()+
    geom_smooth(aes(group=as.factor(max.arch)),method="lm")+theme_bw()+
    scale_colour_discrete(name="Max Archteype")+xlab("Log(Chl-a, ug)")+ylab("Log(TP, ug)")+facet_wrap(~max.arch)+
    stat_poly_eq(formula = my.formula, 
                 aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
                 parse = TRUE) 

ggplot(data=data, aes(x=log(chla_ug),y=log(tp_ug), col=w.arch1))+
    geom_point()+theme_bw()+xlab("Log(Chl-a, ug)")+ylab("Log(TP, ug)")

ggplot(data=data, aes(x=log(chla_ug),y=log(tp_ug), col=w.arch2))+
    geom_point()+theme_bw()+xlab("Log(Chl-a, ug)")+ylab("Log(TP, ug)")

ggplot(data=data, aes(x=log(chla_ug),y=log(tp_ug), col=w.arch3))+
    geom_point()+theme_bw()+xlab("Log(Chl-a, ug)")+ylab("Log(TP, ug)")    


#################################
library(cowplot)



p1<-ggplot(data=data, aes(x=log(chla_ug),y=log(N.P), col=as.factor(max.arch)))+
    geom_point()+
    geom_smooth(aes(group=as.factor(max.arch)),method="lm")+theme_bw()+
    scale_colour_discrete(name="Max Archteype")+xlab("Log(Chl-a, ug)")+ylab("Log(N:P molar)")+facet_wrap(~max.arch)+
    stat_poly_eq(formula = my.formula, 
                 aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
                 parse = TRUE) + theme(legend.position="top")

p2<-ggplot(data=data, aes(x=log(chla_ug),y=log(N.P), col=as.factor(ag_eco9)))+
  geom_point()+
  geom_smooth(aes(group=as.factor(ag_eco9)),method="lm")+theme_bw()+
  scale_colour_discrete(name="Ecoregion")+xlab("Log(Chl-a, ug)")+ylab("Log(N:P molar)")+facet_wrap(~ag_eco9)+
  stat_poly_eq(formula = my.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) + theme(legend.position="top")

plot_grid(p1,p2)



ggplot(data=data, aes(x=log(chla_ug),y=secchi_m, col=as.factor(max.arch)))+
    geom_point()+
    geom_smooth(aes(group=as.factor(max.arch)),method="lm")+theme_bw()+
    scale_colour_discrete(name="Max Archteype")+xlab("Log(Chl-a, ug)")+ylab("Sechhi, m")+facet_wrap(~max.arch, scales="free")+
    stat_poly_eq(formula = my.formula, 
                 aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
                 parse = TRUE) 

ggplot(data=data, aes(y=log(chla_ug),x=lake_connectivity_permanent, col=as.factor(max.arch)))+
    geom_boxplot()+
   scale_colour_discrete(name="Max Archteype")+xlab("")+ylab("Log(Chl-a, ug)")+facet_wrap(~max.arch)+
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

ggplot(data=data, aes(y=log(tp_ug),x=lake_connectivity_permanent, col=as.factor(max.arch)))+
    geom_boxplot()+
    scale_colour_discrete(name="Max Archteype")+xlab("")+ylab("Log(TP, ug)")+facet_wrap(~max.arch)+
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
   

ggplot(data=data, aes(x=log(ammonia_mg),y=log(tn_ug), col=as.factor(max.arch)))+
    geom_point()+
    geom_smooth(aes(group=as.factor(max.arch)),method="lm")+theme_bw()+
    scale_colour_discrete(name="Max Archteype")+xlab("Log(NH3, ug)")+ylab("Log(TN, ug)")+facet_wrap(~max.arch, scales="free")+
    stat_poly_eq(formula = my.formula, 
                 aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
                 parse = TRUE) 

ggplot(data=data, aes(x=log(nitrate_mg),y=log(tn_ug), col=as.factor(max.arch)))+
    geom_point()+
    geom_smooth(aes(group=as.factor(max.arch)),method="lm")+theme_bw()+
    scale_colour_discrete(name="Max Archteype")+xlab("Log(NH3, ug)")+ylab("Log(TN, ug)")+facet_wrap(~max.arch, scales="free")+
    stat_poly_eq(formula = my.formula, 
                 aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
                 parse = TRUE) 

###Trying mixed effects model with correlated slope and intercepts
library(lme4)
library(MuMIn)
mod<-lmer(log(chla_ug) ~ 1 + log(tp_ug) + (1 + log(tp_ug) | max.arch),data=data)
summary(mod)
r.squaredGLMM(mod)
