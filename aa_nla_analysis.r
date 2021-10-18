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

## fit correlation model between tp and chla_ug with archetype weights as interactions
fit.aa=lm(log(tp_ug)~0+w.arch1+w.arch2+w.arch3+w.arch4+w.arch5+w.arch6+w.arch7+log(chla_ug):w.arch1+log(chla_ug):w.arch2+log(chla_ug):w.arch3+log(chla_ug):w.arch4+log(chla_ug):w.arch5+log(chla_ug):w.arch6+log(chla_ug):w.arch7,data=data.with.no.na)
summary(fit.aa)

## fit correlation model between tp and chla_ug with eco-regions as interactions
fit.ecoreg=lm(log(tp_ug)~log(chla_ug)*ag_eco9,data=data.with.no.na)
summary(fit.ecoreg)

## see which has a better fit to the data
AIC(fit.aa)
AIC(fit.ecoreg)


##
## some plots
##

plot(log(data.with.no.na$chla_ug),log(data.with.no.na$tp_ug),col=data.with.no.na$max.arch+1,pch=data.with.no.na$max.arch)
abline(mean(fit.aa$coef[1:7]),mean(fit.aa$coef[8:14]),col=grey(.50),lwd=10)
points(log(data.with.no.na$chla_ug),log(data.with.no.na$tp_ug),col=data.with.no.na$max.arch+1,pch=data.with.no.na$max.arch)



plot(log(data.with.no.na$chla_ug),log(data.with.no.na$tp_ug),col=data.with.no.na$max.arch+1,pch=data.with.no.na$max.arch)
abline(mean(fit$coef[1:7]),mean(fit$coef[8:14]),col=grey(.50),lwd=10)
points(log(data.with.no.na$chla_ug),log(data.with.no.na$tp_ug),col=max.arch+1,pch=max.arch)
for(k in 1:7){
    abline(fit$coef[k],fit$coef[k+7],col=k+1,lty=1,lwd=2)
}

par(mfrow=c(2,4))
for(k in 1:7){
    idx.k=which(max.arch==k)
    if(length(idx.k)>0){
        plot(log(data.with.no.na$chla_ug),log(data.with.no.na$tp_ug),type="p",pch=".",col=grey(.6),main=paste("Arch = ",k,sep=""))
        points(log(data.with.no.na$chla_ug[idx.k]),log(data.with.no.na$tp_ug[idx.k]),col=k+1,main=paste("Arch = ",k,sep=""))
        abline(fit$coef[k],fit$coef[k+7],col=k+1,lty=1,lwd=2)
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


ggplot(data=data, aes(x=log(chla_ug),y=log(N.P), col=as.factor(max.arch)))+
    geom_point()+
    geom_smooth(aes(group=as.factor(max.arch)),method="lm")+theme_bw()+
    scale_colour_discrete(name="Max Archteype")+xlab("Log(Chl-a, ug)")+ylab("Log(N:P molar)")+facet_wrap(~max.arch)+
    stat_poly_eq(formula = my.formula, 
                 aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
                 parse = TRUE) 

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
