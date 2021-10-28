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

## fit correlation model between chla and tp_ug with archetype weights as interactions
fit.aa=lm(log(chla_ug)~0+w.arch1+w.arch2+w.arch3+w.arch4+w.arch5+w.arch6+w.arch7+log(tp_ug):w.arch1+log(tp_ug):w.arch2+log(tp_ug):w.arch3+log(tp_ug):w.arch4+log(tp_ug):w.arch5+log(tp_ug):w.arch6+log(tp_ug):w.arch7,data=data)
summary(fit.aa)


## fit correlation model between chla and tp_ug with max archetype as interactions
fit.aa.cat=lm(log(chla_ug)~log(tp_ug)*max.arch,data=data)
summary(fit.aa.cat)

## fit correlation model between chla and tp_ug with eco-regions as interactions
fit.ecoreg=lm(log(chla_ug)~log(tp_ug)*ag_eco9,data=data)
summary(fit.ecoreg)

#R squared 

summary(fit.aa)$adj.r.squared
summary(fit.aa.cat)$adj.r.squared
summary(fit.ecoreg)$adj.r.squared

## see which has a better fit to the data
AIC(fit.aa)
AIC(fit.aa.cat)
AIC(fit.ecoreg)


plot(log(data$tp_ug),log(data$chla_ug),col=data$max.arch+1,pch=data$max.arch,
     xlab="Log(TP, ug)", ylab="Log(Chla, ug)")
for(k in 1:7){
  abline(fit.aa$coef[k],fit.aa$coef[k+7],col=k+1,lty=1,lwd=2)
}

par(mfrow=c(2,4))
for(k in 1:7){
  idx.k=which(data$max.arch==k)
  if(length(idx.k)>0){
    plot(log(data$tp_ug),log(data$chla_ug),type="p",pch=".",
         xlab="Log(TP, ug)", ylab="Log(Chla, ug)",
         col=grey(.6),main=paste("Arch = ",k,sep=""))
    points(log(data$tp_ug[idx.k]),log(data$chla_ug[idx.k]),col=k+1,main=paste("Arch = ",k,sep=""))
    abline(fit.aa$coef[k],fit.aa$coef[k+7],col=k+1,lty=1,lwd=2)
  }
}

par(mfrow=c(1,1))
##
#TN vs chl-a


hist(data$tn_ug)
hist(log(data$tn_ug))

hist(data$chla_ug)
hist(log(data$chla_ug))

## fit correlation model between chla and tn_ug with archetype weights as interactions
fit.aa=lm(log(chla_ug)~0+w.arch1+w.arch2+w.arch3+w.arch4+w.arch5+w.arch6+w.arch7+log(tn_ug):w.arch1+log(tn_ug):w.arch2+log(tn_ug):w.arch3+log(tn_ug):w.arch4+log(tn_ug):w.arch5+log(tn_ug):w.arch6+log(tn_ug):w.arch7,data=data)
summary(fit.aa)


## fit correlation model between chla and tn_ug with max archetype as interactions
fit.aa.cat=lm(log(chla_ug)~log(tn_ug)*max.arch,data=data)
summary(fit.aa.cat)

## fit correlation model between chla and tn_ug with eco-regions as interactions
fit.ecoreg=lm(log(chla_ug)~log(tn_ug)*ag_eco9,data=data)
summary(fit.ecoreg)

#R squared 

summary(fit.aa)$adj.r.squared
summary(fit.aa.cat)$adj.r.squared
summary(fit.ecoreg)$adj.r.squared

## see which has a better fit to the data
AIC(fit.aa)
AIC(fit.aa.cat)
AIC(fit.ecoreg)

plot(log(data$tn_ug),log(data$chla_ug),col=data$max.arch+1,pch=data$max.arch,
     xlab="Log(TN, ug)", ylab="Log(Chla, ug)")
for(k in 1:7){
  abline(fit.aa$coef[k],fit.aa$coef[k+7],col=k+1,lty=1,lwd=2)
}

par(mfrow=c(2,4))
for(k in 1:7){
  idx.k=which(data$max.arch==k)
  if(length(idx.k)>0){
    plot(log(data$tn_ug),log(data$chla_ug),type="p",pch=".",
         xlab="Log(TN, ug)", ylab="Log(Chla, ug)",
         col=grey(.6),main=paste("Arch = ",k,sep=""))
    points(log(data$tn_ug[idx.k]),log(data$chla_ug[idx.k]),col=k+1,main=paste("Arch = ",k,sep=""))
    abline(fit.aa$coef[k],fit.aa$coef[k+7],col=k+1,lty=1,lwd=2)
  }
}

par(mfrow=c(1,1))


#TP vs chla

hist(data$TP_uM)
hist(log(data$TP_uM))

hist(data$chla_ug)
hist(log(data$chla_ug))

## fit correlation model between chla and TP_uM with archetype weights as interactions
fit.aa=lm(log(chla_ug)~0+w.arch1+w.arch2+w.arch3+w.arch4+w.arch5+w.arch6+w.arch7+log(TP_uM):w.arch1+log(TP_uM):w.arch2+log(TP_uM):w.arch3+log(TP_uM):w.arch4+log(TP_uM):w.arch5+log(TP_uM):w.arch6+log(TP_uM):w.arch7,data=data)
summary(fit.aa)


## fit correlation model between chla and TP_uM with max archetype as interactions
fit.aa.cat=lm(log(chla_ug)~log(TP_uM)*max.arch,data=data)
summary(fit.aa.cat)

## fit correlation model between chla and TP_uM with eco-regions as interactions
fit.ecoreg=lm(log(chla_ug)~log(TP_uM)*ag_eco9,data=data)
summary(fit.ecoreg)


#R squared 

summary(fit.aa)$adj.r.squared
summary(fit.aa.cat)$adj.r.squared
summary(fit.ecoreg)$adj.r.squared

## see which has a better fit to the data
AIC(fit.aa)
AIC(fit.aa.cat)
AIC(fit.ecoreg)


##AA model best


plot(log(data$TP_uM),log(data$chla_ug),col=data$max.arch+1,pch=data$max.arch,
     xlab="Log(TP, uM", ylab="Log(Chla, ug)")
for(k in 1:7){
  abline(fit.aa$coef[k],fit.aa$coef[k+7],col=k+1,lty=1,lwd=2)
}

par(mfrow=c(2,4))
for(k in 1:7){
  idx.k=which(data$max.arch==k)
  if(length(idx.k)>0){
    plot(log(data$TP_uM),log(data$chla_ug),type="p",pch=".",
         xlab="Log(TP, uM)", ylab="Log(Chla, ug)",
         col=grey(.6),main=paste("Arch = ",k,sep=""))
    points(log(data$TP_uM[idx.k]),log(data$chla_ug[idx.k]),col=k+1,main=paste("Arch = ",k,sep=""))
    abline(fit.aa$coef[k],fit.aa$coef[k+7],col=k+1,lty=1,lwd=2)
  }
}

par(mfrow=c(1,1))





#TN vs chla

hist(data$TN_uM)
hist(log(data$TN_uM))

hist(data$chla_ug)
hist(log(data$chla_ug))

## fit correlation model between chla and TN_uM with archetype weights as interactions
fit.aa=lm(log(chla_ug)~0+w.arch1+w.arch2+w.arch3+w.arch4+w.arch5+w.arch6+w.arch7+log(TN_uM):w.arch1+log(TN_uM):w.arch2+log(TN_uM):w.arch3+log(TN_uM):w.arch4+log(TN_uM):w.arch5+log(TN_uM):w.arch6+log(TN_uM):w.arch7,data=data)
summary(fit.aa)


## fit correlation model between chla and TN_uM with max archetype as interactions
fit.aa.cat=lm(log(chla_ug)~log(TN_uM)*max.arch,data=data)
summary(fit.aa.cat)

## fit correlation model between chla and TN_uM with eco-regions as interactions
fit.ecoreg=lm(log(chla_ug)~log(TN_uM)*ag_eco9,data=data)
summary(fit.ecoreg)

#R squared 

summary(fit.aa)$adj.r.squared
summary(fit.aa.cat)$adj.r.squared
summary(fit.ecoreg)$adj.r.squared

## see which has a better fit to the data
AIC(fit.aa)
AIC(fit.aa.cat)
AIC(fit.ecoreg)


##AA model best



plot(log(data$TN_uM),log(data$chla_ug),col=data$max.arch+1,pch=data$max.arch,
     xlab="Log(TN, uM", ylab="Log(Chla, ug)")
for(k in 1:7){
  abline(fit.aa$coef[k],fit.aa$coef[k+7],col=k+1,lty=1,lwd=2)
}

par(mfrow=c(2,4))
for(k in 1:7){
  idx.k=which(data$max.arch==k)
  if(length(idx.k)>0){
    plot(log(data$TN_uM),log(data$chla_ug),type="p",pch=".",
         xlab="Log(TN, uM)", ylab="Log(Chla, ug)",
         col=grey(.6),main=paste("Arch = ",k,sep=""))
    points(log(data$TN_uM[idx.k]),log(data$chla_ug[idx.k]),col=k+1,main=paste("Arch = ",k,sep=""))
    abline(fit.aa$coef[k],fit.aa$coef[k+7],col=k+1,lty=1,lwd=2)
  }
}

par(mfrow=c(1,1))

#######
#Secchi vs chla

hist(data$secchi_m)
hist(log(data$secchi_m))

hist(data$chla_ug)
hist(log(data$chla_ug))


data$log.secchi_m<-c(log(data$secchi_m))
data<-data[data$log.secchi_m >(-5),]


## fit correlation model between chla and secchi_m with archetype weights as interactions
fit.aa=lm(log(chla_ug)~0+w.arch1+w.arch2+w.arch3+w.arch4+w.arch5+w.arch6+w.arch7+log(secchi_m):w.arch1+log(secchi_m):w.arch2+log(secchi_m):w.arch3+log(secchi_m):w.arch4+log(secchi_m):w.arch5+log(secchi_m):w.arch6+log(secchi_m):w.arch7,data=data)
summary(fit.aa)


## fit correlation model between chla and secchi_m with max archetype as interactions
fit.aa.cat=lm(log(chla_ug)~log(secchi_m)*max.arch,data=data)
summary(fit.aa.cat)

## fit correlation model between chla and secchi_m with eco-regions as interactions
fit.ecoreg=lm(log(chla_ug)~log(secchi_m)*ag_eco9,data=data)
summary(fit.ecoreg)

#R squared 

summary(fit.aa)$adj.r.squared
summary(fit.aa.cat)$adj.r.squared
summary(fit.ecoreg)$adj.r.squared

## see which has a better fit to the data
AIC(fit.aa)
AIC(fit.aa.cat)
AIC(fit.ecoreg)

##AA model best



plot(log(data$secchi_m),log(data$chla_ug),col=data$max.arch+1,pch=data$max.arch,
     xlab="Log(Secchi, m", ylab="Log(Chla, ug)")
for(k in 1:7){
  abline(fit.aa$coef[k],fit.aa$coef[k+7],col=k+1,lty=1,lwd=2)
}

par(mfrow=c(2,4))
for(k in 1:7){
  idx.k=which(data$max.arch==k)
  if(length(idx.k)>0){
    plot(log(data$secchi_m),log(data$chla_ug),type="p",pch=".",
         xlab="Log(secchi, m)", ylab="Log(Chla, ug)",
         col=grey(.6),main=paste("Arch = ",k,sep=""))
    points(log(data$secchi_m[idx.k]),log(data$chla_ug[idx.k]),col=k+1,main=paste("Arch = ",k,sep=""))
    abline(fit.aa$coef[k],fit.aa$coef[k+7],col=k+1,lty=1,lwd=2)
  }
}

par(mfrow=c(1,1))

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

## fit correlation model between chla and N.P with archetype weights as interactions
fit.aa=lm(log(chla_ug)~0+w.arch1+w.arch2+w.arch3+w.arch4+w.arch5+w.arch6+w.arch7+log(N.P):w.arch1+log(N.P):w.arch2+log(N.P):w.arch3+log(N.P):w.arch4+log(N.P):w.arch5+log(N.P):w.arch6+log(N.P):w.arch7,data=data)
summary(fit.aa)


## fit correlation model between chla and N.P with max archetype as interactions
fit.aa.cat=lm(log(chla_ug)~log(N.P)*max.arch,data=data)
summary(fit.aa.cat)

## fit correlation model between chla and N.P with eco-regions as interactions
fit.ecoreg=lm(log(chla_ug)~log(N.P)*ag_eco9,data=data)
summary(fit.ecoreg)

#R squared 

summary(fit.aa)$adj.r.squared
summary(fit.aa.cat)$adj.r.squared
summary(fit.ecoreg)$adj.r.squared

## see which has a better fit to the data
AIC(fit.aa)
AIC(fit.aa.cat)
AIC(fit.ecoreg)

##AAmodel best


plot(log(data$N.P),log(data$chla_ug),col=data$max.arch+1,pch=data$max.arch,
     xlab="Log(N:P (molar)", ylab="Log(Chla, ug)")
for(k in 1:7){
  abline(fit.aa$coef[k],fit.aa$coef[k+7],col=k+1,lty=1,lwd=2)
}

par(mfrow=c(2,4))
for(k in 1:7){
  idx.k=which(data$max.arch==k)
  if(length(idx.k)>0){
    plot(log(data$N.P),log(data$chla_ug),type="p",pch=".",
         xlab="Log(N:P (molar))", ylab="Log(Chla, ug)",
         col=grey(.6),main=paste("Arch = ",k,sep=""))
    points(log(data$N.P[idx.k]),log(data$chla_ug[idx.k]),col=k+1,main=paste("Arch = ",k,sep=""))
    abline(fit.aa$coef[k],fit.aa$coef[k+7],col=k+1,lty=1,lwd=2)
  }
}

par(mfrow=c(1,1))

#NH3 vs TN


hist(data$ammonia_mg)
hist(log(data$ammonia_mg))

hist(data$tn_ug)
hist(log(data$tn_ug))

data$log.NH3<-c(log(data$ammonia_mg))
data<-data[data$log.NH3 >(-9),]

## fit correlation model between tn and ammonia_mg with archetype weights as interactions
fit.aa=lm(log(tn_ug)~0+w.arch1+w.arch2+w.arch3+w.arch4+w.arch5+w.arch6+w.arch7+log(ammonia_mg):w.arch1+log(ammonia_mg):w.arch2+log(ammonia_mg):w.arch3+log(ammonia_mg):w.arch4+log(ammonia_mg):w.arch5+log(ammonia_mg):w.arch6+log(ammonia_mg):w.arch7,data=data)
summary(fit.aa)


## fit correlation model between tn and ammonia_mg with max archetype as interactions
fit.aa.cat=lm(log(tn_ug)~log(ammonia_mg)*max.arch,data=data)
summary(fit.aa.cat)

## fit correlation model between tn and ammonia_mg with eco-regions as interactions
fit.ecoreg=lm(log(tn_ug)~log(ammonia_mg)*ag_eco9,data=data)
summary(fit.ecoreg)

#R squared 

summary(fit.aa)$adj.r.squared
summary(fit.aa.cat)$adj.r.squared
summary(fit.ecoreg)$adj.r.squared

## see which has a better fit to the data
AIC(fit.aa)
AIC(fit.aa.cat)
AIC(fit.ecoreg)

##AA model best


plot(log(data$ammonia_mg),log(data$tn_ug),col=data$max.arch+1,pch=data$max.arch,
     xlab="Log(NH3, mg)", ylab="Log(TN, ug)")
for(k in 1:7){
  abline(fit.aa$coef[k],fit.aa$coef[k+7],col=k+1,lty=1,lwd=2)
}

par(mfrow=c(2,4))
for(k in 1:7){
  idx.k=which(data$max.arch==k)
  if(length(idx.k)>0){
    plot(log(data$ammonia_mg),log(data$tn_ug),type="p",pch=".",
         xlab="Log(NH3, mg)", ylab="Log(TN, ug)",
         col=grey(.6),main=paste("Arch = ",k,sep=""))
    points(log(data$ammonia_mg[idx.k]),log(data$tn_ug[idx.k]),col=k+1,main=paste("Arch = ",k,sep=""))
    abline(fit.aa$coef[k],fit.aa$coef[k+7],col=k+1,lty=1,lwd=2)
  }
}

par(mfrow=c(1,1))


#NO3 vs TN

hist(data$nitrate_mg)
hist(log(data$nitrate_mg))

hist(data$tn_ug)
hist(log(data$tn_ug))


data<-data.with.no.na
data$log.NO3<-c(log(data$nitrate_mg))
data<-data[data$log.NO3 >(-10),]

## fit correlation model between tn and nitrate_mg with archetype weights as interactions
fit.aa=lm(log(tn_ug)~0+w.arch1+w.arch2+w.arch3+w.arch4+w.arch5+w.arch6+w.arch7+log(nitrate_mg):w.arch1+log(nitrate_mg):w.arch2+log(nitrate_mg):w.arch3+log(nitrate_mg):w.arch4+log(nitrate_mg):w.arch5+log(nitrate_mg):w.arch6+log(nitrate_mg):w.arch7,data=data)
summary(fit.aa)


## fit correlation model between tn and nitrate_mg with max archetype as interactions
fit.aa.cat=lm(log(tn_ug)~log(nitrate_mg)*max.arch,data=data)
summary(fit.aa.cat)

## fit correlation model between tn and nitrate_mg with eco-regions as interactions
fit.ecoreg=lm(log(tn_ug)~log(nitrate_mg)*ag_eco9,data=data)
summary(fit.ecoreg)

#R squared 

summary(fit.aa)$adj.r.squared
summary(fit.aa.cat)$adj.r.squared
summary(fit.ecoreg)$adj.r.squared

## see which has a better fit to the data
AIC(fit.aa)
AIC(fit.aa.cat)
AIC(fit.ecoreg)


## AA model best

plot(log(data$nitrate_mg),log(data$tn_ug),col=data$max.arch+1,pch=data$max.arch,
     xlab="Log(NO3, mg)", ylab="Log(TN, ug)")
for(k in 1:7){
  abline(fit.aa$coef[k],fit.aa$coef[k+7],col=k+1,lty=1,lwd=2)
}

par(mfrow=c(2,4))
for(k in 1:7){
  idx.k=which(data$max.arch==k)
  if(length(idx.k)>0){
    plot(log(data$nitrate_mg),log(data$tn_ug),type="p",pch=".",
         xlab="Log(NO3, mg)", ylab="Log(TN, ug)",
         col=grey(.6),main=paste("Arch = ",k,sep=""))
    points(log(data$nitrate_mg[idx.k]),log(data$tn_ug[idx.k]),col=k+1,main=paste("Arch = ",k,sep=""))
    abline(fit.aa$coef[k],fit.aa$coef[k+7],col=k+1,lty=1,lwd=2)
  }
}

par(mfrow=c(1,1))


#doc vs Chla

hist(data$doc_mg)
hist(log(data$doc_mg))

hist(data$chla_ug)
hist(log(data$chla_ug))


## fit correlation model between chla and doc_mg with archetype weights as interactions
fit.aa=lm(log(chla_ug)~0+w.arch1+w.arch2+w.arch3+w.arch4+w.arch5+w.arch6+w.arch7+log(doc_mg):w.arch1+log(doc_mg):w.arch2+log(doc_mg):w.arch3+log(doc_mg):w.arch4+log(doc_mg):w.arch5+log(doc_mg):w.arch6+log(doc_mg):w.arch7,data=data)
summary(fit.aa)


## fit correlation model between chla and doc_mg with max archetype as interactions
fit.aa.cat=lm(log(chla_ug)~log(doc_mg)*max.arch,data=data)
summary(fit.aa.cat)

## fit correlation model between chla and doc_mg with eco-regions as interactions
fit.ecoreg=lm(log(chla_ug)~log(doc_mg)*ag_eco9,data=data)
summary(fit.ecoreg)

#R squared 

summary(fit.aa)$adj.r.squared
summary(fit.aa.cat)$adj.r.squared
summary(fit.ecoreg)$adj.r.squared

## see which has a better fit to the data
AIC(fit.aa)
AIC(fit.aa.cat)
AIC(fit.ecoreg)




plot(log(data$doc_mg),log(data$chla_ug),col=data$max.arch+1,pch=data$max.arch,
     xlab="Log(DOC, mg", ylab="Log(Chla, ug)")
for(k in 1:7){
  abline(fit.aa$coef[k],fit.aa$coef[k+7],col=k+1,lty=1,lwd=2)
}

par(mfrow=c(2,4))
for(k in 1:7){
  idx.k=which(data$max.arch==k)
  if(length(idx.k)>0){
    plot(log(data$doc_mg),log(data$chla_ug),type="p",pch=".",
         xlab="Log(DOC, mg)", ylab="Log(Chla, ug)",
         col=grey(.6),main=paste("Arch = ",k,sep=""))
    points(log(data$doc_mg[idx.k]),log(data$chla_ug[idx.k]),col=k+1,main=paste("Arch = ",k,sep=""))
    abline(fit.aa$coef[k],fit.aa$coef[k+7],col=k+1,lty=1,lwd=2)
  }
}

par(mfrow=c(1,1))


#doc vs toc
data<-data.with.no.na

hist(data$doc_mg)
hist(log(data$doc_mg))

hist(data$toc_mg)
hist(log(data$toc_mg))


## fit correlation model between doc and doc_mg with archetype weights as interactions
fit.aa=lm(log(toc_mg)~0+w.arch1+w.arch2+w.arch3+w.arch4+w.arch5+w.arch6+w.arch7+log(doc_mg):w.arch1+log(doc_mg):w.arch2+log(doc_mg):w.arch3+log(doc_mg):w.arch4+log(doc_mg):w.arch5+log(doc_mg):w.arch6+log(doc_mg):w.arch7,data=data)
summary(fit.aa)


## fit correlation model between tn and doc_mg with max archetype as interactions
fit.aa.cat=lm(log(toc_mg)~log(doc_mg)*max.arch,data=data)
summary(fit.aa.cat)

## fit correlation model between tn and doc_mg with eco-regions as interactions
fit.ecoreg=lm(log(toc_mg)~log(doc_mg)*ag_eco9,data=data)
summary(fit.ecoreg)


#R squared 

summary(fit.aa)$adj.r.squared
summary(fit.aa.cat)$adj.r.squared
summary(fit.ecoreg)$adj.r.squared


## see which has a better fit to the data
AIC(fit.aa)
AIC(fit.aa.cat)
AIC(fit.ecoreg)




plot(log(data$doc_mg),log(data$toc_mg),col=data$max.arch+1,pch=data$max.arch,
     xlab="Log(DOC, mg", ylab="Log(TOC, mg)")
for(k in 1:7){
  abline(fit.aa$coef[k],fit.aa$coef[k+7],col=k+1,lty=1,lwd=2)
}

par(mfrow=c(2,4))
for(k in 1:7){
  idx.k=which(data$max.arch==k)
  if(length(idx.k)>0){
    plot(log(data$doc_mg),log(data$toc_mg),type="p",pch=".",
         xlab="Log(DOC, mg)", ylab="Log(TOC, mg)",
         col=grey(.6),main=paste("Arch = ",k,sep=""))
    points(log(data$doc_mg[idx.k]),log(data$toc_mg[idx.k]),col=k+1,main=paste("Arch = ",k,sep=""))
    abline(fit.aa$coef[k],fit.aa$coef[k+7],col=k+1,lty=1,lwd=2)
  }
}

par(mfrow=c(1,1))










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
