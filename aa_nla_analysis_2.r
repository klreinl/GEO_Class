## read in data frame with NLA data and archetype weights
nla.lag=read.csv("nla_lagos_aa_7.csv")
str(nla.lag)

## chla vs tp
plot(log(nla.lag$chla),log(nla.lag$tp_ug),col=nla.lag$max.arch)

## removing NA values
data.with.no.na=nla.lag[which(nla.lag$chla_ug>0 & nla.lag$tp_ug>0),]


## fit correlation model between tp and chla_ug with archetype weights as interactions
## this model has no "intercept", so the model for the K-th archetype is
## E(log(tp)) = beta.w.archK + beta.log(chla):w.archK * log(chla)
fit.aa=lm(log(tp_ug)~0+w.arch1+w.arch2+w.arch3+w.arch4+w.arch5+w.arch6+w.arch7+log(chla_ug):w.arch1+log(chla_ug):w.arch2+log(chla_ug):w.arch3+log(chla_ug):w.arch4+log(chla_ug):w.arch5+log(chla_ug):w.arch6+log(chla_ug):w.arch7,data=data.with.no.na)
summary(fit.aa)


## fits an equivalent model between tp and chla_ug with archetype weights as interactions
## this model has an "intercept", so the model for the K-th archetype is
## E(log(tp)) = (INTERCEPT + beta.w.archK) + (beta.log(chla) + beta.log(chla):w.archK) * log(chla)
fit.aa.no1=lm(log(tp_ug)~w.arch2+w.arch3+w.arch4+w.arch5+w.arch6+w.arch7+log(chla_ug)+log(chla_ug):w.arch2+log(chla_ug):w.arch3+log(chla_ug):w.arch4+log(chla_ug):w.arch5+log(chla_ug):w.arch6+log(chla_ug):w.arch7,data=data.with.no.na)
summary(fit.aa.no1)

## plotting predictions from these two models to show they are the same
plot(predict(fit.aa),predict(fit.aa.no1))
abline(0,1)


fit.max=lm(log(tp_ug)~as.factor(max.arch)*log(chla_ug),data=data.with.no.na)
summary(fit.max)

## fit correlation model between tp and chla_ug with eco-regions as interactions
fit.ecoreg=lm(log(tp_ug)~log(chla_ug)*ag_eco9,data=data.with.no.na)
summary(fit.ecoreg)

## fit global model
fit.global=lm(log(tp_ug)~log(chla_ug),data=data.with.no.na)
summary(fit.global)

## see which has a better fit to the data
AIC(fit.aa)
AIC(fit.aa.no1)
AIC(fit.ecoreg)
AIC(fit.max)
AIC(fit.global)

## ## plots of fitted vs. predicted
## logtp=log(data.with.no.na$tp)
## plot(logtp,predict(fit.max))
## abline(0,1)
## points(logtp,predict(fit.ecoreg),col="red",pch=2)
## points(logtp,predict(fit.aa),col="blue",pch=3)

##
## some plots
##
max.arch=data.with.no.na$max.arch
plot(log(data.with.no.na$chla_ug),log(data.with.no.na$tp_ug),col=max.arch+1,pch=max.arch)
abline(mean(fit.aa$coef[1:7]),mean(fit.aa$coef[8:14]),col=grey(.50),lwd=10)
points(log(data.with.no.na$chla_ug),log(data.with.no.na$tp_ug),col=max.arch+1,pch=max.arch)



plot(log(data.with.no.na$chla_ug),log(data.with.no.na$tp_ug),col=max.arch+1,pch=max.arch)
abline(mean(fit.aa$coef[1:7]),mean(fit.aa$coef[8:14]),col=grey(.50),lwd=10)
points(log(data.with.no.na$chla_ug),log(data.with.no.na$tp_ug),col=max.arch+1,pch=max.arch)
for(k in 1:7){
    abline(fit.aa$coef[k],fit.aa$coef[k+7],col=k+1,lty=1,lwd=2)
}


##
## plot of archetype relationships for the no intercept model
##
par(mfrow=c(2,4))
for(k in 1:7){
    idx.k=which(max.arch==k)
    if(length(idx.k)>0){
        plot(log(data.with.no.na$chla_ug),log(data.with.no.na$tp_ug),type="p",pch=".",col=grey(.6),main=paste("Arch = ",k,sep=""))
        points(log(data.with.no.na$chla_ug[idx.k]),log(data.with.no.na$tp_ug[idx.k]),col=k+1,main=paste("Arch = ",k,sep=""))
        abline(fit.aa$coef[k],fit.aa$coef[k+7],col=k+1,lty=1,lwd=2)
    }
}
savePlot("NoIntModel.jpg")


##
## plot of archetype relationships for the intercept model
## NOTE that these are indistinguishable from the no intercept model
##
par(mfrow=c(2,4))
for(k in 1:7){
    idx.k=which(max.arch==k)
    if(length(idx.k)>0){
        plot(log(data.with.no.na$chla_ug),log(data.with.no.na$tp_ug),type="p",pch=".",col=grey(.6),main=paste("Arch = ",k,sep=""))
        points(log(data.with.no.na$chla_ug[idx.k]),log(data.with.no.na$tp_ug[idx.k]),col=k+1,main=paste("Arch = ",k,sep=""))
        if(k>1){
            ## fitted line for archetypes 2 - 7
            abline(fit.aa.no1$coef[1]+fit.aa.no1$coef[k],fit.aa.no1$coef[8]+fit.aa.no1$coef[k+7],col=k+1,lty=1,lwd=2)
        }else{
            ## fitted line for archetype 1
            abline(fit.aa.no1$coef[1],fit.aa.no1$coef[8],col=k+1,lty=1,lwd=2)
        }
    }
}

savePlot("IntModel.jpg")
