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
abline(mean(fit$coef[1:7]),mean(fit$coef[8:14]),col=grey(.50),lwd=10)
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


