# Load libraries
rm(list=ls())
library(dplyr)
setwd("your dir")
dd=read.csv("CocaCola.csv",sep=",",dec=".",na.strings = c("NA",""," ","  "), stringsAsFactors = FALSE,)
dd=dd[,names(dd)%in% c("pends","value")]

# Check classes and fix inconsistencies
sapply(dd,class)
#View (dd$pends[1:20])
library(lubridate)
dd$pends=mdy(dd$pends)


#Get month on the three month base
dd$season=month(dd$pends)
dd$season=ifelse(dd$season=="3","1",dd$season)
dd$season=ifelse(dd$season=="6","2",dd$season)
dd$season=ifelse(dd$season=="9","3",dd$season)
dd$season=ifelse(dd$season=="12","4",dd$season)
dd$season=as.numeric(dd$season)
windows()
plot(dd$pends,dd$value,type="l",col="blue",lwd=2,panel.first=grid(),xlab="",ylab="")


#Get Values from 2001
dd=dd[dd$pends>="2000-01-01",]
windows()
plot(dd$pends,dd$value,type="l",col="blue",lwd=2,panel.first=grid(),xlab="",ylab="")

# Calculate seasonal factor weigths----
# Derive ybar
ybar=rep(NA,nrow(dd)) 
for (i in 1:nrow(dd)){
  ybar[i]=ifelse(i<2, mean(dd$value[1:(i+1)]),ifelse(i<=nrow(dd)-1,mean(dd$value[(i-2):(i+1)]),mean(dd$value[(i-2):nrow(dd)])))
}

windows()
plot(dd$pends,dd$value,type="l",col="blue",lwd=2,panel.first=grid(),xlab="",ylab="")
lines(dd$pends,ybar,col="red",lwd=2)

# Derive Z 
z=dd$value/ybar 
windows()
plot(dd$pends,z,type="l",col="blue",lwd=2,xlab="",ylab="",panel.first=grid())

# Derive Zbar 
zbar=rep(NA,4) 
for (i in 1:4){
  zbar[i]=mean(z[dd$season==i])
}
zbar
sum(zbar)


# Derive Zwave 
zwave=zbar*4/sum(zbar)
zwave 
sum(zwave)
 
windows()
plot(dd$pends[1:4],zwave,type="l",col="blue",lwd=2,xlab="",ylab="",panel.first=grid())

# Seasonally adjust data ----

W=data.frame(season=c(1:4),W=zwave)
dd=left_join(dd,W,by="season")
dd$FinalValue=dd$value/dd$W 
windows()
plot(dd$pends,dd$FinalValue,col="blue",type="l",lwd=2,xlab="",ylab="",panel.first=grid(),xlim=c(as.Date("2000/3/31"),as.Date("2009/9/30")))


# Fit trend model and derive 1 year ahead forecast----
dd$t=c(1:nrow(dd)) 
eq=lm(FinalValue~t,data=dd)
summary(eq)
f=predict(eq,newdata=data.frame(t=c((nrow(dd)+1):(nrow(dd)+4)))) 
f
windows()
plot(dd$pends,dd$FinalValue,col="blue",type="l",lwd=2,xlab="",ylab="",panel.first=grid(),xlim=c(as.Date("2000/3/31"),as.Date("2009/9/30")))
lines(dd$pends,eq$fitted.values,col="red",lwd=2)
lines(seq(as.Date("2009/10/30"),as.Date("2010/09/30"),"quarter"),f,lwd=2,col="red")

# Adjust forecast with seasonal weigths 
ff=f*W$W 
ff
prediction=data.frame(VALUES=ff,QUARTER=c(1:4))
windows()
plot(seq(as.Date("2009/10/30"),as.Date("2010/09/30"),"quarter"),ff,type="l",lwd=2,col="red",xlab="",ylab="",panel.first=grid(),xlim=c(as.Date("2000/3/31"),as.Date("2009/9/30")),ylim=c(min(dd$value),max(ff)))
lines(dd$pends,dd$value,col="blue",type="l",lwd=2)



