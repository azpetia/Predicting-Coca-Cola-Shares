# Load libraries ----
rm(list=ls())
library(dplyr)
setwd("your dir")
dd=read.csv("CocaCola.csv",sep=",",dec=".",na.strings = c("NA",""," ","  "), stringsAsFactors = FALSE,)
dd=dd[,names(dd)%in% c("pends","value")]

# Check classes and fix inconsistencies ----
sapply(dd,class)
#View (dd$pends[1:20])
library(lubridate)
dd$pends=mdy(dd$pends)

#Get month on the three month base ----
dd$month=month(dd$pends)
dd$month=ifelse(dd$month=="3","1",dd$month)
dd$month=ifelse(dd$month=="6","2",dd$month)
dd$month=ifelse(dd$month=="9","3",dd$month)
dd$month=ifelse(dd$month=="12","4",dd$month)
dd$month=as.numeric(dd$month)
windows()
plot(dd$pends,dd$value,type="l",col="blue",lwd=2,panel.first=grid(),xlab="",ylab="")
 
#Get Values from 2001 ----
dd=dd[dd$pends>="2000-01-01",]
windows()
plot(dd$pends,dd$value,type="l",col="blue",lwd=2,panel.first=grid(),xlab="",ylab="")

# Fit linear trend model with dummy variables accounting for presence of seasonality ----
# Create a set of dummy variables
mm=data.frame(month=c(1:4),MONTHn=as.factor(c("Jan-March","April-June","July-Sept","Oct-Dec")))
dd=left_join(dd,mm,by="month")
windows()
plot(dd$MONTHn,dd$value,col="light blue",lwd=2)
summary(dd$value)
dd$MONTHn=relevel(dd$MONTHn,ref="Jan-March") 

# Insert a trend variable
dd$t=c(1:nrow(dd))

# Fit trend model ----
eq=lm(value~t+MONTHn,data=dd)
summary(eq)

# Examine regression coefficient. 
confint(eq, level=0.95)
library(psych)
cor.test(dd$value,dd$t)
n=nrow(dd)
qt(0.025,df=n-2)
qt(0.975,df=n-2)
eq$coefficients

# Plot actual vs fitted----
windows()
plot(dd$pends,dd$value,type="l",col="blue",lwd=2, panel.first=grid(),xlab="",ylab="")
lines(dd$pends,eq$fitted.values,col="red",lwd=2)


# Modify the regression specificantion ----
dd$AprilJune=ifelse(dd$MONTHn=="April-June",1,0)
dd$JulySept=ifelse(dd$MONTHn=="July-Sept",1,0)
eq2=lm(value~t+AprilJune+JulySept,data=dd)
summary(eq2)

# Plot actual vs fitted ----
windows()
plot(dd$pends,dd$value,type="l",col="blue",lwd=2, panel.first=grid(),xlab="",ylab="")
lines(dd$pends,eq2$fitted.values,col="red",lwd=2)


# Make a forecast for the next year
ddnew=data.frame(t=c((nrow(dd)+1):(nrow(dd)+4)),MONTHn=c("Jan-March","April-June","July-Sep","Oct-Dec"))
ddnew$JanMarch=ifelse(ddnew$MONTHn=="Jan-March",1,0)
ddnew$AprilJune=ifelse(ddnew$MONTHn=="April-June",1,0)
ddnew$JulySept=ifelse(ddnew$MONTHn=="July-Sep",1,0)
ddnew$OctDec=ifelse(ddnew$MONTHn=="Oct-Dec",1,0)

(f=predict(eq2,newdata = ddnew))
f
PredictedValues=data.frame(predictions=f,quarter=c(1,2,3,4))

# Visualize actual vs predicted
windows()
plot(seq(as.Date("2009/10/30"),as.Date("2010/09/30"),"quarter"),f,type="l",lwd=2,col="red",xlab="",ylab="",panel.first=grid(),xlim=c(as.Date("2000/3/31"),as.Date("2009/9/30")),ylim=c(min(dd$value),max(f)))
lines(dd$pends,dd$value,col="blue",type="l",lwd=2)

# Apply the Holt-Winters algorithm -----
y=ts(dd$value,frequency=4,start=c(year(dd$pends)[1],month(dd$pends)[1]))
eq3=HoltWinters(y,alpha=T,beta=T,gamma=T,seasonal="additive",start.periods = 5)
windows() 
plot(eq3)

# Make a forecast
fit=as.data.frame(fitted(eq3))
fit$month=dd$MONTHn[5:nrow(dd)]

n=nrow(fit)
(Lt=fit$level[n]*rep(1,4))
(Tt=fit$trend[n]*rep(1,4))
(Tt=Tt*c(1:4))
(St=fit$season[(n-3):n])
(Ft=Lt+Tt+St)


# Visualize actual vs predicted
windows()
plot(seq(as.Date("2009/10/30"),as.Date("2010/09/30"),"quarter"),Ft,type="l",lwd=2,col="red",xlab="",ylab="",panel.first=grid(),xlim=c(as.Date("2000/3/31"),as.Date("2010/09/30")),ylim=c(min(dd$value),max(Ft)))
lines(dd$pends,dd$value,col="blue",type="l",lwd=2)


# Compare results
sqrt(sum(eq2$residuals^2))
sqrt(eq3$SSE)



