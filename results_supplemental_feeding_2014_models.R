library(nlme)
library(MASS)
library(car)
library(multcomp)
library(MuMIn)
library(plotrix)
library(nortest)
setwd("C:/Users/cornilsj/Dropbox/jessi_und_franz/r/Kot/field")
setwd("C:/Users/Jessica/Dropbox/jessi_und_franz/r/Kot/field")
#dat<-read.csv("results_hormon_analyses_new.csv", header=T)
dat<-read.csv("hormon_analysis_females.csv")
dat$date=as.POSIXlt(strptime(dat$date,format="%d.%m.%Y"))

dat$year2<-dat$year
dat$year3<-dat$date$year+1900
dat$year3<-as.factor(dat$year3)
dat$year2<-as.factor(dat$year2)
dat$month<-as.character(dat$month)
dat$transponder<-as.character(dat$transponder)


#####delete columns to avoid deletion through NA´s#####

#reproduction season and only for gest and 37e
dat2=dat[,c(-13,-14, -18)]

#####only field data and na omit#####
dat2=subset(dat2, dat$treatment=="f")
dat2=na.omit(dat2)





####reproduction over the season####
m<-lme(X37e~month2+weight+age+reproduction_season+ gest44+ time+mast_three+mega_variable+strix_aluco, random= ~1| transponder, data=dat2, method="ML")

#get transformation from script 
source("C:/Users/cornilsj/Google Drive/Statistik/R/Functions and Commands/Boxcox AllClasses.r")
source("C:/Users/Jessica/Google Drive/Statistik/R/Functions and Commands/Boxcox AllClasses.r")

#transform model
dat2$transx37e <- boxcox.ac(m, dat2)
#create backtransformed ylab
y.lab <- bcPower(c(1,10,100,1000,10000), lambda=0.2)


#run transformed model with all variables
m<-lme(transx37e~month2+weight+age+reproduction_season+ gest44+ time+mast_three+ strix_aluco, random= ~1| transponder, data=dat2, method="ML")

#model selection
dredge(m)

#see if your model is normally distributed
qqnorm(resid(m))



#final model with the variables that are in the best model of model selection
m<-lme(transx37e~mast_three+gest44+weight+age+strix_aluco, random= ~1| transponder, data=dat2, method="ML")

Anova(m, type=3)
anova(m)

pplot(m) 

summary(glht(m, linfct=mcp(mast_three="Tukey")))
summary(glht(m, linfct=mcp(strix_aluco="Tukey")))
summary(glht(m, linfct=mcp(reproduction_season="Tukey")))


####reproduction at that time#####
#m<-lme(X37e~month2+weight+age+reproduction+ gest44+ time+mast_three+strix_aluco, random= ~1| transponder, data=dat4, method="ML")
#get transformation from script 
#source("C:/Users/cornilsj/Google Drive/Statistik/R/Functions and Commands/Boxcox AllClasses.r")
#transform model
#dat4$transx37e <- boxcox.ac(m, dat4)
#create backtransformed ylab
#y.lab <- bcPower(c(10,100,1000,10000,100000), lambda=-0.0999999)
#transformed model 
#m<-lme(transx37e~month2+weight+age+reproduction+ gest44+ time+mast_three+strix_aluco, random= ~1| transponder, data=dat4, method="ML")

#dredge(m)
#final model reproduction at that time
#m<-lme(transx37e~month2+gest44+weight+age+mast_three+strix_aluco, random= ~1| transponder, data=dat4, method="ML")

#Anova(m, type=3)
#anova(m)

#pplot(m)

#summary(glht(m, linfct=mcp(mast_three="Tukey")))
#summary(glht(m, linfct=mcp(month2="Tukey")))
#summary(glht(m, linfct=mcp(age="Tukey")))



#####plots####



#plot age
x=pplot(m,term="age")
x$transx37e_real=(x$age.effplusres + mean(dat2$transx37e,na.rm=T)) 
setEPS()
postscript("age_X37e.eps", width = 6, height = 6,  bg="NA")
par(mar=c(3,3,1,0))
par(bty="n")
plot(transx37e_real~age,data = x, pch = 19,cex = 0.6,outline=F, yaxt="n",xaxt="n",
     xlim = c(),
     xlab = list(""),
     ylim = c(3,9),
     ylab = list (""))
axis(1,at=c(1:2), labels=c("Adult", "Yearling") ,  cex.axis=1)
y.lab <- bcPower(c(10,32, 100,158), lambda=0.2)
axis(2,at=y.lab, labels=c(expression(paste("10"^"1")),expression(paste("10"^"1.5")),expression(paste("10"^"2")),expression(paste("10"^"2.2"))), cex.axis=1, pos=0.5, las=1)
#reg=lm(transgest44_real~X37e, data=x)
#ablineclip(reg, x1=0, x2=1750)
title(xlab="",  line=2, cex.lab=0.8)
mtext ("Cort Assay 37e [ng/g]", side= 2,line=1.5, cex=1, at=)
segments(x0=1,x1=2, y0=8.8)
mtext("***", side=3, line=-1.8, at=1.5, cex=1.2)

dev.off()

x=pplot(m,term="mast_three")    
x$transx37e_real=(x$mast_three.effplusres + mean(dat2$transx37e,na.rm=T)) 
setEPS()
postscript("mast_three_x37e.eps", width = 6, height = 6,  bg="NA")
par(mar=c(3,3,1,0))
par(bty="n")
plot(transx37e_real~mast_three,data = x, pch = 19,cex = 0.6,outline=F, yaxt="n",xaxt="n",
     xlim = c(),
     xlab = list("", cex = 1.2),
     ylim = c(3,9),
     ylab = list ("", cex = 1.2))
axis(1,at=c(1,2,3), labels= c("full mast", "non-mast", "supplemental feeding"))
y.lab <- bcPower(c(10,32, 100,158), lambda=0.2)
axis(2,at=y.lab, labels=c(expression(paste("10"^"1")),expression(paste("10"^"1.5")),expression(paste("10"^"2")),expression(paste("10"^"2.2"))), cex.axis=1, pos=0.5, las=1)
mtext ("Cort Assay 37e [ng/g]", side= 2,line=1.5, cex=1.2, at=6)
segments(x0=1,x1=3, y0=8.5, y1=8.5)
mtext("**", side=3, line=-3.3, at=2, cex=1.2)
segments(x0=2,x1=3, y0=8.3, y1=8.3)
mtext("**", side=3, line=-4.2, at=2.5, cex=1.2)

dev.off()

x=pplot(m,term="gest44")    
x$transx37e_real=(x$gest44.effplusres + mean(dat2$transx37e,na.rm=T)) 
setEPS()
postscript("gest44_x37e.eps", width = 6, height = 6,  bg="NA")
par(mar=c(4,5,2,0))
par(bty="n")
plot(transx37e_real~gest44,data = x, pch = 19,cex = 1, yaxt="n",#xaxt="n",
     xlim = c(),
     xlab = list(""),
     ylim = c(3,9),
     ylab = list (""))
#axis(1,at=dat2$gest44, labels= dat2$gest44, pos=1.5)
y.lab <- bcPower(c(10,32, 100,158), lambda=0.2)
axis(2,at=y.lab, labels=c(expression(paste("10"^"1")),expression(paste("10"^"1.5")),expression(paste("10"^"2")),expression(paste("10"^"2.2"))), cex.axis=1, pos=-1000, las=1)
reg=lm(transx37e_real~gest44, data=x)
ablineclip(reg, x1=0, x2=62000)
title(xlab="Assay Gestagene in [ng/g]",  line=2.5, cex.lab=1.2)
mtext ("Cort Assay 37e [ng/g]", side= 2,line=2.5, cex=1.2, at=6)

dev.off()




x=pplot(m,term="weight")    
x$transx37e_real=(x$weight.effplusres + mean(dat2$transx37e,na.rm=T)) 
setEPS()
postscript("bodymass_x37e.eps", width = 6, height = 6,  bg="NA")
par(mar=c(4,5,2,0))
par(bty="n")
plot(transx37e_real~weight,data = x, pch = 19,cex = 1, yaxt="n",xaxt="n",
     xlim = c(),
     xlab = list(""),
     ylim = c(3,8),
     ylab = list (""))
axis(1,at=c(50,60,70,80,90,100,110,120,130,140,150), labels= c(50,60,70,80,90,100,110,120,130,140,150))
y.lab <- bcPower(c(10,32,100), lambda=0.2)
axis(2,at=y.lab, labels=c(expression(paste("10"^"1")),expression(paste("10"^"1.5")),expression(paste("10"^"2"))), cex.axis=1, las=1)
reg=lm(transx37e_real~weight, data=x)
ablineclip(reg, x1=50, x2=150)
title(xlab="body mass [g]",  line=2.5, cex.lab=1.2)
mtext ("Cort Assay 37e [ng/g]", side= 2,line=3, cex=1.2, at=5.3)

dev.off()

summary(reg)$r.squared


x=pplot(m,term="strix_aluco")    
x$transx37e_real=(x$strix_aluco.effplusres + mean(dat2$transx37e,na.rm=T)) 
setEPS()
postscript("strix_aluco_x37e.eps", width = 6, height = 6,  bg="NA")
par(mar=c(4,3,1,0))
par(bty="n")
plot(transx37e_real~strix_aluco,data = x, pch = 19,cex = 0.6,outline=F, yaxt="n",xaxt="n",
     xlim = c(),
     xlab = list("", cex = 1.2),
     ylim = c(3,9),
     ylab = list ("", cex = 1.2))
axis(1,at=c(1,2), labels= c("no", "yes"))
title(xlab="Tawny owl territory",  line=2, cex.lab=1.2)
y.lab <- bcPower(c(10,32, 100,158), lambda=0.2)
axis(2,at=y.lab, labels=c(expression(paste("10"^"1")),expression(paste("10"^"1.5")),expression(paste("10"^"2")),expression(paste("10"^"2.2"))), cex.axis=1, pos=0.55, las=1)
mtext ("Cort Assay 37e [ng/g]", side=2,line=1.5, cex=1.2, at=6)
segments(x0=1,x1=2, y0=8.8, y1=8.8)
mtext("***", side=3, line=-2.2, at=1.5, cex=1.2)

dev.off()



