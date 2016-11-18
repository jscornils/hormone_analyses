library(MASS)
library(car)
library(lme4)
library(nlme)
library(ggplot2)
library(grid)
library(MuMIn)
library(multcomp)
setwd("C:/Users/cornilsj/Dropbox/jessi_und_franz/r/Kot/acth")
#dat=read.csv("animals_ACTH_females.csv")
#dat=read.csv("animals_ACTH_males.csv")
dat=read.csv("animals_ACTH_females.csv")

#loading the sem function into r
sem<-function(x, na.rm=T){
  if (na.rm==T) x=na.omit(x)
  l=length(x)
  return (sd(x)/sqrt(l))
}


head(dat)
dat$date=as.POSIXlt(strptime(dat$date,format="%d.%m.%Y") )

dat$datetime=as.POSIXct(strptime(paste(dat$date, dat$time_smooth), format="%Y-%m-%d %H:%M"))

dat$hours=as.factor(dat$hours)


####multiplot####
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

####plots####

dat=subset(dat, datetime<"2015-07-06 17:00:00 CEST")
dat1=subset(dat, animal=="1")
p1=ggplot(dat1, aes(x=datetime, y=X37e))+
  #geom_errorbar(aes(ymin=kmean-ksem, ymax=kmean+ksem))+
  geom_line()+
  geom_point(aes(colour=dat1$treat), size=2)+
  theme_classic()+
  geom_text(aes(label=dat1$time), size=2)+
  ylim(0,5000)+
  xlab("")+
  ylab("Assay 37e in [ng/g]")


dat2=subset(dat, animal=="2")
p2=ggplot(dat2, aes(x=datetime, y=X37e))+
  #geom_errorbar(aes(ymin=kmean-ksem, ymax=kmean+ksem))+
  geom_line()+
  geom_point(aes(colour=dat2$treat), size=2)+
  theme_classic()+
  geom_text(aes(label=dat2$time), size=2)+
  ylim(0,5000)+
  xlab("")+
  ylab("Assay 37e in [ng/g]")


dat3=subset(dat, animal=="3")
p3=ggplot(dat3, aes(x=datetime, y=X37e))+
  #geom_errorbar(aes(ymin=kmean-ksem, ymax=kmean+ksem))+
  geom_line()+
  geom_point(aes(colour=dat3$treat), size=2)+
  theme_classic()+
  geom_text(aes(label=dat3$time), size=2)+
  ylim(0,5000)+
  xlab("")+
  ylab("Assay 37e in [ng/g]")



dat8=subset(dat, animal=="8")
p8=ggplot(dat8, aes(x=datetime, y=X37e))+
  #geom_errorbar(aes(ymin=kmean-ksem, ymax=kmean+ksem))+
  geom_line()+
  geom_point(aes(colour=dat8$treat), size=2)+
  theme_classic()+
  geom_text(aes(label=dat8$time), size=2)+
  ylim(0,5000)+
  xlab("")+
  ylab("Assay 37e in [ng/g]")

dat9=subset(dat, animal=="9")
p9=ggplot(dat9, aes(x=datetime, y=X37e))+
  #geom_errorbar(aes(ymin=kmean-ksem, ymax=kmean+ksem))+
  geom_line()+
  geom_point(aes(colour=dat9$treat), size=2)+
  theme_classic()+
  geom_text(aes(label=dat9$time), size=2)+
  ylim(0,5000)+
  xlab("")+
  ylab("Assay 37e in [ng/g]")

dat10=subset(dat, animal=="10")
p10=ggplot(dat10, aes(x=datetime, y=X37e))+
  #geom_errorbar(aes(ymin=kmean-ksem, ymax=kmean+ksem))+
  geom_line()+
  geom_point(aes(colour=dat10$treat), size=2)+
  theme_classic()+
  geom_text(aes(label=dat10$time), size=2)+
  ylim(0,5000)+
  xlab("")+
  ylab("Assay 37e in [ng/g]")

dat11=subset(dat, animal=="11")
p11=ggplot(dat11, aes(x=datetime, y=X37e))+
  #geom_errorbar(aes(ymin=kmean-ksem, ymax=kmean+ksem))+
  geom_line()+
  geom_point(aes(colour=dat11$treat), size=2)+
  theme_classic()+
  geom_text(aes(label=dat11$time), size=2)+
  ylim(0,5000)+
  xlab("")+
  ylab("Assay 37e in [ng/g]")

dat12=subset(dat, animal=="12")
p12=ggplot(dat12, aes(x=datetime, y=X37e))+
  #geom_errorbar(aes(ymin=kmean-ksem, ymax=kmean+ksem))+
  geom_line()+
  geom_point(aes(colour=dat12$treat), size=2)+
  theme_classic()+
  geom_text(aes(label=dat12$time), size=2)+
  ylim(0,5000)+
  xlab("")+
  ylab("Assay 37e in [ng/g]")

setEPS()
postscript("females_4days_indiviually_all.eps",width=10, height=7 ,  bg="NA")
multiplot(p1,p2,p3,p8,p9,p10,p11,pall, cols=2)

dev.off()

dat_all=read.csv("animals_ACTH.csv")
dat_all=subset(dat_all, sex=="f")

dat_all$date=as.POSIXlt(strptime(dat_all$date,format="%d.%m.%Y") )
dat_all$datetime=as.POSIXct(strptime(paste(dat_all$date, dat_all$time_smooth), format="%Y-%m-%d %H:%M"))

g=aggregate(dat_all[, "X37e"], list(dat_all$datetime), mean)

u=aggregate(dat_all[, "X37e"], list(dat_all$datetime), sem)

names(g)<- c("datetime", "kmean")
names(u)<- c("datetime", "ksem")

k=merge(g,u, by="datetime")

dat_all2=dat[,c("datetime", "treat", "treatment", "time_smooth")]
k=merge(k, dat_all2, by="datetime")

pall=ggplot(k, aes(x=datetime, y=kmean))+
  geom_errorbar(aes(ymin=kmean-ksem, ymax=kmean+ksem))+
  geom_line()+
  geom_point(aes(colour=k$treat), size=2)+
  theme_classic()+
  geom_text(aes(label=k$time), size=2)+
  ylim(0,5000)+
  xlab("")+
  ylab("Assay 37e in [ng/g]")

#####summarySE#####
summarySE <- function(data=NULL, measurevar, groupvars=NULL, na.rm=FALSE,
                      conf.interval=.95, .drop=TRUE) {
  library(plyr)
  
  # New version of length which can handle NA's: if na.rm==T, don't count them
  length2 <- function (x, na.rm=FALSE) {
    if (na.rm) sum(!is.na(x))
    else       length(x)
  }
  
  # This does the summary. For each group's data frame, return a vector with
  # N, mean, and sd
  datac <- ddply(data, groupvars, .drop=.drop,
                 .fun = function(xx, col) {
                   c(N    = length2(xx[[col]], na.rm=na.rm),
                     mean = mean   (xx[[col]], na.rm=na.rm),
                     sd   = sd     (xx[[col]], na.rm=na.rm)
                   )
                 },
                 measurevar
  )
  
  # Rename the "mean" column    
  datac <- rename(datac, c("mean" = measurevar))
  
  datac$se <- datac$sd / sqrt(datac$N)  # Calculate standard error of the mean
  
  # Confidence interval multiplier for standard error
  # Calculate t-statistic for confidence interval: 
  # e.g., if conf.interval is .95, use .975 (above/below), and use df=N-1
  ciMult <- qt(conf.interval/2 + .5, datac$N-1)
  datac$ci <- datac$se * ciMult
  
  return(datac)
}





#############parallel plots###############################
#dat=subset(dat, datetime<"2015-07-06 17:00:00 CEST")



dat=subset(dat, treat2=="i" | treat2=="h")
#g=aggregate(dat[, "X37e"], list(dat$hours), mean)

#u=aggregate(dat[, "X37e"], list(dat$hours), sem)

#names(g)<- c("hours", "kmean")
#names(u)<- c("hours", "ksem")

#k=merge(g,u, by="hours")

#dat2=dat[,c("hours", "treat", "treatment", "time_smooth")]
#k=merge(k, dat2, by="hours")






tgc <- summarySE(dat, measurevar="X37e", groupvars=c("treat","hours"))

tgc=na.omit(tgc)

pd=position_dodge(0.9)

cbPalette<-c("#000000", "#009E73", "#D55E00")

setEPS()


ggplot(tgc, aes(x=hours, y=X37e, group=treat, colour=treat))+
  geom_errorbar(aes(ymin=X37e-se, ymax=X37e+se), position=pd, colour="black", width=0.5)+
 geom_line(position=pd)+
  geom_point(aes(group=treat),position=pd , size=5, shape=20)+
   theme_classic()+
  theme(legend.position=c(.9,.9))+
  scale_colour_manual(values=cbPalette, name="Treatment", breaks=c("b", "h", "i"),
                      labels=c("Baseline", "Handling", "Injection"))

ggsave("females_parallel.eps", width=10, height=7)
  #geom_text(aes(label=k$time))+
 # xlab("")+
  #ylab("Assay 37e in [ng/g]")


m<-lm(dat$X37e~dat$hours+dat$treat)
 
Anova(m, type=3)

