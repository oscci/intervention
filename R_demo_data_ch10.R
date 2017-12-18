#Demonstration of what data look like
#D.V.M. Bishop, started 17th Dec 2017

library(doBy) #package to make it easy to get tables of means etc
library(beeswarm) #nice plotting package
library(tidyverse)
options(scipen=999)#turn off scientific notation
set.seed(2) #to ensure same values generated on each run - change to any other number for different results

mydata<-data.frame(matrix(NA,nrow=20,ncol=2))
colnames(mydata)<-colnames<-c('group','valuea')
mydata$group<-c(rep(0,10),c(rep(1,10)))
mydata$valuea<-c(rep(1,10),c(rep(2,10)))
mydata$group<-as.factor(mydata$group)
levels(mydata$group)<-c('Control','Trained')
#set up to save plots
pngname<-'beeswarm_2grp_vardemo.png'
png(pngname,width=500,height=250)
par(mfrow=c(1,3))
beeswarm(mydata$valuea~mydata$group ,xlab='',ylab='Vocabulary gain',
         col='red',pch=16,cex.axis=1.4,cex.lab=1.2,ylim=c(0,3),main='A')

mydata$valueb<-mydata$valuea+rnorm(20,0,.2)
beeswarm(mydata$valueb~mydata$group ,xlab='',ylab='',
         col='red',pch=16,cex.axis=1.4,cex.lab=1.2,ylim=c(0,3),main='B')
mydata$valuec<-mydata$valuea+rnorm(20,0,.6)
beeswarm(mydata$valuec~mydata$group ,xlab='',ylab='',
         col='red',pch=16,cex.axis=1.4,cex.lab=1.2,ylim=c(0,3),main='C')


dev.off()
  
#Now generate 9 plots either with no effect or effect as in valuec.
set.seed(4)
myseq<-c(1,1,0,1,0,0,0,1,1)
mypop<-rnorm(10000,2,1)
#set up to save plots
pngname<-'beeswarm_9run_vardemo.png'
png(pngname,width=500,height=500)
par(mfrow=c(3,3))

for (i in 1:9){
  
  mydata[,(i+4)]<-sample(mypop,20)
  mydata[11:20,(i+4)]<-mydata[11:20,(i+4)]+myseq[i]*1
  beeswarm(mydata[,(i+4)]~mydata$group ,xlab='',ylab='Vocabulary gain',
           col='red',pch=16,cex.axis=1.4,cex.lab=1.2,ylim=c(0,5),main=i,spacing=.5)

  
}
dev.off()
