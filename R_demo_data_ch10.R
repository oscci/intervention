#Demonstration of what data look like
#D.V.M. Bishop, started 17th Dec 2017

library(doBy) #package to make it easy to get tables of means etc
library(beeswarm) #nice plotting package
library(tidyverse)
options(scipen=999)#turn off scientific notation
set.seed(12) #to ensure same values generated on each run - change to any other number for different results

#histogram for zscores
pngname<-'zhist.png'
png(pngname,width=500,height=250)
myz<-rnorm(100000,0,1)
hist(myz)
dev.off()

pngname<-'samplingvar_demo.png'
png(pngname,width=500,height=500)
par(mfrow=c(3,3))
for (i in 1:9){
mydata<-sample(myz,20)
mydata[11:20]<-mydata[11:20]+1
mygroup<-c(rep('A',10),rep('B',10))
beeswarm(mydata~mygroup,ylim=c(-3,3.5),xlab='',ylab='Z-score',main=i,cex.axis=.8)
}
dev.off()

mydata<-data.frame(matrix(NA,nrow=20,ncol=2))
colnames(mydata)<-colnames<-c('group','valuea')
mydata$group<-c(rep(0,10),c(rep(1,10)))
mydata$valuea<-c(rep(3,10),c(rep(4,10)))
badd<-c(-2,-1,-1,0,0,0,0,1,1,2)
cadd<-c(-3,-2,-1,-1,0,0,1,1,2,3)
mydata$group<-as.factor(mydata$group)
levels(mydata$group)<-c('Control','Trained')
#set up to save plots
pngname<-'beeswarm_2grp_intvardemo.png' #needs a lot of fiddly formatting to get decent resolution
png(pngname,width=1100,height=400)
#png(pngname,res=300)
par(mfrow=c(1,3))
par(mar=c(5,6,5,2))
beeswarm(mydata$valuea~mydata$group ,xlab='',ylab='Vocabulary gain',
         col='red',pch=20,cex.axis=2.2,cex.lab=2.5,ylim=c(0,8),main='A',cex.main=3,spacing=.5,cex=2)

mydata$valueb<-mydata$valuea+rep(badd,2)
beeswarm(mydata$valueb~mydata$group ,xlab='',ylab='',
         col='red',pch=16,cex.axis=2.2,cex.lab=2.5,ylim=c(0,8),main='B',cex.main=3,spacing=.5,cex=2)
mydata$valuec<-mydata$valuea+rep(cadd,2)
beeswarm(mydata$valuec~mydata$group ,xlab='',ylab='',
         col='red',pch=16,cex.axis=2.2,cex.lab=2.5,ylim=c(0,8),main='C',cex.main=3,spacing=.5,cex=2)
dev.off()
  
#Now generate 9 plots either with no effect or effect as in valuec.
set.seed(4)
myseq<-c(1,1,0,1,0,0,0,1,1)
mypop<-rnorm(100000,2,1)
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

#Sample size demo: Generate 5 plots with effect size 1 and N = 10 per group
#and 5 with same effect size and N = 80
set.seed(17)
effsize=1
mypop<-rnorm(100000,2,1)
#set up to save plots
pngname<-'beeswarm_sizedemo.png'
png(pngname,width=600,height=400)
par(mfrow=c(2,5))

myn<-c(20,160)
for (j in 1:2){
  rangea<-1:(myn[j]/2)
  rangeb<-(1+myn[j]/2):myn[j]
  mygroup<-c(rep('A',myn[j]/2),rep('B',myn[j]/2))
for (i in 1:5){
  myylab<-'z-score'
  if (i>1){myylab<-''}
  temp<-sample(mypop,myn[j])
  temp[rangeb]<-temp[rangeb]+effsize
  beeswarm(temp~mygroup ,xlab='',ylab=myylab,
           col='red',pch=16,cex.axis=1.4,cex.lab=1.2,ylim=c(0,5),spacing=.5)
  segments(x0 = 0.7, x1 = 1.3,
           y0 = mean(temp[rangea]) ,
           lty = 1, lwd = 2,col='black')
  segments(x0 = 1.7, x1 = 2.3,
           y0 = mean(temp[rangeb]), 
           lty = 1, lwd = 2,col='black')
}
}
dev.off()
#-----------------------------------------------------------------------
#Pvalue demo: simulated data with no effect for 10000 runs
#-----------------------------------------------------------------------
#Plots histograms for effect size with N = 10 and N = 80
#NB bin width needs careful crafting to ensure both plots are similar 
set.seed(17)
effsize=0
mypop<-rnorm(100000,2,1) #population data
#set up to save plots
plotsave<-1
if(plotsave==1){
pngname<-'hist_effsize_freq.png'
png(pngname,width=650,height=300)
}
par(mfrow=c(1,2))

myn<-c(20,160)

for (j in 1:2){
  mybreaks<-seq(-1.7,1.7,.15) #have to handcraft to get both histos similar scale
  htextloc<-.7 #horiz location of 'Percent' label
  if (j==2)
  {mybreaks<-seq(-.5,.5,.05)
    htextloc<-.2}
  d.all<-NA
  rangea<-1:(myn[j]/2)
  rangeb<-(1+myn[j]/2):myn[j]
  mygroup<-c(rep('A',myn[j]/2),rep('B',myn[j]/2))
  for (i in 1:10000){
    temp<-sample(mypop,myn[j])
    myvar<-c(var(temp[rangea]),var(temp[rangeb]))
    poolsd<-sqrt(mean(myvar)) #formula for equal N per group
    d.all<-c(d.all, (mean(temp[rangea])-mean(temp[rangeb]))/poolsd)

  }
  myprobs<-c( .95, .99,.999)
  myprobtext<-c('5%','1%','0.1%')

  d.all<-d.all[2:length(d.all)]
  d.all<-d.all[d.all<max(mybreaks)] #remove out of range values so hist will plot
  d.all<-d.all[d.all> min(mybreaks)]
  hist(d.all,breaks=mybreaks,main=paste0('N per group = ',myn[j]/2),
       xlab='Effect size, d',cex.axis=.8,las=1)
  myquants<-quantile(d.all,  probs = myprobs)
  myquants<-round(myquants,2)
  text(htextloc,1300,'    Percent')
  for (q in 1:length(myquants)){
    text(myquants[q],1200,myprobtext[q],cex=.8)
    abline(v=myquants[q],col='red',lty=3)
  }
  
}
if(plotsave==1) {dev.off()}

#-------------------------------------------------------------------------
