# Plots for blogpost: http://deevybee.blogspot.com.es/2017/12/using-simulations-to-understand.html
# D.V.M. Bishop, started 17th Dec 2017

library(beeswarm) #nice plotting package - need to install this if not already installed
options(scipen=999)#turn off scientific notation
set.seed(12) #to ensure same values generated on each run - change to any other number for different results
#seeds here have been set by trial and error to generate plots to demonstrate specific points

#------------------------------------------------------------------
#Fig 1: Fictional data for 2 groups in 3 studies
#------------------------------------------------------------------

mydata<-data.frame(matrix(NA,nrow=20,ncol=2))
colnames(mydata)<-colnames<-c('group','valuea')
mydata$group<-c(rep(0,10),c(rep(1,10)))
mydata$valuea<-c(rep(3,10),c(rep(4,10))) #unreal data - group A is all 3, and group B is all 4
badd<-c(-2,-1,-1,0,0,0,0,1,1,2) #still unreal, but with some variation
cadd<-c(-3,-2,-1,-1,0,0,1,1,2,3) #still unreal, but with more variation
mydata$group<-as.factor(mydata$group)
levels(mydata$group)<-c('A','B')
#set up to save plots
if (saveplot==1){ #set saveplot to zero if you want to see in plot window
  pngname<-'Fig3_1_nonoise_midnoise_bignoise.png' 
  png(pngname,width=3300,height=1200,res=300)#needs a lot of fiddly formatting to get decent resolution
}
par(mfrow=c(1,3)) #print in 1 row and 3 columns
par(mar=c(5,6,5,2)) #fiddle with margins
beeswarm(mydata$valuea~mydata$group ,xlab='',ylab='Vocabulary gain',
         col='red',pch=20,cex.axis=2.2,cex.lab=2.5,ylim=c(0,8),main='1',cex.main=3,spacing=.5,cex=2)

mydata$valueb<-mydata$valuea+rep(badd,2)
beeswarm(mydata$valueb~mydata$group ,xlab='',ylab='',
         col='red',pch=16,cex.axis=2.2,cex.lab=2.5,ylim=c(0,8),main='2',cex.main=3,spacing=.5,cex=2)
mydata$valuec<-mydata$valuea+rep(cadd,2)
beeswarm(mydata$valuec~mydata$group ,xlab='',ylab='',
         col='red',pch=16,cex.axis=2.2,cex.lab=2.5,ylim=c(0,8),main='3',cex.main=3,spacing=.5,cex=2)
if(saveplot==1)
{
  dev.off() #stop sending output to png
}

#------------------------------------------------------------------
#Fig 2: histogram for zscores
#------------------------------------------------------------------
saveplot<-1 #set to zero if you want to see plots in plot window, and set to 1 to save as png file
if(saveplot==1)
{
  pngname<-'Fig3_2_zhist.png' 
  png(pngname,width=500,height=250)
}
myz<-rnorm(100000,0,1) #create 100,000 zscores (mean 0 and SD 1)
hist(myz) #show results in histogram
if(saveplot==1)
{
  dev.off() #stop sending output to png
}

#------------------------------------------------------------------
#Fig 3: 9 beeswarm type plots for 2-groups with effect size=1
#------------------------------------------------------------------
alldat<-data.frame(matrix(NA, nrow=20, ncol=2))
colnames(alldat)<-c('mygroup','mydata')
alldat$mygroup<-c(rep('A',10),rep('B',10))
alldat$mygroup<-as.factor(alldat$mygroup)
saveplot=1
if(saveplot==1){
  pngname<-'Fig3_3_samplingvar_demo.png'
  png(pngname,width=500,height=500)
}
par(mfrow=c(3,3))
for (i in 1:9){
  alldat$mydata<-sample(myz,20) #select 20 values from population at random
  alldat$mydata[11:20]<-alldat$mydata[11:20]+1
  
  beeswarm(alldat$mydata~alldat$mygroup,ylim=c(-3,3.5),xlab='',ylab='Z-score',main=i,
           cex.lab=1.5,cex.axis=1.5,cex.main=2,las=1) #las=1 gives horiz labels
}
if(saveplot==1)
{dev.off()}
#----------------------------------------------------------------------------
#Figure 4: Mix of real and null effects, N = 10 per group
#----------------------------------------------------------------------------

set.seed(400)
effsize=.6
myseq<-c(1,1,0,1,0,0,0,1,1) #predetermine which runs have true effect
#set up to save plots
if(saveplot==1){
  pngname<-'Fig3_4_run9_vardemo.png'
  png(pngname,width=500,height=500)
}
par(mfrow=c(3,3))

for (i in 1:9){
  
  alldat[,(i+2)]<-sample(myz,20)
  alldat[11:20,(i+2)]<-alldat[11:20,(i+2)]+myseq[i]*effsize
  beeswarm(alldat[,(i+2)]~alldat$mygroup ,xlab='',ylab='Z-score',
           cex.axis=1.5,cex.lab=1.5,ylim=c(-3,5),main=i,cex.main=2,spacing=.5,cex=2,las=1)
}
if(saveplot==1)
{dev.off()}
#----------------------------------------------------------------------------
#Sample size demo: 
# Generate 5 plots with effect size .6 and N = 10 per group
# and 5 plots with same effect size and N = 80 per group
#----------------------------------------------------------------------------

set.seed(13)
effsize=.6
mypop<-rnorm(100000,0,1)
#set up to save plots
if(saveplot==1){
  pngname<-'Fig3_5_beeswarm_sizedemo.png'
  png(pngname,width=600,height=400)
}
par(mfrow=c(2,5)) #plot in 2 rows and 5 columns

myn<-c(20,160) #total sample size for the two conditions
for (j in 1:2){
  rangea<-1:(myn[j]/2)
  rangeb<-(1+myn[j]/2):myn[j]
  mygroup<-c(rep('A',myn[j]/2),rep('B',myn[j]/2))
  for (i in 1:5){
    myylab<-'z-score'
    if (i>1){myylab<-''}
    temp<-sample(mypop,myn[j]) #sample from the whole population
    temp[rangeb]<-temp[rangeb]+effsize
    beeswarm(temp~mygroup ,xlab='',ylab=myylab,
             col='red',pch=16,
             cex.axis=1.5,cex.lab=1.5,ylim=c(-3,5),main=i,cex.main=2,spacing=.5,las=1)
    #show the mean for each group to make visual comparison easier
    segments(x0 = 0.7, x1 = 1.3,
             y0 = mean(temp[rangea]) ,
             lty = 1, lwd = 2,col='black') 
    segments(x0 = 1.7, x1 = 2.3,
             y0 = mean(temp[rangeb]), 
             lty = 1, lwd = 2,col='black')
  }
}
if(saveplot==1){
  dev.off()
}
