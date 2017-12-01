#Demonstration of how sample size affects test statistics
#D.V.M. Bishop, started 1st Dec 2017
#Thanks to JK Rideau for hints on scripting 


library(doBy) #package to make it easy to get tables of means etc
library(beeswarm) #nice plotting package
library(tidyverse)
#if you haven't previously installed doBy and beeswarm, do that first
#via Tools|Install packages. You only need do this once.
options(scipen=999)#turn off scientific notation
set.seed(1) #to ensure same values generated on each run - change to any other number for different results

#create a set of values to simulate for true group differences in SD units
trueeff<- .8 #true effect size
alln<-1000#create a vector of sample sizes to compare

#create a data frame with simulated data for all 3 expts 
alldat <- data.frame(cond = c(rep(1,alln/2),rep(2,alln/2)),
                     myscore = rnorm(sum(alln)))

#add the specified effect size to scores of those in intervention condition
range1<-1:(alln/2)
range2<-(1+alln/2):alln
alldat$myscore[range2]<-alldat$myscore[range2]+trueeff #add effect size to those rows
alldat$cond<-as.factor(alldat$cond)
levels(alldat$cond)<-c('Control','Intervention')
mean1<-mean(alldat$myscore[range1])
mean2<-mean(alldat$myscore[range2])

#set up to save plots
pngname<-'beeswarm_bigsample.png'
png(pngname,width=600,height=400)

#plot big dataset
beeswarm(alldat$myscore~alldat$cond ,xlab='',ylab=myylab,
         col='red',pch=16,ylim=my.ylim,cex.axis=1.5,cex.lab=2)
segments(x0 = 0.7, x1 = 1.3,
         y0 = mean1, 
         lty = 1, lwd = 2,col='black')
segments(x0 = 1.7, x1 = 2.3,
         y0 = mean2, 
         lty = 1, lwd = 2,col='black')
dev.off()

#now select subsamples
myn<-c(20,20,20,20,60,60,60,60)

pngname<-'beeswarm_subsamples.png'
png(pngname,width=700,height=400)
par(mfrow=c(2,4))
for (i in 1:length(myn)){
  myindex<-c(sample(range1,myn[i]/2),sample(range2,myn[i]/2)) #select myn at random for each condition
  mydat<-alldat[myindex,] #use these indices to create a subgroup

  myfit <- aov(mydat$myscore ~ mydat$cond) #run Anova to test if conditions differ
  temp<-unlist(summary(myfit))
  #add results to summary table
 myF<-temp[7]
 myp<-temp[9]
  rangea<-1:(myn[i]/2)
  rangeb<-((1+myn[i]/2)):myn[i]
  mean1<-mean(mydat$myscore[rangea])
  mean2<-mean(mydat$myscore[rangeb])

#Show the plot with the stats
  myylab<-''

  my.ylim<-c(-3,3) #y axis limits
  texty<-2.7 #vertical location of text placement for F value

  beeswarm(mydat$myscore~mydat$cond ,xlab=paste0('Total N = ',myn[i]),ylab=myylab,
           col='red',pch=16,ylim=my.ylim,cex.axis=1.3,cex.lab=1.5)
  segments(x0 = 0.7, x1 = 1.3,
           y0 = mean1, 
           lty = 1, lwd = 2,col='black')
  segments(x0 = 1.7, x1 = 2.3,
           y0 = mean2, 
           lty = 1, lwd = 2,col='black')
  
  Fbit<-paste0('F = ',round(myF,2))
  text(1.3,texty,Fbit,cex=1.5)
  
  
}
dev.off() #stop printing to png

