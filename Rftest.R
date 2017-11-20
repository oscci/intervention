#Demonstration of how F-test reflects group effect
library(doBy) #package to make it easy to get tables of means etc
library(beeswarm) #nice plotting package
#if you haven't previously installed doBy and beeswarm, do that first
#via Tools|Install packages. You only need do this once.

#create a set of values to simulate for true group differences in SD units
truediff<-c(.05,.75,2) #here we have 3 values; c stands for cocatenate so we get 3 values
#in a vector; we can refer to them as truediff[1], truediff[2] and truediff[3]
par(mfrow=c(1,3)) #one row and 3 columns for plot output

myn<-40 #N per group
for (i in 1:length(truediff)){
  thisdiff<-truediff[i]
mynum<-rnorm(myn*2) #generate a set of random numbers
mynum[1:myn]<-mynum[1:myn]+thisdiff  #add truediff value to group 1 scores
mygp<-c(rep(2,myn),rep(1,myn))#generate group IDs for equal sized groups 1 and 2
mydat<-data.frame(cbind(mygp,mynum)) #stick it all together in a dataframe
#mydat #uncomment this to look at the dataframe if you like

myfit <- aov(mynum ~ mygp, data=mydat) #run Anova to test if groups differ
#summary(myfit) #uncomment to see Anova table
# We are going to compute F-ratio in steps below, but this gives sanity check
# F-ratio should agree with the one in this summary

#To understand how F-test works, best to compute F-ratio by hand
#see https://www.stat.auckland.ac.nz/~wild/ChanceEnc/Ch10.byhand.pdf

#First we'll get means, sds and variances for each group
#Note: we don't need sds, but they are more familiar when reporting data.
#The variance is just the sd squared

mysum<-summaryBy(mynum ~ mygp, data = mydat,
          FUN = function(x) { c(m = mean(x), s = sd(x),v=var(x)) } )
colnames(mysum)<-c('group','mean','sd','var')
#mysum #uncomment this line to see means, sd and var by group
ssB<-var(mysum$mean)*myn #between subjects SS is variance of means x group size
ssW<-mean(mysum$var) #within subjects SS is average variance per group
myF<-ssB/ssW #F is ratio of the two
myF

#Show the plot with the stats
myylab<-''
if (i==1){myylab<-'Outcome'}
bxplot(mynum~mygp , data = mydat,  xlab='Group',ylab=myylab,
       probs =0.5, col = 'black',lty=1,ylim=c(-3,3))
beeswarm(mynum~mygp , data = mydat,
         col='red',pch=16,add=TRUE)



}

