#Demonstration of how F-test reflects group effect
#D.V.M. Bishop, started 20th Nov 2017

library(doBy) #package to make it easy to get tables of means etc
library(beeswarm) #nice plotting package
#if you haven't previously installed doBy and beeswarm, do that first
#via Tools|Install packages. You only need do this once.
options(scipen=999)#turn off scientific notation
set.seed(42) #to ensure same values generated on each run

#create a set of values to simulate for true group differences in SD units
truediff<-c(.1,.75,1.25) #here we have 3 values; c stands for cocatenate so we get 3 values
#in a vector; we can refer to them as truediff[1], truediff[2] and truediff[3]
truelabel<-c('A','B','C') #to label the plots for each run

nsims<-length(truediff)
bigsummary<-data.frame(matrix(NA,nrow=nsims,ncol=8))
colnames(bigsummary)<-c('Simulation','N.per.gp','Gp1.mean','Gp1.sd','Gp2.mean','Gp2.sd','F','p')
#set up to save plots
pdfname1<-'beeswarm_fratio.pdf'
pdf(pdfname1,width=10,height=4)
par(mfrow=c(1,nsims)) #one row and nsims columns for plot output

myn<-20 #N per group
for (i in 1:nsims){
  thisdiff<-truediff[i]
mynum<-rnorm(myn*2) #generate a set of random numbers
mynum[1:myn]<-mynum[1:myn]+thisdiff  #add truediff value to group 1 scores

#scale so that variances differ rather than means
mynum<-mynum/(i) #just divide by 1, 2 or 3

mygp<-c(rep(2,myn),rep(1,myn))#generate group IDs for equal sized groups 1 and 2
mydat<-data.frame(cbind(mygp,mynum)) #stick it all together in a dataframe
#mydat #uncomment this to look at the dataframe if you like

myfit <- aov(mynum ~ mygp, data=mydat) #run Anova to test if groups differ
if(i==1){myfit1<-myfit}
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

#add results to summary table
bigsummary[i,1]<-i
bigsummary[i,2]<-myn
bigsummary[i,3]<-mysum[1,2]
bigsummary[i,4]<-mysum[1,3]
bigsummary[i,5]<-mysum[2,2]
bigsummary[i,6]<-mysum[2,3]
bigsummary[i,7]<-myF
bigsummary[i,8]<-pf(myF,1,(myn*2-2),lower.tail=F)

#Show the plot with the stats
myylab<-''
if (i==1){myylab<-'Outcome'}
mydat$mygp<-as.factor(mydat$mygp)
levels(mydat$mygp)<-c('Control','Intervention')
bxplot(mynum~mygp , data = mydat,  xlab=truelabel[i],ylab=myylab,
       probs =0.5, col = 'black',lty=2,ylim=c(-3,3))
beeswarm(mynum~mygp , data = mydat,
         col='red',pch=16,add=TRUE)
Fbit<-paste0('F = ',round(myF,2))
text(1.3,2.5,Fbit)


}
bigsummary
dev.off()
