#Demonstration of how sample size affects test statistics
#D.V.M. Bishop, started 1st Dec 2017
#Thanks to JK Rideau for hints on scripting 


library(doBy) #package to make it easy to get tables of means etc
library(beeswarm) #nice plotting package
library(tidyverse)
#if you haven't previously installed doBy and beeswarm, do that first
#via Tools|Install packages. You only need do this once.
options(scipen=999)#turn off scientific notation
#set.seed(42) #to ensure same values generated on each run - change to any other number for different results

#create a set of values to simulate for true group differences in SD units
trueeff<- .5 #true effect size
alln<-c(20,60,120) #create a vector of sample sizes to compare
truelabel<-c('A','B','C') #to label the plots for each run

nsims<-length(myn)

#create a data frame with simulated data for all 3 expts 
alldat <- data.frame(expt = c(rep(1,alln[1]),rep (2,alln[2]) ,rep(3,alln[3])),
                     cond = c(rep('I',alln[1]/2),rep('C',alln[1]/2),
                              rep('I',alln[2]/2),rep('C',alln[2]/2),
                              rep('I',alln[3]/2),rep('C',alln[3]/2)),
                     myscore = rnorm(sum(alln)))

#add the specified effect size to scores of those in intervention condition
w<-which(alldat$cond=='I') #find rows for intervention condition
alldat$myscore[w]<-alldat$myscore[w]+trueeff #add effect size to those rows
alldat$cond<-as.factor(alldat$cond)
levels(alldat$cond)<-c('Control','Intervention')


#set up to save plots
pngname<-'beeswarm_3gp.png'
png(pngname,width=600,height=250)
par(mfrow=c(1,3)) #one row and 3 columns for plot output

mysum<-summaryBy(myscore ~ expt+cond, data=alldat,
                 FUN = function(x) { c(m = mean(x), s = sd(x)) } )

for (i in 1:3){
  #select data for expt i
  mydat<-filter(alldat,expt==i)
  myfit <- aov(mydat$myscore ~ mydat$cond) #run Anova to test if conditions differ
  temp<-unlist(summary(myfit))
  #add results to summary table
  mysum$F[i*2]<-temp[7]
  mysum$p[i*2]<-temp[9]

#Show the plot with the stats
  myylab<-''
  if (i==1){myylab<-'Outcome'}
  my.ylim<-c(-3,3) #y axis limits
  texty<-2.7 #vertical location of text placement for F value

  beeswarm(mydat$myscore~mydat$cond ,xlab='',ylab=myylab,
           col='red',pch=16,ylim=my.ylim,cex.axis=1.5,cex.lab=2)
  segments(x0 = 0.7, x1 = 1.3,
           y0 = mysum[(1+(i-1)*2), 3], 
           lty = 1, lwd = 2,col='black')
  segments(x0 = 1.7, x1 = 2.3,
           y0 = mysum[(2+(i-1)*2), 3], 
           lty = 1, lwd = 2,col='black')
  
  Fbit<-paste0('F = ',round(mysum[(2+(i-1)*2), 5],2))
  text(1.3,texty,Fbit,cex=1.5)
  
  
}
dev.off() #stop printing to png

