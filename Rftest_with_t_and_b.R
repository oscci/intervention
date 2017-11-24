#Demonstration of how F and t are equivalent and relate to regression
#D.V.M. Bishop, started 23rd Nov 2017

library(doBy) #package to make it easy to get tables of means etc
library(beeswarm) #nice plotting package
#if you haven't previously installed doBy and beeswarm, do that first
#via Tools|Install packages. You only need do this once.
options(scipen=999)#turn off scientific notation
set.seed(5) #to ensure same values generated on each run - change to any other number for different results

#create a set of values to simulate for true group differences in SD units
truediff<-c(.1,.5,1.25) #here we have 3 values; c stands for cocatenate so we get 3 values
#in a vector; we can refer to them as truediff[1], truediff[2] and truediff[3]
truelabel<-c('A','B','C') #to label the plots for each run

nsims<-length(truediff)
bigsummary<-data.frame(matrix(NA,nrow=nsims*2,ncol=13))
colnames(bigsummary)<-c('Simulation','N.per.gp','Gp1.mean','Gp1.sd','Gp2.mean','Gp2.sd',
                        'SSB','SSW','F','p','b','b.se','t')

myn<-24 #N per group - you can experiment with changes to this

#set up data frame to hold simulated raw and z data for 3 expts
alldat<-data.frame(matrix(NA,nrow=2*myn,ncol=7)) #col 1 for group, col 2-4 for raw, col 5-7 for z
colnames(alldat)<-c('Group','A','B','C','z.A','z.B','z.C')

alldat$Group<-c(rep(1,myn),rep(2,myn))#generate group IDs for equal sized groups 1 and 2

alldat$A<-rnorm(myn*2) #generate a set of random numbers in first run through
range1<-1:myn
range2<-(myn+1):(2*myn)
#we now create data for 3 expts by adding truediff to group 2
#NB this script bases A, B and C on the same simulated data - they only vary in term of the addition of truediff
for (i in 2:4){
alldat[range1,i]<-alldat$A[range1]
alldat[range2,i]<-alldat$A[range2]+truediff[i-1]
}
#A, B, and C differ in terms of means
#We want to scale the data so that means are similar but variances differ
#This is just done by dividing by (i-1) - just for convenience; 
#This halves the values for B and divides by 3 for C.
for (i in 2:4){
alldat[,i]<-alldat[,i]/(i-1)

#scale to give arbitrary mean 40 and SD 10 for A
alldat[,i]<-40+(alldat[,i]*10)
alldat[,(i+3)]<-scale(alldat[,i])
}


for (j in 1:2){#do plots first for raw data, then for z-scores
  #set up to save plots as png files. These will be written to working directory
  pngname<-'beeswarm_fratio_raw.png'
  if (j==2){pngname<-'beeswarm_fratio_z.png'}
  png(pngname,width=600,height=250)
  par(mfrow=c(1,nsims)) #one row and nsims columns for plot output
  
View(alldat) #can comment this line out if you don't want to look at the dataframe 

for (i in 1:3){
  #select the column for this expt (i) and this j (1 = raw; 2 = z)
  myindex<-(i+1)+(j-1)*3
  mynum<-alldat[,myindex]
myfit <- aov(mynum ~ alldat$Group) #run Anova to test if groups differ
if(i==1){myfit1<-myfit}

#show this is equivalent to ttest; F is just t squared
t.test(mynum~alldat$Group,var.equal=TRUE)

#show this is equivalent to test of regression coefficient, b
bfit<-lm(formula = mynum ~ alldat$Group)

#summary(myfit) #uncomment to see Anova table

#Next section is taken from Rftest.r - computing F-ratio step by step
#First we'll get means, sds and variances for each group
#Note: we don't need sds, but they are more familiar when reporting data.
#The variance is just the sd squared
alldat$temp<-mynum #create temp column so can use summaryBy, which requires ref to data.frame
mysum<-summaryBy(temp ~ Group, data=alldat,
          FUN = function(x) { c(m = mean(x), s = sd(x),v=var(x)) } )
colnames(mysum)<-c('group','mean','sd','var')
#mysum #uncomment this line to see means, sd and var by group
ssB<-var(mysum$mean)*myn #between subjects SS is variance of means x group size
ssW<-mean(mysum$var) #within subjects SS is average variance per group
myF<-ssB/ssW #F is ratio of the two

#add results to summary table
index2<-myindex-1
bigsummary[index2,1]<-myindex-1
bigsummary[index2,2]<-myn
bigsummary[index2,3]<-mysum[1,2]
bigsummary[index2,4]<-mysum[1,3]
bigsummary[index2,5]<-mysum[2,2]
bigsummary[index2,6]<-mysum[2,3]
bigsummary[index2,7]<-ssB
bigsummary[index2,8]<-ssW
bigsummary[index2,9]<-myF
bigsummary[index2,10]<-pf(myF,1,(myn*2-2),lower.tail=F)
bigsummary[index2,11:13]<-summary(bfit)$coefficients[2,1:3]

#Show the plot with the stats
myylab<-''
if (i==1){myylab<-'Outcome'}
alldat$Group<-as.factor(alldat$Group)
levels(alldat$Group)<-c('Control','Intervention')
my.ylim<-c(0,70) #y axis limits
texty<-67 #vertical location of text placement for F value
#adjust these values for z-score plot
if (j==2){my.ylim<-c(-3,3)
   texty<-2.7}
beeswarm(mynum~alldat$Group ,xlab=truelabel[i],ylab=myylab,
         col='red',pch=16,ylim=my.ylim,cex.axis=1.5,cex.lab=2)
#This time we plot the slope rather than the means
segments(x0 = 1, x1 = 2,
         y0 = bigsummary[index2, 3], y1=bigsummary[index2, 5],
         lty = 1, lwd = 2,col='black')



Fbit<-paste0('F = ',round(myF,2))
text(1.3,texty,Fbit,cex=1.5)

}
dev.off() #stop printing to png
}
view(bigsummary)
#Use last run to illustrate how you can compute F from beta
beta<-bigsummary[6,11]
sx<-sd(c(rep(1,myn),rep(2,myn))) #sd for the group term (just 1s and 2s)
sy<-1 #NB because data are z-scores, the sd for Y is just 1
#we can compute the correlation coefficient, r as follows:
r <-beta*sx/sy #with beta we could omit sy as it is equal to 1
#F computed from correlation coefficient
thisF<-(r^2/((1-r^2)/(myn*2-2)))




