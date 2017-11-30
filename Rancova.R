#Demonstration of Ancova
#D.V.M. Bishop, started 29th Nov 2017

#see http://www.plantsciences.ucdavis.edu/agr205/lectures/2011_lectures/l13_ancova.pdf

library(doBy) #package to make it easy to get tables of means etc
library(beeswarm) #nice plotting package
library(Hmisc) #for correlation
options(scipen=999)#turn off scientific notation
#set.seed(42) #to ensure same values generated on each run - change to any other number for different results

require(MASS) #for multivariate random generation and parallel plot
require(psych) #for phi coefficient

#-------------------------------------------------------------------------------------
# For nsub subjects simulate nvar variables with specified correlation mycorrel
#-------------------------------------------------------------------------------------

nvar<-2  # baseline and outcome values to simulate for each subject; 
nsub<-16 #specify an even number: half will be group 1 and half group 2
mycorrel<-.85 #intercorrelation between vars in population; will be in mysigma matrix
mysigma<-matrix(c(rep(mycorrel,nvar*nvar)),nvar,nvar)#
diag(mysigma)<-rep(1,nvar)
mymu<-rep(0,nvar) #mean 
effsize<-.8 #can set this to whatever you like
mydata<- data.frame(mvrnorm (nsub, mymu, mysigma,nvar)) #create multivariate normal data array
colnames(mydata)<-c('baseline','outcome') #create names for variables
#check correlation
myr<-rcorr(mydata[,1:2])$r[2] #2nd value in cell gives correl between 2 variables
#NB this will not be the same as mycorrel, because correl is computed from a sample

range1<-1:(nsub/2)
range2<-(nsub/2)+range1
mydata$group[range1]<-1
mydata$group[range2]<-2
#now add effect size at outcome for group 2 only
mydata[range2,2]<-mydata[range2,2]+effsize
mydata[,1:2]<-10*mydata[,1:2]+50
#-------------------------------------------------------------------------------------
# First run Anova for outcome measure
#-------------------------------------------------------------------------------------
myfitaov <- aov(mydata$outcome ~ mydata$group)
summary(myfitaov)
#-------------------------------------------------------------------------------------
# Now add baseline as covariate
#-------------------------------------------------------------------------------------
myfitcov <- aov(mydata$outcome ~ mydata$baseline+mydata$group)
summary(myfitcov)

#-------------------------------------------------------------------------------------
# Use lm for regression analysis: confirm result is the same
#-------------------------------------------------------------------------------------
myfitlm <- lm(mydata$outcome ~ mydata$baseline+mydata$group)
summary(myfitlm)

#-------------------------------------------------------------------------------------
# Calculate difference score and compare on F-test
#-------------------------------------------------------------------------------------
mydata$diff<-mydata$outcome-mydata$baseline
myfitdiffaov <- aov(mydata$diff~mydata$group)
summary(myfitdiffaov)

#-------------------------------------------------------------------------------------
#Why does it differ? 
#Need to compare the difference scores and the residuals
myfitcovonly <- lm(mydata$outcome ~ mydata$baseline)
mydata$resid<-residuals(myfitcovonly)
mydata$fitted<-fitted.values(myfitcovonly)
plot(mydata$diff,mydata$resid)
plot(mydata$outcome,mydata$fitted)
plot(mydata$baseline,mydata$fitted)
myb<-myfitcovonly$coefficients[2]
meany<-mean(mydata$outcome)
meanx<-mean(mydata$baseline)
mydata$adjoutcome<-mydata$outcome-myb*(mydata$baseline-meanx)
plot(mydata$outcome,mydata$adjoutcome)


# Do parallel plot with parcoord function from MASS
my_colors<-c('blue','red')
# Plot parallel plots
c1<-which(colnames(mydata)=='baseline')
c2<-which(colnames(mydata)=='outcome')
c3<-which(colnames(mydata)=='fitted')
c4<-which(colnames(mydata)=='adjoutcome')

#par(mfrow=c(2,2)) #
#parcoord(mydata[,c(c1,c2)] , col= my_colors[mydata$group]  )
parcoord(mydata[,c(c1,c2,c4)] , col= my_colors[mydata$group] ,var.label=TRUE)
#parcoord(mydata[,c(c2,c4)] , col= my_colors[mydata$group]  )
#parcoord(mydata[,c(c1,c4)] , col= my_colors[mydata$group]  )

#Hmm, this doesn't work because it rescales everything.
#Need instead to plot a line for each subject
mylines<-mydata[,c(c1,c2,c4)]
# set up the plot
xrange=c(1,3)
yrange=c(20,80)
plot(xrange, yrange, type="n", xlab="",
     ylab="Score" ,xaxt = "n",)
colors <- my_colors


axis(1, at=1:3, labels=c('Baseline','Outcome','Adj.Outcome'))

# add lines
for (i in 1: nsub) {
  lines(c(1,2,3), mylines[i,],col=my_colors[mydata$group[i]])
} 
abline(v=2,col='grey')
abline(h=mean(mydata$baseline),lty=2)