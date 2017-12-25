#Demonstration of p-values
#D.V.M. Bishop, started 17th Dec 2017

#Simulate repeated sampling from population with null effect, plot distribution of obtained effect sizes
#NB this script is not an efficient way to do this - designed so each step makes sense for beginners


options(scipen=999)#turn off scientific notation
set.seed(12) #to ensure same values generated on each run - change to any other number for different results
#-----------------------------------------------------------------------
#Pvalue demo: simulated data with no effect for 10000 runs
#-----------------------------------------------------------------------
#Plots histograms for effect size with N = 10 and N = 80
#NB bin width needs careful crafting to ensure both plots are similar 
#See histo help: this explains that if you try to set binwidth it is treated as 'suggestion'
set.seed(17)
mypop<-rnorm(100000,2,1) #population data
#set up to save plots
plotsave<-1 #if you just want plots in plot window, set to zero
if(plotsave==1){
pngname<-'hist_effsize_freq.png'
png(pngname,width=650,height=300)
}
par(mfrow=c(1,2)) #plot in one row and 2 columns

myn<-c(20,160) #two possible values of N (both groups combined)

for (j in 1:2){ #run through twice: once with 10 per group and once with 80 per group
  mybreaks<-seq(-1.7,1.7,.15) #have to handcraft to get both histos similar scale
  htextloc<-.7 #horiz location of 'Percent' label
  if (j==2)
  {mybreaks<-seq(-.5,.5,.05)
    htextloc<-.2}
  d.all<-NA #variable to save all the effect sizes from this run
  rangea<-1:(myn[j]/2)
  rangeb<-(1+myn[j]/2):myn[j]
  mygroup<-c(rep('A',myn[j]/2),rep('B',myn[j]/2))
  for (i in 1:10000){
    temp<-sample(mypop,myn[j]) 
    myvar<-c(var(temp[rangea]),var(temp[rangeb]))
    poolsd<-sqrt(mean(myvar)) #formula for pooled SD if there is equal N per group
    d.all<-c(d.all, (mean(temp[rangea])-mean(temp[rangeb]))/poolsd) #compute d and stick on end of list

  }
  myprobs<-c( .95, .99,.999)
  myprobtext<-c('5%','1%','0.1%')

  d.all<-d.all[2:length(d.all)] #remove the initial NA value
  d.all<-d.all[d.all<max(mybreaks)] #remove out of range values so hist will plot
  d.all<-d.all[d.all> min(mybreaks)]
  hist(d.all,breaks=mybreaks,main=paste0('N per group = ',myn[j]/2),
       xlab='Effect size, d',cex.axis=.8,las=1) #cex.axis determines size of axis text, las rotates text
  myquants<-quantile(d.all,  probs = myprobs)
  myquants<-round(myquants,2)
  text(htextloc,1300,'    Percent') #add text to the graph
  for (q in 1:length(myquants)){
    text(myquants[q],1200,myprobtext[q],cex=.8) #add text to the graph
    abline(v=myquants[q],col='red',lty=3) #draw a line for each cutoff
  }
  
}
if(plotsave==1) {dev.off()} #stop writing to png

#-------------------------------------------------------------------------
