MI<-function(xx,yy){
library("rJava")
library(doParallel)
.jinit()
.jaddClassPath("/gpfs/ahrenshsmfs/user/pothapak/COSMO_ASSEMBLY/LIANG/RESULTS/infodynamics-dist-1/infodynamics.jar")
teCalc<-.jnew("infodynamics/measures/continuous/gaussian/MutualInfoCalculatorMultiVariateGaussian")
#.jcall(teCalc,"V","initialise",1L)
.jcall(teCalc,"V","setObservations",xx,yy)
result<-.jcall(teCalc,"D","computeAverageLocalOfObservations")
########### START BOOT STARP  #####
#cl<-5
#registerDoParallel(cl)
CMID<-numeric();
for (i in 1:100){
resample<-sample(length(xx)-1,length(xx),replace=TRUE)
x<-xx[resample];y<-yy[resample];
.jcall(teCalc,"V","setObservations",x,y)
CMID[i]<-.jcall(teCalc,"D","computeAverageLocalOfObservations")
#return(rekult)
}
if(result<0){result=0}
if(result-(1.96*sd(CMID)/sqrt(50))<0){result=0}
return(result)}
