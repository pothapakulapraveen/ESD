MISD<-function(xx,yy){
library("rJava")
.jinit()
#CMID<-numeric();
#for (i in 1:nboot){
resample<-sample(length(xx)-1,length(xx),replace=TRUE)
resample2<-resample+1
x<-xx[resample];y<-yy[resample2];
.jaddClassPath("/home/pp28jofe/infodynamics/infodynamics.jar")
#teCalc<-.jnew("infodynamics/measures/continuous/gaussian/MutualInfoCalculatorMultiVariateGaussian")
teCalc<-.jnew("infodynamics/measures/continuous/kraskov/MutualInfoCalculatorMultiVariateKraskov1")
#.jcall(teCalc,"V","initialise",1L,1L)
.jcall(teCalc,"V","setProperty", "k", "40")
.jcall(teCalc,"V","setObservations",x,y)
result<-.jcall(teCalc,"D","computeAverageLocalOfObservations")
#CMID[i]<-result}
#sd_CMID<-sd(CMID)
return(result)}
##### The result is in returned in NATS #####
