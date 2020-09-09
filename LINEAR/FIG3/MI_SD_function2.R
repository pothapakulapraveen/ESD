MISD<-function(xx,yy,nboot){
library("rJava")
.jinit()
CMID<-numeric();
for (i in 1:nboot){
resample<-sample(length(xx)-1,length(xx),replace=TRUE)
resample2<-resample+1
x<-xx[resample];y<-yy[resample2];
#.jaddClassPath("/gpfs/ahrenshsmfs/user/pothapak/COSMO_ASSEMBLY/LIANG/RESULTS/infodynamics-dist-1/infodynamics.jar")
.jaddClassPath("/home/pp28jofe/infodynamics/infodynamics.jar")
teCalc<-.jnew("infodynamics/measures/continuous/gaussian/MutualInfoCalculatorMultiVariateGaussian")
#.jcall(teCalc,"V","initialise",1L)
.jcall(teCalc,"V","setObservations",x,y)
result<-.jcall(teCalc,"D","computeAverageLocalOfObservations")
#if(result<0){result=0}
CMID[i]<-result}
sd_CMID<-sd(CMID)
return(sd_CMID)}
