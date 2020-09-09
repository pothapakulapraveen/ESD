MI<-function(x,y){
library("rJava")
.jinit()
#.jaddClassPath("/gpfs/ahrenshsmfs/user/pothapak/COSMO_ASSEMBLY/LIANG/RESULTS/infodynamics-dist-1/infodynamics.jar")
.jaddClassPath("/home/pp28jofe/infodynamics/infodynamics.jar")
#teCalc<-.jnew("infodynamics/measures/continuous/gaussian/MutualInfoCalculatorMultiVariateGaussian")
teCalc<-.jnew("infodynamics/measures/continuous/kraskov/MutualInfoCalculatorMultiVariateKraskov1")
#.jcall(teCalc,"V","initialise",1L,1L)
.jcall(teCalc,"V","setProperty", "k", "40")
.jcall(teCalc,"V","setObservations",x,y)
result<-.jcall(teCalc,"D","computeAverageLocalOfObservations")
#if(result<0){result=0}
return(result)}
##### The result is in returned in NATS #####
