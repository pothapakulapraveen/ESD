CMI<-function(x,y,z){
library("rJava")
.jinit()
#.jaddClassPath("/gpfs/ahrenshsmfs/user/pothapak/COSMO_ASSEMBLY/LIANG/RESULTS/infodynamics-dist-1/infodynamics.jar")
.jaddClassPath("/home/pp28jofe/infodynamics/infodynamics.jar")
#teCalc<-.jnew("infodynamics/measures/continuous/gaussian/ConditionalMutualInfoCalculatorMultiVariateGaussian")
teCalc<-.jnew("infodynamics/measures/continuous/kraskov/ConditionalMutualInfoCalculatorMultiVariateKraskov1")
x= cbind(x,x);y=cbind(y,y);z=cbind(z,z);
.jcall(teCalc,"V","initialise",2L,2L,2L)
.jcall(teCalc,"V","setProperty", "k", "8")
.jcall(teCalc,"V","setObservations", .jarray(x, "[D",dispatch=TRUE),
         .jarray(y, "[D",dispatch=TRUE),
         .jarray(z, "[D",dispatch=TRUE))
result<-.jcall(teCalc,"D","computeAverageLocalOfObservations")
#if(result<0){result=0}
#if(result==Inf){result=0}
return(result)}
##### The result is in returned in NATS #####
