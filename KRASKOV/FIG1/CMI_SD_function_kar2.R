SDCMI<-function(x,y,z){
library("rJava")
.jinit()
.jaddClassPath("/home/pp28jofe/infodynamics/infodynamics.jar")
teCalc<-.jnew("infodynamics/measures/continuous/kraskov/ConditionalMutualInfoCalculatorMultiVariateKraskov1")
x= cbind(x,x);y=cbind(y,y);z=cbind(z,z);
.jcall(teCalc,"V","initialise",2L,2L,2L)
.jcall(teCalc,"V","setProperty", "k", "40")
.jcall(teCalc,"V","setObservations", .jarray(x, "[D",dispatch=TRUE),
         .jarray(y, "[D",dispatch=TRUE),
         .jarray(z, "[D",dispatch=TRUE))
nullDist <-.jcall(teCalc,"Linfodynamics/utils/EmpiricalMeasurementDistribution;","computeSignificance", 10L)
result<-.jcall(nullDist,"D","getStdOfDistribution")
return(sd(result))}
