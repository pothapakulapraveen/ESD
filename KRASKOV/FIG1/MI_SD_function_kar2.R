MISD<-function(x,y){
library("rJava")
.jinit()
.jaddClassPath("/home/pp28jofe/infodynamics/infodynamics.jar")
#teCalc<-.jnew("infodynamics/measures/continuous/gaussian/MutualInfoCalculatorMultiVariateGaussian")
teCalc<-.jnew("infodynamics/measures/continuous/kraskov/MutualInfoCalculatorMultiVariateKraskov1")
#.jcall(teCalc,"V","initialise",1L,1L)
.jcall(teCalc,"V","setProperty", "k", "40")
.jcall(teCalc,"V","setObservations",x,y)
nullDist <- .jcall(teCalc,"Linfodynamics/utils/EmpiricalMeasurementDistribution;","computeSignificance", 50L)
result<-.jcall(nullDist,"D","getStdOfDistribution")
return(result)}
##### The result is in returned in NATS #####
