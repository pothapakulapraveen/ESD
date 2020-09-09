MISD<-function(x,y){
library("rJava")
.jinit()
#.jaddClassPath("/gpfs/ahrenshsmfs/user/pothapak/COSMO_ASSEMBLY/LIANG/RESULTS/infodynamics-dist-1/infodynamics.jar")
.jaddClassPath("/home/pp28jofe/infodynamics/infodynamics.jar")
teCalc<-.jnew("infodynamics/measures/continuous/gaussian/MutualInfoCalculatorMultiVariateGaussian")
#.jcall(teCalc,"V","initialise",1L)
.jcall(teCalc,"V","setObservations",x,y)
nullDist <- .jcall(teCalc,"Linfodynamics/utils/EmpiricalMeasurementDistribution;","computeSignificance", 1000L)
result<-.jcall(nullDist,"D","getStdOfDistribution")
#if(result<0){result=0}
return(result)}
