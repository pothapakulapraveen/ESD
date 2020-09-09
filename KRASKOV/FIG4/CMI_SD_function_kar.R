SDCMI<-function(x,y,z){
library("rJava")
.jinit()
#CMID<-numeric();
#for (i in 1:nboot){
resample<-sample((length(x)-1),length(x),replace=TRUE)
resample2<-resample+1
x<-x[resample];y<-y[resample2];z<-z[resample]
.jaddClassPath("/home/pp28jofe/infodynamics/infodynamics.jar")
teCalc<-.jnew("infodynamics/measures/continuous/kraskov/ConditionalMutualInfoCalculatorMultiVariateKraskov1")
x= cbind(x,x);y=cbind(y,y);z=cbind(z,z);
.jcall(teCalc,"V","initialise",2L,2L,2L)
.jcall(teCalc,"V","setProperty", "k", "8")
.jcall(teCalc,"V","setObservations", .jarray(x, "[D",dispatch=TRUE),
         .jarray(y, "[D",dispatch=TRUE),
         .jarray(z, "[D",dispatch=TRUE))
result<-.jcall(teCalc,"D","computeAverageLocalOfObservations")
#CMID[i]<-result
#}
return(result)}
