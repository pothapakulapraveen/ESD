rm(list=ls())
#library("Hmisc")
#source("TE_function_kar.R")
source("kernel_TE.R")
source("kernel_CMI.R")
source("kernel_MI.R")
#source("TE_mean.R")
#source("TE_sd.R")
#source("MI_function_kar.R")
#source("CMI_function_kar.R")
#load("linear1.RData")
load("../../henon.RData")
library(doParallel)
#cl<-makeCluster(detectCores())
cl<-5
registerDoParallel(cl)
################# LONG time series ######################
result4<-foreach(i = 1:21,.combine='rbind')%dopar%{
A<-CMI(x1[i,94999:99999],y1[i,95000:100000],y1[i,94999:99999],0.5)
B<-MI(x1[i,94999:99999],y1[i,95000:100000],0.5)
C<-MI(y1[i,94999:99999],y1[i,95000:100000],0.5)
#D<-TE(x1[i,95000:100000],y1[i,95000:100000],0.5)
return(c(A,B,C))}
################### 1000 time units ############
#result3<-foreach(i = 1:11,.combine='rbind')%dopar%{
#A<-TE(y[99000:100000],x[i,99000:100000])
#B<-MI(y[98999:99999],x[i,99000:100000])
#C<-MI(x[94999:99999],x[i,95000:100000])
#return(c(A,B,C))}
#########################    500 time units      ###########
#result2<-foreach(i = 1:11,.combine='rbind')%dopar%{
#A<-TE(y[99500:100000],x[i,99500:100000])
#B<-MI(y[99499:99999],x[i,99500:100000])
#C<-MI(x[94999:99999],x[i,95000:100000])
#return(c(A,B))}
#########################   200 time units       #############
#result1<-foreach(i = 1:11,.combine='rbind')%dopar%{
#A<-TE(y[99800:100000],x[i,99800:100000])
#B<-MI(y[99799:99999],x[i,99800:100000])
#return(c(A,B))}
####### 2 standard deviations ######
NS<-result4[,1]-result4[,2]
IXY<-result4[,1]+result4[,3]
############################################################
#cp<-seq(0,1,0.1)
cp<-seq(0,0.8,0.04)
plot(cp[2:15],IXY[2:15]*2,type="l",col="red",lwd=2.5,ylim=c(0,3),lty=1,pch=18,xlab="coupling coefficient",ylab="Information exchange (nats)",cex.lab=1.2,cex.axis=1.5,main="Kernel")
#lines(cp,result3[,1],type="l",col="red",lwd=2.5)
#lines(cp,result2[,1],type="l",col="red",lwd=2.5)
#lines(cp,result1[,1],type="l",col="red",lwd=2.5)
lines(cp[2:15],result4[2:15,2]*2,type="l",col="blue",lwd=2.5)
lines(cp[2:15],result4[2:15,3]*2,type="l",col="green",lwd=2.5)
lines(cp[2:15],NS[2:15]*2,type="l",col="black",lwd=2.5)
#lines(cp[2:15],result4[2:15,4],type="l",col="brown",lwd=2.5)
abline(h = 0)
#lines(cp,result2[,2],type="l",col="blue",lwd=2.5)
#lines(cp,result1[,2],type="l",col="blue",lwd=2.5)
lt<-c(1,2,3,4,5,1,2,3,4,5)
pc<-c(18,23,20,21,22,18,23,20,21,22)
colors<-c("red","red","red","red","red",rep("white",5),"blue","blue","blue","blue","blue")
#legend("top",legend=c("n=5000","n=3000","n=1000","n=500","n=200",rep(" ",5),"n=5000","n=3000","n=1000","n=500","n=200"),ncol=3,col=colors,pch=pc,bty="n",cex=1.4,horiz=F,lty=lt,lwd=c(rep(1.4,5),rep(0,5),rep(1.4,5)),,text.width=c(0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.2))
legend("topright",legend=c("I(Yt;Xt-1|Yt-1)","I(Yt;Xt-1)","I(Yt;Xt-1)","Net Synergy"),col=c("red","blue","green","black"),lty=1,lwd=1.4,bty="n",cex=1.4)
