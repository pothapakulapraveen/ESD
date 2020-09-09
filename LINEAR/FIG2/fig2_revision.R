rm(list=ls())
library("Hmisc")
source("CMI_function_own.R")   ### Conditional Mutual Information Own!
source("MI_function.R")        #### Mutual Information JDIT 
source("MI_SD_function2.R")    ### Boot Strappig for MI using JDIT
source("CMI_SD_function.R")    #### Boot Strapping for CMI using own algorithim
load("../../linear2.RData")    #### Loading Linear Data
library(doParallel)            ###   Parallel Library for Parallel execution using multiple processor
cl<-5
registerDoParallel(cl)
################# Long time series ####################
result4<-foreach(i = 1:11,.combine='rbind')%dopar%{
A<-CMI(y[i,94999:99999],x[i,95000:100000],x[i,94999:99999])
B<-MI(y[i,94999:99999],x[i,95000:100000])
C<-MI(x[i,94999:99999],x[i,95000:100000])
D<-CMI(x[i,94999:99999],x[i,95000:100000],y[i,94999:99999])
return(c(A,B,C,D))}
################### 1000 time units ############
sd1<-foreach(i = 1:11,.combine='rbind')%dopar%{
A<-MISD(y[i,95000:100000],x[i,95000:100000],100)    ## I(y_n-1;x_n)
B<-MISD(x[i,95000:100000],x[i,95000:100000],100)   ## I(x_n-1; x_n)
return(c(A,B))}
#########################    500 time units      ###########
sd2<-foreach(i = 1:11,.combine='rbind')%dopar%{
A<-SDCMI(y[i,95000:100000],x[i,95000:100000],x[i,95000:100000],100)   ## I(y_n-1,x_n,/x_n-1)
D<-SDCMI(x[i,95000:100000],x[i,95000:100000],y[i,95000:100000],100)   ## I(x_n-1,x_n/y_n-1))
return(c(A,D))
}
####### Calculation of Information flow Metrics ######
NS<-result4[,1]-result4[,2]    #### Net Synergy #####
sdNS<-sqrt(sd2[1:9,1]^2 +  sd1[1:9,1]^2)   ####  Standard dviations of Net Synergy #####
######
IXY<-result4[,1]+result4[,3]              #####  IXY
sdixy<- sqrt(sd2[1:9,1]^2 +  sd1[1:9,2]^2) ##### Standard deviations 
IXX<-result4[,2] + result4[,4]
############################################################
cp<-seq(0,1,0.1)
plot(cp[1:9],IXY[1:9],type="l",col="red",lwd=2.5,ylim=c(0,0.4),lty=1,pch=18,xlab="coupling coefficient",ylab="Information exchange (nats)",cex.lab=1.2,cex.axis=1.5,main="Linear")
errbar(cp[1:9],IXY[1:9],IXY[1:9]+2*sdixy[1:9],IXY[1:9]-2*sdixy[1:9],type="l",col='red',lwd=2.5,lty=1,pch=23,ylim=c(0,0.4),xlab="coupling coefficient",ylab="Transfer entropy",cex.lab=1.2,cex.axis=1.5,errbar.col='red',add=TRUE)
##### MI SD ##
errbar(cp[1:9],result4[1:9,2],result4[1:9,2]+2*sd1[1:9,1],result4[1:9,2]-2*sd1[1:9,1],type="l",col='blue',lwd=2.5,lty=1,pch=23,ylim=c(0,0.4),xlab="coupling coefficient",ylab="Transfer entropy",cex.lab=1.2,cex.axis=1.5,errbar.col='blue',add=TRUE)
errbar(cp[1:9],result4[1:9,3],result4[1:9,3]+2*sd1[1:9,2],result4[1:9,3]-2*sd1[1:9,2],type="l",col='green',lwd=2.5,pch=23,add=TRUE,errbar.col='green')
#############
errbar(cp[1:9],NS[1:9],NS[1:9]+2*sdNS[1:9],NS[1:9]-2*sdNS[1:9],type="l",col='black',lwd=2.5,pch=23,add=TRUE,errbar.col='black')
legend("topleft",legend=c("I(Xt;Yt-1,Xt-1)","I(Xt;Yt-1)","I(Xt;Xt-1)","NET SYNERGY"),col=c("red","blue","green","black"),lty=1,lwd=1.4,bty="n",cex=1.4)
