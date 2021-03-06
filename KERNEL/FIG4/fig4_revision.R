rm(list=ls())
library("Hmisc")
#source("CMI_function_kar.R")   ### Conditional Mutual Information Own!
source("kernel_CMI.R")
source("kernel_MI.R")
source("kernel_MI_SD.R")
source("kernel_CMI_SD.R")
#source("MI_function_kar.R")        #### Mutual Information JDIT 
#source("MI_SD_function_kar2.R")    ### Boot Strappig for MI using JDIT
#source("CMI_SD_function_kar.R")    #### Boot Strapping for CMI using own algorithim
load("../../henon.RData")    #### Loading Linear Data
library(doParallel)            ###   Parallel Library for Parallel execution using multiple processor
cl<-5
registerDoParallel(cl)
################# Long time series ####################
result4<-foreach(i = 1:21,.combine='rbind')%dopar%{
A<-CMI(x1[i,94999:99999],y1[i,95000:100000],y1[i,94999:99999],0.5)
B<-MI(x1[i,94999:99999],y1[i,95000:100000],0.5)
C<-MI(y1[i,94999:99999],y1[i,95000:100000],0.5)
D<-CMI(y1[i,94999:99999],y1[i,95000:100000],x1[i,94999:99999],0.5)
return(c(A,B,C,D))}
################### 1000 time units ############
sd1<-foreach(i = 1:21,.combine='rbind')%dopar%{
A<-MISD(x1[i,95000:100000],y1[i,95000:100000],0.5,100)    ## I(y_n-1;x_n)
B<-MISD(y1[i,95000:100000],y1[i,95000:100000],0.5,100)   ## I(x_n-1; x_n)
return(c(A,B))}
#########################    500 time units      ###########
sd2<-foreach(i = 1:21,.combine='rbind')%dopar%{
A<-SDCMI(x1[i,95000:100000],y1[i,95000:100000],y1[i,95000:100000],0.5,100)   ## I(y_n-1,x_n,/x_n-1)
D<-SDCMI(y1[i,95000:100000],y1[i,95000:100000],x1[i,95000:100000],0.5,100)   ## I(x_n-1,x_n/y_n-1))
return(c(A,D))
}
####### Calculation of Information flow Metrics ######
NS<-result4[,1]-result4[,2]    #### Net Synergy #####
sdNS<-sqrt(sd2[1:21,1]^2 +  sd1[1:21,1]^2)   ####  Standard dviations of Net Synergy #####
######
IXY<-result4[,1]+result4[,3]              #####  IXY
sdixy<- sqrt(sd2[1:21,1]^2 +  sd1[1:21,2]^2) ##### Standard deviations 
IXX<-result4[,2] + result4[,4]
############################################################
cp<-seq(0,0.8,0.04)
plot(cp[2:15],IXY[2:15],type="l",col="red",lwd=2.5,ylim=c(0,3),lty=1,pch=18,xlab="coupling coefficient",ylab="Information exchange (nats)",cex.lab=1.2,cex.axis=1.5,main="Kernel")
errbar(cp[2:15],IXY[2:15],IXY[2:15]+2*sdixy[2:15],IXY[2:15]-2*sdixy[2:15],type="l",col='red',lwd=2.5,lty=1,pch=23,ylim=c(0,0.4),xlab="coupling coefficient",ylab="Transfer entropy",cex.lab=1.2,cex.axis=1.5,errbar.col='red',add=TRUE)
##### MI SD ##
errbar(cp[2:15],result4[2:15,2],result4[2:15,2]+2*sd1[2:15,1],result4[2:15,2]-2*sd1[2:15,1],type="l",col='blue',lwd=2.5,lty=1,pch=23,ylim=c(0,0.4),xlab="coupling coefficient",ylab="Transfer entropy",cex.lab=1.2,cex.axis=1.5,errbar.col='blue',add=TRUE)
errbar(cp[2:15],result4[2:15,3],result4[2:15,3]+2*sd1[2:15,2],result4[2:15,3]-2*sd1[2:15,2],type="l",col='green',lwd=2.5,pch=23,add=TRUE,errbar.col='green')
#############
errbar(cp[2:15],NS[2:15],NS[2:15]+2*sdNS[2:15],NS[2:15]-2*sdNS[2:15],type="l",col='black',lwd=2.5,pch=23,add=TRUE,errbar.col='black')
legend("topleft",legend=c("I(Yt;Yt-1,Xt-1)","I(Yt;Xt-1)","I(Yt;Yt-1)","NET SYNERGY"),col=c("red","blue","green","black"),lty=1,lwd=1.4,bty="n",cex=1.4)
