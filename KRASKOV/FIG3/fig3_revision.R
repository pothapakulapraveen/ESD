rm(list=ls())
library("Hmisc")
source("CMI_function_kar.R")   ### Conditional Mutual Information Own!
source("MI_function_kar.R")        #### Mutual Information JDIT 
source("MI_SD_function_kar.R")    ### Boot Strappig for MI using JDIT
source("CMI_SD_function_kar.R")    #### Boot Strapping for CMI using own algorithim
load("../../linear3.RData")    #### Loading Linear Data
library(doParallel)            ###   Parallel Library for Parallel execution using multiple processor
cl<-5
registerDoParallel(cl)
opts<-list(chuncksize=2)
################# Long time series ####################
result4<-foreach(i = 1:9,.combine='rbind')%dopar%{
A<-CMI(y[94999:99999],x[i,95000:100000],z[94999:99999])
B<-MI(y[94999:99999],x[i,95000:100000])
C<-MI(z[94999:99999],x[i,95000:100000])
D<-CMI(z[94999:99999],x[i,95000:100000],y[94999:99999])
return(c(A,B,C,D))}
################### 1000 time units ############
sd1<-foreach(i = 1:9,.combine='rbind',.options.nws=opts)%:%
foreach(j=1:100,.combine='cbind')%dopar%{
A<-MISD(y[95000:100000],x[i,95000:100000])    ## I(y_n-1;x_n)
}
sd1<-apply(sd1,c(1),sd)
sd2<-foreach(i = 1:9,.combine='rbind',.options.nws=opts)%:%
foreach(j=1:100,.combine='cbind')%dopar%{
B<-MISD(z[95000:100000],x[i,95000:100000])   ## I(x_n-1; x_n)
}
sd2<-apply(sd2,c(1),sd)
#########################    500 time units      ###########
sd3<-foreach(i = 1:9,.combine='rbind',.options.nws=opts)%:%
  foreach(j=1:100,.combine='cbind')%dopar%{
A<-SDCMI(y[95000:100000],x[i,95000:100000],z[95000:100000])   ## I(y_n-1,x_n,/x_n-1)
#D<-SDCMI(x[i,99000:100000],x[i,99000:100000],y[99000:100000])   ## I(x_n-1,x_n/y_n-1))
#return(c(A,D))
}
sd3<-apply(sd3,c(1),sd)
#sd3<-foreach(i = 1:9,.combine='rbind',.options.nws=opts)%:%
#  foreach(j=1:5,.combine='cbind')%dopar%{
#A<-SDCMI(y[99000:100000],x[i,99000:100000],x[i,99000:100000])   ## I(y_n-1,x_n,/x_n-1)
#D<-SDCMI(x[i,99000:100000],x[i,99000:100000],y[99000:100000])   ## I(x_n-1,x_n/y_n-1))
#return(c(A,D))
#}



####### Calculation of Information flow Metrics ######
NS<-result4[,1]-result4[,2]    #### Net Synergy #####
sdNS<-sqrt(sd3[1:9]^2 +  sd1[1:9]^2)   ####  Standard dviations of Net Synergy #####
######
IXY<-result4[,1]+result4[,3]              #####  IXY
sdixy<- sqrt(sd3[1:9]^2 +  sd2[1:9]^2) ##### Standard deviations 
IXX<-result4[,2] + result4[,4]
############################################################
cp<-seq(0,1,0.1)
plot(cp[1:9],IXY[1:9],type="l",col="red",lwd=2.5,ylim=c(0,0.6),lty=1,pch=18,xlab="coupling coefficient",ylab="Information exchange (nats)",cex.lab=1.2,cex.axis=1.5,main="Kraskov")
errbar(cp[1:9],IXY[1:9],IXY[1:9]+2*sdixy[1:9],IXY[1:9]-2*sdixy[1:9],type="l",col='red',lwd=2.5,lty=1,pch=23,ylim=c(0,0.4),xlab="coupling coefficient",ylab="Transfer entropy",cex.lab=1.2,cex.axis=1.5,errbar.col='red',add=TRUE)
##### MI SD ##
errbar(cp[1:9],result4[1:9,2],result4[1:9,2]+2*sd1[1:9],result4[1:9,2]-2*sd1[1:9],type="l",col='blue',lwd=2.5,lty=1,pch=23,ylim=c(0,0.4),xlab="coupling coefficient",ylab="Transfer entropy",cex.lab=1.2,cex.axis=1.5,errbar.col='blue',add=TRUE)
errbar(cp[1:9],result4[1:9,3],result4[1:9,3]+2*sd2[1:9],result4[1:9,3]-2*sd2[1:9],type="l",col='green',lwd=2.5,pch=23,add=TRUE,errbar.col='green')
#############
errbar(cp[1:9],NS[1:9],NS[1:9]+2*sdNS[1:9],NS[1:9]-2*sdNS[1:9],type="l",col='black',lwd=2.5,pch=23,add=TRUE,errbar.col='black')
legend("topleft",legend=c("I(Xt;Yt-1,Zt-1)","I(Xt;Yt-1)","I(Xt;Zt-1)","NET SYNERGY"),col=c("red","blue","green","black"),lty=1,lwd=1.4,bty="n",cex=1.4)
