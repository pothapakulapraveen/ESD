CMI<-function(xx,yy,zz){
CMID<-numeric();
##########
a<-cor(xx,yy)
b<-cor(yy,zz)
c<-cor(xx,zz)
NR<- (1-b^2) * (1-c^2)
DR<- (1-(a^2+b^2+c^2)+2*a*b*c)
result<-0.5*log(NR/DR)
if(result<0){result=0}
if(result==Inf){result=0}
##############
for (i in 1:100){
resample<-sample(length(xx)-1,length(xx),replace=TRUE)
x<-xx[resample];y<-yy[resample];z<-zz[resample]
a<-cor(x,y)
b<-cor(y,z)
c<-cor(x,z)
NR<- (1-b^2) * (1-c^2)
DR<- (1-(a^2+b^2+c^2)+2*a*b*c)
CMID[i]<-0.5*log(NR/DR)
}
if(result-(1.96*sd(CMID)/sqrt(50))<0){result=0}
return(result)}

