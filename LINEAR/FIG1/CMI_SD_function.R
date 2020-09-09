SDCMI<-function(x,y,z,nboot){    #### (x_n-1, y_n , x_n-1)
CMID<-numeric();
for (i in 1:nboot){
resample<-sample((length(x)-1),length(x),replace=TRUE)
resample2<-resample+1
x<-x[resample];y<-y[resample2];z<-z[resample]
a<-cor(x,y)
b<-cor(y,z)
c<-cor(x,z)
NR<- (1-b^2) * (1-c^2)
DR<- (1-(a^2+b^2+c^2)+2*a*b*c)
result<-0.5*log(NR/DR)
CMID[i]<-result
}
return(sd(CMID))}
##### The result is in returned in NATS #####
