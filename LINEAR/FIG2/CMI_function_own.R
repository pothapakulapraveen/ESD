CMI<-function(x,y,z){
a<-cor(x,y)
b<-cor(y,z)
c<-cor(x,z)
NR<- (1-b^2) * (1-c^2)
DR<- (1-(a^2+b^2+c^2)+2*a*b*c)
result<-0.5*log(NR/DR)
if(result<0){result=0}
if(result==Inf){result=0}
return(result)}
