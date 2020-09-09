######    x is the source and y is the destination    ########
MISD<-function(xx,yy,r,nboot){
CMID<-numeric()
for (pa in 1:nboot){
resample<-sample((length(xx)-1),length(xx),replace=TRUE)
resample2<-resample+1
x<-xx[resample];y<-yy[resample2];
le<-length(x)
norx<-(x-mean(x)/sd(x))
nory<-(y-mean(y)/sd(y))
yn<-nory
xn<-norx
###############
pr<-numeric();
midsx<-seq(floor(min(xn)),ceiling(max(xn)),r)
midsy<-seq(floor(min(yn)),ceiling(max(yn)),r)
#### Forming a rectangles #########
nbinx<-length(seq(floor(min(xn)),ceiling(max(xn)),r))-1
nbiny<-length(seq(floor(min(yn)),ceiling(max(yn)),r))-1
#nbiny1<-length(seq(floor(min(yn1)),ceiling(max(yn1)),r))-1
pr<-array(NA,c(nbinx,nbiny))
T<-array(NA,c(nbinx,nbiny))
##### looping throuhg the whole space ########
for (i in 1:nbinx){
for (j in 1:nbiny){
#for (k in 1:nbiny1){
y<-length((which(xn>midsx[i] & xn<=midsx[i+1] & yn>midsy[j] & yn<=midsy[j+1])))
pr[i,j]<-y/length(x)
}}
####### Entropy Calculation ######
tm1<-apply(pr,c(1),sum)
tm2<-apply(pr,c(2),sum)
#tm3<-apply(pr,c(2,3),sum)
for (i in 1:nbinx){
for (j in 1:nbiny){
#for (k in 1:nbiny1){
if(pr[i,j]==0 || tm1[i]==0 || tm2[j]==0){next()}
T[i,j]<-pr[i,j]* log((pr[i,j])/(tm1[i]*tm2[j]))
}}
CMID[pa]<-sum(T,na.rm=T)}
return(sd(CMID))}
