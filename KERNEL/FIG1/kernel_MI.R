######    x is the source and y is the destination    ########
MI<-function(x,y,r){
#x<-rnorm(100,0,1)
#y<-rnorm(100,0,1)
#r<-0.3
#nor<-1
le<-length(x)
#if(nor ==1){
norx<-(x-mean(x)/sd(x))
nory<-(y-mean(y)/sd(y))
#yn1<-nory[2:le]
#yn<-nory[1:(le-1)]
#xn<-norx[1:(le-1)]
#else {

######   FOR TE CALCULATION ######
#yn1<-y[2:le]
#yn<-y[1:(le-1)]
#xn<-x[1:(le-1)]
######## FOR MI CMI CALCULATION ######
#yn1<-y
yn<-nory
xn<-norx
###############
#}
#min<-min(nor)
pr<-numeric();
#pr<-matrix(NA,nrow=nbinx,ncol=nbiny)
#midsx<-seq(min(floor(min(norx)),floor(min(nory))),max(ceiling(max(norx)),ceiling(max(nory))),r)
midsx<-seq(floor(min(xn)),ceiling(max(xn)),r)
midsy<-seq(floor(min(yn)),ceiling(max(yn)),r)
#midsy1<-seq(floor(min(yn1)),ceiling(max(yn1)),r)
######nbins<-length(seq(min(floor(min(norx)),floor(min(nory))),max(ceiling(max(norx)),ceiling(max(norx))),r)) -1
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
return(sum(T,na.rm=T))
}
##### The result is in returned in NATS #####
#p1<-apply(pr,c(1,2),sum)
#p2<-apply(pr,c(1,3),sum)
#p3<-apply(pr,c(2,3),sum)
####### Transfer Entropy Calculation ######



#plot(midsx[1:length(midsx)-1],pr,type=",main="probability",xlab="Mids",ylab="Probability")
