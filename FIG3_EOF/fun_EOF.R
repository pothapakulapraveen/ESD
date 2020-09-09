PC<-function(Lon,Lat,precnc,Time){
#library(ncdf4)
library(matrixStats)
#library(fields)
#nc=ncdf4::nc_open("air.mon.mean.nc")
#Lon <- ncvar_get(nc, "lon")
#lat <- ncvar_get(nc, "lat")
#Time<- ncvar_get(nc, "time")
#precnc<- ncvar_get(nc, "air")
#Write the data as space-time matrix with a header
precst=matrix(0,nrow=length(Lon)*length(Lat),ncol=length(Time))
#temp=as.vector(precnc[,,1])
for (i in 1:length(Time)) {precst[,i]=as.vector(precnc[ , , i])}
#gpcpJ=precst
#monJ=seq(1,length(Time),12)
#gpcpJ=precst[,monJ]
#climJ<-rowMeans(gpcpJ)
#sdJ<-rowSds(gpcpJ)
#anomJ=(gpcpJ-climJ)/sdJ
svdJ=svd(precst)
mapmat=matrix(svdJ$u[,2],nrow=length(Lon))
#image.plot(Lon,Lat[c(73:37,36:1)],mapmat[,c(73:37,36:1)])
pcdat<-svdJ$v[,2]
#plot(-pcdat, type="o")
return(list(EOF=mapmat,PC=pcdat))
#return(pcdat)
}
