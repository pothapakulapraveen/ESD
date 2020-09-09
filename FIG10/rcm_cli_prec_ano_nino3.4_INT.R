rm(list=ls())
library(fields)
require(graphics)
require(maps)
library(ncdf4)
library(mapdata)
library(RColorBrewer)
#x11(width=12,height=8)
source("image.plot_Erwan.R")
#par(mfrow=c(2,3),mar=c(4,3,4,4))
#par(oma=c(3,3,1,1))
#########   SEGRIGATING THE PRECIPITATION FOR LANINA and ELNINA  ######
dir7<-"/gpfs/ahrenshsmfs/user/pothapak/PAPER-2/WAS_22_MPI-ESM/"
nc7<-nc_open(paste(dir7,"pr_WAS-22_MPI-M-MPI-ESM-LR_historical_r1i1p1_CLMcom-ETH-COSMO-crCLIM-v1-1_v1_mon_195101-200512_APH_1.nc",sep=""))
MPI<-ncvar_get(nc7,"pr")
#MPI1<-MPI[,,bjja]
lx_mpi<-ncvar_get(nc7,"longitude")
ly_mpi<-ncvar_get(nc7,"latitude")
tm_mpi<-ncvar_get(nc7,"time")
lx_mpi_lim<-which(lx_mpi>65 & lx_mpi<100)
ly_mpi_lim<-which(ly_mpi>5 & ly_mpi<40)
####
bjja<-numeric()
bjja<-c(6,7,8,9)
nm<-(length(tm_mpi)/12)*4
for (i in 5:(nm)){
bjja[i]<-bjja[i-4]+12
}
#####
MPI1<-MPI[,,bjja]
load("pc_nino_mpi.RData")
#######
dir8<-"//gpfs/ahrenshsmfs/user/pothapak/PAPER-2/WAS_22_NCC-NorESM1/"
nc8<-nc_open(paste(dir8,"pr_WAS-22_NCC-NorESM1-M_195101_200512_APH_1.nc",sep=""))
NOR<-ncvar_get(nc8,"pr")
NOR1<-NOR[,,bjja]
lx_nor<-ncvar_get(nc8,"longitude")
ly_nor<-ncvar_get(nc8,"latitude")
lx_nor_lim<-which(lx_nor>65 & lx_nor<100)
ly_nor_lim<-which(ly_nor>5 & ly_nor<40)
load("pc_nino_nor.RData")
########
dir9<-"/gpfs/ahrenshsmfs/user/pothapak/PAPER-2/WAS_22_EC-EARTH/"
nc9<-nc_open(paste(dir9,"pr_WAS-22_ICHEC-EC-EARTH_historical_r12i1p1_CLMcom-ETH-COSMO-crCLIM-v1-1_v1_mon_195101-200512_APH_1.nc",sep=""))
EC<-ncvar_get(nc9,"pr")
EC1<-EC[,,bjja]
lx_ec<-ncvar_get(nc9,"longitude")
ly_ec<-ncvar_get(nc9,"latitude")
lx_ec_lim<-which(lx_ec>65 & lx_ec<100)
ly_ec_lim<-which(ly_ec>5 & ly_ec<40)
load("pc_nino_EC.RData")
#########
############
B7<-apply(MPI[,,bjja],c(1,2),mean)   * 2592000
B71<-apply(MPI1[,,which(PC_mpi>quantile(PC_mpi,0.75))],c(1,2),mean)   * 2592000
B8<-apply(NOR[,,bjja],c(1,2),mean)  * 2592000
B81<-apply(NOR1[,,which(PC_nor>quantile(PC_nor,0.75))],c(1,2),mean)  * 2592000
B9<-apply(EC[,,bjja],c(1,2),mean)  * 2592000
B91<-apply(EC1[,,which(PC_ec>quantile(PC_ec,0.75))],c(1,2),mean)  * 2592000
#########
##############
colorss<-c('white','#fff5f0','#fee0d2','#fcbba1','#fc9272','#fb6a4a','#ef3b2c','#cb181d','#a50f15','#67000d')
colorss<-c('#a50026','#d73027','#f46d43','#fdae61','#fee090','#e0f3f8','#abd9e9','#74add1','#4575b4','#313695')
colorss<-c('#313695','#4575b4','#74add1','#abd9e9','#e0f3f8','#fee090','#fdae61','#f46d43','#d73027','#a50026')
colors<-brewer.pal(10, "Spectral")
colfunc<-colorRampPalette(c("red","yellow","springgreen","royalblue"))
colfunc<-colorRampPalette(c("red","white","royalblue"))
colorss<-c('#67001f','#b2182b','#d6604d','#f4a582','#fddbc7','white','white','#d1e5f0','#92c5de','#4393c3','#2166ac','#053061')
#### Figure 1####
se<-seq(-60,60,120/12)
par(mar=c(3,6,3,6))
diff71<-B71[lx_mpi_lim,ly_mpi_lim]-B7[lx_mpi_lim,ly_mpi_lim]
diff71[which(diff71< -60)]<- -60
diff71[which(diff71>  60)]<- 60
image(lx_mpi[lx_mpi_lim],ly_mpi[ly_mpi_lim],diff71,xlab=~degree~E,ylab=~degree~N,main='',col=colorss,breaks=se,cex.main=1.4,cex.axis=2,cex.lab=1.4,legend.args=list(text='', side=4, font=2, line=2.5, cex=1))
box(which = "plot", lty = "solid",lwd=1.4)
mtext(side=3, "Downscaled-MPI-ESM-LR", col="black", font=2, cex=1.2)
text(x=92, y=36, "El-Nino",col="black", font=2, cex=1.4)
world.add<-map("world", xlim=range(lx_mpi[lx_mpi_lim]), ylim=range(ly_mpi[ly_mpi_lim]),plot=T ,add=TRUE)
map("world2",col="black",lwd=2,interior=T,add=T)
##########
par(mar=c(3,6,3,6))
#se<-seq(-120,120,240/12)
diff81<-B81[lx_nor_lim,ly_nor_lim]-B8[lx_nor_lim,ly_nor_lim]
diff81[which(diff81< -60)]<- -60
diff81[which(diff81>  60)]<- 60
image(lx_nor[lx_nor_lim],ly_nor[ly_nor_lim],diff81,xlab=~degree~E,ylab=~degree~N,main='',col=colorss,breaks=se,cex.main=1.4,cex.axis=2,cex.lab=1.4,legend.args=list(text='', side=4, font=2, line=2.5, cex=1))
box(which = "plot", lty = "solid",lwd=1.4)
mtext(side=3, "Downscaled-Nor-ESM-M", col="black", font=2, cex=1.2)
text(x=92, y=36, "El-Nino",col="black", font=2, cex=1.4)
world.add<-map("world", xlim=range(lx_nor[lx_nor_lim]), ylim=range(ly_nor[ly_nor_lim]),plot=T ,add=TRUE)
map("world2",col="black",lwd=2,interior=T,add=T)
#######
par(mar=c(3,6,5,6))
#se<-seq(-120,120,240/12)
diff91<-B91[lx_ec_lim,ly_ec_lim]-B9[lx_ec_lim,ly_ec_lim]
diff91[which(diff91< -60)]<- -60
diff91[which(diff91>  60)]<- 60
image.plot.erwan(lx_ec[lx_ec_lim],ly_ec[ly_ec_lim],diff91,xlab=~degree~E,ylab=~degree~N,main='',col=colorss,breaks=se,cex.main=1.4,cex.axis=2,cex.lab=1.4,legend.args=list(text='', side=4, font=2, line=2.5, cex=1))
box(which = "plot", lty = "solid",lwd=1.4)
text(x=92, y=36, "El-Nino",col="black", font=2, cex=1.4)
mtext(side=3,"Downscaled-EC-EARTH", col="black", font=2, cex=1.2)
world.add<-map("world", xlim=range(lx_nor[lx_nor_lim]), ylim=range(ly_nor[ly_nor_lim]),plot=T ,add=TRUE)
map("world2",col="black",lwd=2,interior=T,add=T)
########xx
#source("rot_NOR_el.R")


### HERE COMES THE RCM MODEL ###### 
#source("rot_MPI_el.R")
#source("rot_NOR_el.R")
#source("rot_EC_el.R")
##dev.print(postscript,'Fig7a_new.eps',width=12,height=8)
