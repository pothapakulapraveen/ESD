rm(list=ls())
library(fields)
require(graphics)
require(maps)
library(ncdf4)
library(mapdata)
library(RColorBrewer)
#x11(width=10,height=6)
#par(mfrow=c(1,3),mar=c(4,3,4,4))
#par(oma=c(3,3,1,1))
#dir1<-"/work/scratch/pp28jofe/PAPER-2/OBS/"
dir1<-"/gpfs/ahrenshsmfs/user/pothapak/PAPER-2/OBS/"
nc1<-nc_open(paste(dir1,"gpcc_V5_1951-2005_JJAS.nc",sep=""))
OCC<-ncvar_get(nc1,"prcp")
lx_occ<-ncvar_get(nc1,"longitude")
ly_occ<-ncvar_get(nc1,"latitude")
lx_occ_lim<-which(lx_occ>40 & lx_occ<120)
#lx_acc_lim<-lx_acc[c(291:360,1:30)]
ly_occ_lim<-which(ly_occ>-10 & ly_occ<40)
###########
tm_occ<-ncvar_get(nc1,"time")
#####
OCC1<-OCC[lx_occ_lim,ly_occ_lim,]
OCCM<-apply(OCC1,c(1,2),mean,na.rm=T)
#load("nino3.4_PC_obs.RData")
load("iod_PC_obs_JJAS.RData")
########
elnino<-which(PC_obs>quantile(PC_obs,0.75))
lanina<-which(PC_obs<quantile(PC_obs,0.25))
#########   SEGRIGATING THE PRECIPITATION FOR LANINA and ELNINA  ######
PC_obs_elni<-apply(OCC1[,,elnino],c(1,2),mean)
PC_obs_lani<-apply(OCC1[,,lanina],c(1,2),mean)

nc7<-nc_open(paste(dir1,"APHRO_mon_1951_2005_JJAS.nc",sep=""))
APH<-ncvar_get(nc7,"precip")
#MPI1<-MPI[,,bjja]
lx_aph<-ncvar_get(nc7,"longitude")
ly_aph<-ncvar_get(nc7,"latitude")
tm_aph<-ncvar_get(nc7,"time")
lx_aph_lim<-which(lx_aph>40 & lx_aph<120)
ly_aph_lim<-which(ly_aph>-20 & ly_aph<40)
####
APH1<-APH[lx_aph_lim,ly_aph_lim,]
APHM<-apply(APH1,c(1,2),mean,na.rm=T)
PC_obs_elni_aph<-apply(APH1[,,elnino],c(1,2),mean)
PC_obs_lani_aph<-apply(APH1[,,lanina],c(1,2),mean)
##########
nc4<-nc_open(paste(dir1,"ncep_precip_mon_JJAS.nc",sep=""))
NCE<-ncvar_get(nc4,"prate")
NCE<-NCE[,94:1,]
#NCE<-NCE[,c(seq(94:48),seq(47:1)),]
#MPI1<-MPI[,,bjja]
lx_nce<-ncvar_get(nc4,"lon")
ly_nce<-ncvar_get(nc4,"lat")
ly_nce<-ly_nce[94:1]
#ly_nce<-c(ly_nce[94:48],ly_nce[47:1])
tm_nce<-ncvar_get(nc4,"time")
lx_nce_lim<-which(lx_nce>40 & lx_nce<120)
ly_nce_lim<-which(ly_nce>-20 & ly_nce<40)
####
NCE1<-NCE[lx_nce_lim,ly_nce_lim,]
NCEM<-apply(NCE1,c(1,2),mean,na.rm=T)
load("iod_PC_obs_ncep_JJAS.RData")
elninone<-which(PC_obs>quantile(PC_obs,0.75))
laninane<-which(PC_obs<quantile(PC_obs,0.25))
PC_obs_elni_nce<-apply(NCE1[,,elninone],c(1,2),mean)
PC_obs_lani_nce<-apply(NCE1[,,laninane],c(1,2),mean)
#########
colorss<-c('white','#fff5f0','#fee0d2','#fcbba1','#fc9272','#fb6a4a','#ef3b2c','#cb181d','#a50f15','#67000d')
colorss<-c('#a50026','#d73027','#f46d43','#fdae61','#fee090','#e0f3f8','#abd9e9','#74add1','#4575b4','#313695')
colorss<-c('#313695','#4575b4','#74add1','#abd9e9','#e0f3f8','#fee090','#fdae61','#f46d43','#d73027','#a50026')
colors<-brewer.pal(10, "Spectral")
colfunc<-colorRampPalette(c("red","yellow","springgreen","royalblue"))
colfunc<-colorRampPalette(c("red","white","royalblue"))
colorss<-c('#67001f','#b2182b','#d6604d','#f4a582','#fddbc7','white','white','#d1e5f0','#92c5de','#4393c3','#2166ac','#053061')
se<-seq(-60,60,120/12)
par(mar=c(3,4,5,4))
image(lx_occ[lx_occ_lim],ly_occ[ly_occ_lim],PC_obs_elni-OCCM,xlab=~degree~E,ylab=~degree~N,main='GPCC',col=colorss,breaks=se,cex.main=1.4,cex.axis=2,cex.lab=1.4,legend.args=list(text='', side=4, font=2, line=2.5, cex=1))
box(which = "plot", lty = "solid",lwd=1.4)
world.add<-map("world", xlim=range(lx_occ[lx_occ_lim]), ylim=range(ly_occ[ly_occ_lim]),plot=T ,add=TRUE)
map("world2",col="black",lwd=2,interior=T,add=T)
#### Figure 1####
se<-seq(-60,60,120/12)
par(mar=c(3,4,5,4))
image(lx_aph[lx_aph_lim],ly_aph[ly_aph_lim],PC_obs_elni_aph-APHM,xlab=~degree~E,ylab=~degree~N,main='APHRODITE',col=colorss,breaks=se,cex.main=1.4,cex.axis=2,cex.lab=1.4,legend.args=list(text='', side=4, font=2, line=2.5, cex=1))
box(which = "plot", lty = "solid",lwd=1.4)
world.add<-map("world", xlim=range(lx_aph[lx_aph_lim]), ylim=range(ly_aph[ly_aph_lim]),plot=T ,add=TRUE)
map("world2",col="black",lwd=2,interior=T,add=T)
##########
par(mar=c(3,4,5,4))
image.plot(lx_nce[lx_nce_lim],ly_nce[ly_nce_lim],PC_obs_elni_nce-NCEM,xlab=~degree~E,ylab=~degree~N,main='NCEP',col=colorss,breaks=se,cex.main=1.4,cex.axis=2,cex.lab=1.4,legend.args=list(text='', side=4, font=2, line=2.5, cex=1))
box(which = "plot", lty = "solid",lwd=1.4)
world.add<-map("world", xlim=range(lx_nce[lx_nce_lim]), ylim=range(ly_nce[ly_nce_lim]),plot=T ,add=TRUE)
map("world2",col="black",lwd=2,interior=T,add=T)
#dev.print(postscript,'obs_Fig7a_new_iodp.eps',width=10,height=6)
