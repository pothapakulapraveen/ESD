rm(list=ls())
library(fields)
require(graphics)
require(maps)
library(ncdf4)
library(mapdata)
library(RColorBrewer)
#library(ncdf4Utils)
x11(width=16,height=12)
par(mfrow=c(3,3),mar=c(4,3,4,4))
par(oma=c(3,3,1,1))
########## GCM Models #####
dir1<-"/gpfs/ahrenshsmfs/user/pothapak/PAPER-2/ACCESS1-3/"
nc1<-nc_open(paste(dir1,"pr_Amon_ACCESS1-3_historical_r1i1p1_185001-200512.nc",sep=""))
ACC<-ncvar_get(nc1,"pr")
lx_acc<-ncvar_get(nc1,"lon")
ly_acc<-ncvar_get(nc1,"lat")
lx_acc_lim<-which(lx_acc>40 & lx_acc<100)
ly_acc_lim<-which(ly_acc>-20 & ly_acc<40)
tm_acc<-ncvar_get(nc1,"time")
bjja<-numeric();
bjja<-c(6,7,8)
nm<-(length(tm_acc)/12)*3
for (i in 4:(nm-1)){
bjja[i]<-bjja[i-3]+12
}
ACC1<-ACC[,,bjja]
load("/gpfs/ahrenshsmfs/user/pothapak/PAPER-2/PLOTS/PC_nino/pc_nino_ac.RData")
####
dir2<-"/gpfs/ahrenshsmfs/user/pothapak/PAPER-2/CNRM-CERFACS/"
nc2<-nc_open(paste(dir2,"pr_Amon_CNRM-CM5_historical_r1i1p1_185001-200512.nc",sep=""))
CNRM<-ncvar_get(nc2,"pr")
CNRM1<-CNRM[,,bjja]
lx_cnrm<-ncvar_get(nc2,"lon")
ly_cnrm<-ncvar_get(nc2,"lat")
lx_cnrm_lim<-which(lx_cnrm>40 & lx_cnrm<100)
ly_cnrm_lim<-which(ly_cnrm>-20 & ly_cnrm<40)
load("/gpfs/ahrenshsmfs/user/pothapak/PAPER-2/PLOTS/PC_nino/pc_nino_cnrm.RData")
#####
dir3<-"/gpfs/ahrenshsmfs/user/pothapak/PAPER-2/ERA5/"
nc3<-nc_open(paste(dir3,"tp_1979_2019_ERA5.nc",sep=""))
ERA5<-ncvar_get(nc3,"tp")
lx_era5<-ncvar_get(nc3,"longitude")
ly_era5<-ncvar_get(nc3,"latitude")
#####
dir4<-"/gpfs/ahrenshsmfs/user/pothapak/PAPER-2/GISS-E2-R/"
nc4<-nc_open(paste(dir4,"pr_Amon_GISS-E2-R_historical_r1i1p1_185001-200512.nc",sep=""))
GISS<-ncvar_get(nc4,"pr")
GISS1<-GISS[,,bjja]
lx_giss<-ncvar_get(nc4,"lon")
ly_giss<-ncvar_get(nc4,"lat")
lx_giss_lim<-which(lx_giss>40 & lx_giss<100)
ly_giss_lim<-which(ly_giss>-20 & ly_giss<40)
load("/gpfs/ahrenshsmfs/user/pothapak/PAPER-2/PLOTS/PC_nino/pc_nino_giss.RData")
#####
dir5<-"/gpfs/ahrenshsmfs/user/pothapak/PAPER-2/IPSL-CM5A-LR/"
nc5<-nc_open(paste(dir5,"pr_Amon_IPSL-CM5A-LR_historical_r1i1p1_185001-200512.nc",sep=""))
IPSLLR<-ncvar_get(nc5,"pr")
IPSLLR1<-IPSLLR[,,bjja]
lx_ipsllr<-ncvar_get(nc5,"lon")
ly_ipsllr<-ncvar_get(nc5,"lat")
lx_ipsllr_lim<-which(lx_ipsllr>40 & lx_ipsllr<100)
ly_ipsllr_lim<-which(ly_ipsllr>-20 & ly_ipsllr<40)
load("/gpfs/ahrenshsmfs/user/pothapak/PAPER-2/PLOTS/PC_nino/pc_nino_ipsllr.RData")
#####
dir6<-"/gpfs/ahrenshsmfs/user/pothapak/PAPER-2/IPSL-CM5A-MR/"
nc6<-nc_open(paste(dir6,"pr_Amon_IPSL-CM5A-MR_historical_r1i1p1_185001-200512.nc",sep=""))
IPSLMR<-ncvar_get(nc6,"pr")
IPSLMR1<-IPSLMR[,,bjja]
lx_ipslmr<-ncvar_get(nc6,"lon")
ly_ipslmr<-ncvar_get(nc6,"lat")
lx_ipslmr_lim<-which(lx_ipslmr>40 & lx_ipslmr<100)
ly_ipslmr_lim<-which(ly_ipslmr>-20 & ly_ipslmr<40)
load("/gpfs/ahrenshsmfs/user/pothapak/PAPER-2/PLOTS/PC_nino/pc_nino_ipslmr.RData")
#####
dir7<-"/gpfs/ahrenshsmfs/user/pothapak/PAPER-2/MPIESM-LR/"
nc7<-nc_open(paste(dir7,"pr_Amon_MPI-ESM-LR_historical_r1i1p1_185001-200512.nc",sep=""))
MPI<-ncvar_get(nc7,"pr")
MPI1<-MPI[,,bjja]
lx_mpi<-ncvar_get(nc7,"lon")
ly_mpi<-ncvar_get(nc7,"lat")
lx_mpi_lim<-which(lx_mpi>40 & lx_mpi<100)
ly_mpi_lim<-which(ly_mpi>-20 & ly_mpi<40)
load("/gpfs/ahrenshsmfs/user/pothapak/PAPER-2/PLOTS/PC_nino/pc_nino_mpi.RData")
#######
dir8<-"/gpfs/ahrenshsmfs/user/pothapak/PAPER-2/NorESM1-M/"
nc8<-nc_open(paste(dir8,"pr_Amon_NorESM1-M_historical_r1i1p1_185001-200512.nc",sep=""))
NOR<-ncvar_get(nc8,"pr")
NOR1<-NOR[,,bjja]
lx_nor<-ncvar_get(nc8,"lon")
ly_nor<-ncvar_get(nc8,"lat")
lx_nor_lim<-which(lx_nor>40 & lx_nor<120)
ly_nor_lim<-which(ly_nor>-20 & ly_nor<40)
load("/gpfs/ahrenshsmfs/user/pothapak/PAPER-2/PLOTS/PC_nino/pc_nino_nor.RData")
########
B1<-apply(ACC[,,bjja],c(1,2),mean) * 2592000
B11<-apply(ACC1[,,which(PC_acc[1:467]*100>3)],c(1,2),mean) * 2592000
B2<-apply(CNRM[,,bjja],c(1,2),mean)*2592000
B21<-apply(CNRM1[,,which(PC_cnrm[1:467]*100>3)],c(1,2),mean) * 2592000
#B3<-apply(ERA5,c(1,2),mean) * 2592000
B4<-apply(GISS[,,bjja],c(1,2),mean) * 2592000
B41<-apply(GISS1[,,which(PC_giss[1:467]*100>3)],c(1,2),mean) * 2592000
B5<-apply(IPSLLR[,,bjja],c(1,2),mean) * 2592000
B51<-apply(IPSLLR1[,,which(PC_ipsllr[1:467]*100>3)],c(1,2),mean) * 2592000
B6<-apply(IPSLMR[,,bjja],c(1,2),mean) * 2592000
B61<-apply(IPSLMR1[,,which(PC_ipslmr[1:467]*100>3)],c(1,2),mean) * 2592000
B7<-apply(MPI[,,bjja],c(1,2),mean)   * 2592000
B71<-apply(MPI1[,,which(PC_mpi[1:467]*100>3)],c(1,2),mean)   * 2592000
B8<-apply(NOR[,,bjja],c(1,2),mean)  * 2592000
B81<-apply(NOR1[,,which(PC_nor[1:467]*100>3)],c(1,2),mean)  * 2592000
#########
colorss<-c('white','#fff5f0','#fee0d2','#fcbba1','#fc9272','#fb6a4a','#ef3b2c','#cb181d','#a50f15','#67000d')
colorss<-c('#a50026','#d73027','#f46d43','#fdae61','#fee090','#e0f3f8','#abd9e9','#74add1','#4575b4','#313695')
colorss<-c('#313695','#4575b4','#74add1','#abd9e9','#e0f3f8','#fee090','#fdae61','#f46d43','#d73027','#a50026')
colors<-brewer.pal(10, "Spectral")
colfunc<-colorRampPalette(c("red","yellow","springgreen","royalblue"))
colfunc<-colorRampPalette(c("red","white","royalblue"))
se<-seq(-100,100,200/100)
#pdf( "SA.pdf", width = 12, height = 10)
#### Figure 1####
par(mar=c(5,4,5,4))
image(lx_acc[lx_acc_lim],ly_acc[ly_acc_lim],B11[lx_acc_lim,ly_acc_lim]-B1[lx_acc_lim,ly_acc_lim],xlab=~degree~E,ylab=~degree~N,main='ACC',col=rev(colfunc(100)),breaks=se,cex.main=1.4,cex.axis=2,cex.lab=1.4,legend.args=list(text='', side=4, font=2, line=2.5, cex=1))
box(which = "plot", lty = "solid",lwd=1.4)
world.add<-map("world", xlim=range(lx_acc[lx_acc_lim]), ylim=range(ly_acc[ly_acc_lim]),plot=T ,add=TRUE)
map("world2",col="black",lwd=2,interior=T,add=T)
####### Figure 2 #####
par(mar=c(5,4,5,4))
image(lx_cnrm[lx_cnrm_lim],ly_cnrm[ly_cnrm_lim],B21[lx_cnrm_lim,ly_cnrm_lim]-B2[lx_cnrm_lim,ly_cnrm_lim],xlab=~degree~E,ylab=~degree~N,main='CNRM',col=rev(colfunc(100)),breaks=se,cex.main=1.4,cex.axis=2,cex.lab=1.4,legend.args=list(text='', side=4, font=2, line=2.5, cex=1))
box(which = "plot", lty = "solid",lwd=1.4)
world.add<-map("world", xlim=range(lx_cnrm[lx_cnrm_lim]), ylim=range(ly_cnrm[ly_cnrm_lim]),plot=T ,add=TRUE)
map("world2",col="black",lwd=2,interior=T,add=T)
######## Figure 3 #######
par(mar=c(3,4,5,4))
image(lx_giss[lx_giss_lim],ly_giss[ly_giss_lim],B41[lx_giss_lim,ly_giss_lim]-B4[lx_giss_lim,ly_giss_lim],xlab=~degree~E,ylab=~degree~N,main='GISS',col=rev(colfunc(100)),breaks=se,cex.main=1.4,cex.axis=2,cex.lab=1.4,legend.args=list(text='', side=4, font=2, line=2.5, cex=1))
box(which = "plot", lty = "solid",lwd=1.4)
world.add<-map("world", xlim=range(lx_giss[lx_giss_lim]), ylim=range(ly_giss[ly_giss_lim]),plot=T ,add=TRUE)
map("world2",col="black",lwd=2,interior=T,add=T)
######## Figure 4 ######
par(mar=c(3,4,5,4))
image(lx_ipsllr[lx_ipsllr_lim],ly_ipsllr[ly_ipsllr_lim],B51[lx_ipsllr_lim,ly_ipsllr_lim]-B5[lx_ipsllr_lim,ly_ipsllr_lim],xlab=~degree~E,ylab=~degree~N,main='IPSL-LR',col=rev(colfunc(100)),breaks=se,cex.main=1.4,cex.axis=2,cex.lab=1.4,legend.args=list(text='', side=4, font=2, line=2.5, cex=1))
box(which = "plot", lty = "solid",lwd=1.4)
world.add<-map("world", xlim=range(lx_ipsllr[lx_ipsllr_lim]), ylim=range(ly_ipsllr[ly_ipsllr_lim]),plot=T ,add=TRUE)
map("world2",col="black",lwd=2,interior=T,add=T)
####### Figure 5 ########
par(mar=c(3,4,5,4))
image(lx_ipslmr[lx_ipslmr_lim],ly_ipslmr[ly_ipslmr_lim],B61[lx_ipslmr_lim,ly_ipslmr_lim]-B6[lx_ipslmr_lim,ly_ipslmr_lim],xlab=~degree~E,ylab=~degree~N,main='IPSL-MR',col=rev(colfunc(100)),breaks=se,cex.main=1.4,cex.axis=2,cex.lab=1.4,legend.args=list(text='', side=4, font=2, line=2.5, cex=1))
box(which = "plot", lty = "solid",lwd=1.4)
world.add<-map("world", xlim=range(lx_ipslmr[lx_ipslmr_lim]), ylim=range(ly_ipslmr[ly_ipslmr_lim]),plot=T ,add=TRUE)
map("world2",col="black",lwd=2,interior=T,add=T)
######### Figure 5 #######
par(mar=c(3,4,5,4))
image(lx_mpi[lx_mpi_lim],ly_mpi[ly_mpi_lim],B71[lx_mpi_lim,ly_mpi_lim]-B7[lx_mpi_lim,ly_mpi_lim],xlab=~degree~E,ylab=~degree~N,main='MPIESM',col=rev(colfunc(100)),breaks=se,cex.main=1.4,cex.axis=2,cex.lab=1.4,legend.args=list(text='', side=4, font=2, line=2.5, cex=1))
box(which = "plot", lty = "solid",lwd=1.4)
world.add<-map("world", xlim=range(lx_mpi[lx_mpi_lim]), ylim=range(ly_mpi[ly_mpi_lim]),plot=T ,add=TRUE)
map("world2",col="black",lwd=2,interior=T,add=T)
##########
par(mar=c(3,4,5,4))
image.plot(lx_nor[lx_nor_lim],ly_nor[ly_nor_lim],B81[lx_nor_lim,ly_nor_lim]-B8[lx_nor_lim,ly_nor_lim],xlab=~degree~E,ylab=~degree~N,main='NORESM',col=rev(colfunc(100)),breaks=se,cex.main=1.4,cex.axis=2,cex.lab=1.4,legend.args=list(text='', side=4, font=2, line=2.5, cex=1))
box(which = "plot", lty = "solid",lwd=1.4)
world.add<-map("world", xlim=range(lx_nor[lx_nor_lim]), ylim=range(ly_nor[ly_nor_lim]),plot=T ,add=TRUE)
map("world2",col="black",lwd=2,interior=T,add=T)
###
#source("rot.R")
#par(mar=c(3,4,5,4))
#image.plot(lx_pri,ly_pri,B5,xlab=~degree~E,ylab=~degree~N,main='MOD',col=rev(colfunc(100)),breaks=se,cex.main=1.4,cex.axis=2,cex.lab=1.4,legend.args=list(text='', side=4, font=2, line=2.5, cex=1))
#box(which = "plot", lty = "solid",lwd=1.4)
#world.add<-map("world", xlim=range(lx_pri), ylim=range(ly_pri),plot=T ,add=TRUE)
#map("world2",col="black",lwd=2,interior=T,add=T)
#################
dev.print(postscript,'nino_prec_comp.eps',width=16,height=12)
#grid()
dev.off()
