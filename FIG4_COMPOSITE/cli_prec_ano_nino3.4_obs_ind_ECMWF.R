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
#dir1<-"/gpfs/ahrenshsmfs/user/pothapak/PAPER-2/OBS/"
dir1<-"/gpfs/ahrenshsmfs/user/pothapak/PAPER-2/ECMWF-ERAINT/"
nc1<-nc_open(paste(dir1,"ECMWF_GCM.nc",sep=""))
ACC<-ncvar_get(nc1,"tp")
lx_acc<-ncvar_get(nc1,"longitude")
ly_acc<-ncvar_get(nc1,"latitude")
ly_acc<-ly_acc[c(361:181,180:1)]
tm_acc<-ncvar_get(nc1,"time")
ACC<-ACC[,c(361:181,180:1),]
#######
bjja<-numeric();
bjja<-c(6,7,8,9)
nm<-(length(tm_acc)/12)*4
for (i in 5:(nm)){
bjja[i]<-bjja[i-4]+12
}
######
lx_acc_lim<-which(lx_acc>65 & lx_acc<100)
#lx_acc_lim<-lx_acc[c(291:360,1:30)]
ly_acc_lim<-which(ly_acc>5 & ly_acc<40)
###########
tm_acc<-ncvar_get(nc1,"time")
#####
ACC1<-ACC[lx_acc_lim,ly_acc_lim,bjja]
ACCM<-apply(ACC1,c(1,2),mean,na.rm=T)*10000
#load("nino3.4_PC_obs.RData")
load("/gpfs/ahrenshsmfs/user/pothapak/PAPER-2/PLOTS/PC_nino/LICHT/nino3.4_PC_obs_ECMWF.RData")
########
elnino<-which(PC_obs>quantile(PC_obs,0.75))
lanina<-which(PC_obs<quantile(PC_obs,0.25))
#########   SEGRIGATING THE PRECIPITATION FOR LANINA and ELNINA  ######
PC_obs_elni<-apply(ACC1[,,elnino],c(1,2),mean)*10000
PC_obs_lani<-apply(ACC1[,,lanina],c(1,2),mean)*10000

#########
colorss<-c('white','#fff5f0','#fee0d2','#fcbba1','#fc9272','#fb6a4a','#ef3b2c','#cb181d','#a50f15','#67000d')
colorss<-c('#a50026','#d73027','#f46d43','#fdae61','#fee090','#e0f3f8','#abd9e9','#74add1','#4575b4','#313695')
colorss<-c('#313695','#4575b4','#74add1','#abd9e9','#e0f3f8','#fee090','#fdae61','#f46d43','#d73027','#a50026')
colors<-brewer.pal(10, "Spectral")
colfunc<-colorRampPalette(c("red","yellow","springgreen","royalblue"))
colfunc<-colorRampPalette(c("red","white","royalblue"))
colorss<-c('#67001f','#b2182b','#d6604d','#f4a582','#fddbc7','white','white','#d1e5f0','#92c5de','#4393c3','#2166ac','#053061')
se<-seq(-18,18,36/12)
par(mar=c(3,5,3,3))
image(lx_acc[lx_acc_lim],ly_acc[ly_acc_lim],PC_obs_elni-ACCM,xlab=~degree~E,ylab=~degree~N,main='',col=colorss,breaks=se,cex.main=1.4,cex.axis=2,cex.lab=1.4,legend.args=list(text='', side=4, font=2, line=2.5, cex=1))
box(which = "plot", lty = "solid",lwd=1.4)
mtext(side=3, "ERA-Interim", col="black", font=2, cex=1.2)
text(x=92, y=36, "El-Nino",col="black", font=2, cex=1.4)
world.add<-map("world", xlim=range(lx_acc[lx_acc_lim]), ylim=range(ly_acc[ly_acc_lim]),plot=T ,add=TRUE)
map("world2",col="black",lwd=2,interior=T,add=T)
#### Figure 1####
#se<-seq(-60,60,120/12)

#dev.print(postscript,'obs_Fig7a_new.eps',width=10,height=6)
