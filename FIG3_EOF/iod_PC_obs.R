rm(list=ls())
library(fields)
require(graphics)
require(maps)
library(ncdf4)
library(mapdata)
library(RColorBrewer)
source("fun_PC_dt.R")
source("preprocess.R")
#library(ncdf4Utils)
#x11(width=8,height=6)
#par(mfrow=c(3,3),mar=c(4,3,4,4))
#par(oma=c(3,3,1,1))
########## GCM Models #####
#####
###########
dir1<-"/gpfs/ahrenshsmfs/user/pothapak/PAPER-2/OBS/"
#dir1<-"/work/scratch/pp28jofe/PAPER-2/OBS/"
nc1<-nc_open(paste(dir1,"HadISST_sst_in_1951_2005.nc",sep=""))
ACC<-ncvar_get(nc1,"sst")
lx_acc<-ncvar_get(nc1,"lon")
ly_acc<-ncvar_get(nc1,"lat")
#ly_acc<-ly_acc[c(180:91,90:1)]
#ACC<-ACC[,c(180:91,90:1),]
lx_acc_lim<-which(lx_acc>40 & lx_acc<120)
#lx_acc_lim<-lx_acc[c(291:360,1:30)]
ly_acc_lim<-which(ly_acc>-20 & ly_acc<20)
###########
tm_acc<-ncvar_get(nc1,"time")
#####
PCC<-ACC[lx_acc_lim,ly_acc_lim,]
PCC1<-pre(PCC)
#####  REMOVING NAN Values #####
for (i in 1:length(PCC1[,1,1])){
 
 for (j in 1:length(PCC1[1,,1])){

if(is.na(PCC1[i,j,1])) {PCC1[i,j,]<- 0}
}}
###############################
##### Reshaping from 90 to -90 to -90 to 90 ###
#####  Zooming the region  ########
########################
#######
#B3<-apply(ERA5,c(1,2),mean) 
#####   Selecting the months JJA  #######
bjja<-numeric();
bjja<-c(6,7,8,9)
#bjja<-c(8,9,10,11)
nm<-(length(tm_acc)/12)*4
for (i in 5:(nm)){
bjja[i]<-bjja[i-4]+12
}

#########     Calculating the Principal Components  #########
B3<-PC(lx_acc[lx_acc_lim],ly_acc[ly_acc_lim],PCC1[,,bjja],bjja)
EOF<-B3$EOF
PC<-B3$PC
PC_obs<-PC
EOF_obs<-EOF*10
#########
#save(PC_obs,EOF_obs,file="iod_PC_obs_JJAS.RData")
####
colorss<-c('white','#fff5f0','#fee0d2','#fcbba1','#fc9272','#fb6a4a','#ef3b2c','#cb181d','#a50f15','#67000d')
colorss<-c('#a50026','#d73027','#f46d43','#fdae61','#fee090','#e0f3f8','#abd9e9','#74add1','#4575b4','#313695')
colorss<-c('#313695','#4575b4','#74add1','#abd9e9','#e0f3f8','#fee090','#fdae61','#f46d43','#d73027','#a50026')
colorss<-c('#67001f','#b2182b','#d6604d','#f4a582','#fddbc7','white','white','#d1e5f0','#92c5de','#4393c3','#2166ac','#053061')
colors<-brewer.pal(10, "Spectral")
colfunc<-colorRampPalette(c("red","yellow","springgreen","royalblue"))
colfunc<-colorRampPalette(c("red","white","royalblue"))

#se<-seq(270,310,(310-270)/100)

se<-seq(-0.6,0.6,1.2/12)
#pdf( "SA.pdf", width = 12, height = 10)
#### Figure 1####
######## Figure 3 #######
par(mar=c(5,4,5,4))
image.plot(lx_acc[lx_acc_lim],ly_acc[ly_acc_lim],EOF*10,xlab=~degree~E,ylab=~degree~N,main='HadISST (EOF2)',col=rev(colorss),breaks=se,cex.main=1.4,cex.axis=2,cex.lab=1.4,legend.args=list(text='', side=4, font=2, line=2.5, cex=1))
box(which = "plot", lty = "solid",lwd=1.4)
world.add<-map("world", xlim=range(lx_acc[lx_acc_lim]), ylim=range(ly_acc[ly_acc_lim]),plot=T ,add=TRUE)
map("world2",col="black",lwd=2,interior=T,add=T)
#dev.print(postscript,'obs_2_iod.eps',width=8,height=6)
#dev.off()
