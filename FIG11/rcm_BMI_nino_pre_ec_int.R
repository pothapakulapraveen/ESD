rm(list=ls())
library(fields)
require(graphics)
require(maps)
library(ncdf4)
library(mapdata)
library(RColorBrewer)
#source("fun_PC.R")
source("preprocess.R")
source("MI_function.R")
source("CMI_function_own.R")
#library(ncdf4Utils)
#x11(width=12,height=6)
#par(mfrow=c(1,3),mar=c(4,3,4,4))
#par(oma=c(3,3,1,1))
########## GCM Models #####
#####
###########
#dir1<-"/gpfs/ahrenshsmfs/user/pothapak/PAPER-2/MPIESM-LR/"
dir1<-"/gpfs/ahrenshsmfs/user/pothapak/PAPER-2/EC-EARTH/"
nc1<-nc_open(paste(dir1,"pr_mon_EC-EARTH_rcp85_r12i1p1_g025_1951_2005_APH_1.nc",sep=""))
ACC<-ncvar_get(nc1,"pr")
lx_acc<-ncvar_get(nc1,"longitude")
ly_acc<-ncvar_get(nc1,"latitude")
#ly_acc<-ly_acc[c(180:91,90:1)]
#ACC<-ACC[,c(180:91,90:1),]
lx_acc_lim<-which(lx_acc>65 & lx_acc<100)
#lx_acc_lim<-lx_acc[c(291:360,1:30)]
ly_acc_lim<-which(ly_acc>5 & ly_acc<40)
###########
tm_acc<-ncvar_get(nc1,"time")
#####
PCC<-ACC[lx_acc_lim,ly_acc_lim,]*2592000
PCC1<-pre(PCC)
#save(
#load("gpcc_preprocess.RData")
bjja<-numeric();
bjja<-c(6,7,8,9)
nm<-(length(tm_acc)/12)*4
for (i in 5:(nm)){
bjja[i]<-bjja[i-4]+12
}
PCC2<-PCC1[,,bjja]
#PCC2<-PCC2[,,201:420]
#save(PCC2,file="ec_preprocess.RData")
##load("ec_preprocess.RData")
#load("gpcc_preprocess.RData")
#####  REMOVING NAN Values #####
load("/gpfs/ahrenshsmfs/user/pothapak/PAPER-2/PLOTS/COMPOSITE/pc_nino_EC.RData")
nino<-PC_ec
load("/gpfs/ahrenshsmfs/user/pothapak/PAPER-2/PLOTS/COMPOSITE/iod_PC_EC_JJAS.RData")
iod<-PC_ec
#PC1_nino<-PC[93:407]*100
##############################
########   Calulating the Information Transfer  ########
result21<-matrix(NA,nrow=length(PCC2[,1,1]),ncol=length(PCC2[1,,1]))
result21_mi<-matrix(NA,nrow=length(PCC2[,1,1]),ncol=length(PCC2[1,,1]))
result21_cmi<-matrix(NA,nrow=length(PCC2[,1,1]),ncol=length(PCC2[1,,1]))
result12_mi<-matrix(NA,nrow=length(PCC2[,1,1]),ncol=length(PCC2[1,,1]))
###########
for (i in 1:length(PCC2[,1,1])){
    for (j in 1:length(PCC2[1,,1])){
if((is.na(PCC2[i,j,1]))){next()}
result21_mi[i,j]<-MI(iod,PCC2[i,j,])
result12_mi[i,j]<-MI(nino,PCC2[i,j,])
result21_cmi[i,j]<-CMI(nino,PCC2[i,j,],iod)
}}
net_syn<- result21_cmi + result21_mi
net_syn_org<- result21_cmi - result12_mi

net_syn_org[which(net_syn_org*100 < -3)]=-0.03
net_syn_org[which(net_syn_org*100 > 3)]=0.03
result21_mi[which(result21_mi*100 >7.5)]=0.075
result12_mi[which(result12_mi*100 >7.5)]=0.075
net_syn[which(net_syn*100 < -3)]=0.075


#result21<-result21_mi + result21_cmi
#########   SEGRIGATING THE PRECIPITATION FOR LANINA and ELNINA  ######
#########
#plot(PC1,col="red",xlab="Time",type="l",ylab="PC & Precipitation",main='',cex.main=1.4,cex.axis=1.2,cex.lab=1.2)
#lines(PCC2,col="blue",type="l",2L,2L,2Li2)

############
#for (i in 1:length(PCC1[,1,1])){
 
# for (j in 1:length(PCC1[1,,1])){

#if(is.na(PCC1[i,j,1])) {PCC1[i,j,]<- 0}
#}}
###############################
##### Reshaping from 90 to -90 to -90 to 90 ###
#####  Zooming the region  ########
########################
#######
#B3<-apply(ERA5,c(1,2),mean) 
#####   Selecting the months JJA  #######
#bjja<-numeric();
#bjja<-c(6,7,8)
#nm<-(length(tm_acc)/12)*3
#for (i in 4:(nm-1)){
#bjja[i]<-bjja[i-3]+12
#}

#########     Calculating the Principal Components  #########
#B3<-PC(lx_acc[lx_acc_lim],ly_acc[ly_acc_lim],PCC1[,,bjja],bjja)
#EOF<-B3$EOF
#PC<-B3$PC
#########
#colorss<-c('white','#fff5f0','#fee0d2','#fcbba1','#fc9272','#fb6a4a','#ef3b2c','#cb181d','#a50f15','#67000d')
#colorss<-c('#a50026','#d73027','#f46d43','#fdae61','#fee090','#e0f3f8','#abd9e9','#74add1','#4575b4','#313695')
#colorss<-c('#313695','#4575b4','#74add1','#abd9e9','#e0f3f8','#fee090','#fdae61','#f46d43','#d73027','#a50026')
colors<-brewer.pal(10, "Spectral")
colfunc<-colorRampPalette(c("red","yellow","springgreen","royalblue","white"))
#colfunc<-colorRampPalette(c("red","white","royalblue"))

se<-seq(0,7.5,7.5/15)

#se<-seq(0,800,800/100)
#pdf( "SA.pdf", width = 12, height = 10)
#### Figure 1####

#              PCC1 is the climatology,  PC_elni , PC_lani , 
######## Figure 3 #######
par(mar=c(3,6,3,3))
image(lx_acc[lx_acc_lim],ly_acc[ly_acc_lim],result21_mi*100,xlab=~degree~E,ylab=~degree~N,main='',col=rev(colfunc(15)),breaks=se,cex.main=1.4,cex.axis=2,cex.lab=1.4,legend.args=list(text='', side=4, font=2, line=2.5, cex=1))
box(which = "plot", lty = "solid",lwd=1.4)
mtext(side=3, "EC-EARTH", col="black", font=2, cex=1.2)
text(x=92, y=36, "I(PREC;IOD)",col="black", font=2, cex=1.0)
world.add<-map("world", xlim=range(lx_acc[lx_acc_lim]), ylim=range(ly_acc[ly_acc_lim]),plot=T ,add=TRUE)
map("world2",col="black",lwd=2,interior=T,add=T)
#dev.print(postscript,'IXYZ_gpcp.eps',width=8,height=6)
par(mar=c(3,4,3,3))
image(lx_acc[lx_acc_lim],ly_acc[ly_acc_lim],result12_mi*100,xlab=~degree~E,ylab=~degree~N,main='',col=rev(colfunc(15)),breaks=se,cex.main=1.4,cex.axis=2,cex.lab=1.4,legend.args=list(text='', side=4, font=2, line=2.5, cex=1))
box(which = "plot", lty = "solid",lwd=1.4)
text(x=90, y=36, "I(PREC;ENSO)",col="black", font=2, cex=1.0)
mtext(side=3, "EC-EARTH", col="black", font=2, cex=1.2)
world.add<-map("world", xlim=range(lx_acc[lx_acc_lim]), ylim=range(ly_acc[ly_acc_lim]),plot=T ,add=TRUE)
map("world2",col="black",lwd=2,interior=T,add=T)
#dev.off()
par(mar=c(3,4,5,8))
image.plot(lx_acc[lx_acc_lim],ly_acc[ly_acc_lim],net_syn*100,xlab=~degree~E,ylab=~degree~N,main='',col=rev(colfunc(15)),breaks=se,cex.main=1.4,cex.axis=2,cex.lab=1.4,legend.args=list(text='', side=4, font=2, line=2.5, cex=1))
box(which = "plot", lty = "solid",lwd=1.4)
text(x=90, y=36, "I(PREC;IOD,ENSO)",col="black", font=2, cex=1.0)
mtext(side=3,"EC-EARTH", col="black", font=2, cex=1.2)
world.add<-map("world", xlim=range(lx_acc[lx_acc_lim]), ylim=range(ly_acc[ly_acc_lim]),plot=T ,add=TRUE)
map("world2",col="black",lwd=2,interior=T,add=T)
########
par(mar=c(3,4,5,8))
se2<-seq(-3,3,6/15)
colfunc2<-colorRampPalette(c("red","white","royalblue"))
image.plot(lx_acc[lx_acc_lim],ly_acc[ly_acc_lim],net_syn_org*100,xlab=~degree~E,ylab=~degree~N,main='',col=rev(colfunc2(15)),breaks=se2,cex.main=1.4,cex.axis=2,cex.lab=1.4,legend.args=list(text='', side=4, font=2, line=2.5, cex=1))
box(which = "plot", lty = "solid",lwd=1.4)
text(x=90, y=36, "NET SYNERGY",col="black", font=2, cex=1.2)
mtext(side=3,"EC-EARTH", col="black", font=2, cex=1.2)
world.add<-map("world", xlim=range(lx_acc[lx_acc_lim]), ylim=range(ly_acc[ly_acc_lim]),plot=T ,add=TRUE)
map("world2",col="black",lwd=2,interior=T,add=T)
