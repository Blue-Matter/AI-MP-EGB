### Figures for paper
setwd("C:/Users/tcarruth/Documents/GitHub/AI-MP-EGB")
setwd("C:/GitHub/AI-MP-EGB")


# --- Figure 1 Projections of catch and biomass for the 6 MPs -----------------------------

files<-paste0("./MSEs/Run_1_",1:6,".rda")

projplot<-function(MSE,sloty="Catch",ylim=NA,sims=1:2,xax=F){
  
  dat<-slot(MSE,sloty)[,1,]/1000
  qs<-apply(dat,2,quantile,p=c(0.1,0.25,0.5,0.75,0.9))
  yrs<-2020:2049
  ny<-length(yrs)
  x[x==Inf]<-NA
  if(is.na(ylim))ylim=max(qs)
  plot(range(yrs),c(0,ylim),col="white",xlab="",ylab="",yaxs="i",axes=F)
  axis(2)
  if(xax)axis(1)
  if(!xax)axis(1,at=seq(2015,2050,by=5),labels=NA)
  grid()
  polygon(c(yrs,yrs[ny:1]),c(qs[1,],qs[5,ny:1]),border=NA,col='light grey')
  polygon(c(yrs,yrs[ny:1]),c(qs[2,],qs[4,ny:1]),border=NA,col='grey')
  lines(yrs,qs[3,],lwd=2,col="white")
  for(i in 1:2)lines(yrs,dat[sims[i],],col="black",lty=i)
  
}

MPnams<-paste(rep(c("F=0.2","F=0.4","F=0.6"),2),rep(c("CV=0.1","CV=0.3"),each=3))

jpeg("./Figures for paper/Figure 1.jpg",res=600, width=6,height=8,units="in")

  par(mfrow=c(6,2),mai=c(0.1,0.3,0.2,0.01),omi=c(0.4,0.2,0.2,0.01))
  sims<-c(14,25)
  
  for(i in 1:6){
    MSE<-readRDS(files[i])
    projplot(MSE,ylim=12,sims=sims,xax=(i==6))
    mtext(MPnams[i],line=0.1,cex=0.85,adj=0.1)
    projplot(MSE,sloty="VB",ylim=45,sims=sims,xax=(i==6))
  }
  mtext("Year",1,line=1.8,outer=T,cex=0.9)
  mtext("Kilotonnes",2,line=0.3,outer=T,cex=0.9)
  mtext(c("Catch","Vulnerable Biomass"),3,at=c(0.3,0.8),line=0.2,cex=0.9,outer=T)

dev.off()


# --- Figure 2, vulnerable biomass values used in training ------------------------------

TD<-readRDS("C:/temp/Sim_Data/simdataL4.rda")

keep<-apply(TD,1,function(x)!is.na(sum(x)))&apply(TD,1,function(x)!any(x==-Inf))
sum(keep)/length(keep)
TD<-TD[keep,]
high<-array(rep(apply(TD,2,quantile,p=0.995),each=nrow(TD)),dim(TD))
keep2<-apply(TD<high,1,sum)==ncol(TD)
sum(keep2)/length(keep2)
TD<-TD[keep2,]

keep3<-TD[,1]<40000 & TD[,1]>100
sum(keep3)/length(keep3)
TD<-TD[keep3,]
TD[,1]<-TD[,1]/1000

jpeg("./Figures for paper/Figure 2.jpg",res=600, width=7,height=3,units="in")
  par(mfrow=c(1,2),mai=c(0.7,0.7,0.2,0.1))  
  hist(TD[,1],xlab="",ylab="",border="white",main="")
  mtext("Simulated Vulnerable Biomass (kt)",1,line=2.3)
  mtext("Frequency",2,line=2.5)
  hist(log(TD[,1]),xlab="",ylab="",border="white",main="")
  mtext("Log Simulated Vulnerable Biomass (kt)",1,line=2.3)
dev.off()


# --- Figure 3 ---- Simulated data


MSE<-readRDS("MSEs/Run_1_1.rda")
PPD<-MSE@PPD[[1]]
keep<-c(1,2,3,8,22)
Indlab<-c("Age 2","Age 3","Age 4","Age 9+","All ages")
yrs<-47:64
ylab=(2020-(51-47)):(2020+(64-51))
sims<-c(2,3)
tyr<-16
tyrlab<-ylab[tyr]
yrind<-tyr-c(9,8,7,1)
timelab=c("t-9","t-8","t-7","t-1")
yrlabs<-ylab[yrind]
cols<-c("#ff000090","#0000ff90")
Ind<-PPD@AddInd[sims,keep,yrs]
Inds<-PPD@AddInd[sims,1:8,yrs]
Ind[,5,]<-apply(Inds,c(1,3),sum)
VB<-cbind(MSE@SSB_hist[sims,47:51],MSE@SSB[sims,1,1:13])*1.4

mat<-matrix(c(c(1,1,22,23,24),
              c(45,46,47,48,49),
              1+ c(1,2,3,24,4,
              5, 6, 7, 25,8,
              9, 10,11,26,12,
              28,29,30,31,32,
              13,14,15,27,16,
              17,18,19,33,20,
              34,35,36,37,38,
              39,40,41,42,43)),byrow=T,nrow=10)


Iplot<-function(Idat,ylab,yr,yri,cols,tlab,tyr,type="p",dox=F,doy=F){
  inc<-(max(Idat,na.rm=T)-min(Idat,na.rm=T))/15
  matplot(ylab,t(Idat),type='l',col="white",lty=1,yaxs="i",ylim=range(Idat,na.rm=T)+c(-inc,inc),axes=F)
  ats<-seq(2000,3000,by=5)
  atsy<-(-10:100)
  if(!dox)axis(1,ats,rep(NA,length(ats)))
  if(dox)axis(1,ats,ats)
  if(!doy)axis(2,atsy,rep(NA,length(atsy)))
  if(doy)axis(2,atsy,atsy,cex.axis=0.8)
  #axis(2)
   grid()
  if(type=="p"){
    abline(v=c(2020.5,yr,tlab),lty=c(1,1,2),col=c("black","green","black"))
    arrows(2020.5,max(Idat,na.rm=T),2022.5,max(Idat,na.rm=T),length=0.05)
    
    matplot(ylab,t(Idat),col=cols,lty=1,type=type,pch=19,add=T,cex=0.65)
    points(rep(yr,2),Idat[,yri],col=cols,pch=19,cex=1.05)
  }else{
    abline(v=c(2020.5,tlab),lty=c(1,2))
    arrows(2020.5,max(Idat,na.rm=T),2022.5,max(Idat,na.rm=T),length=0.05)
    
    matplot(ylab,t(Idat),col=cols,lty=1,type=type,pch=19,add=T,cex=0.65)
    points(rep(tlab,2),Idat[,tyr],col=cols,pch=19,cex=1.05)
  }
  
}
nullplot<-function()plot(1,1,col='white',axes=F,xlab="",ylab="")

liney=0.25
lineyY=2.2
lcex=0.85

jpeg("./Figures for paper/Figure 3.jpg",res=600, width=7,height=8,units="in")
  
  par(mai=c(0.1,0.2,0.01,0.01), omi=c(0.1,0.3,0.3,0.01))
  layout(mat,widths=c(1,1,1,0.35,1),heights=c(1.5,0.75,1,1,1,0.35,1,1,0.5,0.35))
  
  Iplot(Idat=log(VB),ylab,dox=T,doy=T,yr=yrlabs[y],yri=yrind[y],cols=cols,tlab=tlab,tyr=tyr,type="l")
  mtext("Simulated 'true' vulnerable biomass (log)(output layer)     ",cex=0.8,font=2,line=1)
  text(2025.4,11.82,"Projection years",cex=1)
  text(2031.5,11.8,"t",cex=1,font=2)
  j<-0
  for(i in 1:5){for(y in 1:4){
      j<-j+1
      Iplot(Idat=log(Ind[,i,]),ylab,doy=(y==1|y==4),dox=(i==5|i==3),yr=yrlabs[y],yri=yrind[y],col=cols,tlab=tlab,tyr=tyr,type="p")
      if(j==1)mtext("Survey index 1 (log)(input layer)   ",cex=0.8,font=2,line=1.5)
      if(i==1)mtext(timelab[y],line=liney,cex=lcex,col='green')
      if(y==1)mtext(Indlab[i],2,line=lineyY,cex=lcex)
  }}
  
  for(np in 1:3)nullplot()
  legend('center',legend=paste("Simulation",1:2),text.col=c('red','blue'),bty='n')
  nullplot()
  mtext("...",line=liney+0.1,cex=lcex,at=0.8)
  for(np in 1:4)nullplot()
  mtext("...",line=lineyY+0.1,cex=lcex,side=2)
  for(np in 1:6)nullplot()
  mtext("Survey index 2 (log)(input layer) ...",cex=0.8,font=2,line=-3)
  for(np in 1:5)nullplot()
  mtext("Survey index 3 (log)(input layer) ...",cex=0.8,font=2,line=-2)

dev.off()


# ---- Figure 4 plotting network convergence --------------


#dotrain=F; source("./4_Train_ANN.R")

#source("./Source/build_model.r")
Filenames<-list.files("./Fits")
fullnames<-list.files("./Fits",full.names=T)
Filenames<-Filenames[grepl('AIEGB',Filenames)]
first<-sapply(Filenames,function(x)strsplit(x,"_")[[1]][2])
second<-sapply(Filenames,function(x)strsplit(x,"_")[[1]][3])
histfiles<-paste0(getwd(),"/Fits/history_",first,"_",second,"_fds.rda")
# cbind(Filenames, firsty,secondy,histfiles)
nl<-length(Filenames)
R2_test<-mae_test<-mae_train<-mae_val<-mae_val_rat<-rep(NA,nl)

obs<-pred<-list()

plothist<-function(hist,lab="",dox=F,doy=F,cols=c("#ff000090","#0000ff90"),ylim=c(0.19,0.25)){
  
  dat<-cbind(hist$metrics$mean_absolute_error,hist$metrics$val_mean_absolute_error)
  ind<-nrow(dat)-(9:0)
  maes<-apply(dat[ind,],2,mean)
  ytick<-seq(0,1,by=0.01)
  xtick<-seq(0,1000,by=10)
  matplot(dat,type="l",lty=1,col="white",ylim=ylim,axes=F)
  grid()
  abline(v=nrow(dat)-9.5,col="darkgrey")
  abline(h=0.2,col="grey")
  matplot(dat,type="l",lty=1,col=cols,add=T)
  matplot(dat,type="p",add=T,pch=19,cex=0.8,col=cols)
  if(dox)axis(1,xtick,xtick)
  if(dox)mtext("Epoch",1,line=2.3,cex=0.9)
  if(!dox)axis(1,xtick,rep(NA,length(xtick)))
  if(doy)axis(2,ytick,ytick)
  if(!doy)axis(2,ytick,rep(NA,length(ytick)))
  mtext(lab,adj=1.5,line=0.1,cex=0.85)
  legend('topright',paste("MAE =",rev(format(round(maes,3),nsmall=3))),cex=0.8,text.col=c("blue","red"),bty='n')
  
}

plotcor<-function(x,y,dox=F,doy=F,lims=c(-0.5,4)){
  #x<-seq(0,4,length.out=100)
  #y<-x*rlnorm(100,0,0.2)
  R2<-cor(x,y)^2
  mae<-mean(abs(x-y))
  plot(x,y,col="white",axes=F,xlim=lims,ylim=lims)
  grid()
  lines(c(-1E10,1E10),c(-1E10,1E10),col="darkgrey")
  points(x,y,col="#00ff0030",pch=19,cex=0.5)
  xtick<-ytick<-(-4:10)
  if(dox)axis(1,xtick,xtick)
  if(dox)mtext("Obs. log(VB)",1,line=2.3,cex=0.9,col="dark green")
  if(!dox)axis(1,xtick,rep(NA,length(xtick)))
  if(doy)axis(2,ytick,ytick)
  if(!doy)axis(2,ytick,rep(NA,length(ytick)))
  legend('topleft',legend=paste0("MAE = ",format(round(mae,3), nsmall = 3)),bty='n',cex=0.8,text.col="darkgreen")
  legend('bottomright',legend=paste0("R2 = ",format(round(R2,3),nsmall=3)),bty='n',cex=0.8,text.col="darkgreen")
}


mat0<-matrix(1:30,ncol=6,byrow=T)
mat<-rbind(31:38,cbind(mat0[,1:2],39:43,mat0[,3:4],44:48,mat0[,5:6]))


jpeg("./Figures for paper/Figure 4.jpg",res=600, width=7.5,height=8,units="in")
  
  par(mai=c(0.1,0.1,0.25,0.01))
  par(oma=c(2.6,2.6,0.01,2.2))
  
  layout(mat,widths=c(1,0.66,0.1,1,0.66,0.1,1,0.66),heights=c(0.4,1,1,1,1,1))
  
  j<-0
  ind<-29:43
  namy<-paste0("(",letters[1:15],") ",first[ind]," - ",second[ind])
  summ<-readRDS("Results/Fits/Summary.rda")
  if(!(all(summ$firsty==first)&all(summ$secondy==second)))print("!!! WARNING order mismatch !!!")
  for(ll in ind){
    j<-j+1
    hist<-readRDS(histfiles[ll])
    plothist(hist,dox=(j>12),doy=(j%in%c(1,4,7,10,13)),lab=namy[j])
    plotcor(x=summ$obs[[ll]],y=summ$pred[[ll]],dox=(j>12))
    
  }
  nullplot<-function()plot(1,1,col='white',axes=F,xlab="",ylab="")
  for(i in 1:6)nullplot()
    nullplot()
    legend('left',legend=c(" Training","Validation"),text.col=c("red","blue"),bty='n',cex=0.9,text.font=2)
    nullplot()
    legend('left',legend=c("Testing"),text.col="dark green",bty='n',cex=0.9,text.font=2)
    
  
  
  mtext("Mean Absolute Error (MAE)",2,line=1.5,outer=T,cex=0.9)
  mtext("Predicted log(VB)",4,line=0.5,outer=T,cex=0.9,srt=180,col="dark green")

dev.off()


