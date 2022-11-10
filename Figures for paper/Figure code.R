### Figures for paper
setwd("C:/Users/tcarruth/Documents/GitHub/AI-MP-EGB")
setwd("C:/GitHub/AI-MP-EGB")


# Figure 1 Projections of catch and biomass for the 6 MPs

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

# Figure 2, vulnerable biomass values used in training
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
