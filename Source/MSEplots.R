# MSE plots


TplotAI<-function(MSEtest,proyears=30,nHy=51,lp=c(0.25,0.75)){
  cols=c("black","red","green","blue","purple")
  muC<-apply(MSEtest@Catch,2,mean)
  #muSSB<-apply(MSEtest@SSB,2,mean)
  SSB0<-MSEtest@RefPoint$Dynamic_Unfished$SSB0[,nHy+1:proyears]
  ind<-MSEtool:::TEG(dim(MSEtest@SSB))
  SSBrel<-array(NA,dim(MSEtest@SSB))
  SSBrel[ind]<-MSEtest@SSB[ind]/SSB0[ind[,c(1,3)]]
  SSBrelmu<-apply(SSBrel,3:2,mean)
  par(mfrow=c(1,2),mai=c(0.5,0.5,0.01,0.01))
  muSSB<-apply(SSBrelmu,2,mean)
  matplot(SSBrelmu,type="l",col=rep(cols,each=3),lty=rep(1:3,4))
  
  lpC<-apply(MSEtest@Catch,2,quantile,p=lp)
  lpSSB<-apply(SSBrelmu,2,quantile,p=lp)
  xlim=range(lpSSB); ylim=range(lpC)
  plot(muSSB,muC ,col='white',xlim=xlim,ylim=ylim)
  text(muSSB,muC,cex=0.8,MSEtest@MPs,col=rep(cols,each=3))
 
  for(i in 1:length(muSSB)){lines(c(muSSB[i],muSSB[i]),lpC[,i],col=rep(cols,each=3)[i]); lines(lpSSB[,i],c(muC[i],muC[i]),col=rep(cols,each=3)[i])} 
  
  #lpC<-apply(MSEtest@Catch,2,quantile,p=lp)
  #lpSSB<-apply(MSEtest@SSB,2,quantile,p=lp)
  #plot(lpSSB,lpC ,col='white')
  #text(lpSSB,lpC,cex=0.8,MSEtest@MPs,col=rep(cols,each=3))
  
  
}


