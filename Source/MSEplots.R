# MSE plots


TplotAI<-function(MSEtest,proyears=30,nHy=51){
  cols=c("black","red","green","blue")
  muC<-apply(MSEtest@Catch,2,mean)
  muSSB<-apply(MSEtest@SSB,2,mean)
  SSB0<-MSEtest@RefPoint$Dynamic_Unfished$SSB0[,nHy+1:proyears]
  ind<-MSEtool:::TEG(dim(MSEtest@SSB))
  SSBrel<-array(NA,dim(MSEtest@SSB))
  SSBrel[ind]<-MSEtest@SSB[ind]/SSB0[ind[,c(1,3)]]
  SSBrelmu<-apply(SSBrel,3:2,mean)
  par(mfrow=c(1,2),mai=c(0.5,0.5,0.01,0.01))
  matplot(SSBrelmu,type="l",col=rep(cols,each=3),lty=rep(1:3,4))
  plot(muSSB,muC ,col='white')
  text(muSSB,muC,cex=0.8,MSEtest@MPs,col=rep(cols,each=3))
}


