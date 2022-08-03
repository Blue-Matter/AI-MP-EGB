#### Table code
setwd("C:/Users/tcarruth/Documents/GitHub/AI-MP-EGB")
setwd("C:/GitHub/AI-MP-EGB")

# Table 1. Index statistics

sddat<-readRDS("./Sim_Data/sddat.rda")
apply(sddat,2,mean)


# Table 2.  performance of alternative neural network designs

dotrain=F
source("./4_Train_ANN.R")

layering0<-expand.grid(c(8,12,16,20),c(4,8,12,16))

layering<-layering0[layering0[,2]<=layering0[,1],]
layering<-rbind(cbind(c(10,20,30,40,50,60),rep(0,6)),layering)
nl<-nrow(layering)


Fits<-paste0("./Fits/AIEGB_",layering[,1],"_",layering[,2],"_wts.h5")
cory<-mse<-rep(NA,nl)
par(mfrow=c(5,5),mai=c(0.1,0.1,0.1,0.1))
for(ll in 1:nl){
  
  firsty<-layering[ll,1]
  secondy<-layering[ll,2]
  AIEGB <- build_model(firsty,secondy)

  AIEGB %>% load_model_weights_hdf5(filepath = Fits[ll])
  test_predictions <- AIEGB %>% predict(test_df %>% dplyr::select(-label))
  plot(test_df$label,test_predictions[ , 1],xlab="VB obs",ylab="VB Pred"); lines(c(0,1E10),c(0,1E10),col='#ff000050',lwd=2)
  x<-test_df$label
  y<-test_predictions[ , 1]
  cory[ll]<-cor(x,y)^2
  mse[ll]<-(1/length(x))*sum((x-y)^2)
  legend('bottomright',legend=round(cor(x,y)^2,3),text.col='red',bty="n") 
  legend('topleft',legend=round( mse[ll],3),text.col='blue',bty="n") 
  
}


cbind(layering,cory,mse)

Fits<-c("./Fits/AIEGB_12_8_wts.h5","./Fits/AIEGB_12_8_wts_ut.h5")
trans<-c(T,F)
nl<-length(Fits)
cory<-mse<-rep(NA,nl)
par(mfrow=c(2,2),mai=c(0.1,0.1,0.1,0.1))

for(ll in 1:nl){
  
  firsty<-layering[ll,1]
  secondy<-layering[ll,2]
  AIEGB <- build_model(12,8)
  
  AIEGB %>% load_model_weights_hdf5(filepath = Fits[ll])
  test_predictions <- AIEGB %>% predict(test_df %>% dplyr::select(-label))
  if(trans[ll])y<-exp(test_predictions[ , 1])
  if(!trans[ll])y<-test_predictions[ , 1]
  x<-test_df$label
  plot(x,y,xlab="VB obs",ylab="VB Pred"); lines(c(0,1E10),c(0,1E10),col='#ff000050',lwd=2)
  
 
  cory[ll]<-cor(x,y)^2
  mse[ll]<-(1/length(x))*sum((x-y)^2)
  legend('bottomright',legend=round(cor(x,y)^2,3),text.col='red',bty="n") 
  legend('topleft',legend=round( mse[ll],3),text.col='blue',bty="n") 
  
}

