#### Table code
setwd("C:/Users/tcarruth/Documents/GitHub/AI-MP-EGB")
setwd("C:/GitHub/AI-MP-EGB")

# Table 1. Index statistics

sddat<-readRDS("./Sim_Data/sddat.rda")


# Table 2.  performance of alternative neural network designs

dotrain=F
source("./4_Train_ANN.R")
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