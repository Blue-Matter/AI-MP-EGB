#### Table code
setwd("C:/Users/tcarruth/Documents/GitHub/AI-MP-EGB")
setwd("C:/GitHub/AI-MP-EGB")

# Table 1. Index statistics

sddat<-readRDS("./Sim_Data/sddat.rda")
apply(sddat,2,mean)


# Table 2.  performance of alternative neural network designs

dotrain=F; source("./4_Train_ANN.R")

source("./Source/build_model.r")
Filenames<-list.files("./Fits")
fullnames<-list.files("./Fits",full.names=T)
Filenames<-Filenames[grepl('AIEGB',Filenames)]
firsty<-sapply(Filenames,function(x)strsplit(x,"_")[[1]][2])
secondy<-sapply(Filenames,function(x)strsplit(x,"_")[[1]][3])
histfiles<-paste0(getwd(),"/Fits/history_",firsty,"_",secondy,"_fds.rda")
# cbind(Filenames, firsty,secondy,histfiles)
nl<-length(Filenames)
cory<-mse<-maerat<-rep(NA,nl)

obs<-pred<-list()
par(mfrow=c(5,5),mai=c(0.1,0.1,0.1,0.05))
for(ll in 1:nl){
  
  AIEGB <- build_model(as.numeric(firsty[ll]),as.numeric(secondy[ll]))
  AIEGB %>% load_model_weights_hdf5(filepath = fullnames[ll])
  test_predictions <- AIEGB %>% predict(test_df %>% dplyr::select(-label))
  
  x<-test_df$label
  y<-test_predictions[ , 1]
  pred[[ll]]<-y
  obs[[ll]]<-x
  plot(x,y,xlab="VB obs",ylab="VB Pred"); lines(c(0,1E10),c(0,1E10),col='#ff000050',lwd=2)
  
  cory[ll]<-cor(x,y)^2
  mse[ll]<-(1/length(x))*sum((x-y)^2)
  
  legend('bottomright',legend=round(cor(x,y)^2,3),text.col='red',bty="n") 
  legend('topleft',legend=round( mse[ll],3),text.col='blue',bty="n") 
  legend('top',legend=paste(firsty[ll],"-",secondy[ll]),text.col='black',bty="n") 
  
  hist<-readRDS(histfiles[ll])
  mae<-hist$metrics$mean_absolute_error
  mae_val<-hist$metrics$val_mean_absolute_error
  ind<-length(mae)-(9:0)
  maerat[ll]<-mean(mae[ind]/mae_val[ind])
  legend('bottom',legend=round(maerat[ll],3),text.col='green',bty="n") 
  
  
}

saveRDS(list(pred=pred,obs=obs,cory=cory,mse=mse,mae=mae,mae_val=mae_val,maerat=maerat),"./Results/Fits/Summary.rda")

