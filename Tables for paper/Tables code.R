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
R2_test<-mae_test<-mae_train<-mae_val<-mae_val_rat<-rep(NA,nl)

obs<-pred<-list()
par(mfrow=c(4,5),mai=c(0.1,0.1,0.1,0.05))

for(ll in 1:nl){
  
  AIEGB <- build_model(as.numeric(firsty[ll]),as.numeric(secondy[ll]))
  AIEGB %>% load_model_weights_hdf5(filepath = fullnames[ll])
  test_predictions <- AIEGB %>% predict(test_df %>% dplyr::select(-label))
  
  x<-test_df$label
  y<-test_predictions[ , 1]
  pred[[ll]]<-y
  obs[[ll]]<-x
  plot(x,y,xlab="VB obs",ylab="VB Pred",xlim=c(-1,4),ylim=c(-1,4)); lines(c(0,1E10),c(0,1E10),col='#ff000050',lwd=2)
  
  R2_test[ll]<-cor(x,y)^2
  mae_test[ll]<-mean(abs(x-y))
  
  legend('bottomright',legend=round(R2_test[ll],3),text.col='red',bty="n") 
  legend('topleft',legend=round( mae_test[ll],3),text.col='blue',bty="n") 
  legend('top',legend=paste(firsty[ll],"-",secondy[ll]),text.col='black',bty="n") 
  
  hist<-readRDS(histfiles[ll])
  nts<-length(hist$metrics$mean_absolute_error)
  mae_train[ll]<-hist$metrics$mean_absolute_error[nts]
  mae_val[ll]<-hist$metrics$val_mean_absolute_error[nts]
  ind<-nts-(9:0)
  mae_val_rat[ll]<-mean(hist$metrics$mean_absolute_error[ind]/hist$metrics$val_mean_absolute_error[ind])
  legend('bottom',legend=round(mae_val_rat[ll],3),text.col='green',bty="n") 
  
  
}

saveRDS(list(pred=pred, obs=obs, R2_test=R2_test,
             mae_test=mae_test, mae_train=mae_train, mae_val=mae_val,
             mae_val_rat=mae_val_rat,firsty=firsty,secondy=secondy),
        "./Results/Fits/Summary.rda")


# make table

out<-readRDS("./Results/Fits/Summary.rda")

tab<-data.frame(First=out$firsty, Second=out$secondy,MAE_train = out$mae_train,
                MAE_val=out$mae_val, MAE_ratio = out$mae_val_rat, 
                MAE_test=out$mae_test, R2_test = out$R2_test)





