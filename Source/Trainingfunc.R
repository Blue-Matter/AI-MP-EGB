
trainfunc<-function(layering,savdir="./Fits",epochs=30,code=""){
  
  nl<-nrow(layering)
  mse<-cory<-rep(NA,nl)
  
  for(ll in 1:nl){ # ll<-17
    
   
    firsty<-layering[ll,1]
    secondy<-layering[ll,2]
    AIEGB <- build_model(firsty,secondy)
    
    history <- AIEGB %>% fit(
      x = train_df %>% dplyr::select(-label),
      y = train_df$label,
      epochs = epochs,
      validation_split = 0.2,
      verbose = 2
    )
    
    test_predictions <- AIEGB %>% predict(test_df %>% dplyr::select(-label))
    x<-test_df$label
    y<-test_predictions[ , 1]
    
    plot(x,y,xlab="VB obs",ylab="VB Pred"); lines(c(0,1E10),c(0,1E10),col='#ff000050',lwd=2)
    cory[ll]<-cor(x,y)^2
    mse[ll]<-(1/length(x))*sum((x-y)^2)
    print(paste("1st =",firsty,"  2nd =",secondy,"   cor =",cory[ll],"   mse =",mse[ll]))
    
    saveRDS(history,paste0(savdir,"/history_",firsty,"_",secondy,"_fds.rda"))
    save_model_weights_hdf5(AIEGB, paste0(savdir,"/AIEGB_",firsty,"_",secondy,"_wts_fds.h5"))
    print(paste(ll,"of",nl, "completed"))
  }
}
