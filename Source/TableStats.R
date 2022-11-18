TableStats<-function(first,second,histfiles,fullnames){
  
  nl<-length(first)
  R2_test<-mae_test<-mae_train<-mae_val<-mae_val_rat<-rep(NA,nl)
  obs<-pred<-list()
  
  for(ll in 1:nl){
    
    print(ll)
    AIEGB <- build_model(as.numeric(first[ll]),as.numeric(second[ll]))
    AIEGB %>% load_model_weights_hdf5(filepath = fullnames[ll])
    test_predictions <- AIEGB %>% predict(test_df %>% dplyr::select(-label))
    
    x<-test_df$label
    y<-test_predictions[ , 1]
    
    pred[[ll]]<-y
    obs[[ll]]<-x
    plot(x,y,xlab="VB obs",ylab="VB Pred"); lines(c(0,1E10),c(0,1E10),col='#ff000050',lwd=2)
    
    R2_test[ll]<-cor(x,y)^2
    mae_test[ll]<-mean(abs(x-y))
    
    legend('bottomright',legend=round(R2_test[ll],3),text.col='red',bty="n") 
    legend('topleft',legend=round( mae_test[ll],3),text.col='blue',bty="n") 
    legend('top',legend=paste(first[ll],"-",second[ll]),text.col='black',bty="n") 
    
    hist<-readRDS(histfiles[ll])
    nts<-length(hist$metrics$mean_absolute_error)
    ind<-nts-(9:0)
    
    mae_train[ll]<-mean(hist$metrics$mean_absolute_error[ind])
    mae_val[ll]<-mean(hist$metrics$val_mean_absolute_error[ind])
    
    mae_val_rat[ll]<-mean(hist$metrics$mean_absolute_error[ind]/hist$metrics$val_mean_absolute_error[ind])
    legend('bottom',legend=round(mae_val_rat[ll],3),text.col='green',bty="n") 
    
  }
  list(pred=pred, obs=obs, R2_test=R2_test,
       mae_test=mae_test, mae_train=mae_train, mae_val=mae_val,
       mae_val_rat=mae_val_rat,firsty=first,secondy=second)
}


maketab<-function(out,rem1=c(20,18,16)){
  tab<-data.frame(First=out$firsty, Second=out$secondy,MAE_train = out$mae_train,
                  MAE_val=out$mae_val, 
                  MAE_test=out$mae_test, R2_test = out$R2_test,MAE_ratio = out$mae_val_rat )
 
  tab<-tab[!(tab$First%in%rem1),]
  tab[order(tab$MAE_test),] 
}
