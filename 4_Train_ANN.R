
# ================================================================================================================================================
# === Designing an AI MP for EGB Haddock =========================================================================================================
# ================================================================================================================================================

library(openMSE)


# 3. Process Simulation Data

# Installation

install=F
if(install){
  install.packages("tensorflow")
  install.packages("keras")
  install.packages("Rcpp")
  library("Rcpp")
  install.packages("devtools")
  install.packages("reticulate") #devtools::install_github("rstudio/reticulate", force=TRUE)
  devtools::install_github("r-lib/processx")
  library(processx)
  devtools::install_github("rstudio/tensorflow")
  devtools::install_github("rstudio/keras")
  library(keras)
  install_keras(tensorflow = "cpu")
  #install_keras(method = c("auto", "virtualenv", "conda"), conda = "auto",  tensorflow = "gpu", extra_packages = NULL)
  library(tensorflow)
  install_tensorflow(version="cpu")
  install.packages('tfdatasets')
  install.packages('progress')
}

# Prerequisites

library("Rcpp")
library(processx)
library(keras)
library(tensorflow)
library(reticulate)
library(dplyr)
library(tfdatasets)
library(ggplot2)
library(progress)

#setwd("C:/Users/tcarruth/Documents/GitHub/AI-MP-EGB")
#setwd("C:/GitHub/AI-MP-EGB")

# https://tensorflow.rstudio.com/tutorials/beginners/basic-ml/tutorial_basic_regression/

# Load data

# ==============================================================================================================================================
# ===== EGB ===================================================================================================================================
# ==============================================================================================================================================

# --- Load and format training data -------------------------------------
TDsmall=F
source("./Source/make_train_data.r")


# --- AI Model builder -------------------------------------

source("./Source/build_model.r")


# --- Loop over layering options -----------------------

# layering0<-expand.grid(c(10,20,30),c(0,6,12))
# layering0<-expand.grid(c(4,6,8),c(2,4,6))

layering0 <-expand.grid(c(8,10,12,14,16,18,20),c(0,2,4,6,8));   epochs=30;  code=""
#layering0 <-cbind(c(12,20,8,10,10,12),c(4,0,2,6,4,2)); epochs=250; code="_250"
#layering0 <-cbind(c(20,20,12,12,14,14),c(2,4,6,8,2,4)); epochs=250; code="_250"
#layering0 <-cbind(c(20,20,12,12,14,14),c(2,4,6,8,2,4)); epochs=50; code=""

layering<-layering0[layering0[,2]<=layering0[,1],]
nl<-nrow(layering)
mse<-cory<-rep(NA,nl)

# Train models
af<-"relu"
logy<-T
if(dotrain){
  for(ll in 1:nl){ # ll<-17

    firsty<-layering[ll,1]
    secondy<-layering[ll,2]
    AIEGB <- build_model(firsty,secondy,af)

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
    if(logy){
      x<-exp(test_df$label)
      y<-exp(test_predictions[ , 1])
    }
    plot(x,y,xlab="VB obs",ylab="VB Pred"); lines(c(0,1E10),c(0,1E10),col='#ff000050',lwd=2)
    cory[ll]<-cor(x,y)^2
    mse[ll]<-(1/length(x))*sum((x-y)^2)
    print(paste("1st =",firsty,"  2nd =",secondy,"   cor =",cory[ll],"   mse =",mse[ll]))

    saveRDS(history,paste0("./Fits/history_",firsty,"_",secondy,"_fds",code,".rda"))
    save_model_weights_hdf5(AIEGB, paste0("./Fits/AIEGB_",firsty,"_",secondy,"_wts_fds",code,".h5"))
    print(paste(ll,"of",nl, "completed"))
  }
}

# ---- 150 epochs -----------------------------------------------------------------------

layering <-cbind(c(20,18,16,20,18,14,16,18,14,16,18,8,12,18,16),
                  c(8,8,4,2,6,6,8,4,8,0,0,4,8,2,6))

epochs=60; code="_150"
nl<-nrow(layering)
mse<-cory<-rep(NA,nl)

# Train models
af<-"relu"
logy<-T
if(dotrain){
  for(ll in 1:nl){ # ll<-17
    
    firsty<-layering[ll,1]
    secondy<-layering[ll,2]
    AIEGB <- build_model(firsty,secondy,af)
    
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
    if(logy){
      x<-exp(test_df$label)
      y<-exp(test_predictions[ , 1])
    }
    plot(x,y,xlab="VB obs",ylab="VB Pred"); lines(c(0,1E10),c(0,1E10),col='#ff000050',lwd=2)
    cory[ll]<-cor(x,y)^2
    mse[ll]<-(1/length(x))*sum((x-y)^2)
    print(paste("1st =",firsty,"  2nd =",secondy,"   cor =",cory[ll],"   mse =",mse[ll]))
    
    saveRDS(history,paste0("./Fits_150/history_",firsty,"_",secondy,"_fds",code,".rda"))
    save_model_weights_hdf5(AIEGB, paste0("./Fits_150/AIEGB_",firsty,"_",secondy,"_wts_fds",code,".h5"))
    print(paste(ll,"of",nl, "completed"))
  }
}


# ----- test of min-max scaler -------------------------------------

# --- Load and format training data -------------------------------------
dotrain=T; TDsmall=F
source("./Source/make_train_data_minmax.r")


# --- AI Model builder -------------------------------------

source("./Source/build_model.r")
layering0 <-expand.grid(c(8,10,12,14,16,18,20),c(0,2,4,6,8));   epochs=30;  code=""

layering<-layering0[layering0[,2]<=layering0[,1],]
nl<-nrow(layering)
mse<-cory<-rep(NA,nl)

# Train models
af<-"relu"
logy<-T
if(dotrain){
  for(ll in 1:nl){ # ll<-17
    
    firsty<-layering[ll,1]
    secondy<-layering[ll,2]
    AIEGB <- build_model(firsty,secondy,af)
    
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
    if(logy){
      x<-exp(test_df$label)
      y<-exp(test_predictions[ , 1])
    }
    plot(x,y,xlab="VB obs",ylab="VB Pred"); lines(c(0,1E10),c(0,1E10),col='#ff000050',lwd=2)
    cory[ll]<-cor(x,y)^2
    mse[ll]<-(1/length(x))*sum((x-y)^2)
    print(paste("1st =",firsty,"  2nd =",secondy,"   cor =",cory[ll],"   mse =",mse[ll]))
    
    saveRDS(history,paste0("./Fits_minmax/history_",firsty,"_",secondy,"_fds",code,".rda"))
    save_model_weights_hdf5(AIEGB, paste0("./Fits_minmax/AIEGB_",firsty,"_",secondy,"_wts_fds",code,".h5"))
    print(paste(ll,"of",nl, "completed"))
  }
}

