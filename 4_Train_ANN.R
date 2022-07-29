
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
setwd("C:/GitHub/AI-MP-EGB")

# https://tensorflow.rstudio.com/tutorials/beginners/basic-ml/tutorial_basic_regression/

# Load data

# ==============================================================================================================================================
# ===== EGB ===================================================================================================================================
# ==============================================================================================================================================

# --- Load and format data -------------------------------------

TD<-readRDS("C:/temp/Sim_Data/simdataL3.rda")
keep<-apply(TD,1,function(x)!is.na(sum(x)))&apply(TD,1,function(x)!any(x==-Inf))
sum(keep)/length(keep)
TD<-TD[keep,]
high<-array(rep(apply(TD,2,quantile,p=0.99),each=nrow(TD)),dim(TD))
keep2<-apply(TD<high,1,sum)==ncol(TD)
sum(keep2)/length(keep2)
TD<-TD[keep2,]

hist(TD[,1])
TD<-TD[TD[,1]<40000,]
hist(TD[,1])
TD<-TD[TD[,1]>100,]
#TD<-log(TD)
hist(TD[,1])
#TD<-TD[1:50000,]
TD[,1]<-TD[,1]/1000

# log everything
TD<-log(TD)

nr<-nrow(TD)
nc<-ncol(TD)
ind<-(1:nr)%in%sample(1:nr,floor(nr*0.05),replace=FALSE)

train_data<-TD[!ind,2:nc]
train_labels<-TD[!ind,1]
test_data<-TD[ind,2:nc]
test_labels<-TD[ind,1]

# Check for NAs # for(i in 1:ncol(train_data))print(sum(is.na(train_data)))

column_names <- colnames(TD)[2:nc]

train_df <- train_data %>%
  as_tibble(.name_repair = "minimal") %>%
  setNames(column_names) %>%
  mutate(label = train_labels)

test_df <- test_data %>%
  as_tibble(.name_repair = "minimal") %>%
  setNames(column_names) %>%
  mutate(label = test_labels)

spec <- feature_spec(train_df, label ~ . ) %>%
  step_numeric_column(all_numeric(), normalizer_fn = scaler_standard()) %>%
  fit()

layer <- layer_dense_features(
  feature_columns = dense_features(spec),
  dtype = tf$float32
)

layer(train_df)

# --- Model builder -------------------------------------

build_model <- function(firsty=16,secondy=8) {
  
  input <- layer_input_from_dataset(train_df %>% dplyr::select(-label))
  
  if(secondy>1){
    output <- input %>%
      layer_dense_features(dense_features(spec)) %>%
      layer_dense(units = firsty, activation = "relu") %>%  # "linear", "selu", "sigmoid"
      layer_dense(units = secondy, activation = "relu") %>%
      layer_dense(units = 1)
  }else{
    output <- input %>%
      layer_dense_features(dense_features(spec)) %>%
      layer_dense(units = firsty, activation = "relu") %>%  # "linear", "selu", "sigmoid"
      layer_dense(units = 1)
  }
  
  AIEGB <- keras_model(input, output)
  
  AIEGB %>%
    compile(
      loss = 'mean_squared_error',#"mse",#mae",#"mse",
      optimizer = 'adam',# 'sgd',#adam',#'ftrl',#'nadam',#'adamax',#'adadelta',# 'rmsprop',#'adam',#optimizer_rmsprop(),
      metrics = list("mean_absolute_error")
    )
  
  AIEGB
}

# --- Loop over layering options -----------------------

layering0<-expand.grid(c(10,20),c(0,6,12))
layering<-layering0[layering0[,2]<=layering0[,1],]
nl<-nrow(layering)
mse<-cory<-rep(NA,nl)
# Train models
logy<-T
if(dotrain){
for(ll in 1:nl){
  
  firsty<-layering[ll,1]
  secondy<-layering[ll,2]
  if(secondy==0)AIEGB <- build_model1(firsty,secondy)
  if(secondy>1)AIEGB <- build_model2(firsty,secondy)
  
  history <- AIEGB %>% fit(
    x = train_df %>% dplyr::select(-label),
    y = train_df$label,
    epochs = 30,
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
  
  saveRDS(history,paste0("./Fits/history_",firsty,"_",secondy,"_fds.rda"))
  save_model_weights_hdf5(AIEGB, paste0("./Fits/AIEGB_",firsty,"_",secondy,"_wts_fds.h5"))
  print(paste(ll,"of",nl, "completed"))
}
}


