
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

setwd("C:/Users/tcarruth/Documents/GitHub/AI-MP-EGB")
setwd("C:/GitHub/AI-MP-EGB")

# https://tensorflow.rstudio.com/tutorials/beginners/basic-ml/tutorial_basic_regression/

# Load data

# ==============================================================================================================================================
# ===== EGB ===================================================================================================================================
# ==============================================================================================================================================


TD<-readRDS("./Sim_Data/simdata.rda"); hist(TD[,1])
TD<-TD[TD[,1]<quantile(TD[,1],0.9),]
TD<-TD[TD[,1]>1000,]

TD<-log(TD)

keep<-apply(TD,1,function(x)!is.na(sum(x)))&apply(TD,1,function(x)!any(x==-Inf))
TD<-TD[keep,]
hist(TD[,1])

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

print("1/3. Making ANN ")
layer(train_df)


# Model building function for reuse

build_model <- function() {
  input <- layer_input_from_dataset(train_df %>% dplyr::select(-label))
  
  output <- input %>%
    layer_dense_features(dense_features(spec)) %>%
    layer_dense(units = 16, activation = "relu") %>%  # "linear", "relu"
    layer_dense(units = 16, activation = "relu") %>%
    layer_dense(units = 1)
  
  AIEGB <- keras_model(input, output)
  
  AIEGB %>%
    compile(
      loss = 'mean_squared_error',#"mse",#mae",#"mse",
      optimizer = 'adam',# 'sgd',#adam',#'ftrl',#'nadam',#'adamax',#'adadelta',# 'rmsprop',#'adam',#optimizer_rmsprop(),
      metrics = list("mean_absolute_error")
    )
  
  AIEGB
}

# Train model

print_dot_callback <- callback_lambda(
  on_epoch_end = function(epoch, logs) {
    if (epoch %% 80 == 0) cat("\n")
    cat(".")
  }
)

early_stop <- callback_early_stopping(monitor = "val_loss", patience = 20)

AIEGB <- build_model()

#if(train){
  print("Retraining the ANN")
  history <- AIEGB %>% fit(
    x = train_df %>% dplyr::select(-label),
    y = train_df$label,
    epochs = 500,
    validation_split = 0.2,
    verbose = 2#,
    #callbacks = list(print_dot_callback)
  )
  
  test_predictions <- AIEGB %>% predict(test_df %>% dplyr::select(-label))
  x<-exp(test_df$label)
  y<-exp(test_predictions[ , 1])
  par(mfrow=c(1,2),mai=c(0.55,0.55,0.01,0.01))
  plot(x,y,ylim=c(0,quantile(y,0.99)),xlab="VB obs",ylab="VB Pred"); lines(c(0,1E10),c(0,1E10),col='#ff000050',lwd=2)
  legend('bottomright',legend=round(cor(x,y)^2,3),text.col='red',bty="n")
  x<-log(x)
  y<-log(y)
  plot(x,y,xlab="log VB obs",ylab="log VB Pred"); lines(c(0,1E10),c(0,1E10),col='#ff000050',lwd=2)
  
  
  
  
  plot(history)
  saveRDS(history,'history_1_8_8.rda')
  save_model_weights_hdf5(AIEGB, "AIEGB_wts.h5")

}else{

  print("Using saved trained model weights")
  AIE %>% load_model_weights_hdf5(filepath = "AIEGB_wts.h5")
  test_predictions <- AIE %>% predict(test_df %>% dplyr::select(-label))
  plot(exp(test_df$label),exp(test_predictions[ , 1]),xlab="VB obs",ylab="VB Pred"); lines(c(0,1E10),c(0,1E10),col='#ff000050',lwd=2)

  }

