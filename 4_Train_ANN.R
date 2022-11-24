
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

# --- Training function -----------------------------------

source("./Source/Trainingfunc.r")


# --- 30 epochs ------------------------------------------------------------------------

layering0 <-expand.grid(c(2,4,6,8,10,12,14,16,18,20),c(0,2,4,6,8))
# layering0 <-cbind(c(6,6,6,4,4,2),c(4,2,0,2,0,0))
layering<-layering0[layering0[,2]<=layering0[,1],]

trainfunc(layering,savdir="./Fits",epochs=30)


# ---- 100 epochs -----------------------------------------------------------------------

layering <-cbind(c(12,10,12,8,10,14,14,10,8,8),
                 c(4, 2, 0, 8,0, 6, 8, 4, 6,4))

layering <-cbind(c(12,10,12,10,14,14,10),
                 c(4, 2, 0, 0, 6, 8, 4))

layering=matrix(c(10,0),nrow=1)
trainfunc(layering,savdir="./Fits_100",epochs=100)


# --- 500 epochs -----------------------------------------------------------------------

layering=matrix(c(8,4),nrow=1)
trainfunc(layering,savdir="./Fits_500",epochs=500,wtfile="./Fits_100/AIEGB_8_8_wts_fds.h5")




# === End of script =====================================================================================



