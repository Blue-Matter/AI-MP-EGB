# ===============================================================================================================================
# ===================== make a constant harvest rate MP =========================================================================
# ===============================================================================================================================

# Tom Carruthers
# 10/11/2022

# --- Installation -----------------------------------------------------------------------------------------------------------

# devtools::install_github("timjmiller/wham",dependencies=T)
# devtools::install_github("kaskr/adcomp/TMB")

# setwd("C:/Program Files/R/R-4.2.1/library/wham/libs/x64")
# dyn.load(dynlib('wham'))

# --- Packages ---------------------------------------------------------------------------------------------------------------

library(openMSE)
library("Rcpp")
library(processx)
library(keras)
library(tensorflow)
library(reticulate)
library(dplyr)
library(tfdatasets)
library(ggplot2)
library(progress)
setwd("C:/GitHub/AI-MP-EGB/")
setwd("C:/Users/tcarruth/Documents/GitHub/AI-MP-EGB")

# --- MSE plotting code -----------------------------------------

source("./Source/MSEplots.R")


# --- get MPs ---------------------------------------------------

maketestdata=T
source("./Source/MPs.R")

# --- get OM and run MSE -----------------------------------------

OM<-SubCpars(readRDS("./Operating_Models/Base_dat.rda"),sims=1:64)
OM@seed<-1
Hist2<-runMSE(OM,Hist=T,extended=T)

MSEtest=Project(Hist2,MPs=c(paste0("PBS_",c(5,10,20)),
                            paste0("PI_",c(3,5,9)),
                            paste0("AI1_",c(3,5,9)),
                            paste0("AI2_",c(3,5,9)),
                            paste0("AI3_",c(3,5,9))))

saveRDS(MSEtest,"C:/temp/MSEtest.rda")
TplotAI(MSEtest)

