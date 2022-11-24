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

source("./Source/MPs.R")

# --- get OM and run MSE -----------------------------------------

OM<-SubCpars(readRDS("./Operating_Models/Base_dat.rda"),sims=1:2)
OM@seed<-1
Hist2<-runMSE(OM,Hist=T,extended=T)
MSEtest=Project(Hist2,MPs=c(paste0("AI3_",c(3,5,9)),
                            paste0("AI2_",c(3,5,9)),
                            paste0("AI1_",c(3,5,9))))#MSEtest=Project(Hist2,MPs="AI_5")
TplotAI(MSEtest)

MSEtest2=Project(Hist2,MPs="AI1_5")#MSEtest=Project(Hist2,MPs="AI_5")
TplotAI(MSEtest)

MSEtest3=Project(Hist2,MPs=c(paste0("PI_",c(3,5,9)),paste0("AI1_",c(3,5,9))))#MSEtest=Project(Hist2,MPs="AI_5")
TplotAI(MSEtest3)
Pplot(MSEtest3)


Data<-readRDS("C:/temp/Data.rda")

MSEtest@Catch





## ---------- test of MP data borrowing

library(openMSE)
testMP<-function (x, Data, reps = 100, plot = FALSE){
  nyears <- length(Data@Misc$FleetPars$Find[x, ]) # No. historical years
  curyr<-ncol(Data@Cat)                           # get current year
  ny<-curyr-nyears                                # calculate projection year
  Rec <- new("Rec")                               # New MP recommendation object
  Rec@Effort <- 1                                 # keep constant recent effort
  if(ny==1){                                      # if first year of projection, start counter
    Rec@Misc[[1]]<-x
  } else {                                        # else add 1 to make a vector 
    pastinfo<-unlist(Data@Misc[[x]])
    Rec@Misc[[1]]=c(pastinfo,max(pastinfo)+x)
    print(Rec@Misc[[1]])
  }
  Rec
}

class(testMP)<-'MP'
OM<-SubCpars(testOM,sims=1:2)
OM@interval<-1
MSEtest=runMSE(OM,MPs="testMP")
Misc<-MSEtest@PPD[[1]]@Misc
for(sim in 1:2)print(Misc[[sim]][[1]])


