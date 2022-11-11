# ===============================================================================================================================
# ===================== make a constant harvest rate MP =========================================================================
# ===============================================================================================================================

# Tom Carruthers
# 10/11/2022

# --- Installation -----------------------------------------------------------------------------------------------------------

devtools::install_github("timjmiller/wham",dependencies=T)
devtools::install_github("kaskr/adcomp/TMB")
setwd("C:/Program Files/R/R-4.2.1/library/wham/libs/x64")
dyn.load(dynlib('wham'))

# --- Packages ---------------------------------------------------------------------------------------------------------------

library(openMSE)
library(wham)
setwd("C:/GitHub/AI-MP-EGB/")
setwd("C:/Users/tcarruth/Documents/GitHub/AI-MP-EGB")

# --- Make an example dataset ------------------------------------------------------------------------------------------------

obj<-readRDS("./Data/Base.rda") # WHAM assessment object
nsim<-5
OM <- MSEtool:::WHAM2OM(obj, report=F, nsim=nsim, LowerTri = 1,interval=1) # report = T produces a diagnostic showing WHAM vs OM matching of numbers at age
Base<-readRDS("./Operating_Models/Base_dat.rda")
OM@cpars$Data<-Base@cpars$Data
OM@cpars$AddIbeta <-Base@cpars$AddIbeta[1:nsim,]

OM@seed<-1
Hist<-runMSE(OM,Hist=T,extended=T)
MSE<-Project(Hist,MPs="FMSYref50",extended=T)
Data<-MSE@PPD[[1]]



AI_CU<-function(x,Data.reps=1){

  colnams<- c(paste0("mui_",1:nI),paste0("hi_",1:nI),paste0("ri4_",1:nI),paste0("ri8_",1:nI),paste0("si4_",1:nI),paste0("si6_",1:nI),"cmu1","cmu2","ch1","ch2","iyr")
  test_data<-matrix(rep(c(mui,hi,ri4,ri8,si4,si6,cmu1,cmu2,ch1,ch2,thisyr),each=2),nrow=2)
  colnames(test_data)<-colnams

  # as_tibble(setNames(mutate(test_data,label=as.numeric(c(2.0,2.0)))))

  new_df <- test_data %>%
    as_tibble(.name_repair = "minimal") %>%
    setNames(colnams) %>%
    mutate(label = as.numeric(c(2.0,2.0)))

  keep<-rep(T,dim(MSE@PPD[[1]]@AddInd)[2])

  getind<-function(j,MSE,Years2){
    Ind<-MSE@PPD[[1]]@AddInd[j,keep,]
    yind<-Years2[j]-9:1 # the nine previous years of observations
    as.vector(t(Ind[,yind]))
  }

  getcat<-function(j,MSE,Years2){
    caty<-c(MSE@CB_hist[j,],MSE@Catch[j,1,])
    yind<-Years2[j]-9:1 # the nine previous years of observations
    as.vector(caty[yind])
  }


  cbind(VB,t(sapply(1:nsim,getind,MSE=MSE,Years2=Years2)),t(sapply(1:nsim,getcat,MSE=MSE,Years2=Years2)))

  selecto<-dplyr::select

  predBt<- AIE %>% predict(new_df %>% selecto(-label))
  Bt<-exp(predBt[1,1])*1000
  #Bt_PI<-dset[[AS]]$Bt_PI #; Bt/Bt_PI
  TACnew <- Bt*UE

 Rec<-new('Rec')

}
