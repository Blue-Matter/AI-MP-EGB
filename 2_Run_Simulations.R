
# ================================================================================================================================================
# === Designing an AI MP for EGB Haddock =========================================================================================================
# ================================================================================================================================================


library(openMSE)
library(wham)

# 2. Run Simulations
setwd("C:/Users/tcarruth/Documents/GitHub/AI-MP-EGB")
setwd("C:/GitHub/AI-MP-EGB")
Base<-readRDS("./Operating_Models/Base_dat.rda")


# --- Make exploratory MPs -------------------

FMP<-function(x, Data, reps = 100, plot = FALSE, Flev=0.6, CV=0.1) {

  Rec <- new("Rec")
  y <- max(Data@Year) - Data@LHYear + 1

  nyears <- length(Data@Misc$FleetPars$Find[x, ])
  q <- Data@Misc$FleetPars$qs[x]
  qvar <- Data@Misc$FleetPars$qvar[x, y]
  if (length(qvar) < 1)   qvar <- 1
  qinc <- Data@Misc$FleetPars$qinc[x]
  qcur <- qvar * q * (1 + qinc/100)^y
  HistE <- Data@OM$FinF[x]
  MSYE <- Flev/qcur
  Rec@Effort <- MSYE/HistE * rlnorm(1,0,CV)

  Rec
}

F_hi<-function(x,Data,reps)FMP(x=x,Data=Data,reps=reps,Flev=0.8)
F_med<-function(x,Data,reps)FMP(x=x,Data=Data,reps=reps,Flev=0.4)
F_low<-function(x,Data,reps)FMP(x=x,Data=Data,reps=reps,Flev=0.2)
F_hi_v<-function(x,Data,reps)FMP(x=x,Data=Data,reps=reps,Flev=0.8, CV=0.3)
F_med_v<-function(x,Data,reps)FMP(x=x,Data=Data,reps=reps,Flev=0.4, CV=0.3)
F_low_v<-function(x,Data,reps)FMP(x=x,Data=Data,reps=reps,Flev=0.2, CV=0.3)

class(F_hi)<-class(F_med)<-class(F_low)<-class(F_hi_v)<-class(F_med_v)<-class(F_low_v)<-"MP"
simMPs <- c("F_hi","F_med","F_low","F_hi_v","F_med_v","F_low_v")
# test2 <- runMSE(MPs=simMPs); plot(test2)


obj<-readRDS("./Data/Base.rda") # WHAM assessment object


ni<-180
MPs<-1:length(simMPs)
inds<-expand.grid(1:ni,MPs)

if(error){ # if for some reason the whole set did not fully complete, this filters inds according to the files calculated
  files<-list.files("C:/temp/MSEs5/")
  nis<-sapply(files,function(x)strsplit(x,split="_")[[1]][2])
  MPss<-sapply(files,function(x)substr(strsplit(x,split="_")[[1]][3],1,1))
  fcode<-paste(nis,MPss,sep="_")
  icode<-paste(as.character(inds[,1]),as.character(inds[,2]),sep="_")
  keep<-!(icode%in%fcode)
  inds<-inds[keep,]
}

makeObsErr = function(Hist, iCV=0.2){
  iArr =Hist@SampPars$Obs$AddIerr
  dims=dim(iArr)
  Hist@SampPars$Obs$AddIerr = array(trlnorm(prod(dims),1,iCV),dims)
  Hist 
}

parrun<-function(x,inds,obj,simMPs, Base, CV = 0.15){
    i<-inds[x,1]
    MP<-inds[x,2]
    seed<-(i*100)+i*MP
    set.seed(seed)
    OM <- MSEtool:::WHAM2OM(obj, report=F, nsim=200, LowerTri = 1,interval=1) # report = T produces a diagnostic showing WHAM vs OM matching of numbers at age
    OM@cpars$Data<-Base@cpars$Data
    OM@cpars$AddIbeta <-Base@cpars$AddIbeta
    OM@seed<-seed
    Hist<-runMSE(OM,Hist=T,extended=T)
    Hist = makeObsErr(Hist,0.1)
    MSE<-Project(Hist,MPs=simMPs[MP],extended=T)
    saveRDS(MSE,paste0("C:/temp/MSEs_CV10/Run_",i,"_",MP,".rda"))
    #print(paste("i =",i,"  MP =",MP))
}

setup()
sfLibrary(wham)
sfExport(list=list("FMP","F_hi","F_med","F_low","F_hi_v","F_med_v","F_low_v"))

iss<-(1:nrow(inds))#[inds[,1]>60]
sfSapply(iss,parrun,inds=inds,obj=obj,simMPs=simMPs,Base=Base)

# end of script



