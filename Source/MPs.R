# --- Define the MP ------------------------------------------------------------------------------------------------------

# --- Make an example dataset ------------------------------------------------------------------------------------------------

if(maketestdata){
  OM<-SubCpars(readRDS("./Operating_Models/Base_dat.rda"),sims=1:2)
  OM@seed<-1
  Hist<-runMSE(OM,Hist=T,extended=T)
  MSE=Project(Hist,MPs="FMSYref50")
  Data<-MSE@PPD[[1]]
}

# ============ Make AI CMP ====================================

# --- get a small dataset (get the shape and input layer names) ------------------------------------------------------------------------------------

TDsmall=T
source("./Source/make_train_data.r")

# --- build model -----------------------------------------------------------------------------------------------------------------------

source("./Source/build_model.R") # uses data shape
AIEGB<- build_model(8,4)
AIEGB %>% load_model_weights_hdf5(filepath = "./Fits_100/AIEGB_8_4_wts_fds.h5")


# --- process NN data ---------------------------------------------------------------------------------------------------

makeNNdat<-function(x,Data,ysel=NULL){
  if(is.null(ysel))termyr<-dim(Data@AddInd)[3]
  if(!is.null(ysel))termyr<-ysel
  yind<-termyr-(8:0)
  NNdat<-matrix(rep(c(as.vector(t(Data@AddInd[x,,yind])),as.vector(Data@Cat[x,yind])),2),nrow=2,byrow=T)
  NNdat[is.na(NNdat)]<-1
  colnames(NNdat)<-paste0("IV",1:ncol(NNdat))
  log(NNdat)
}

send_info<-function(x,Data,newinfo,Rec,nHy=51){
  curyr<-ncol(Data@Cat)                           # get current year
  ny<-curyr-nHy+1                                  # calculate projection year
  if(ny==1){                                      # if first year of projection, start counter
    Rec@Misc[[1]]<-newinfo
  } else {                                        # else add 1 to make a vector 
    pastinfo<-Data@Misc[[x]][[1]]
    Rec@Misc[[1]]=c(pastinfo,newinfo)
  }
  Rec
}


smooth<-function(xx,plot=F,enp.mult,plotname=""){
  tofill<-!is.na(xx)
  xx[xx==0]<-1E3
  predout<-rep(NA,length(xx))
  dat<-data.frame(x=1:length(xx),y=log(xx))
  enp.target<-sum(tofill)*enp.mult
  out<-loess(y~x,dat=dat,enp.target=enp.target)
  predout[tofill]<-exp(predict(out))
  if(plot){
    plot(xx,type="p",xlab="x",ylab="y",main=plotname)
    lines(predout,col="#ff000090",lwd=2)
  }
  predout
}


AI_CU<-function(x,Data,reps=1,targF=0.2,wts=c(0,1),nHy=51,dosmth=F,enp.mult=0.3){ # x=1; reps=1
  termyr<-dim(Data@AddInd)[3] 
  curyr<-ncol(Data@Cat)                           # get current year
  ny<-curyr-nHy+1      
  test_data<-makeNNdat(x,Data)
  new_df <- test_data %>% as_tibble(.name_repair = "minimal") %>%
    setNames(colnames(test_data)) %>%  mutate(label = as.numeric(c(2.0,2.0)))
  selecto<-dplyr::select
  predBt<- AIEGB %>% predict(new_df %>% selecto(-label))
  VBest<-exp(predBt[1,1])*1000
  TACrec=VBest*targF
  calcs=unlist(Data@Misc[[x]])
  lastcalc=calcs[length(calcs)]
  Rec<-new('Rec')
  
  if(ny==1)Rec@TAC<-TACrec
  if(ny>1)Rec@TAC<-weighted.mean(c(lastcalc,TACrec),w=wts)
  if(dosmth){
    if(ny>5){
      #print(c(calcs,TACrec))
      smrec<-smooth(c(calcs,TACrec),plot=F,enp.mult=enp.mult)
      Rec@TAC<-smrec[length(smrec)]
    }
  }
  #Rec@TAC<-TACrec
  #print(targF)
  #saveRDS(Data,"C:/temp/Data.rda")
  Rec<-send_info(x=x,Data=Data,newinfo=TACrec,Rec=Rec)
  #print(Rec@Misc[[1]])
  Rec
}
class(AI_CU)<-"MP"

AI1_3<-function(x,Data,reps)AI_CU(x=x,Data=Data,reps=reps,targF=0.3)
AI1_5<-function(x,Data,reps)AI_CU(x=x,Data=Data,reps=reps,targF=0.5)
AI1_7<-function(x,Data,reps)AI_CU(x=x,Data=Data,reps=reps,targF=0.7)
AI1_9<-function(x,Data,reps)AI_CU(x=x,Data=Data,reps=reps,targF=0.9)

AI2_3<-function(x,Data,reps)AI_CU(x=x,Data=Data,reps=reps,targF=0.3,wts=c(1,1))
AI2_5<-function(x,Data,reps)AI_CU(x=x,Data=Data,reps=reps,targF=0.5,wts=c(1,1))
AI2_7<-function(x,Data,reps)AI_CU(x=x,Data=Data,reps=reps,targF=0.7,wts=c(1,1))
AI2_9<-function(x,Data,reps)AI_CU(x=x,Data=Data,reps=reps,targF=0.9,wts=c(1,1))

AI3_3<-function(x,Data,reps)AI_CU(x=x,Data=Data,reps=reps,targF=0.3,wts=c(1,1),dosmth=T)
AI3_5<-function(x,Data,reps)AI_CU(x=x,Data=Data,reps=reps,targF=0.5,wts=c(1,1),dosmth=T)
AI3_7<-function(x,Data,reps)AI_CU(x=x,Data=Data,reps=reps,targF=0.7,wts=c(1,1),dosmth=T)
AI3_9<-function(x,Data,reps)AI_CU(x=x,Data=Data,reps=reps,targF=0.9,wts=c(1,1),dosmth=T)


class(AI1_3)<-class(AI1_5)<-class(AI1_7)<-class(AI1_9)<-"MP"
class(AI2_3)<-class(AI2_5)<-class(AI2_7)<-class(AI2_9)<-"MP"
class(AI3_3)<-class(AI3_5)<-class(AI3_7)<-class(AI3_9)<-"MP"



# === Make Plan B MP =============================================

pBs<-function(x,y,plot=F,enp.mult=0.6,family="gaussian",inter=1){
  
  if(plot)plot(x,y,pch=19,cex=1)
  mod<-loess(y~x,data.frame(x=x,y=y),enp.target=length(x)*enp.mult,family=family)
  newdata<-data.frame(x=seq(min(x),max(x),length.out=length(x)*inter))
  py=predict(mod,newdata)
  if(plot)lines(newdata$x,py,col="red")
  cbind(x=newdata$x,y=py)
  
}

slp3<-function(y){
  
  y<-y[!is.na(y)]
  y<-log(y)
  x1<-1:length(y)
  mux<-mean(x1)
  muy<-mean(y,na.rm=T)
  SS<-sum((x1-mux)^2,na.rm=T)
  (1/SS)*sum((x1-mux)*(y-muy),na.rm=T)
  
}

PBS<-function(x,Data,reps=1,enp.mult=0.6,yrec=3,startTAC=14){
  ny<-length(Data@Year)
  if(ny==51){
    oldTAC=startTAC*1000#Data@Cat[x,51]
  }else{
    oldTAC=Data@MPrec[x]
  }
  Rec<-new('Rec')
  Binds<-Data@AddInd[x,25:27,]
  
  muInd<-apply(Binds,2,mean,na.rm=T)
  prInd<-muInd[!is.na(muInd)]
  y<-pBs(x=1:length(prInd),prInd,enp.mult = enp.mult)[,2]
  ydat<-y[length(y)-(0:(yrec-1))]
  slp<-slp3(log(ydat))
  oldTAC*exp(slp)
  Rec@TAC<-oldTAC*exp(slp)
  Rec
}
class(PBS)<-"MP"

PBS_5<-function(x,Data,reps=1)PBS(x=x,Data=Data,reps=reps,startTAC=5)
PBS_10<-function(x,Data,reps=1)PBS(x=x,Data=Data,reps=reps,startTAC=10)
PBS_20<-function(x,Data,reps=1)PBS(x=x,Data=Data,reps=reps,startTAC=20)
class(PBS_5)<-class(PBS_10)<-class(PBS_20)<-"MP"





# === Make perfect information MP ================================

PI_CU<-function(x,Data,reps=1,targF=0.2){
  y=max(Data@Year) - Data@LHYear + 1
  nyears <- length(Data@Misc$FleetPars$Find[x, ])
  q <- Data@Misc$FleetPars$qs[x]
  qvar <- Data@Misc$FleetPars$qvar[x, y]
  if (length(qvar) < 1)   qvar <- 1
  qinc <- Data@Misc$FleetPars$qinc[x]
  qcur <- qvar * q * (1 + qinc/100)^y
  HistE <- Data@OM$FinF[x]
  MSYE <- targF/qcur
  Rec <- new("Rec")
  Rec@Effort <- MSYE/HistE
  Rec
}

class(PI_CU)<-"MP"

PI_3<-function(x,Data,reps)PI_CU(x=x,Data=Data,reps=reps,targF=0.3)
PI_5<-function(x,Data,reps)PI_CU(x=x,Data=Data,reps=reps,targF=0.5)
PI_9<-function(x,Data,reps)PI_CU(x=x,Data=Data,reps=reps,targF=0.9)

class(PI_3)<-class(PI_5)<-class(PI_9)<-"MP"