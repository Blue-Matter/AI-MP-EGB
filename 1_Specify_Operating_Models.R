
# ================================================================================================================================================
# === Designing an AI MP for EGB Haddock =========================================================================================================
# ================================================================================================================================================

# 1. Specify Operating Models


# --- Prerequisites ---------------------------------------------------------------------------------------

library(openMSE)
setwd("C:/Users/tcarruth/Documents/GitHub/AI-MP-EGB")


# A --- Load Operating Model derivied from Base EGB Assessment ----------------------------------------------

Base<-readRDS("./Operating_Models/Base.rda")
proyears<-Base@nyears+1:Base@proyears                          # index for imputing values in future years
rec_proyears<-dim(Base@cpars$Perr_y)[2]-((Base@proyears-1):0)  # index for imputing values for rec devs in future years



# B --- Add data for simulation ---------------------------------------------------------------

obj<-readRDS("./Data/Base.rda")

# Notes
# There is an option to append real data to an openMSE operating model. When doing this, the statistical properties
#       of the index are calculated internally including precision, lag-1 autocorrelation in residuals and non-
#       linearity (for indices)
# The CVs for data are not used to simulate data but are the assumed CVs that are sent in the simulated data to MPs.
#       So, for example, a model may be conditioned with a catch CV of 0.1. When fitted, fit is much better at 0.06 for
#       simulation 1. Annual catch data are then simulated with a CV of 0.06. These data are sent to MPs, but the reported
#       CV is the same as that assumed in the conditioning of the OM: 0.1.

dat<-new('Data')
dat@Year <- obj$years # The years in data file

# Aggregate annual catches ---

dat@Cat <- matrix(rep(obj$input$data$agg_catch[,1],each=OM@nsim),nrow=OM@nsim)
dat@CV_Cat <- matrix(rep(obj$input$data$agg_catch_sigma[,1],each=OM@nsim),nrow=OM@nsim) # this is the assumed CV reported to MPs when simulated data are supplied

# Abundance indices ---

# Values

indices<-t(obj$input$data$agg_indices) # transpose to index x year

BInd<-read.csv("./Data/EGB_2021/EGB_HADDOCK_BIOMASS INDEX_3.csv")
BInd<-BInd[BInd$Year>1986,] # Trim to last 33 years
keep<-match(BInd$Year,(OM@CurrentYr-((OM@nyears-1):0)))

Bindices<-array(NA,dim(indices))
Bindices[,keep]<-t(BInd[,c(3,2,4)])/1E6

Sindices<-array(NA,c(dim(indices)[1]*3,dim(indices)[2]))

selex<-list(c(0,0.5,1,1,1,1,0.7,0.5,0.3),c(0,0,0.5,1,1,1,1,0.7,0.5),c(0,0,0,0.5,1,1,1,1,0.7))
selexV<-array(NA,c(OM@nsim,9,10))
j<-0


for(ss in 1:3){
  for(i in 1:3){
    j<-j+1
    selexV[,j,]<-rep(c(0,selex[[ss]]),each=OM@nsim)
  }
}

j<-0


for(ss in 1:3){
  for(i in 1:3){
    j<-j+1
    sel<-t(array(selex[[ss]],c(obj$input$data$n_ages, obj$input$data$n_years_model)))
    Sindices[j,]<-apply(obj$input$data$index_paa[i,,]*obj$input$data$agg_indices[,i]*obj$input$data$waa[i+1,,]*sel,1,sum)
  }
}

allind<-rbind(indices,Bindices,Sindices)

dat@AddInd <- array(rep(allind,each=OM@nsim),c(OM@nsim,dim(allind)))
dat@AddInd[dat@AddInd==0]<-NA # set zero values to NA
dat@AddInd[,,1:(OM@nyears-10)]<-NA # trim old values


# CVs

ind_CV <- t(cbind(obj$input$data$agg_index_sigma,obj$input$data$agg_index_sigma,obj$input$data$agg_index_sigma,obj$input$data$agg_index_sigma,obj$input$data$agg_index_sigma))
dat@CV_AddInd <- array(rep(ind_CV,each=OM@nsim),c(OM@nsim,dim(allind))) # this is the assumed CV reported to MPs when simulated data are supplied

# Vulnerability at age
dat@AddIndV <- abind(OM@Misc$WHAM$AddIndV[,3:5,],OM@Misc$WHAM$AddIndV[,3:5,],selexV,along=2) # the final three selectivities are those of the indices, first two are the fleet

# Type
dat@AddIunits<-c(rep(0,3),rep(1,3),rep(1,9))  # 0 Numbers index (1 is biomass)
dat@AddIndType<-rep(3,dim(allind)[1]) # Vulnerable stock (1 is total stock, 2 is spawning stock)

OM@cpars$Data <- dat # append data to operating model
OM@cpars$AddIbeta <- matrix(1, nrow=OM@nsim, ncol=dim(OM@cpars$Data@AddInd)[2])














# --- Copy Base as template for other OMs -----------------------------------------------------------------

M_Int <- M_Low <- Rec_Low <- Rec_High <- Sel_Yng <- Base


# --- M Scenarios -----------------------------------------------------------------------------------------

RecentM<-Base@cpars$M_ageArray[,1,Base@nyears]
HistM<-Base@cpars$M_ageArray[,1,1]

Mmed<-(RecentM+HistM)/2

M_Int@cpars$M_ageArray[,,proyears]<-Mmed  # Intermediate (mean historical and recent M)
M_Low@cpars$M_ageArray[,,proyears]<-HistM # Low (historical) M

saveRDS(M_Int,"./Operating_Models/M_Int.rda")
saveRDS(M_Low,"./Operating_Models/M_Low.rda")


# --- Recruitment Scenarios -------------------------------------------------------------------------------

Proj<-apply(Base@cpars$Perr_y[,rec_proyears],1,mean)
Hist<-apply(Base@cpars$Perr_y[,1:30],1,mean)
Recent<-apply(Base@cpars$Perr_y[,31:60],1,mean)

temparr<-Base@cpars$Perr_y[,rec_proyears]
Rec_Low@cpars$Perr_y[,rec_proyears]<-temparr*Hist # projections include a reduction to historical low recruitment levels relative to R0
Rec_High@cpars$Perr_y[,rec_proyears]<-temparr*Recent # projection include an increase to recent high recruitment levels relative to R0

saveRDS(Rec_Low,"./Operating_Models/Rec_Low.rda")
saveRDS(Rec_High,"./Operating_Models/Rec_High.rda")


# --- Selectivity Scenarios -------------------------------------------------------------------------------

HistSel<-Base@cpars$V[,,1]
Sel_Yng@cpars$V[,,proyears]<-array(HistSel,dim(Sel_Yng@cpars$V[,,proyears]))
saveRDS(Sel_Yng,"./Operating_Models/Sel_Yng.rda")


# === End of Script ===============================================================================================================================



