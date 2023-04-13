#### Table code
setwd("C:/Users/tcarruth/Documents/GitHub/AI-MP-EGB")
setwd("C:/GitHub/AI-MP-EGB")
source("./Source/Tabulation_default.R")
library(openxlsx)

# Table 1. Index statistics

sddat<-readRDS("./Sim_Data/sddat.rda")
apply(sddat,2,mean)


# Table 2.  performance of alternative neural network designs

dotrain=F; source("./4_Train_ANN.R")
#source("./Source/make_train_data.r")
source("./Source/build_model.r")
source("./Source/TableStats.r")

Filenames<-list.files("./Fits2")
fullnames<-list.files("./Fits2",full.names=T)
Filenames<-Filenames[grepl('AIEGB',Filenames)]
firsty<-sapply(Filenames,function(x)strsplit(x,"_")[[1]][2])
secondy<-sapply(Filenames,function(x)strsplit(x,"_")[[1]][3])
histfiles<-paste0(getwd(),"/Fits2/history_",firsty,"_",secondy,"_fds.rda")
par(mfrow=c(7,7),mai=c(0.1,0.1,0.1,0.05))
out<-TableStats(firsty,secondy,histfiles,fullnames)
saveRDS(out,"./Results/Fits/Summary.rda")


# make table

out<-readRDS("./Results/Fits/Summary.rda")

doXL(maketab(out,rem1=c(20,18,16)),"./Tables for paper/Table 2v2.xlsx")


# Table 3  performance of alternative neural network designs (longer training)

dotrain=F; source("./4_Train_ANN.R")

source("./Source/build_model.r")
Filenames<-list.files("./Fits_100")
fullnames<-list.files("./Fits_100",full.names=T)
Filenames<-Filenames[grepl('AIEGB',Filenames)]
first<-sapply(Filenames,function(x)strsplit(x,"_")[[1]][2])
second<-sapply(Filenames,function(x)strsplit(x,"_")[[1]][3])
histfiles<-paste0(getwd(),"/Fits_100/history_",first,"_",second,"_fds.rda")
# cbind(Filenames, firsty,secondy,histfiles)
par(mfrow=c(5,3),mai=c(0.1,0.1,0.1,0.05))
out<-TableStats(first,second,histfiles,fullnames)
saveRDS(out,"./Results/Fits/Summary_100.rda")



# make table

out<-readRDS("./Results/Fits/Summary_100.rda")
doXL(maketab(out,rem1=c(20,18,16,6)),"./Tables for paper/Table 3.xlsx")


