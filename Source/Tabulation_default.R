
doXL<-function(tab,filenam,sheet="Raw"){
  wb <- loadWorkbook(filenam)
  writeData(wb, sheet = sheet, tab,rowNames=T)
  saveWorkbook(wb,filenam,overwrite = T)
}


