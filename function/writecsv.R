writecsv <- function(dataName,fileName){
  path <- paste0('C:/Users/82102/Documents/workingDIR/data/',fileName)
  write.csv(dataName,path,row.names = FALSE, fileEncoding = 'UTF-8')
}