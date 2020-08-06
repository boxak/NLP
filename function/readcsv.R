readcsv <- function(fileName){
  library('readr')
  url <- paste0('C:/Users/82102/Documents/workingDIR/data/',fileName)
  result <- read_csv(url,locale = locale())
  return(result)
}