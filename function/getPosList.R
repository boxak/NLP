getPosList <- function(data){
  setwd('C:/Users/82102/Documents/workingDIR/function')
  source(file = "findStem.R",encoding = "UTF-8")
  totalLen <- nrow(data)
  PosList <- vector()
  for(n in 1:totalLen){
    str <- stringr::str_remove_all(as.character(data[n,6]),"[^[ㄱ-힣\\s]]")
    PosTable <- RcppMeCab::pos(str,format = "data.frame")
    tempVector <- vector()
    size <- nrow(PosTable)
    result <- ''
    for(i in 1:size){
      if(PosTable[i,5]=="NNG" | PosTable[i,5]=="NNP"){
        tempVector <- c(tempVector,as.character(PosTable[i,4]))
      }
      else if(PosTable[i,5]=="VV"){
        addStr <- paste0(as.character(PosTable[i,4]),"다")
        tempVector <- c(tempVector,addStr)
      }
      else if(str_detect(PosTable[i,5],"VV")){
        text <- findStem(as.character(PosTable[i,4]))
        tempVector <- c(tempVector,text)
      }
    }
    if(length(tempVector)>0){
      result <- tempVector[[1]]
      if(length(tempVector)>1){
        for(i in 2:length(tempVector)){
          result <- paste(result,tempVector[[i]],sep = ", ")
        }
      }
    }
    PosList <- c(PosList,result)
  }
  return(PosList)
}