library('dplyr')
library('readr')
library('httr')
library('rvest')
library('tidyverse')
library('tidytext')
library('stringr')


data <- read_csv('C:/Users/82102/Documents/workingDIR/data/joinData.csv',locale = locale())
PosList <- vector()
for(i in 1:12){
  temp <- read_csv(paste0('C:\\Users\\82102\\Documents\\workingDIR\\data\\tempData',as.character(i),'.csv'),locale = locale())
  PosList <- rbind(PosList,temp)
}
nrow(PosList)

joinData <- cbind(data,PosList)

joinData <- joinData[,c(1,2,3,4,5,6,18,7,8,9,10,11,12,13,14,15,16,17)]
joinData$'PosList' <- gsub('^NA,\\s|NA,\\s|, NA$','',joinData$'PosList')

### 기존 데이터랑 PosList랑 합치기



setwd('C:\\Users\\82102\\Documents\\workingDIR\\function')
source('findSup.R',encoding = 'UTF-8')

tempVector <- vector()

tempVector <- foreach(i=1:100, .combine = rbind) %do% {
  strVector <- strsplit(joinData['PosList'][[1]][i],split = ", ")
  wordList <- vector()
  len <- length(strVector[[1]])
  wordList <- sapply(strVector, function(str){
    return(findSup(str))
  })
  result <- wordList[[1]]
  if(length(wordList)>1){
    for(k in 2:length(wordList)){
      result <- paste(result,wordList[[k]],sep = ", ")
    }
  }
  
  return(result)
}

### 1행부터 100행까지만 숫자 붙여줌.

setwd('C:\\Users\\82102\\Documents\\workingDIR\\function')
source('readcsv.R',encoding = 'UTF-8')

dict <- readcsv('DICT.csv')
dict <- dict[dict$'난이도'=='하',]
tempVector <- readcsv('wordList.csv')


tempList <- vector()
graph <- vector()
for(i in 1:nrow(tempVector)){
  strVector <- strsplit(tempVector[[1]][i],split = ", ")
  len <- length(strVector[[1]])
  wordList <- vector()
  nameList <- vector()
  for(j in 1:len){
    wordList <- c(wordList,strVector[[1]][j])
    nameList <- c(nameList,dict['의미어'][[1]][i])
  }
  ### 콤마를 기준으로 파싱한 걸 strVector에 담은 후,
  ### 그 길이만큼 nameList에는 기준이 되는 의미어
  ### wordList는 파싱된 단어들을 넣는다.
  ### 그러고 나서 합쳐서 tempList에 넣는다.
  tempList <- cbind(nameList,wordList)
  graph <- rbind(graph,tempList)
}

tempTable <- data.frame(tempList)
write.table(tempTable,file = "C:/Users/82102/Documents/workingDIR/data/graph.csv",row.names = FALSE
            ,col.names = TRUE,append=TRUE,fileEncoding = "UTF-8")
