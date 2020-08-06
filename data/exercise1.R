library('httr')
library('rvest')
library('urltools')
library('dplyr')
library('stringr')
library('readr')
library('tidytext')
library('tidyverse')
install.packages('XML')
library('XML')

Encoding("안드로이드")
Encoding(iconv("안드로이드",to = "UTF-8"))
URLEncode("안드로이드")
URLencode("예쁜")

url <- "https://dict.naver.com/search.nhn?dicQuery=%EC%9B%80%EC%A7%81%EC%9D%B4%EB%8A%94&query=%EC%9B%80%EC%A7%81%EC%9D%B4%EB%8A%94&target=dic&ie=utf8&query_utf=&isOnlyViewEE="
html <- read_html(url)

item <- html_node(html,xpath = '//*[@id="content"]/div[1]/ul/li[1]/p[1]/a[1]/span/strong')
text <- html_text(item,trim=TRUE)

###url2 <- "https://dict.naver.com/search.nhn?dicQuery=%EB%A8%B9%EB%8A%94&query=%EB%A8%B9%EB%8A%94&target=dic&ie=utf8&query_utf=&isOnlyViewEE="
###html2 <- read_html(url2)
###item2 <- html_node(html,)

library('RcppMeCab')
RcppMeCab::pos("움직였다")
typeof(RcppMeCab::pos("움직였다"))
str_detect(as.character(RcppMeCab::pos("움직였다")),"VV\"")
str <- str_extract(RcppMeCab::pos("움직이다"),"^\"")
str <- grep("VV",RcppMeCab::pos("움직이다"))
str
str_extract(as.character(RcppMeCab::pos("움직이다")),"[ㄱ-힣]{1,10}")
str_detect(as.character(RcppMeCab::pos("중국인")),"NNG|VV")
as.character(RcppMeCab::pos("움직였다"))

getwd()
library('readr')
data <- read_csv("C:/Users/82102/Documents/workingDir/data/joinData.csv",locale = locale())

testData <- as.character(data[1,6])
testData
testData <- stringr::str_remove_all(testData,"[^[ㄱ-힣\\s]]")
testData

### testData에 뜻풀이 문자열을 넣고 한자를 비롯해 한글 외의 
### 글자들 제거

tempTable <- strsplit(testData,split = " ")
tempTable
RcppMeCab::pos("한글 자모의 첫째 글자")
tempTable[[1]][2]
library('KoNLP')
library('RcppMeCab')
library('RmecabKo')
SimplePos09("한글 자모의 첫째 글자")
SimplePos09("자모의")

PosTable <- RcppMeCab::pos(testData,format="data.frame")
size <- nrow(PosTable)

tempVector <- vector()

for(i in 1:size){
  if(PosTable[i,5]=="NNG" | PosTable[i,5]=="NNP"){
    print(PosTable[i,4])
    tempVector <- c(tempVector,as.character(PosTable[i,4]))
  }
}
tempVector

len <- length(tempTable[[1]])
for(i in 1:len){
  word <- tempTable[[1]][i]
  text <- ''
  if(str_detect(as.character(RcppMeCab::pos(word)),"VV")){
    url <- paste0("https://dict.naver.com/search.nhn?dicQuery=",URLencode(word),"&query=",URLencode(word),"&target=dic&ie=utf8&query_utf=&isOnlyViewEE=")
    html <- read_html(url)
    
    item <- html_node(html,xpath = '//*[@id="content"]/div[1]/ul/li[1]/p[1]/a[1]/span/strong')
    text <- html_text(item,trim=TRUE)
    if(text=='') {
      next
    }
    if(?ext!=word){
      tempVector <- c(tempVector,text)
    }
    
    else{
      item2 <- html_node(html,xpath = '//*[@id="content"]/div[1]/ul/li[1]/p[2]/text()[2]')
      text2 <- html_text(item2,trim=TRUE)
      if(str_detect(text2,"활용형")){
        item3 <- html_node(html,xpath = '//*[@id="content"]/div[1]/ul/li[1]/p[2]/a/strong')
        text3 <- html_text(item3,trim=TRUE)
        tempVector <- c(tempVector,text3)
      }
      else{
        tempVector <- c(tempVector,text2)
      }
    }
  }
}

RcppMeCab::pos(data[10000,6])

### This is final function...

tempVector <- vector()

for(i in 1:size){
  if(PosTable[i,5]=="NNG" | PosTable[i,5]=="NNP"){
    tempVector <- c(tempVector,as.character(PosTable[i,4]))
  }
  else if(PosTable[i,5]=="VV"){
    addStr <- paste0(as.character(PosTable[i,4]),"다")
    tempVector <- c(tempVector,addStr)
  }
  else if(str_detect(PosTable[i,5],"VV")){
    url <- paste0("https://dict.naver.com/search.nhn?dicQuery=",URLencode(as.character(PosTable[i,4])),"&query=",URLencode(as.character(PosTable[i,4])),"&target=dic&ie=utf8&query_utf=&isOnlyViewEE=")
    html <- read_html(url)
    
    item <- html_node(html,css = '#content > div.kr_dic_section.search_result.dic_kr_entry > ul > li:nth-child(1) > p:nth-child(1) > a > span > strong')
    text <- html_text(item,trim=TRUE)
    if(text==PosTable[i,4]){
      item <- html_node(html,css = '#content > div.kr_dic_section.search_result.dic_kr_entry > ul > li:nth-child(1) > p:nth-child(2) > a > strong')
      text <- html_text(item,trim=TRUE)
    }
    tempVector <- c(tempVector,text)
  }
}
tempVector

###
url <- paste0("https://dict.naver.com/search.nhn?dicQuery=",URLencode("만든"),"&query=",URLencode("만든"),"&target=dic&ie=utf8&query_utf=&isOnlyViewEE=")
html <- read_html(url)

item <- html_node(html,css = '#content > div.kr_dic_section.search_result.dic_kr_entry > ul > li:nth-child(1) > p:nth-child(1) > a > span > strong')
text <- html_text(item,trim=TRUE)

if(text=="만든"){
  item <- html_node(html,css = '#content > div.kr_dic_section.search_result.dic_kr_entry > ul > li:nth-child(1) > p:nth-child(2) > a > strong')
  text <- html_text(item,trim=TRUE)
}
tempVector <- c(tempVector,text2)
tempVector
URLencode(as.character(PosTable[1,4]))

setwd("C:/Users/82102/Documents/workingDIR/function")
source("findStem.R")

tempVector <- vector()

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
tempVector

### Now...

totalLen <- nrow(data)
PosList <- vector()
for(n in 1:100){
  str <- stringr::str_remove_all(as.character(data[n,6]),"[^[ㄱ-힣\\s]]|[一-龥]")
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


source(file = "findStem.R",encoding = "UTF-8")
n <- 300200
str <- stringr::str_remove_all(as.character(data[n,6]),"[^[ㄱ-힣\\s]]|[一-龥]")
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
    if(!is.na(text)){
      tempVector <- c(tempVector,text)
    }
  }
}
if(length(tempVector)>0){
  result <- tempVector[[1]]
  for(i in 2:length(tempVector)){
    result <- paste(result,tempVector[[i]],sep = ", ")
  }
}

str
result
