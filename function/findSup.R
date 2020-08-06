findSup <- function(str){
  url <- paste0("https://dict.naver.com/search.nhn?dicQuery=",URLencode(str),
                "&query=",URLencode(str),"&target=dic&ie=utf8&query_utf=&isOnlyViewEE=")
  html <- read_html(url)
  item <- html_node(html,css = '#content > div.kr_dic_section.search_result.dic_kr_entry > ul > li:nth-child(1) > p:nth-child(1) > sup')
  supNumber <- html_text(item,trim = TRUE)
  
  if(!is.na(supNumber)){
    if(as.integer(supNumber)<10){
      str <- paste0(str,'00',supNumber)
    }
    if(as.integer(supNumber)>=10){
      str <- paste0(str,'0',supNumber)
    }
  }
  if(is.na(supNumber)){
    str <- paste0(str,'001')
  }
  return(str)
}