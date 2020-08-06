findStem <- function(str){
  url <- paste0("https://dict.naver.com/search.nhn?dicQuery=",URLencode(str),"&query=",URLencode(str),"&target=dic&ie=utf8&query_utf=&isOnlyViewEE=")
  html <- read_html(url)
  item <- html_node(html,css = '#content > div.kr_dic_section.search_result.dic_kr_entry > ul > li:nth-child(1) > p:nth-child(1) > a:nth-child(1) > span > strong')
  text <- html_text(item,trim = TRUE)
  inx <- 1
  while(is.na(text) | stringr::str_sub(text,nchar(text),nchar(text))!="다"){
    inx <- inx+1
    cssPath = paste0('#content > div.kr_dic_section.search_result.dic_kr_entry > ul > li:nth-child(',as.character(inx),') > p:nth-child(1) > a:nth-child(1) > span > strong')
    item <- html_node(html,css = cssPath)
    text <- html_text(item,trim = TRUE)
    if(inx>10) break
  }
  inx <- 1
  while(is.na(text) | stringr::str_sub(text,nchar(text),nchar(text))!="다"){
    cssPath = paste0('#content > div.en_dic_section.search_result.dic_en_entry > dl > dt:nth-child(',as.character(inx),') > a:nth-child(1) > span > strong')
    item <- html_node(html,css = cssPath)
    text <- html_text(item,trim = TRUE)
    inx <- inx+1
    if(inx>10) break
  }
  
  return(text)
}