#Autorid: Taavi Unt ja Andres Võrk
#Kõik šokolaadid meile
esa_tables_fn <-  function(lang) {
  require(rvest)
  require(dplyr)
  require(stringr)
  
  url=paste0("http://andmebaas.stat.ee/?lang=", lang) 
  page = read_html(url)
  datasets=page %>% 
    html_nodes(".ds") %>%
    html_text() %>% 
    str_split_fixed(":", 2) %>% 
    data.frame()
  colnames(datasets) = if(lang=="et") {
    c("Tabel","Selgitus")
  } else {
    c("Table", "Description")
  }
  return(datasets)
} 

search_esa_fn <- function(keyword, lang="et", ignore.case=TRUE ) {
  datasets= esa_tables_fn(lang)
  datasets[(grepl(keyword, datasets[,2], ignore.case = ignore.case)),]
  }

#Näide / Example
# 
# search_esa_fn("tualett")
# search_esa_fn("tualett", "et", TRUE)
# search_esa_fn("research", "en", FALSE)
#And then use  rsdmx_esa to download the table
# df <- rsdmx_esa("LET48")
