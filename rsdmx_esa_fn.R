#Teen funktsiooni
rsdmx_esa <-  function(mytable) {
  require(rsdmx)
  require(dplyr)
  tf <- tempfile(tmpdir = tdir <- tempdir())
  download.file(paste0("http://andmebaas.stat.ee/restsdmx/sdmx.ashx/GetData/", mytable, "/all"), tf)
  df <- readSDMX(tf, isURL = FALSE) %>% as.data.frame()
  dfstruct <- readSDMX(paste0("http://andmebaas.stat.ee/restsdmx/sdmx.ashx/GetDataStructure/", mytable))
  varnames <- as.data.frame(dfstruct@concepts)
  varnames$Name.et <- iconv(varnames$Name.et, "UTF-8")
  labellist <- slot(dfstruct, "codelists")
  labelnames <- sapply(slot(labellist, "codelists"), function(x) slot(x, "id"))
  for(i in labelnames) {
    assign(substr(i,nchar(paste0("CL_", mytable, "_"))+1, nchar(i)), as.data.frame(slot(dfstruct, "codelists"), codelistId = i))
  }
  for(i in grep("DIM", names(df), value=TRUE)) {
    df <- merge(df, get(i), by.x=i, by.y = "id" )
    names(df)[names(df) == 'label.en'] <- paste0(i,'label.en')
    names(df)[names(df) == 'label.et'] <- paste0(i,'label.et')
  }
  return(df)
} 

#Example
# df <- rsdmx_esa("LE211")
# df %>% filter(DIM2label.et=="Osatähtsus (kulutused kokku = 100), %", DIM3label.et=="Tervishoid") %>%
#   ggplot(aes(x=as.numeric(obsTime), y=obsValue, color=DIM4label.et)) +
#   geom_line() +
#   geom_point()+
#   #scale_x_continuous(breaks = seq(2010, 2017, 1)) +
#   labs(y = "Osakaal, %",
#        x = "",
#        colour = "",
#        title = "Tervishoiukulutuste osakaal leibkondade eelarves \n tulukvintiili järgi",
#        subtitle = "Andmed: Statistikaamet, tabel LE211") +
#   theme(text=element_text(size=14))
