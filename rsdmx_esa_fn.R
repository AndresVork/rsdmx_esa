#Function to download data from Statistics Estonia database. Needs the acronym of the table name as an input.
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
  
  for(i in setdiff(labelnames, paste0("CL_", mytable, "_OBS_STATUS"))) {
    assign(substr(i,nchar(paste0("CL_", mytable, "_"))+1, nchar(i)), 
           as.data.frame(slot(dfstruct, "codelists"), codelistId = i) %>% 
             dplyr::select(id, starts_with("label")))
  }
  
  for(i in setdiff(names(df), c("obsTime", "obsValue"))) {
    df <- merge(df, get(i), by.x=i, by.y = "id" )
    names(df)[names(df) == 'label.en'] <- paste0(i,'label.en')
    names(df)[names(df) == 'label.et'] <- paste0(i,'label.et')
  }
  return(df)
} 

                       
#If you want to add restrictions to data, add restrictions to 
rsdmx_esa_long <- function(mytable, mytablewithrestrictions) {
  require(rsdmx)
  require(dplyr)
  tf <- tempfile(tmpdir = tdir <- tempdir())
  download.file(paste0("http://andmebaas.stat.ee/restsdmx/sdmx.ashx/GetData/", mytablewithrestrictions), tf)
  df <- readSDMX(tf, isURL = FALSE) %>% as.data.frame()
  dfstruct <- readSDMX(paste0("http://andmebaas.stat.ee/restsdmx/sdmx.ashx/GetDataStructure/", mytable))
  varnames <- as.data.frame(dfstruct@concepts)
  varnames$Name.et <- iconv(varnames$Name.et, "UTF-8")
  labellist <- slot(dfstruct, "codelists")
  labelnames <- sapply(slot(labellist, "codelists"), function(x) slot(x, "id"))
  
  for(i in setdiff(labelnames, paste0("CL_", mytable, "_OBS_STATUS"))) {
    assign(substr(i,nchar(paste0("CL_", mytable, "_"))+1, nchar(i)), 
           as.data.frame(slot(dfstruct, "codelists"), codelistId = i) %>% 
             dplyr::select(id, starts_with("label")))
  }
  
  for(i in setdiff(names(df), c("obsTime", "obsValue"))) {
    df <- merge(df, get(i), by.x=i, by.y = "id" )
    names(df)[names(df) == 'label.en'] <- paste0(i,'label.en')
    names(df)[names(df) == 'label.et'] <- paste0(i,'label.et')
  }
  return(df)
}

                       
                       
#Example
# df <- rsdmx_esa("LE211")
# require(ggplot2)                       
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

# df <- rsdmx_esa_long("TT64", "TT64/37+39+44+49+51+57+59+65+67+70+74+78+82+84+86.1+2+3+4+5+6+7+8+9+10+11+12/all?startTime=2005&endTime=2017")
# 
# require(ggplot2)
# df %>% 
#   ggplot(aes(x=as.Date(paste0(obsTime, "-", DIM3, "-01")), 
#              y=obsValue, colour=DIM2label.et)) +
#   geom_line() +
#   geom_point()+
#   facet_wrap(~DIM2label.et, scales = "free_y") +
#   scale_x_date(date_labels = "%b%Y")+
#   guides(colour = FALSE) +
#   labs(y = "Registreeritud töötud, %",
#        x = "",
#        colour = "",
#        title = "Registreeritud töötud kuu lõpus",
#        subtitle = "Andmed: Statistikaamet, tabel TT64") +
#   theme(text=element_text(size=14))
                       
                       
