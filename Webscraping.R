library(rvest)
library(R.utils)



url_constructor <- function(word, from=1, to=50){
  return(paste0("https://www.n-tv.de/suche/?q=",word,"&at=m&page=",c(from:to)))
}

get_links <- function(url, categoryPage=FALSE){
  tryCatch({
    page <- read_html(url)
    if(!categoryPage){
      links <- html_nodes(page, ".teaser__content")
      
      links <- sapply(links, function(x) tryCatch({xml_attrs(xml_child(x, 2))[["href"]]}
                                                  ,error=function(e){
                                                    xml_attrs(xml_child(x))[["href"]]
                                                  }))
    }else if(categoryPage){
      links <- html_nodes(page, ".teaser")
      links <- sapply(links, function(x) tryCatch({xml_attrs(xml_child(xml_children(x)[[1]], 1))[["href"]]},
                                                  error=function(e){
                                                    xml_attrs(xml_children(x)[[1]])[["href"]]
                                                  }))
    }else{
      stop(paste("Invalid categoryPage Argument:",class(categoryPage),"specified where TRUE/FALSE is expected"))
    }
    ellist <- strsplit(links,"/")
    ntv <- sapply(ellist, function(x) x[3]=="www.n-tv.de")
    category <- sapply(ellist, function(x) x[4]%in%c("sport","politik","wirtschaft","leute","leben","panorama","technik","ratgeber","wissen","auto"))
    links <- links[which(ntv&category)]
    return(links)
  }, error=function(e){
    warning(paste("An error has been caught at URL", url,":",e))
    return(NA)
  })
}



parse_ntv <- function(url){
  tryCatch({
    page <- read_html(url)
    text_element <- html_nodes(page, "p")
    text <- html_text(text_element)
    text <- paste(text[which(nchar(text)>50)], collapse=" ")
    title <- html_text(html_nodes(page, ".article__headline"))
    date <- html_text(html_nodes(page, ".article__date"))
    label <- strsplit(url,"/")[[1]][4]
    if(!length(text)>0){
      text <- NA
    }
    if(!length(title)>0){
      title <- NA
    }
    if(!length(date)>0){
      date <- NA
    }
    if(!length(label)>0){
      label <- NA
    }
    return(data.frame(title, date, text,label))
  }, error=function(e){
    warning(paste("An error has been caught while parsing", url,":",e))
    return(data.frame(title=NA, date=NA, text=NA, label=NA))
  })
}


query_words <- c("die", "der", "und", "in", "zu", "den", "das", "nicht", "von", "sie",
                 "ist", "des", "sich", "mit", "dem", "dass", "er", "es", "ein", "ich", 
                 "auf", "so", "eine", "auch", "als", "an", "nach", "wie", "im", "für","man", "aber", "aus", "durch", 
                 "wenn", "nur", "war", "noch", "werden", "bei",
                 "hat", "wir", "was", "wird", "sein", "einen", "welche", "sind", "oder", "zur",
                 "um", "haben", "einer", "mir", "über", "ihm", "diese", "einem", "ihr", "uns",
                 "da", "zum", "kann", "doch", "vor", "dieser", "mich", "ihn", "du", "hatte",
                 "seine", "mehr", "am", "denn", "nun", "unter", "sehr", "selbst", "schon", "hier",
                 "bis", "habe", "ihre", "dann", "ihnen", "seiner", "alle", "wieder", "meine", "Zeit",
                 "gegen", "vom", "ganz", "einzelnen", "wo", "muss", "ohne", "eines", "können", "sei")


# Actual code

# Constructing URLs
queries <- character()
for (x in query_words) {
  queries <- append(queries, url_constructor(x,1,100))
}

total <- length(queries)
cat("Now retrieving Article urls. This may take a while.")
pb <- txtProgressBar(min = 0, max = total, style = 3)
urls <- character()
for(x in 1:length(queries)){
  urls2 <- append(urls, get_links(queries[x]))
  setTxtProgressBar(pb, x)
}
close(pb)

urls <- urls[!duplicated(urls)]

# Collecting garbage
rm(queries, query_words)
# Crawling

articles <- data.frame()
total <- length(urls)
cat("Now crawling",total, "articles. This may take several hours. Please stand by.")
pb <- txtProgressBar(min = 0, max = total, style = 3)
for(x in 1:length(urls)){
  articles <- rbind(articles,parse_ntv(urls[x]))
  setTxtProgressBar(pb, x)
}
close(pb)

# recovering from timeouts:
retry <- which(is.na(articles$title))
articles.re <- data.frame()
total <- length(retry)
cat("Trying to re-crawl",total, "articles. Please stand by.")
pb <- txtProgressBar(min = 0, max = total, style = 3)
for(x in 1:length(retry)){
  articles.re <- rbind(articles.re,parse_ntv(urls[retry[x]]))
  setTxtProgressBar(pb, x)
}
close(pb)


################
## CHECKPOINT ##
################

# Merging
articles <- rbind(articles, articles.re)
rm(articles.re)

# Scraping underrepresented categories
leben <- c("https://www.n-tv.de/leben/","https://www.n-tv.de/leben/Liebe_und_Familie/","https://www.n-tv.de/leben/Gesundheit/",
          "https://www.n-tv.de/leben/reise/","https://www.n-tv.de/leben/Wohnen/","https://www.n-tv.de/leben/essen/",
          "https://www.n-tv.de/Spezial/dubai/")

technik <- c("https://www.n-tv.de/technik/")

wissen <- c("https://www.n-tv.de/wissen/", "https://www.n-tv.de/thema/frage-antwort","https://www.n-tv.de/wissen/Fakten_und_Mythen/",
            "https://www.n-tv.de/thema/fundsache","https://www.n-tv.de/wissen/Helmholtz/")

auto <- c("https://www.n-tv.de/auto/","https://www.n-tv.de/auto/praxistest/","https://www.n-tv.de/auto/gebrauchte/")

ratgeber <- c("https://www.n-tv.de/ratgeber/", "https://www.n-tv.de/ratgeber/tests/")

tmp <- c(leben, technik, wissen, auto, ratgeber)
add_urls <- character()

for(y in tmp){
  add_urls <- append(add_urls, get_links(y, categoryPage = TRUE))
}
rm(tmp,leben, technik, wissen, auto, ratgeber)
add_urls <- add_urls[!duplicated(add_urls)]
add_urls <- add_urls[!add_urls %in% urls]

# Scraping
articles.add <- data.frame()
total <- length(add_urls)
cat("Trying to crawl",total, "articles. Please stand by.")
pb <- txtProgressBar(min = 0, max = total, style = 3)
for(x in 1:length(add_urls)){
  articles.add <- rbind(articles.add,parse_ntv(add_urls[x]))
  setTxtProgressBar(pb, x)
}
close(pb)

# Merging and cleaning
articles <- rbind(articles, articles.add)
rm(articles.add)
articles <- articles[!is.na(articles$title),]
articles <- articles[!is.na(articles$text),]
articles <- articles[!is.na(articles$label),]
articles <- articles[!duplicated(articles),]

empty <- which(articles$text=="")
articles <- articles[-empty,]

# Standardise Date format
# Remove Day of Week (german)
articles$date <- trimws(gsub(".*,","",articles$date))
articles$date <- trimws(gsub("Januar","01.",articles$date))
articles$date <- trimws(gsub("Februar","02.",articles$date))
articles$date <- trimws(gsub("März","03.",articles$date))
articles$date <- trimws(gsub("April","04.",articles$date))
articles$date <- trimws(gsub("Mai","05.",articles$date))
articles$date <- trimws(gsub("Juni","06.",articles$date))
articles$date <- trimws(gsub("Juli","07.",articles$date))
articles$date <- trimws(gsub("August","08.",articles$date))
articles$date <- trimws(gsub("September","09.",articles$date))
articles$date <- trimws(gsub("Oktober","10.",articles$date))
articles$date <- trimws(gsub("November","11.",articles$date))
articles$date <- trimws(gsub("Dezember","12.",articles$date))
articles$date <- trimws(gsub(" ","",articles$date))

# Save
write.csv(articles, "articles.csv")

# Save Image
rm(x,y,retry,empty,pb)
save.image("ntv_complete.RData")
summary(articles$label)


