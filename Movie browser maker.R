
library(dplyr)
library(tidyr)
library(stringdist)
library(ggplot2)
library(rvest)
library(htmltools)


###### Fix path here:
paff <- "K:/Movies and TV/Movies"
#########

######## PART 1 get list of movies with basic metadata ################
## Load files or get list:
if (!file.exists("files.csv")){
  filesList <- list.files(paff)
  write.csv(filesList,"files.csv")
} else {
  filesList <- read.csv("files.csv",stringsAsFactors = F)
  filesList <- filesList[,2]
}
# grep("\\.",filesList,value = T)

## Make a data.frame
f <- data.frame(n=filesList,y="",m="")

## Get years if they are there... not a huge deal
f$y <- gsub(".+(\\d{4}).+","\\1",f$n) %>% as.numeric()

## Figure out what extensions
gsub(".+\\.","",f$n) %>% table() %>% data.frame %>%
  arrange(Freq) %>% tail(10)

## and remove em
f$n <- gsub("\\.m4v|\\.mov|\\.avi|\\.mp4|\\.mkv|\\.divx|\\.vob|\\.flv|\\.mpg","",f$n)

## then remove other garbage
gsub("\\-1.*","",f$n) -> f$n
gsub("\\[xX][vV][iI][dD].*","",f$n) -> f$n
gsub("\\[.+|\\(.+|\\{.+","",f$n) -> f$n
gsub("(?<=.)\\d{4}.*","",f$n,perl = T) -> f$n

## And fix spaces
gsub("\\.|\\_"," ",f$n) -> f$n
gsub("  "," ",f$n) -> f$n
gsub("^\\s+|\\s+$", "", f$n) -> f$n
tolower(f$n) -> f$n

## Now lastly trim everything:
trim <- function (x) gsub("^\\s+|\\s+$", "", x)
f$n <- trim(f$n)

## Now search for imdb links, putting the year if I got it.
## The surrounding quotes will help give good catches
f$v <- paste(' imdb "',f$n,'" (',f$y,')',sep="")
f$v <- gsub(" \\(NA\\)","",f$v)
f$v <- gsub(" ","+",f$v)


## OK, go get the data!
if(!file.exists("hits.csv")){
  ## dim the list that will get the bing results
  hits <- data.frame(RealTitle=0,SearchTitle=0,imdbLink=0,YoutubeLink=0,RTscore=0,Paff=0)
  
  ## OK, now feed each into Bing to get the search results. Look for:
  ##- CORRECT imdb link
  ##- youtube trailer link 
  ##- rotten tomatoes/metacritic score 
  
  #length(f$v)
  st <- Sys.time()
  for (i in 1:length(f$v)){
    hits[i,1] <- f$n[i]
    hits[i,6] <- f$n[i]
    
    d <- read_html(paste("http://www.bing.com/search?q=",f$v[i],sep=""))
    # e <- d %>% html_nodes("h2 a")
    e <- d %>% html_nodes(".b_srtxtstarcolor , h2 a , .b_algo a strong") 
  
    ## Real name. Look at all text and find the most frequent... that's probably the name
    # hits[i,2] <- html_text(e) %>% table %>% head(1) %>% as.data.frame() %>% row.names()
    try(hits[i,2] <- gsub(" *\\(\\d{4}\\).*","",html_text(e)[grep("imdb.com/title/tt\\d+/$",html_attr(e,"href"))])[1])
  
    ## imdb. Look for this format: www.imdb.com/title/tt0315733/
    try(hits[i,3] <- html_attr(e,"href")[grep("imdb.com/title/tt\\d+/$",html_attr(e,"href"))][1])
    
    ## youtube trailer
    try(hits[i,4] <- html_attr(e,"href")[grep("youtube.+[Tt]railer",e)][1])
    
    ## Get scores
    try(hits[i,5] <-  paste(html_text(e)[grep("b_srtxtstarcolor.+%",e)-2],html_text(e)[grep("b_srtxtstarcolor.+%",e)])[1])
    cat(paste(i,"'"))
    }
  et <- Sys.time()  
  et-st
  
  ## Ok, that does a pretty good job.. but not a perfect job. Let's find out which movies were missed 
  ## and retry the search without the quotes tho... that should let Bing's NLP engine help us out a bit.
  
  hits.bad <- hits %>% filter(is.na(SearchTitle),!is.na(RealTitle))
  hits.bad$RealTitle <- gsub(" ","%20",hits.bad$RealTitle)
  
  for (i in 1:nrow(hits.bad)){
    d <- read_html(paste('http://www.bing.com/search?q=imdb+',hits.bad$RealTitle[i],sep=""))
    # e <- d %>% html_nodes("h2 a")
    e <- d %>% html_nodes(".b_srtxtstarcolor , h2 a , .b_algo a strong") 
    try(hits.bad[i,3] <- html_attr(e,"href")[grep("imdb.com/title/tt\\d+/$",html_attr(e,"href"))][1])
    try(hits.bad[i,4] <- html_attr(e,"href")[grep("youtube.+[Tt]railer",e)][1])
    try(hits.bad[i,5] <-  paste(html_text(e)[grep("b_srtxtstarcolor.+%",e)-2],html_text(e)[grep("b_srtxtstarcolor.+%",e)])[1])
    cat(paste(i,"'"))
  }
  
  hits %>% filter(!is.na(SearchTitle)) %>%
    bind_rows(hits.bad) -> hits
  
  ## Save it for posterity  
  write.csv(hits,"hits.csv")
  
  ######### PART 2 ################
  ## Could also get the Script and do EmoMo and feeling chart...............................................
  
  ## OK, come up w/ some reports:
  ## Are there any duplicates?:  
  hits$RealTitle %>% table %>% as.data.frame() %>% filter(Freq>1) -> Dups
  
  ## What about movies that were STILL not recognized?
  gsub("%20"," ",hits %>% filter(is.na(SearchTitle)) %>% select(RealTitle)) -> UnFound
  
  ## OK, now go get what I need out of imdb... 
  ## I want Year, Length, Category, Rating, Description, Pic, Awards, popularity
  ## (mebbe actors, director?) ...............................
  
  for (i in 1:nrow(hits)){
    if(!is.na(hits$imdbLink[i])){
      a <- read_html(hits$imdbLink[i])
      try(hits$yr[i] <- a %>% html_node("#titleYear a") %>% html_text() %>% as.numeric())
      try(hits$le[i] <- a %>% html_node("#title-overview-widget time") %>% html_text() %>% unlist)
      try(hits$ca[i] <- a %>% html_node(".subtext .itemprop") %>% html_text())
      try(hits$ra[i] <- a %>% html_node(".ratingValue span") %>%
            html_text() %>% as.numeric())
      try(hits$de[i] <- a %>% html_nodes(".summary_text") %>% html_text())
      try(hits$pi[i] <- a %>% html_node("#title-overview-widget img") %>% html_attr("src"))
      try(hits$aw[i] <- a %>% html_nodes("#titleAwardsRanks span:nth-child(3)") %>% html_text() %>% unlist)
      try(hits$po[i] <- gsub(",","",a %>% html_nodes("#title-overview-widget .small") %>% html_text()) %>% as.numeric() %>% max(na.rm=T))
      
      cat(paste(i,";",sep="")) ## for long waits
      
      ## Get rid of 10 things I hate about you... not sure wtf...
      hits$pi[hits$pi =="http://ia.media-imdb.com/images/M/MV5BMTI4MzU5OTc2MF5BMl5BanBnXkFtZTYwNzQxMjc5._V1_UY268_CR3,0,182,268_AL_.jpg"] <- NA
      
      write.csv(hits,"hits.csv")
    }
  }
} else {
  ## or just read in the hits
  hits <- read.csv("hits.csv",stringsAsFactors = F)
  hits <- hits[,-1]
}

## Create image of imdb rating vs popularity: 
RaPo <- ggplot(hits,aes(x=po,y=ra,label=RealTitle))+geom_point()
# Rapo
# ggplotly()

# xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx change over to single filter + Sort
# xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx Investigate link b/w imdb, rottentomatoes and metacritic... maybe only one is needed?
# XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX investigate relationship RealTitle vs SearchTitle
# xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx http://www.imsdb.com ... just saying.... emomo? Cluster analysis on emomo shape category? Other features?
# xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx Change scrollbar css http://beautifulpixels.com/goodies/create-custom-webkit-scrollbar/
## Prepare youtube previews, if they exist
hits$ytl[!is.na(hits$YoutubeLink)] <-   paste('<a href="',hits$YoutubeLink[!is.na(hits$YoutubeLink)],'"><img class="buttons" height="20px" src="yt.png"></a>',sep="")
hits$ytl[is.na(hits$YoutubeLink)] <- ""

## Prepare imdb links, if they exist
hits$imdbl[!is.na(hits$imdbLink)] <-   paste('<a href="',hits$imdbLink[!is.na(hits$imdbLink)],'"><img class="buttons" height="20px" src="imdb.png"></a>',sep="")
hits$imdbl[is.na(hits$imdbLink)] <- ""

## Prepare awards
# xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx 

## And arrange them into a nice lil codeblock
hits %>%
  arrange(desc(yr)) %>% 
  mutate(code=paste('<div class="element-item ', ca, '">',
                    '<table><tr><td width="50%"><img class="main" src="', pi,'" width="167px"></td>',
                    '<td width="50%">',
                    '<h4>', SearchTitle,'</h4>',
                    '<h5>(', RealTitle,')</h5>',
                    '<span class="ca">', ca, '</span>',
                    '<span class="year"> (', yr, ') </span>',
                    '<span class="le"> ', le, ' </span>',
                    '<span class="rating"> <', ra, '> </span>',
                    '<span><a title="',RTscore,'"></a>',
                    imdbl, ytl,
                    '<a title="',aw,'">Awards</a>',
                    '</span><p class="abstract">',de ,'</p>',
                    '</td></tr></table>',
                    '</div>',
                    sep="")) %>% select(code)-> code

## Done! Now put some HTML above and below that. And win.
writeLines(c(readLines("top.txt"),
    code[,1],readLines("end.txt")),con = paste("isotope-docs/newTry-",Sys.Date() ,".html",sep="")) 


