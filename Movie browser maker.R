
library(dplyr)
library(tidyr)
library(stringdist)
library(ggplot2)
library(rvest)


###### Fix path here:
paff <- "K:/Movies and TV/Movies"
#########

######## PART 1 ################
## Load files or get list:
if (!file.exists("files.csv")){
  filesList <- list.files(paff)
} else {
  filesList <- read.csv("files.csv",stringsAsFactors = F)
  filesList <- filesList[,2]}

# grep("\\.",filesList,value = T)

## Make a data.frame
f <- data.frame(n=filesList,y="",m="")

## Get years if they are there... not a huge deal
f$y <- gsub(".+(\\d{4}).+","\\1",f$n) %>% as.numeric()

## Figure out what extensions
# gsub(".+\\.","",f$n) %>% table() %>% data.frame %>% 
#   arrange(Freq) %>% tail(10)

## and remove em
f$n <- gsub("\\.m4v|\\.mov|\\.avi|\\.mp4|\\.mkv|\\.divx|\\.vob|\\.flv|\\.mpg","",f$n)

## then remove other garbage
gsub("\\-1.*","",f$n) -> f$n
gsub("\\[xX][vV][iI][dD].*","",f$n) -> f$n
gsub("\\[.+|\\(.+|\\{.+","",f$n) -> f$n
gsub("\\d{4}.*","",f$n) -> f$n

## And fix spaces
gsub("\\.|\\_"," ",f$n) -> f$n
gsub("  "," ",f$n) -> f$n
gsub("^\\s+|\\s+$", "", f$n) -> f$n
tolower(f$n) -> f$n

## Now search for imdb links, putting the year if I got it.
## The surrounding quotes will help give good catches
f$v <- paste('movie imdb "',f$n,'" (',f$y,')',sep="")
f$v <- gsub(" \\(NA\\)","",f$v)
f$v <- gsub(" ","+",f$v)

## dim the list that will get the bing results
hits <- data.frame(SearchTitle=0,Link=0,Title=0,dist=0)

## OK, now feed each into Bing to get the search results. Look for:
##- CORRECT imdb link
##- youtube trailer link 
##- rotten tomatoes score
##- 

for (i in 1:length(f$v)){
  d <- read_html(paste("http://www.bing.com/search?q=",f$v[i],sep=""))
  # e <- d %>% html_nodes("h2 a")
  e <- d %>% html_nodes(".b_srtxtstarcolor , h2 a , .b_algo a strong") 
  ## imdb. Look for this format: www.imdb.com/title/tt0315733/
  
  }
  

xxxxxxxxxxxxxxxxx what do I capture?
  
  
  
  hits[i,1] <- f$n[i]
  hits[i,2] <- as.character(winner[1,1])
  hits[i,3] <- as.character(winner[1,2])
  hits[i,4] <- stringdist(tolower(hits[i,1]),tolower(hits[i,3]),method="qgram")
  cat(paste(i,";",sep="")) ## for long waits
}

hits %>% View

# or just read in the hits
write.csv(hits,"hits.csv")


######### PART 2 ################

hits$Title %>% table %>% as.data.frame() %>% filter(Freq>1)

hits$Link <- gsub("awards","",hits$Link)
hits$Link <- paste("http://",gsub("/\"","/",hits$Link),sep="")

hits$paf <- filesList

## OK, now go get what I need out of imdb... huh... httr dont work, use rvest
## I want Year, Length, Category, Rating, Desc, Pic, mebbe actors
library(rvest)

for (i in 133:nrow(hits)){
  if(grepl("title",hits$Link[i])){
    a <- read_html(hits$Link[i])
    try(hits$yr[i] <- a %>% html_node(".header a") %>% html_text() %>% as.numeric())
    try(hits$le[i] <- a %>% html_node("#overview-top time") %>% html_text() %>% unlist)
    try(hits$ca[i] <- a %>% html_node(".infobar .itemprop") %>% html_text())
    try(hits$ra[i] <- a %>% html_node("strong span") %>%
      html_text() %>% as.numeric())
    try(hits$de[i] <- a %>% html_nodes("#overview-top p") %>%.[2]%>% html_text())
    try(hits$pi[i] <- a %>% html_node("#img_primary img") %>% html_attr("src"))
    try(hits$ac[i] <- a %>% html_nodes("#titleCast .itemprop span") %>% html_text() %>% unlist)
    cat(paste(i,";",sep="")) ## for long waits
  }
}

hits %>%
  arrange(desc(yr)) %>% 
  mutate(code=paste('<tr><td width="300px"><img width="300px" src="',pi,'">',
                    '<a target="_blank" href="', paf,'"><h3>',
                    Title,' (',yr,')', '</h3></a><br>',
                    de,
                    ' <br><br>Rating: ',ra,' || ',le,' || ',ca,
                    '<br><a target="_blank" href="',Link,'"> >>IMDB<< </a><br>',
                    '</td></tr>',
                    sep="")) %>%
  select(code)-> code


cat("<html><body><br><h1>My movies!!</h1><table border=1>",
    code[,1],
    "</table></body></html>",
    file="Try 1 - all.html")

## By rating!
hits %>% arrange(desc(ra)) %>%
  mutate(code=paste('<tr><td width="300px"><img width="300px" src="',pi,'">',
                    '<a target="_blank" href="', paf,'"><h3>',
                    Title,' (',yr,')', '</h3></a><br>',
                    de,
                    ' <br><br>Rating: ',ra,' || ',le,' || ',ca,
                    '<br><a target="_blank" href="',Link,'"> >>IMDB<< </a><br>',
                    '</td></tr>',
                    sep="")) %>%
  select(code)-> code


cat("<html><body><br><h1>Movies by rating</h1><table border=1>",
    code[,1],"</table></body></html>",file="Try - by rating.html")

## Make function to spit out as many files as I wanna!!
makePage <- function(y){
  hits %>%
    filter(ca == y) %>%
    arrange(desc(yr)) %>%
    mutate(code=paste('<tr><td width="300px"><img width="300px" src="',pi,'">',
                    '<a target="_blank" href="', paf,'"><h3>',
                    Title,' (',yr,')', '</h3></a><br>',
                    de,
                    ' <br><br>Rating: ',ra,' || ',le,' || ',ca,
                    '<br><a target="_blank" href="',Link,'"> >>IMDB<< </a><br>',
                    '</td></tr>',
                    sep="")) %>%
    select(code)-> code


    cat(paste("<html><body><br><h1>My ",y," movies</h1><table border=1>",sep=""),
    code[,1],
    "</table></body></html>",
    file=paste("Page for - ",y,".html",sep=""))
}

makePage("Action")
makePage("Comedy")
makePage("Adventure")
makePage("Drama")
makePage("Crime")
makePage("Biography")
makePage("Documentary")
makePage("Horror")
makePage("Animation")

# ## Find the bad matches
# hits %>% 
#   filter(dist>9) %>%
#   select(SearchTitle, Title) %>% View
# 
# a
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# ## Now compare against movies df
# stringdistmatrix(f$n,tolower(movies$title)) -> mat
# 
# rownames(mat) <- f$n
# colnames(mat) <- movies$ti
# result <- t(sapply(seq(nrow(mat)), function(i) {
#   j <- which.min(mat[i,])
#   c(paste(rownames(mat)[i], colnames(mat)[j], sep='/'), mat[i,j])
# }))
# 
# result <- as.data.frame(result)
# 
# 
# ## and identify problems
# result %>% 
#   filter(as.numeric(V2)<1) %>%
#   nrow
# ###################################### NEed to come back to this...
# 
# a$forVideo <- paste('Cartoon "',
#                     a$title,'" (',a$year,')',sep="")
# a$surch <- paste(a$title,'</strong> (<strong>',a$year,'</strong>)',sep="")
# 
# ## OK, now feed each of these 'forVideo' strings into Bing to get
# ## the search results
# 
# ## dim the list that will get the bing results
# # hits <- as.list(rep("0",length(L)))
# hits <- data.frame(SearchTitle=0,Link=0,Title=0,dist=0)
# 
# ## Sometimes this loop breaks for an unknown reason. If it does, just get value of i
# ## and replace "1" in for with current value of i.
# for (i in 1:length(a$forVideo)){
#   d <- GET("http://www.bing.com/", 
#            path = "search", 
#            query = list(q = a$forVideo[i]),as="text")
#   e <- content(d)
#   
#   ## OK, I have a problem... in theory, I should be xpathing this: //div[@class='b_title']
#   ## but for some strange reason it doesn't capture all the links... so go one element above
#   ## and then clean more. :-\
#   
#   stuff <- xpathSApply(e,"//li[@class='b_algo']",saveXML)
#   # stuff <- gsub('.+"http://','',stuff)
#   
#   data.frame(Link = gsub(".+www\\.|.+//","",gsub('h=\\\".+','',stuff)),
#              Title = gsub('.+>','',gsub('</strong.+','',stuff))) %>%
#     filter(grepl("youtube.com",Link)) %>% head(1) -> winner
#   
#   hits[i,1] <- a$title[i]
#   hits[i,2] <- as.character(winner[1,1])
#   hits[i,3] <- as.character(winner[1,2])
#   hits[i,4] <- stringdist(tolower(hits[i,1]),tolower(hits[i,3]),method="qgram")
#   cat(paste(i,";",sep="")) ## for long waits
# }
# 
# 
# grep("hrek",movies$title,value=T)
# 
# 
# 
# 
# 
