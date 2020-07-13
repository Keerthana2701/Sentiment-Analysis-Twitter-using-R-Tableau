
install.packages("tm")
install.packages("ggmap")
install.packages("wordcloud")


library(twitteR)
library(ROAuth)
library(RCurl)
library(httr)

library(stringr)
library(plyr)
library(dplyr)
library(tm)

library(ggmap)
library(wordcloud)


key="0H1vydAKPKuMD0MuaGqIICbGi"
secret="CAv00pN4SxRMqLC9TXjBSPl4SGRGsrAFUIaIySN3CTL0ECIbam"

atoken ="1273394433136836610-HKGoJ4tg3UnuLPj1VXK9nL689aB9Pe"
asecret ="LXhryOMcKxMwZuwyStZA1fqvNM450sEQRdqz69gQuE4dP"

setup_twitter_oauth(key, secret, atoken, asecret)

searchTwitter("Samsung")
searchTwitter("from:Udemy")

sample=searchTwitter("from:Udemy")

tweets=searchTwitter("apple+iphone", n=2000, 
                     lang="en", 
                     geocode="34.1,-118.2,150mi")


## extracting the text 

tweettext=sapply(tweets,function(x) x$getText())
View(tweettext)


## extracting the date

tweetdate=lapply(tweets,function(x) x$getCreated())
tweetdate=sapply(tweetdate,function(x) strftime(x, format="%Y-%m-%d %H:%M:%S",tz="UTC"))

View(tweetdate)

##first cleaning stage

tweettext=lapply(tweettext,function(x) iconv(x,"latin1","ASCII",sub=""))

tweettext=lapply(tweettext,function(x) gsub("htt.*",' ',x))
tweettext=lapply(tweettext,function(x) gsub("#",' ',x))
tweettext


##lexicons

pos=readLines("positive-words.txt")
neg=readLines("negative-words.txt")

neg2=c(neg, "bearish", "fraud")
tail(neg2)

##sentiment score function

sentimentfun=function(tweettext, pos, neg, .progress='non')
{
scores=laply(tweettext,
             function(singletweet, pos, neg ) 
             {
               singletweet=gsub("[[:punct:]]","",singletweet)
               singletweet=gsub("[[:cntrl:]]","",singletweet)
               singletweet=gsub("\\d+","",singletweet)
              
               tryTolower=function(x)
               {
                 
                y=NA
                try_error=tryCatch(tolower(x),error=function(e) e)
                if(!inherits(try_error,"error"))
                  y=tolower(x)
                return(y)
                
               }
               singletweet=sapply(singletweet, tryTolower) 
               
               word.list=str_split(singletweet,"\\s+")
               words=unlist(word.list)
               pos.matches=match(words,pos)
               neg.matches=match(words,neg)
               
               pos.matches=!is.na(pos.matches)
               neg.matches=!is.na(neg.matches)
               
               score=sum(pos.matches)-sum(neg.matches)
               return(score)
             } ,pos,neg,.progress=.progress )
  
  sentiment.df=data.frame(text=tweettext,score=scores)
  return(sentiment.df)
  
  
}


## scores

scores = sentimentfun(tweettext, pos, neg, .progress='text')
View(scores)

##Creating a dataset and exporting as excel

isretweet=sapply(tweets, function(x) x$getIsRetweet())
retweetcount=sapply(tweets, function(x) x$getRetweetCount())

favoritecount=sapply(tweets, function(x) x$getFavoriteCount())
data=as.data.frame(cbind(ttext=tweettext %>% unlist(),
                         date=tweetdate %>% unlist(),
                         isretweet=isretweet %>% unlist(), 
                         retweetcount=retweetcount %>% unlist(),
                         favoritecount=favoritecount %>% unlist(),
                         score = scores$score %>% unlist(),
                         product = "Apple Iphone" %>% unlist(),
                         city = "Los Angeles"%>% unlist(), country = "USA" %>% unlist()))



View(data)

data2 = duplicated(data[,1])
data$duplicate=data2
View(data)


write.csv(data, file= "ResultFile.csv")

typeof(data$ttext)
typeof(data$date)

typeof(data$isretweet)

typeof(data$retweetcount)

typeof(data$favoritecount)
typeof(data$score)
typeof(data$product)



