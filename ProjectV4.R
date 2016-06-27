
#Libraies:

#For general stats
library(stringi)

library(stringr)
library(tm)
library(ggplot2)
library(wordcloud)
library(dplyr)
#library(slam)
#require(reshape2)
library(ngram)


##Load data:
#Articles:
art=tolower(stopwords("english"))
#US:
US.Blogdir=readLines("D:/Coursera/DataScience/CapStone/final/en_US/en_US.blogs.txt")
US.Newsdir=readLines("D:/Coursera/DataScience/CapStone/final/en_US/en_US.news.txt")
US.Twitdir=readLines("D:/Coursera/DataScience/CapStone/final/en_US/en_US.twitter.txt")
#DE:
# DE.Blogdir=readLines("D:/Coursera/DataScience/CapStone/final/de_DE/de_DE.blogs.txt")
# DE.Newsdir=readLines("D:/Coursera/DataScience/CapStone/final/de_DE/de_DE.news.txt")
# DE.Twitdir=readLines("D:/Coursera/DataScience/CapStone/final/de_DE/de_DE.twitter.txt")
# #FI
# FI.Blogdir=readLines("D:/Coursera/DataScience/CapStone/final/fi_FI/fi_FI.blogs.txt")
# FI.Newsdir=readLines("D:/Coursera/DataScience/CapStone/final/fi_FI/fi_FI.news.txt")
# FI.Twitdir=readLines("D:/Coursera/DataScience/CapStone/final/fi_FI/fi_FI.twitter.txt")
  
blogs <- US.Blogdir
news <- US.Newsdir
twitter <- US.Twitdir 


# blogs <- DE.Blogdir
# news <- DE.Newsdir
# twitter <- DE.Twitdir 

# blogs <- FI.Blogdir
# news <- FI.Newsdir
# twitter <- FI.Twitdir


#Exploratory analysis:

#Table of Statistics
stat=t(sapply(list(blogs,news,twitter),stri_stats_general))
WxL=lapply(list(blogs,news,twitter),function(x) stri_count_words(x))
totalw=unlist(lapply(WxL,function(x) sum(x)))
totalwxl=rbind(summary(WxL[[1]]),summary(WxL[[2]]),summary(WxL[[3]]))
stat=cbind(c("blogs","news","twitter"),stat,totalw,totalwxl)
stat

#Histograms
par(mfrow=c(1,3))
hist(WxL[[1]], breaks=100, main="Blogs Frecuency per line", xlab="Words per Line")
abline(v=mean(WxL[[1]]),col=2)
hist(WxL[[2]], breaks=100, main="News Frecuency per line",xlab="Words per Line")
abline(v=mean(WxL[[2]]),col=2)
hist(WxL[[3]], breaks=100, main="Twitter Frecuency per line",xlab="Words per Line")
abline(v=mean(WxL[[3]]),col=2)




Info=list(blogs,news,twitter)

#Separar palabras en un vector

# palabras = list()
# for (i in 1:length(sInfo))
# {
#   #palabras=strsplit(sInfo[[1]], "[ ]")
#   palabras[[i]]=sInfo[[i]]
#   palabras[[i]]=unlist(strsplit(palabras[[i]], "[ ]"))
# }
# 
# pblogs = palabras[[1]]
# pnews = palabras[[2]]
# ptwitter = palabras[[3]]
# 
# #Eliminar Artículos
# pblogs=pblogs[-which(pblogs %in% art)]
# pnews=pnews[-which(pnews %in% art)]
# ptwitter=ptwitter[-which(ptwitter %in% art)]
# 
# p=c(pblogs,pnews,ptwitter)
# 
# p=paste(p, collapse=" ")

#Sampling the data,

#The data is sampled with 1% of the data:

sInfo=list()
for (j in 1:length(Info))
{
  i=1:length(Info[[j]])
  lInfo=sample(i,trunc(length(i)*0.2))
  sInfo[[j]]=Info[[j]][lInfo]
}

#Cleaning data

#Uncommon characters are erased.

for (i in 1:length(Info))
{
  for(j in 1:length(Info[[i]]))
  {
    Line=Info[[i]][j]
    Rline=iconv(Line,to="ASCII//TRANSLIT")
    Info[[i]][j] <- Rline
  }
}

#Second,  puncutation, white spaces, numbers and stopwords are removed and the upper case characters are transformed to lower case.

#Separar palabras en un vector
# 
# palabras = list()
# for (i in 1:length(Info))
# {
#   #palabras=strsplit(sInfo[[1]], "[ ]")
#   palabras[[i]]=Info[[i]]
#   palabras[[i]]=unlist(strsplit(palabras[[i]], "[ ]"))
# }


library(qdap)
(o <- check_spelling_interactive(head(palabras)))

for (i in 1:length(Info))
{
  #Eliminar espacios, numeros y signos de puntuación
  Info[[i]] = sub("([[:space:]])","",Info[[i]])
  Info[[i]] = gsub("([[:digit:]])","",Info[[i]])
  Info[[i]] = gsub("([[:punct:]])","",Info[[i]])
  #Colocar texto en minúsculas
  Info[[i]] = tolower(Info[[i]])
}


####Para Bigrams
#Separar palabras en un vector

palabras = list()
for (i in 1:length(sInfo))
{
  palabras[[i]]=sInfo[[i]]
  palabras[[i]]=unlist(strsplit(palabras[[i]], "[ ]"))
}

pblogs = palabras[[1]]
pnews = palabras[[2]]
ptwitter = palabras[[3]]

#Eliminar Artículos
pblogs=pblogs[-which(pblogs %in% art)]
pnews=pnews[-which(pnews %in% art)]
ptwitter=ptwitter[-which(ptwitter %in% art)]

p=c(pblogs,pnews,ptwitter)

p=paste(p, collapse=" ")

#Bigram
bigram=ngram_asweka(p, min = 2, max = 2, sep = " ")
bigram=as.data.frame(table(bigram),stringsAsFactors = FALSE)
colnames(bigram) <- c('bigram', 'Count')
bigram <- bigram[order(bigram$Count,decreasing = TRUE),]

####3-4grams

for (j in 1:length(sInfo))
{
  sInfo[[i]]=paste(sInfo[[i]], collapse=" ")
}


trigram = list()
Quagram = list()
for (i in 1:length(sInfo))
{

  #Trigram
  trigram[[i]]=ngram_asweka(sInfo[[i]], min = 3, max = 3, sep = " ")
  trigram[[i]]=as.data.frame(table(trigram[[i]]),stringsAsFactors = FALSE)
  colnames(trigram[[i]]) <- c('trigram', 'Count')
  trigram[[i]] <- trigram[[i]][order(trigram[[i]]$Count,decreasing = TRUE),]
  
  #Quadgram
  Quagram[[i]]=ngram_asweka(sInfo[[i]], min = 4, max = 4, sep = " ")
  Quagram[[i]]=as.data.frame(table(Quagram[[i]]),stringsAsFactors = FALSE)
  colnames(Quagram[[i]]) <- c('Quagram', 'Count')
  Quagram[[i]] <- Quagram[[i]][order(Quagram[[i]]$Count,decreasing = TRUE),]
}


#Predictions

#Sentence to look for
w="arctic monkeys"

# First remove the non-alphabatical characters

w=iconv(w,to="ASCII//TRANSLIT")

#w = sub("([[:space:]])","",w)
w = gsub("([[:digit:]])","",w)
w = gsub("([[:punct:]])","",w)
#Colocar texto en minúsculas
w = tolower(w)

#Organazing the sentence:
inStr <- unlist(strsplit(w, split=" "))
inStrLen <- length(inStr);

quadsentlist=list()
trisentlist=list()
for (i in 1:length(sInfo))
{
  #Prediction with the QuadGram
  
  inStr1 <- paste(inStr[(inStrLen-2):inStrLen], collapse=" ");
  searchStr <- paste("^",inStr1, sep = "");
  
  quadsentlist[[i]] <- Quagram[[i]][grep (searchStr, Quagram[[i]]$Quagram), ]
  
  #Prediction with the TriGram
  
  inStr1 <- paste(inStr[(inStrLen-1):inStrLen], collapse=" ");
  searchStr <- paste("^",inStr1, sep = "");
  
  trisentlist[[i]] <- trigram[[i]][grep (searchStr, trigram[[i]]$trigram), ]
  
}


#Prediction with Bigram
w1=unlist(strsplit(w, "[ ]"))
w1=w1[-which(w1 %in% art)]
w1=paste(w1, collapse=" ")
inStrB <- unlist(strsplit(w1, split=" "))
inStrLenB <- length(inStrB);

inStr1B <- paste(inStrB[inStrLenB], collapse=" ");
searchStrB <- paste("^",inStr1B, sep = "");

bisentlist <- bigram[grep (searchStrB, bigram$bigram), ]

quadsentlist
trisentlist
t=bisentlist
#Plotting a Word Cloud

# set.seed(456)  
# par(mfrow = c(1, 3))  # Establish Plotting Panel
# headings = c("US Blogs", "US News", "US Twits")
# for (i in 1:length(sInfo))
# {
#   wordcloud(words = colnames(tdm[[i]]), freq = col_sums(tdm[[i]]), 
#             scale = c(3, 1), max.words = 100, random.order = FALSE, rot.per = 0.35, 
#             use.r.layout = FALSE, colors = brewer.pal(8, "Dark2"))
#   title(headings[i])  
# }
