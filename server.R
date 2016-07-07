#For general stats
#library(stringi)

library(stringr)
library(tm)
# library(ggplot2)
# library(wordcloud)
# library(dplyr)
# #library(slam)
# #require(reshape2)
# library(ngram)



library(shiny)
#Load the Ngrams
load("BQuagram.RData")
load("BTrigram.RData")
load("NQuagram.RData")
load("NTrigram.RData")
load("TQuagram.RData")
load("TTrigram.RData")
load("Bigram.RData")

#Organizing the sentence that was written fo Qua and Trigram:
osentence=function(w,b)
{
  w=iconv(w,to="ASCII//TRANSLIT")
  #w = sub("([[:space:]])","",w)
  w = gsub("([[:digit:]])","",w)
  w = gsub("([[:punct:]])","",w)
  #Colocar texto en minúsculas
  w = tolower(w)
  inStr <- unlist(strsplit(w, split=" "))
  if(b==1)
  {
    art=tolower(stopwords("english"))
    w1=unlist(strsplit(w, "[ ]"))
    w1=w1[-which(w1 %in% art)]
    w1=paste(w1, collapse=" ")
    inStr <- unlist(strsplit(w1, split=" "))
  }
  return(inStr)
}

#Function with qua and trigram
modelQT=function(sen)
{
  inStr=osentence(sen,0)
  inStrLen=length(inStr)
  Quagram=rbind(BQuagram,NQuagram,TQuagram)
  Quagram <- Quagram[order(Quagram$Count,decreasing = TRUE),]
  Trigram=rbind(Btrigram,Ntrigram,Ttrigram)
  Trigram <- Trigram[order(Trigram$Count,decreasing = TRUE),]
  if(inStrLen>2)
  {
    #Prediction with the QuadGram
    inStr1 <- paste(inStr[(inStrLen-2):inStrLen], collapse=" ");
    searchStr <- paste("^",inStr1, sep = "");
    
    quadsentlist <- Quagram[grep (searchStr, Quagram$Quagram), ]
    pw=as.data.frame(word(quadsentlist[,1],-1))
    if(nrow(pw)!=0)
    {  
      names(pw)="Next_Word"
      return(pw)
    }
  }
  else if(inStrLen>1)
  {
    #Prediction with the TriGram
    
     inStr1 <- paste(inStr[(inStrLen-1):inStrLen], collapse=" ")
     searchStr <- paste("^",inStr1, sep = "")
     trisentlist <- Trigram[grep (searchStr, Trigram$trigram), ]
     pw=as.data.frame(word(trisentlist[,1],-1))
     if(nrow(pw)!=0)
     {  
       names(pw)="Next_Word"
       return(pw)
     }     
  }
  else
  {
    #Prediction with Bigram
    inStr=osentence(sen,1)
    inStrLen=length(inStr)
    inStr <- paste(inStr[inStrLen], collapse=" ");
    searchStr <- paste("^",inStr, sep = "");
    bisentlist <- Bigram[grep (searchStr, Bigram$bigram), ]
    pw=as.data.frame(word(bisentlist[,1],-1))
    if(nrow(pw)!=0)
    {  
      names(pw)="Next_Word"
      return(pw)
    }
  }
}

shinyServer
(  
  function(input, output,session) 
  { 
    observeEvent(input$Calculo,output$modelo<-renderDataTable(modelQT(input$word)))
#Mostrar las primeras 10 filas con los asesores top del modelo

#     output$grafica<-renderPlot({ggplot(modelfraude1, aes(x=AsesorSinDirecto ,y=value,fill=variable)) +
#         geom_bar(stat="identity",position="dodge")})
# 
#     observeEvent(input$InfAsesor,
#                               {
#                                 output$CelRep<-renderDataTable(CelRep(input$Asesor))
#                                 output$AseClien<-renderDataTable(AseClien(input$Asesor))
#                                 output$NroCelRep<-renderDataTable(NroCelRep(input$Asesor))
#                               }
#                  )
  }
)