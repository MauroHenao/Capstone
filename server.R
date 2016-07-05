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
osentence=function(w)
{
  w=iconv(w,to="ASCII//TRANSLIT")
  #w = sub("([[:space:]])","",w)
  w = gsub("([[:digit:]])","",w)
  w = gsub("([[:punct:]])","",w)
  #Colocar texto en minúsculas
  w = tolower(w)
  inStr <- unlist(strsplit(w, split=" "))
  return(inStr)
}

#Function with qua and trigram
modelQT=function(inStr,inStrLen)
{
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
}


shinyServer
(  
  function(input, output,session) 
  { 
    
    #Qua and trigram
    inStr <-osentence(input$word)
    inStrLen <- length(inStr);
    
#Mostrar las primeras 10 filas con los asesores top del modelo
    output$modelo<-renderDataTable(modelfraude)
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