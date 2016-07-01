shinyUI
(
  navbarPage
  (
    "CAPSTONE PROJECT",
    tabPanel
    (
      "Prediction",  
      sidebarPanel
      (    
        textInput('word', 'Enter the sentence',value = ""),  
        actionButton('Calculo','Predict')
      ),
     mainPanel
     (
        h4("The next word is one of these options:"),
        plotOutput("grafica"),
        dataTableOutput("modelo")
     )
    )
  )
)