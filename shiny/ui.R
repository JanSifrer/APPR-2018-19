library(shiny)

shinyUI(fluidPage(
  
  titlePanel("Slovenske občine"),
  
  tabsetPanel(
    tabPanel("Število priseljenih po letih",
             sidebarPanel(
              selectInput("obcina", "Izberi občino", 
                          c("Kranj", "Ajdovščina", "Bled"))
             ),
             uiOutput("naselja")),
    
    tabPanel("Združitev občin po skupinah",
             sidebarPanel(
               sliderInput(inputId="num", label="Izberi število:",
                           value=5, min=1, max=10)
             ),
             mainPanel(plotOutput("zemljevid")))
  )
))
