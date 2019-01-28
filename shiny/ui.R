library(shiny)

shinyUI(fluidPage(
  
  titlePanel("Slovenske občine"),
  
  tabsetPanel(
    tabPanel("Število priseljenih po občinah",
             sidebarPanel(
              selectInput("obcina", "Izberi občino", 
                          c(selitveno.gibanje$obcina))
             ),
             mainPanel(plotOutput("naselja"))),
    
    tabPanel("Združitev občin po skupinah",
             sidebarPanel(
               sliderInput(inputId="num", label="Izberi število:",
                           value=5, min=1, max=10)
             ),
             mainPanel(plotOutput("zemljevid")))
  )
))
