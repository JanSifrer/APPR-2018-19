library(shiny)

shinyUI(fluidPage(
  
   titlePanel(" "),
  
  tabsetPanel(
    tabPanel("Število priseljenih po občinah",
             sidebarPanel(
              selectInput("naloga", "Izberite vrsto:",
                          c("Priseljeni", "Izseljeni")),
              selectInput("obcina", "Izberite občino", 
                          sort(unique(selitveno.gibanje$obcina)))
             ),
             mainPanel(dygraphOutput("naselja"))),
    
    tabPanel("Združitev občin po skupinah",
             sidebarPanel(
               sliderInput(inputId="num", label="Izberite število:",
                           value=3, min=1, max=10)
             ),
             mainPanel(plotOutput("zemljevid")))
  )
))
