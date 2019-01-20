library(shiny)

shinyUI(fluidPage(
  
  titlePanel("Slovenske občine"),
  
  tabsetPanel(
      tabPanel("Zemljevid Slovenije",
               sidebarPanel(
                  uiOutput("stevilo")
                ),
               mainPanel("stevilo"))
    )
))
