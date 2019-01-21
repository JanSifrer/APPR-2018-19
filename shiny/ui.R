library(shiny)

pageWithSidebar(
  headerPanel('Razrezana Slovenija :D'),
  sidebarPanel(
    sliderInput(inputId="num", label="izberi število",
                value=5, min=1, max=10)
  ),
  mainPanel(
    plotOutput("Yolo")
  )
)