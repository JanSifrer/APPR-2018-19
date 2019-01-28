library(shiny)

function(input, output) {
  output$Yolo <- renderPlot({
  zemljevid.slovenije(input$num)
  })
}
