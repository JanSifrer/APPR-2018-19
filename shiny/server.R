library(shiny)

function(input, output, session) {
  
  output$naselja <- renderPlot({
    graf.obcin(input$obcina)
  })
  output$zemljevid <- renderPlot({
  zemljevid.slovenije(input$num)
  })
}