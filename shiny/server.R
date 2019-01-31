library(shiny)

function(input, output, session) {
  
  output$naselja <- renderDygraph({
    graf.obcin(input$obcina, input$naloga)
  })
  output$zemljevid <- renderPlot({
  zemljevid.slovenije(input$num)
  })
}