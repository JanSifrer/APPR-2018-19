library(shiny)

shinyServer(function(input, output) {
  output$stevilo <- renderUI(
    numericInput(inputId = "stevilo", label="Izberi stevilo",value=10,min=1, max=10, step=1)
  )
  output$obcine <- renderPlot({
    main <- "Razdelitev Slovenije"
    return(inp)
  })
})
