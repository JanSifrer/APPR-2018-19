library(shiny)

function(input, output) {
  output$Yolo <- renderPlot({
    k <- kmeans(imf.norm, input$num, nstart=1000)
    skupina <- data.frame(obcina=imf$obcina, skupina=factor(k$cluster))
    print(ggplot() + geom_polygon(data=left_join(zemljevid,skupina, 
                                                 by=c("OB_UIME"="obcina")), 
                                  aes(x=long, y=lat, group=group, 
                                      fill=skupina)))
  })
}
