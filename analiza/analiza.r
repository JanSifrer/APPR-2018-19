# 4. faza: Analiza podatkov

imf <- razlika.obcine %>% mutate(obcina=parse_factor(obcina, levels(zemljevid$OB_UIME)))
imf.norm <- imf %>% select(-obcina) %>% scale()
rownames(imf.norm) <- imf$obcina

zemljevid.slovenije <- function(n) {
  k <- kmeans(imf.norm, n, nstart=1000)
  skupina <- data.frame(obcina=imf$obcina, skupina=factor(k$cluster))
  print(ggplot() + geom_polygon(data=left_join(zemljevid,skupina, 
                                            by=c("OB_UIME"="obcina")), 
                                aes(x=long, y=lat, group=group, 
                                    fill=skupina)) + xlab("") + ylab(""))
}




meddrzavne.priseljeni <- meddrzavne.selitve.vsi %>%
  filter(starost=="Starostne skupine - SKUPAJ") %>%
  filter(spol=="Spol - SKUPAJ") %>%
  filter(vrsta=="Priseljeni iz tujine")

meddrzavni.priseljeni <- function() {
  don<-xts(x = meddrzavne.priseljeni$stevilo, order.by=as.yearqtr.default(meddrzavne.priseljeni$leto))
  dygraph(don) %>%
    dyOptions(labelsUTC = TRUE, fillGraph=TRUE, fillAlpha=0.1, drawGrid = FALSE, colors="#D8AE5A") %>%
    dyRangeSelector() %>%
    dyCrosshair(direction = "vertical") %>%
    dyHighlight(highlightCircleSize = 5, highlightSeriesBackgroundAlpha = 0.2, hideOnMouseOut = FALSE)  %>%
    dyRoller(rollPeriod = 1) %>%
    dySeries("V1", label = "Število priseljenih")
}

graf.obcin <- function(ime){
  zenske <- selitveno.gibanje %>% filter(vrsta=="Priseljeni iz tujine" &
                                           obcina==ime & spol=="Ženske")
  moski <- selitveno.gibanje %>% filter(vrsta=="Priseljeni iz tujine" &
                                          obcina==ime & spol=="Moški")
  moski$stevilo <- moski$stevilo + zenske$stevilo
  don<-xts(x = moski$stevilo, order.by=as.yearqtr.default(moski$leto))
  dygraph(don) %>%
    dyOptions(labelsUTC = TRUE, fillGraph=TRUE, fillAlpha=0.1, 
              drawGrid = FALSE, colors="#D8AE5A") %>%
    dyRangeSelector() %>%
    dyCrosshair(direction = "vertical") %>%
    dyHighlight(highlightCircleSize = 5, highlightSeriesBackgroundAlpha = 0.2, 
                hideOnMouseOut = FALSE)  %>%
    dyRoller(rollPeriod = 1) %>%
    dySeries("V1", label = "Število priseljenih")
}


#Brez veze... nič se ne vidi.
#library(ggplot2)
#library(plotly)
#library(gapminder)

#p <- meddrzavne.selitve.vsi %>%
#   filter(starost != "Starostne skupine - SKUPAJ" & starost != "Povprečna starost") %>%
#   filter(spol=="Spol - SKUPAJ") %>%
#   ggplot( aes(stevilo, leto, size = stevilo, color=starost)) +
#   geom_point() +
#   theme_bw()
# 
# ggplotly(p)
