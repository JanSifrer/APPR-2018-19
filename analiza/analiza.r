# 4. faza: Analiza podatkov

imf <- razlika.obcine %>% mutate(obcina=parse_factor(obcina, levels(zemljevid$OB_UIME)))
imf.norm <- imf %>% select(-obcina) %>% scale()
rownames(imf.norm) <- imf$razlika

zemljevid.slovenije <- function(n) {
  k <- kmeans(imf.norm, n, nstart=1000)
  skupina <- data.frame(obcina=imf$obcina, skupina=factor(k$cluster))
  print(ggplot() + geom_polygon(data=left_join(zemljevid,skupina, 
                                            by=c("OB_UIME"="obcina")), 
                                aes(x=long, y=lat, group=group, 
                                    fill=skupina)))
}
