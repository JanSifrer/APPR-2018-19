library(rgdal)
library(rgeos)
library(mosaic)
library(maptools)
library(reshape2)
library(ggplot2)
library(munsell)

#Probam narediti nekaj grafov:
graf_drzavljanov <- ggplot(data = meddrzavne.selitve.drzavljanstvo,
                           mapping = aes(x=leto, y=stevilo, fill=drzava)) +
  geom_bar(stat = 'identity', position = 'dodge')


#Probam narediti lepši graf, in sicer da nebi vzel vseh BDP-jev, ampak samo njihovo povprečje
graf_bdp <- ggplot(data = bdp %>% drop_na(3), mapping = aes(x=Cas,y=mean_(bdp$BDP, 
                                                                    groups=bdp$Drzava),
                                                            fill=Drzava)) + 
  geom_bar(stat = 'identity', position = 'dodge', show.legend = FALSE)

#plot(graf_bdp)

graf_odseljeni <- ggplot(data = odseljeni.prebivalci, mapping = aes(x = leto, y = stevilo,
                         fill = drzava)) + 
  geom_bar(stat = 'identity', position = 'dodge', show.legend = TRUE)

#plot(graf_odseljeni)

graf_priseljeni <-  ggplot(data = priseljeni.prebivalci, mapping = aes(x = leto, y = stevilo,
                                                                      fill = drzava)) + 
  geom_bar(stat = 'identity', position = 'dodge', show.legend = TRUE)

#plot(graf_priseljeni)

graf_gibanj <- ggplot(data = selitveno.gibanje, mapping = aes(x = leto, y = stevilo,
                                                                  fill = obcina)) + 
  geom_bar(stat = 'identity', position = 'dodge', show.legend = FALSE)

#plot(graf_gibanj)



# Uvozimo zemljevid Sveta
# source("https://raw.githubusercontent.com/jaanos/APPR-2018-19/master/lib/uvozi.zemljevid.r")
source("lib/uvozi.zemljevid.r") #Nastavi pravo datoteko

svet <- uvozi.zemljevid("https://www.naturalearthdata.com/http//www.naturalearthdata.com/download/50m/cultural/ne_50m_admin_0_countries.zip",
                        "ne_50m_admin_0_countries") %>% fortify()

#Nekaj mi ne dela. Zemljevid mi prenese, shrani v mapo 'zemljevidi' pol pa javi
#neko napako: Cannot open data source??


# Zemljevid sveta skrčimo na zemljevid Evrope
europe <- filter(svet, CONTINENT == "Europe")
europe <- filter(europe, long < 55 & long > -45 & lat > 30 & lat < 85)

ggplot(europe, aes(x=long, y=lat, group=group, fill=NAME)) +
  geom_polygon() +
  labs(title="Evropa - osnovna slika") +
  theme(legend.position="none")



