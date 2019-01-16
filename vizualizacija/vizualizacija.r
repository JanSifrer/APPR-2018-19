# 3. faza: Vizualizacija podatkov

# Uvozimo zemljevid.
zemljevid <- uvozi.zemljevid("http://baza.fmf.uni-lj.si/OB.zip", "OB",
                             pot.zemljevida="OB", encoding="Windows-1250")
levels(zemljevid$OB_UIME) <- levels(zemljevid$OB_UIME) %>%
  { gsub("Slovenskih", "Slov.", .) } %>% { gsub("-", " - ", .) }
zemljevid$OB_UIME <- factor(zemljevid$OB_UIME, levels=levels(obcine$obcina))
zemljevid <- fortify(zemljevid)

# Izračunamo povprečno velikost družine
povprecja <- druzine %>% group_by(obcina) %>%
  summarise(povprecje=sum(velikost.druzine * stevilo.druzin) / sum(stevilo.druzin))

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


# ggplot(data=odseljeni.prebivalci %>% group_by(drzava, drzavljanstvo) %>% summarise(Stevilo=sum(stevilo)),
#        aes(x = drzava, y = Stevilo, fill = drzava)) +
#   geom_bar(stat = 'identity', position = 'dodge', show.legend = FALSE) +
#   facet_grid(~ drzavljanstvo) + coord_flip()


