#Probam narediti nekaj grafov:
graf_drzavljanov <- ggplot(data = meddrzavne.selitve.drzavljanstvo,
                           mapping = aes(x=leto, y=stevilo, fill=drzava)) +
  geom_bar(stat = 'identity', position = 'dodge')


#Probam narediti lepši graf, in sicer da nebi vzel vseh BDP-jev, ampak samo njihovo povprečje
funkcija.bdp <- function() {
  podatki <- bdp %>% drop_na(3)
  podatki$BDP <- parse_number(podatki$BDP)
  povprecje <- podatki %>% group_by(Drzava) %>% summarise(Povprecje.bdp = mean(BDP)) %>% arrange(desc(Povprecje.bdp))
  return(povprecje)
}
  
graf_bdp <- ggplot(data = funkcija.bdp(), mapping = aes(x=reorder(Drzava, Povprecje.bdp) ,y=Povprecje.bdp,
                                                            fill=Drzava)) + 
  geom_bar(stat = 'identity', position = 'dodge', show.legend = FALSE)

#plot(graf_bdp) + coord_flip()

#Probam sešteti za vsako državo koliko se je določenih državljanov preselilo vanjo. To bom kasneje probal še
#narisati na zemljevid Evrope.

# ggplot(data=odseljeni.prebivalci %>% group_by(drzava, drzavljanstvo) %>% summarise(Stevilo=sum(stevilo)),
#        aes(x = drzava, y = Stevilo, fill = drzava)) +
#   geom_bar(stat = 'identity', position = 'dodge', show.legend = FALSE) +
#   facet_grid(~ drzavljanstvo) + coord_flip()
# 
# ggplot(data=odseljeni.prebivalci %>% group_by(drzava, drzavljanstvo) %>% summarise(Stevilo=sum(stevilo)),
#        aes(x = drzava, y = Stevilo, fill = drzavljanstvo)) +
#   geom_bar(stat = 'identity', position = 'dodge') + ggtitle("Odseljeni prebivalci glede na drzavljanstvo") +
#   coord_flip()


#naredil graf v katerem se vidi iz katerih držav je prišlo največ ljudi z določeno izobrazbo.
#npr. iz Bosne in Hercegovine je prišlo največ ljudi - tako s srednešolsko kot tudi z osnovnošolsko izobrazbo.

# ggplot(data=priseljeni.prebivalci %>% group_by(drzava, izobrazba) %>% summarise(Stevilo=sum(stevilo)),
#        aes(x = drzava, y = Stevilo, fill = izobrazba)) +
#   geom_bar(stat = 'identity', position = 'dodge') + ggtitle("Priseljeni prebivalci glede na izobrazbo") +
#   coord_flip()

#plot(graf_priseljeni)

#probal bom narisati zemljevid iz kje  kerih občin se je največ Slovencev izselilo,
#in v katere občine se je največ Slovencev priselilo.

odseljeni.obcine <- selitveno.gibanje %>% filter(vrsta == "Odseljeni v tujino") %>% 
                              group_by(obcina) %>% summarise(vsota=sum(stevilo))

priseljeni.obcine <- selitveno.gibanje %>% filter(vrsta == "Priseljeni iz tujine") %>% 
                              group_by(obcina) %>% summarise(vsota=sum(stevilo))
razlika.obcine <- merge(odseljeni.obcine, priseljeni.obcine, by="obcina")
razlika.obcine["razlika"] <- priseljeni.obcine$vsota - odseljeni.obcine$vsota

# ggplot() + geom_polygon(data=left_join(zemljevid, razlika.obcine, by=c("OB_UIME"="obcina")),
#                         aes(x=long, y=lat, group=group, fill=razlika)) +
#   ggtitle("Razlika priseljenih in odseljenih") + xlab("") + ylab("") +
#   guides(fill=guide_colorbar(title="Število"))


#Uvozim zemljevid Slovenije
zemljevid <- uvozi.zemljevid("http://baza.fmf.uni-lj.si/OB.zip", "OB",
                             pot.zemljevida="OB", encoding="Windows-1250")
levels(zemljevid$OB_UIME) <- levels(zemljevid$OB_UIME) %>%
{ gsub("Slovenskih", "Slov.", .) } %>% { gsub("-", " - ", .) }
zemljevid$OB_UIME <- factor(zemljevid$OB_UIME, levels=levels(obcine$obcina))
zemljevid <- fortify(zemljevid)
