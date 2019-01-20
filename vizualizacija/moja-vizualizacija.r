#Probam narediti nekaj grafov:
graf.meddrzavne.selitve <- ggplot(data=meddrzavne.selitve.drzavljanstvo %>% group_by(vrsta, drzava)
                           %>% summarise(Stevilo=sum(stevilo)), 
                           mapping = aes(x=reorder(drzava, Stevilo), y=Stevilo, fill=vrsta)) + 
  geom_bar(stat = 'identity', position = 'dodge') + xlab("Država") + ylab("Število") +
  ggtitle("Graf števila odseljenih in priseljenih po državah") + coord_flip()


graf.meddrzavne.selitve.odseljeni <- ggplot(data=meddrzavne.selitve.osnovni %>% 
                                              filter(vrsta=="Odseljeni v tujino") %>% 
                                       group_by(spol, starost, spol) %>% 
                                       summarise(Stevilo=sum(stevilo)), 
                                     mapping = aes(x=starost, y=Stevilo, fill=spol)) + 
                                       geom_bar(stat = 'identity', position = 'dodge') +
  xlab("Starost") + ylab("Število")+ ggtitle("Graf odseljenih Slovencev po letih") + coord_flip()

graf.meddrzavne.selitve.priseljeni <- ggplot(data=meddrzavne.selitve.osnovni %>% 
                                               filter(vrsta=="Priseljeni iz tujine") %>% 
                                                 group_by(spol, starost) %>% 
                                                 summarise(Stevilo=sum(stevilo)), 
                                               mapping = aes(x=starost, y=Stevilo, fill=spol)) + 
  geom_bar(stat = 'identity', position = 'dodge') +
  xlab("Starost") + ylab("Število")+ ggtitle("Graf priseljenih Slovencev po letih") + coord_flip()


funkcija.bdp <- function() {
  podatki <- bdp %>% drop_na(3)
  podatki$BDP <- parse_number(podatki$BDP)
  povprecje <- podatki %>% group_by(Drzava) %>% summarise(Povprecje.bdp = mean(BDP)) %>% arrange(desc(Povprecje.bdp))
  return(povprecje)
}
graf_bdp <- ggplot(data = funkcija.bdp(), mapping = aes(x=reorder(Drzava, Povprecje.bdp) ,y=Povprecje.bdp,
                                                            fill=Drzava)) + 
  geom_bar(stat = 'identity', position = 'dodge', show.legend = FALSE) +
  xlab("Država") + ylab("BDP") + ggtitle("Povprečni BDP po Evropskih državah") + coord_flip()


graf.odseljeni.prebivalci <- ggplot(data=odseljeni.prebivalci %>% group_by(drzava, drzavljanstvo) %>% summarise(Stevilo=sum(stevilo)),
       aes(x = drzava, y = Stevilo, fill = drzavljanstvo)) +
  geom_bar(stat = 'identity', position = 'dodge') + 
  xlab("Država") + ylab("Število") + ggtitle("Odseljeni prebivalci glede na državljanstvo") +
  coord_flip()


#naredil graf v katerem se vidi iz katerih držav je prišlo največ ljudi z določeno izobrazbo.
#npr. iz Bosne in Hercegovine je prišlo največ ljudi - tako s srednešolsko kot tudi z osnovnošolsko izobrazbo.

graf.priseljeni.prebivalci.izo <- ggplot(data=priseljeni.prebivalci %>% group_by(drzava, izobrazba) %>% summarise(Stevilo=sum(stevilo)),
       aes(x = drzava, y = Stevilo, fill = izobrazba)) +
  geom_bar(stat = 'identity', position = 'dodge') +
  xlab("Država") + ylab("Število") +
  ggtitle("Priseljeni prebivalci glede na izobrazbo") +
  coord_flip()

graf.odseljeni.prebivalci.izo <- ggplot(data=odseljeni.prebivalci %>% group_by(drzava, izobrazba) %>% summarise(Stevilo=sum(stevilo)),
                                     aes(x = drzava, y = Stevilo, fill = izobrazba)) +
  geom_bar(stat = 'identity', position = 'dodge') +
  xlab("Država") + ylab("Število") +
  ggtitle("Odseljeni prebivalci glede na izobrazbo") +
  coord_flip()


#probal bom narisati zemljevid iz kje  kerih občin se je največ Slovencev izselilo,
#in v katere občine se je največ Slovencev priselilo.

odseljeni.obcine <- selitveno.gibanje %>% filter(vrsta == "Odseljeni v tujino") %>% 
                              group_by(obcina) %>% summarise(vsota=sum(stevilo))

odseljeni.obcine[["obcina"]] <- factor(odseljeni.obcine[["obcina"]]) #rabim za zemljevid

priseljeni.obcine <- selitveno.gibanje %>% filter(vrsta == "Priseljeni iz tujine") %>% 
                              group_by(obcina) %>% summarise(vsota=sum(stevilo))
razlika.obcine <- merge(odseljeni.obcine, priseljeni.obcine, by="obcina")
razlika.obcine["razlika"] <- (priseljeni.obcine$vsota - odseljeni.obcine$vsota)/prebivalstvo$stevilo


# ggplot() + geom_polygon(data=left_join(zemljevid, razlika.obcine, by=c("OB_UIME"="obcina")),
#                         aes(x=long, y=lat, group=group, fill=razlika)) +
#   ggtitle("Razlika priseljenih in odseljenih") + xlab("") + ylab("") +
#   guides(fill=guide_colorbar(title="Število"))


#Uvozim zemljevid Slovenije
#source("lib/uvozi.zemljevid.r")
zemljevid <- uvozi.zemljevid("http://baza.fmf.uni-lj.si/OB.zip", "OB",
                             pot.zemljevida="OB", encoding="Windows-1250")
levels(zemljevid$OB_UIME) <- levels(zemljevid$OB_UIME) %>%
{ gsub("Slovenskih", "Slov.", .) } %>% { gsub("-", " - ", .) }
zemljevid$OB_UIME <- factor(zemljevid$OB_UIME, levels=levels(prebivalstvo$obcina))
zemljevid <- fortify(zemljevid)
