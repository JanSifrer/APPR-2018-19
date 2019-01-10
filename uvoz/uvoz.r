# 2. faza: Uvoz podatkov

sl <- locale("sl", decimal_mark=",", grouping_mark=".")


#Funkcija za uvoz vseh podatkov o meddržavnih selitvah

uvozi.meddrzavne.selitve.vsi <- function() {
  data <- read_csv2("podatki/deltabele1.csv", skip=3, na=c("", "..."),
                    locale=locale(encoding="Windows-1250")) %>% fill(1) %>% drop_na(2)
  colnames(data) <- c("vrsta", "starost",
                      paste(colnames(data)[-(1:2)] %>% strapplyc("^([^_]*)") %>% unlist(),
                      matrix(rep(1991:2017, 3), nrow=3, byrow=TRUE), sep="_"))
  data <- melt(data, id.vars=c("vrsta", "starost"), value.name="stevilo") %>%
    separate(variable, c("spol", "leto"), sep="_") %>%
    mutate(leto=parse_number(leto))
  return(data)
}
meddrzavne.selitve.vsi <- uvozi.meddrzavne.selitve.vsi()


#Funkcija za uvoz osnovnih podatkov o meddržavnih selitvah

uvozi.meddrzavne.selitve.osnovni <- function() {
  data <- read_csv2("podatki/tabela1.csv", skip=3, na=c("", "..."),
                    locale=locale(encoding="windows-1250")) %>% fill(1) %>% drop_na(2)
  colnames(data) <- c("vrsta", "starost",
                      paste(colnames(data)[-(1:2)] %>% strapplyc("^([^_]*)") %>% unlist(),
                      matrix(rep(1991:2017, 2), nrow=2, byrow=TRUE), sep="_"))
  data <- melt(data, id.vars=c("vrsta", "starost"), value.name="stevilo") %>%
    separate(variable, c("spol", "leto"), sep="_") %>%
    mutate(leto=parse_number(leto))
  return(data)
}
meddrzavne.selitve.osnovni <- uvozi.meddrzavne.selitve.osnovni()




#Funkcija za uvoz državljanstva selilcev

uvozi.meddrzavne.selitve.drzavljanstvo <- function() {
  data <- read_csv2("podatki/meddrzavne.selitve2.csv", skip=3, na=c("", "...", "EVROPA"),
                    locale=locale(encoding="windows-1250")) %>% fill(1) %>% drop_na(2)
  colnames(data) <- c("vrsta", "drzava",
                      paste(colnames(data)[-(1:2)] %>% strapplyc("^([^_]*)") %>% unlist(),
                            matrix(rep(1996:2017, 2), nrow=2, byrow=TRUE), sep="_"))
  data <- melt(data, id.vars=c("vrsta", "drzava"), value.name="stevilo") %>%
    separate(variable, c("spol", "leto"), sep="_") %>%
    mutate(leto=parse_number(leto)) %>% drop_na(5)
  return(data)
}
meddrzavne.selitve.drzavljanstvo <- uvozi.meddrzavne.selitve.drzavljanstvo()




#Funkcija za uvoz selitvenega gibanja

uvozi.selitveno.gibanje <- function() {
  data <- read_csv2("podatki/selitveno.gibanje.csv", skip=3, na=c("", "...", "-"),
                    locale=locale(encoding="windows-1250"))
  colnames(data) <- c("obcina",
                      paste(colnames(data)[-(1)] %>% strapplyc("^([^_]*)") %>% unlist(),
                            matrix(rep(1995:2017, 4), nrow=4, byrow=TRUE), sep="_"))
  data <- melt(data, id.vars=c("obcina"), value.name="stevilo") %>%
    separate(variable, c("vrsta", "spol"), sep=" - ") %>%
    separate(spol, c("spol", "leto"), sep="_") %>%
    mutate(leto=parse_number(leto)) %>% drop_na(5)
  data <- data[c(2,3,1,4,5)]
  return(data)
}
selitveno.gibanje <- uvozi.selitveno.gibanje()



#Funkcija za uvoz odseljencev

uvozi.odseljene <- function() {
  data <- read_csv2("podatki/odseljeni.prebivalci.novo2.csv", skip=2, na=c("", "...", "-"),
                    locale=locale(encoding="windows-1250")) %>% fill(1,2,3,4) %>% drop_na(6)
  colnames(data) <- c("spol", "drzava", "leto", "izobrazba", "drzavljanstvo", "stevilo")
  data <- data[c(5,2,1,3,6)]
  data <- data %>% mutate(drzava=drzava %>% strapplyc("^[.]*(.*)") %>% unlist())
  return(data)
}
odseljeni.prebivalci <- uvozi.odseljene()



#Funkcija za uvoz priseljencev

uvozi.priseljene <- function() {
  data <- read_csv2("podatki/priseljeni.prebivalci.novo2.csv", skip=2, na=c("", "...", "-"),
                    locale=locale(encoding="windows-1250")) %>% fill(1:4) %>% drop_na(6)
  colnames(data) <- c("spol", "drzava", "leto", "izobrazba", "drzavljanstvo", "stevilo")
  data <- data[c(5,2,1,3,6)]
  data <- data %>% mutate(drzava=drzava %>% strapplyc("^[.]*(.*)") %>% unlist())
  return(data)
}
priseljeni.prebivalci <- uvozi.priseljene()


#Funkcija za uvoz iz spletne strani
uvozi.bdp <- function() {
  data <- read.csv("podatki/bdp.csv", encoding="windows-1250", na=c(":"), 
                   col.names=c("Cas", "Drzava", "X1", "X2", "BDP", "X3"))
  data <- data[,-(3:4)] 
  data <- data[,-4]
}
bdp <- uvozi.bdp()



# Funkcija, ki uvozi občine iz Wikipedije
uvozi.obcine <- function() {
  link <- "http://sl.wikipedia.org/wiki/Seznam_ob%C4%8Din_v_Sloveniji"
  stran <- html_session(link) %>% read_html()
  tabela <- stran %>% html_nodes(xpath="//table[@class='wikitable sortable']") %>%
    .[[1]] %>% html_table(dec=",")
  for (i in 1:ncol(tabela)) {
    if (is.character(tabela[[i]])) {
      Encoding(tabela[[i]]) <- "UTF-8"
    }
  }
  colnames(tabela) <- c("obcina", "povrsina", "prebivalci", "gostota", "naselja",
                        "ustanovitev", "pokrajina", "regija", "odcepitev")
  tabela$obcina <- gsub("Slovenskih", "Slov.", tabela$obcina)
  tabela$obcina[tabela$obcina == "Kanal ob Soči"] <- "Kanal"
  tabela$obcina[tabela$obcina == "Loški potok"] <- "Loški Potok"
  for (col in c("povrsina", "prebivalci", "gostota", "naselja", "ustanovitev")) {
    tabela[[col]] <- parse_number(tabela[[col]], na="-", locale=sl)
  }
  for (col in c("obcina", "pokrajina", "regija")) {
    tabela[[col]] <- factor(tabela[[col]])
  }
  return(tabela)
}



# Funkcija, ki uvozi podatke iz datoteke druzine.csv
uvozi.druzine <- function(obcine) {
  data <- read_csv2("podatki/druzine.csv", col_names=c("obcina", 1:4),
                    locale=locale(encoding="Windows-1250"))
  data$obcina <- data$obcina %>% strapplyc("^([^/]*)") %>% unlist() %>%
    strapplyc("([^ ]+)") %>% sapply(paste, collapse=" ") %>% unlist()
  data$obcina[data$obcina == "Sveti Jurij"] <- "Sveti Jurij ob Ščavnici"
  data <- data %>% melt(id.vars="obcina", variable.name="velikost.druzine",
                        value.name="stevilo.druzin")
  data$velikost.druzine <- parse_number(data$velikost.druzine)
  data$obcina <- factor(data$obcina, levels=obcine)
  return(data)
}

# Zapišimo podatke v razpredelnico obcine
obcine <- uvozi.obcine()

# Zapišimo podatke v razpredelnico druzine.
druzine <- uvozi.druzine(levels(obcine$obcina))

# Če bi imeli več funkcij za uvoz in nekaterih npr. še ne bi
# potrebovali v 3. fazi, bi bilo smiselno funkcije dati v svojo
# datoteko, tukaj pa bi klicali tiste, ki jih potrebujemo v
# 2. fazi. Seveda bi morali ustrezno datoteko uvoziti v prihodnjih
# fazah.

