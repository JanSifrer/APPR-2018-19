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
starosti <- c("0-4 let", "5-9 let", "10-14 let", "15-19 let", "20-24 let", "25-29 let",
              "30-34 let", "35-39 let", "40-44 let", "45-49 let", "50-54 let", "55-59 let",
              "60-64 let", "65-69 let", "70-74 let", "75-79 let", "80-84 let", "85 + let")
uvozi.meddrzavne.selitve.osnovni <- function() {
  data <- read_csv2("podatki/tabela1.csv", skip=3, na=c("", "..."),
                    locale=locale(encoding="windows-1250")) %>% fill(1) %>% drop_na(2)
  colnames(data) <- c("vrsta", "starost",
                      paste(colnames(data)[-(1:2)] %>% strapplyc("^([^_]*)") %>% unlist(),
                      matrix(rep(1991:2017, 2), nrow=2, byrow=TRUE), sep="_"))
  data <- melt(data, id.vars=c("vrsta", "starost"), value.name="stevilo") %>%
    separate(variable, c("spol", "leto"), sep="_") %>%
    mutate(leto=parse_number(leto), starost=parse_factor(starost, starosti, ordered=TRUE))
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
  data <- data[c(5,4,2,1,3,6)]
  data <- data %>% mutate(drzava=drzava %>% strapplyc("^[.]*(.*)") %>% unlist())
  return(data)
}
odseljeni.prebivalci <- uvozi.odseljene()



#Funkcija za uvoz priseljencev

uvozi.priseljene <- function() {
  data <- read_csv2("podatki/priseljeni.prebivalci.novo2.csv", skip=2, na=c("", "...", "-"),
                    locale=locale(encoding="windows-1250")) %>% fill(1:4) %>% drop_na(6)
  colnames(data) <- c("spol", "drzava", "leto", "drzavljanstvo", "izobrazba", "stevilo")
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


#Sposodil sem si že dani primer uvoza, kajti na SISTATu nisem našel podatka
#o številu prebivalcev na občino, so samo polletni in od 2008 dalje

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
obcine <- uvozi.obcine()
