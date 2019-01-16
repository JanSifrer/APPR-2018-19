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