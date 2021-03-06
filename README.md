## Analiza podatkov s programom R, 2018/19

**Jan Šifrer**

Repozitorij z gradivi pri predmetu APPR v študijskem letu 2018/19

* [![Shiny](http://mybinder.org/badge.svg)](http://beta.mybinder.org/v2/gh/JanSifrer/APPR-2018-19/master?urlpath=shiny/APPR-2018-19/projekt.Rmd) Shiny
* [![RStudio](http://mybinder.org/badge.svg)](http://beta.mybinder.org/v2/gh/JanSifrer/APPR-2018-19/master?urlpath=rstudio) RStudio

## Analiza preseljevanja Slovencev

Za projektno nalogo sem si izbral analizo preseljevanja Slovencev. Podatke bom dobil na statističnemu uradu Republike Slovenije (SI-STAT), pod tematiko demografsko in socialno področje – selitve. Moj cilj je, da prikažem koliko Slovencev se seli, pri kakšni starosti se selijo in v katere države se največ Slovencev preseli. Zanimalo pa me bo tudi obratno, torej kaj je razlog da se ljudje priseljujejo v Slovenijo, ter iz kje prhajajo.

## Tabele
* tabela 1: vrsta selitve, starostne skupine, spol, leto, število
* tabela 2: vrsta selitve, država selitve, spol, leto, število
* tabela 3: Selitveno gibanje prebivalstva po občinah, Slovenija, leto
* tabela 4: Odseljeni prebivalci, stari 15 ali več let, po izobrazbi, državi prihodnjega prebivališča, spolu in državljanstvu, Slovenija, leto
* tabela 5: Priseljeni prebivalci, stari 15 ali več let, po izobrazbi, državi prihodnjega prebivališča, spolu in državljanstvu, Slovenija, leto


## Viri
SI-STAT:

* https://pxweb.stat.si/pxweb/Database/Dem_soc/Dem_soc.asp

Podatki o meddržavnih selitvah:

* https://pxweb.stat.si/pxweb/Database/Dem_soc/05_prebivalstvo/40_selitve/05_05N10_meddrzavne/05_05N10_meddrzavne.asp

Socioekonomske značilnosti:

* https://pxweb.stat.si/pxweb/Database/Dem_soc/05_prebivalstvo/40_selitve/20_05N32_soc_ek_znac_odsel/20_05N32_soc_ek_znac_odsel.asp (za odseljevanje)

* https://pxweb.stat.si/pxweb/Database/Dem_soc/05_prebivalstvo/40_selitve/15_05N31_soc_ek_znac_prisel/15_05N31_soc_ek_znac_prisel.asp (za priseljevanje)


## Program

Glavni program in poročilo se nahajata v datoteki `projekt.Rmd`.
Ko ga prevedemo, se izvedejo programi, ki ustrezajo drugi, tretji in četrti fazi projekta:

* obdelava, uvoz in čiščenje podatkov: `uvoz/uvoz.r`
* analiza in vizualizacija podatkov: `vizualizacija/moja-vizualizacija.r`
* napredna analiza podatkov: `analiza/analiza.r`

Vnaprej pripravljene funkcije se nahajajo v datotekah v mapi `lib/`.
Podatkovni viri so v mapi `podatki/`.
Zemljevidi v obliki SHP, ki jih program pobere,
se shranijo v mapo `../zemljevidi/` (torej izven mape projekta).

## Potrebni paketi za R

Za zagon tega vzorca je potrebno namestiti sledeče pakete za R:

* `knitr` - za izdelovanje poročila
* `rmarkdown` - za prevajanje poročila v obliki RMarkdown
* `shiny` - za prikaz spletnega vmesnika
* `DT` - za prikaz interaktivne tabele
* `rgdal` - za uvoz zemljevidov
* `digest` - za zgoščevalne funkcije (uporabljajo se za shranjevanje zemljevidov)
* `readr` - za branje podatkov
* `rvest` - za pobiranje spletnih strani
* `reshape2` - za preoblikovanje podatkov v obliko *tidy data*
* `dplyr` - za delo s podatki
* `gsubfn` - za delo z nizi (čiščenje podatkov)
* `ggplot2` - za izrisovanje grafov
* `mosaic` - za pretvorbo zemljevidov v obliko za risanje z `ggplot2`
* `maptools` - za delo z zemljevidi
* `extrafont` - za pravilen prikaz šumnikov (neobvezno)
* `dygraphs` - za izrisovanje grafov
* `xts` - za delo s podatki

## Binder

Zgornje [povezave](#analiza-podatkov-s-programom-r-201819)
omogočajo poganjanje projekta na spletu z orodjem [Binder](https://mybinder.org/).
V ta namen je bila pripravljena slika za [Docker](https://www.docker.com/),
ki vsebuje večino paketov, ki jih boste potrebovali za svoj projekt.

Če se izkaže, da katerega od paketov, ki ji potrebujete, ni v sliki,
lahko za sprotno namestitev poskrbite tako,
da jih v datoteki [`install.R`](install.R) namestite z ukazom `install.packages`.
Te datoteke (ali ukaza `install.packages`) **ne vključujte** v svoj program -
gre samo za navodilo za Binder, katere pakete naj namesti pred poganjanjem vašega projekta.

Tako nameščanje paketov se bo izvedlo pred vsakim poganjanjem v Binderju.
Če se izkaže, da je to preveč zamudno,
lahko pripravite [lastno sliko](https://github.com/jaanos/APPR-docker) z želenimi paketi.

Če želite v Binderju delati z git,
v datoteki `gitconfig` nastavite svoje ime in priimek ter e-poštni naslov
(odkomentirajte vzorec in zamenjajte s svojimi podatki) -
ob naslednjem.zagonu bo mogoče delati commite.
Te podatke lahko nastavite tudi z `git config --global` v konzoli
(vendar bodo veljale le v trenutni seji).
