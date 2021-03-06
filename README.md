# Analiza podatkov s programom R, 2020/21

Repozitorij z gradivi pri predmetu APPR v študijskem letu 2020/21

* [![Shiny](http://mybinder.org/badge.svg)](http://mybinder.org/v2/gh/nezakrzan/APPR-2020-21/master?urlpath=shiny/APPR-2020-21/projekt.Rmd) Shiny
* [![RStudio](http://mybinder.org/badge.svg)](http://mybinder.org/v2/gh/nezakrzan/APPR-2020-21/master?urlpath=rstudio) RStudio

# Analiza bruto mesečnih plač v Sloveniji

Analizirala bom višino povprečne bruto mesečne plače v odvisnosti od gospodarske dejavnosti, izobrazbe ter spola. Zanimal me bo trend spreminjanja višine plače glede na gospodarsko dejavnost in višina povprečne plače po regijah po spolu. Podtake bom vizualizirala s pomočjo zemljevida in grafov.
Primerjala bom tudi plače v javnem in zasebnem sektorju glede na izobrazbo in spol.
Na koncu bom primerjala tudi bruto mesečne plače v času krize leta 2008 s krizo Covid-19 leta 2020.

### Potek dela
1. Pridobitev in ureditev podatkov
2. Primerjava višine plače glede na gospodarsko dejavnost
3. Primerjava višine povprečne plače po regijah
4. Vizualizacija primerjav
5. Višina plače v javnem in zasebnem sekturju
6. Primerjava povprečne bruto mesečne plače z evropskim povprečjem

### Tabele

1. tabela: Povprečna bruto mesečna plača glede na gospodarsko dejavnost, izobrazbo in spol.
* Stolpci: Gospodarska dejavnost, izobrazba, spol, leto, plača

2. tabela: Povprečna bruto mesečna plača glede na regijo in spol.
* Stolpci: Regija, spol, leto, plača

3. tabela: Povprečna bruto mesečna plača v javnem in zasebnem sektorju glede na izobrazbo in spol.
* Stolpci: Javni sektor, zasebi sektor, izobrazba, spol, leto, plača

4. tabela: Povprečna bruto mesečna plača v Sloveniji leta 2007, 2008 in 2009.
* Stolpci: Leto, plača

5. tabela: Povprečna bruto mesečna plača v Sloveniji leta 2019 in 2020.
* Stolpci: Leto, plača


### Viri:
* SiStat: https://pxweb.stat.si/SiStat/sl/Podrocja/Index/98/place-in-stroski-dela


## Program

Glavni program in poročilo se nahajata v datoteki `projekt.Rmd`.
Ko ga prevedemo, se izvedejo programi, ki ustrezajo drugi, tretji in četrti fazi projekta:

* obdelava, uvoz in čiščenje podatkov: `uvoz/uvoz.r`
* analiza in vizualizacija podatkov: `vizualizacija/vizualizacija.r`
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
* `rgeos` - za podporo zemljevidom
* `digest` - za zgoščevalne funkcije (uporabljajo se za shranjevanje zemljevidov)
* `readr` - za branje podatkov
* `rvest` - za pobiranje spletnih strani
* `tidyr` - za preoblikovanje podatkov v obliko *tidy data*
* `dplyr` - za delo s podatki
* `gsubfn` - za delo z nizi (čiščenje podatkov)
* `ggplot2` - za izrisovanje grafov
* `mosaic` - za pretvorbo zemljevidov v obliko za risanje z `ggplot2`
* `maptools` - za delo z zemljevidi
* `tmap` - za izrisovanje zemljevidov
* `extrafont` - za pravilen prikaz šumnikov (neobvezno)

## Binder

Zgornje [povezave](#analiza-podatkov-s-programom-r-202021)
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
ob naslednjem zagonu bo mogoče delati commite.
Te podatke lahko nastavite tudi z `git config --global` v konzoli
(vendar bodo veljale le v trenutni seji).
