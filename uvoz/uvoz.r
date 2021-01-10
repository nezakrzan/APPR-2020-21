require(dplyr)
require(tidyr)
require(readr)
require(readxl)

sl <- locale("sl", decimal_mark=",", grouping_mark=".")

#Povprečna bruto mesečna plača glede na gospodarsko dejavnost, izobrazbo in spol
gospodarskadejavnost <- read_csv2("podatki/placa_dejavnost.csv",
                                  col_names=c("dejavnost","izobrazba","spol","leto","placa"),
                                  skip=3, na="-",
                                  locale=locale(encoding="Windows-1250"))
gospodarskadejavnost <- head(gospodarskadejavnost, -54)
gospodarskadejavnost <- separate(gospodarskadejavnost, col=1, into=c("oznaka", "dejavnost"), sep="(?<=^.)\\s")

gospodarskadejavnost2 <- read_csv2("podatki/spolskupaj.csv",
                                  col_names=c("gospodarska.dejavnost","izobrazba","leto","spol","placa"),
                                  skip=3, na="-",
                                  locale=locale(encoding="Windows-1250"))
gospodarskadejavnost2 <- head(gospodarskadejavnost2, -8)
  

#Povprečna bruto mesečna plača glede na regijo in spol
regija_starost <- read_csv2("podatki/regija_starost.csv",
                          col_names=c("regija","starost","leto", "placa"),
                          skip=3, na="-",
                          locale=locale(encoding="Windows-1250"))

#regija_starost <- regija_starost %>% fill(1) %>% drop_na(2) %>% 
  #melt(id.vars=stolpci2[1:2],variable.name="leto", value.name = "placa", na.rm = TRUE) %>%
  #mutate(leto=parse_number(as.character(leto)))

povp_starost <- regija_starost %>% filter(starost=="15-64 let") %>% select(-starost)
regija_starost <- regija_starost[!(regija_starost$starost=="15-64 let"), ]

sprememba <- povp_starost %>% filter(leto =="2010" | leto =="2014")

#Povprečna bruto mesečna plača v javnem in zasebnem sektorju glede na izobrazbo in spol
javnisektor <- read_csv2("podatki/sektor.csv",
                         col_names=c("sektor","izobrazba","spol","leto","placa"),
                         skip=3, na="-",
                         locale=locale(encoding="Windows-1250"))

javnisektor_spolskupaj <- read_csv2("podatki/javnisektor2.csv",
                                    col_names=c("sektor","spol","izobrazba","leto","placa"),
                                    skip=3, na="-",
                                    locale=locale(encoding="Windows-1250"))


#Povprečna bruto mesečna plača(kriza)
kriza2008 <- read_csv2("podatki/kriza2008.csv",
                       col_names=c("leto","tip place", "placa"),
                       skip=4, na="-",
                       locale=locale(encoding="Windows-1250")) %>% select(c(-2))

kriza2020 <- read_xlsx("podatki/kriza2020.xlsx",
                       col_names=c("leto","tip place", "placa"),
                       skip=2, n_max=21) %>% select(-"tip place")

kriza_1 <- read_xlsx("podatki/kriza.xlsx",
                       col_names=c("leto","tip place", "placa"),
                       skip=2, n_max=58) %>% select(-"tip place")

mesec <- c("10", "09","08","07","06","05","04","03","02","01",
           "12", "11","10","09","08","07","06","05","04","03","02","01",
           "12", "11","10","09","08","07","06","05","04","03","02","01",
           "12", "11","10","09","08","07","06","05","04","03","02","01",
           "12", "11","10","09","08","07","06","05","04","03","02","01")
leto <- c("2020","2020","2020","2020","2020","2020","2020","2020","2020","2020",
          "2019","2019","2019","2019","2019","2019","2019","2019","2019","2019","2019","2019",
          "2009","2009","2009","2009","2009","2009","2009","2009","2009","2009","2009","2009",
          "2008","2008","2008","2008","2008","2008","2008","2008","2008","2008","2008","2008",
          "2007","2007","2007","2007","2007","2007","2007","2007","2007","2007","2007","2007")
kriza <- data.frame(leto, mesec, kriza_1$placa)


#Minimalne plače po državah
min_place <- read_html("podatki/minimalne_place.htm") %>% html_node(xpath="//table[@class='infoData']") %>%
  html_table() %>% pivot_longer(-timegeo, names_to="Leto", values_to="Placa", names_transform=c(Leto=parse_number)) %>%
  mutate(Placa=parse_number(Placa, na=c(":", ":(z)"), locale=locale(decimal_mark=".", grouping_mark=","))) %>%
  rename(Drzava=timegeo) %>% drop_na(Placa)

