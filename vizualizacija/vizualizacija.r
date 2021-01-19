# 3. faza: Vizualizacija podatkov

require(ggplot2)
require(dplyr)
library(stringr)
library(tmap) 

#PLAČA GLEDE NA REGIJO IN SPOL
visina.place <- povp_starost %>% filter(leto=="2018") %>% select(-leto)
visina.place$regija[visina.place$regija == "Posavska"] <- "Spodnjeposavska"
visina.place$regija[visina.place$regija == "Primorsko-notranjska"] <- "Notranjsko-kraska"


zemljevid.place <- zemljevid_slovenije %>% left_join(visina.place, by=c("NAME_1"="regija"))

map <- ggplot(zemljevid.place, aes(x=long, y=lat, fill=placa, label=paste0(NAME_1, "\n", placa))) +
  geom_polygon(aes(group=group)) +
  geom_text(data=zemljevid.place %>% group_by(NAME_1, placa)  %>% 
              summarise(long=mean(long), lat=mean(lat)), size=3, colour="black") +
  theme(axis.line=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        panel.background=element_blank(),
        panel.border=element_blank(),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        plot.background=element_blank()) +
  labs(fill="Plača(€)", title ="Višina povp. bruto plače po regijah Slovenije") 


#ANALIZA PLAČE GLEDE NA JAVNI IN ZASEBNI SEKTOR
javnisektor_osmanj_moski <- javnisektor %>%
  filter(izobrazba == "Osnovnošolska ali manj", spol== "Moški",
         sektor== "11 Javni sektor - SKUPAJ")
javnisektor_osmanj_zenske <- javnisektor %>%
  filter(izobrazba == "Osnovnošolska ali manj", spol== "Ženske",
         sektor== "11 Javni sektor - SKUPAJ")
javnisektor_sr_moski <- javnisektor %>%
  filter(izobrazba == "Srednješolska", spol== "Moški",
         sektor== "11 Javni sektor - SKUPAJ")
javnisektor_sr_zenske <- javnisektor %>%
  filter(izobrazba == "Srednješolska", spol== "Ženske",
         sektor== "11 Javni sektor - SKUPAJ")
javnisektor_vs_moski <- javnisektor %>%
  filter(izobrazba == "Višješolska, visokošolska", spol== "Moški",
         sektor== "11 Javni sektor - SKUPAJ")
javnisektor_vs_zenske <- javnisektor %>%
  filter(izobrazba == "Višješolska, visokošolska", spol== "Ženske",
         sektor== "11 Javni sektor - SKUPAJ")
zasebnisektor_osmanj_moski <- javnisektor %>%
  filter(izobrazba == "Osnovnošolska ali manj", spol=="Moški",
         sektor== "12 Zasebni sektor - SKUPAJ")
zasebnisektor_osmanj_zenske <- javnisektor %>%
  filter(izobrazba == "Osnovnošolska ali manj", spol=="Ženske",
         sektor== "12 Zasebni sektor - SKUPAJ")
zasebnisektor_sr_moski <- javnisektor %>%
  filter(izobrazba == "Srednješolska", spol=="Moški",
         sektor== "12 Zasebni sektor - SKUPAJ")
zasebnisektor_sr_zenske <- javnisektor %>%
  filter(izobrazba == "Srednješolska", spol=="Ženske",
         sektor== "12 Zasebni sektor - SKUPAJ")
zasebnisektor_vs_moski <- javnisektor %>%
  filter(izobrazba == "Višješolska, visokošolska", spol=="Moški",
         sektor== "12 Zasebni sektor - SKUPAJ")
zasebnisektor_vs_zenske <-  javnisektor %>%
  filter(izobrazba == "Višješolska, visokošolska", spol=="Ženske",
         sektor== "12 Zasebni sektor - SKUPAJ")

#Graf-javni sektor
ggplot(javnisektor_osmanj_moski, aes(x=leto, y=placa)) +
  geom_point(color="blue", size=2) +
  geom_point(data=javnisektor_osmanj_zenske, aes(x=leto, y=placa),color="light blue", size=2) +
  geom_point(data=javnisektor_sr_moski, aes(x=leto, y=placa),color="red", size=2) +
  geom_point(data=javnisektor_sr_zenske, aes(x=leto, y=placa),color="pink", size=2) +
  geom_point(data=javnisektor_vs_moski, aes(x=leto, y=placa),color="green", size=2) +
  geom_point(data=javnisektor_vs_zenske, aes(x=leto, y=placa),color="dark green", size=2) +
  labs(title="Primerjava plače v javnem sektorju glede na izobrazbo in spol") +
  ylab("Višina plače(€)") +
  xlab("Leto")

#Graf-zasebni sektor
ggplot(zasebnisektor_osmanj_moski, aes(x=leto, y=placa)) +
  geom_point(color="blue", size=2) +
  geom_point(data=zasebnisektor_osmanj_zenske, aes(x=leto, y=placa),color="light blue", size=2) +
  geom_point(data=zasebnisektor_sr_moski, aes(x=leto, y=placa),color="red", size=2) +
  geom_point(data=zasebnisektor_sr_zenske, aes(x=leto, y=placa),color="pink", size=2) +
  geom_point(data=zasebnisektor_vs_moski, aes(x=leto, y=placa),color="green", size=2) +
  geom_point(data=zasebnisektor_vs_zenske, aes(x=leto, y=placa),color="dark green", size=2) +
  labs(title="Primerjava plače v zasebnem sektorju glede na izobrazbo in spol") +
  ylab("Višina plače(€)") +
  xlab("Leto")

#Graf-analiza povprečne plače po letih v javnem in zasebnem sektorju
graf2 <- ggplot(javnisektor ,aes(x=leto, y=placa, fill=factor(sektor))) + 
  geom_col(position="dodge")  + 
  coord_flip() +
  guides(fill=guide_legend("Sektor")) +
  xlab("Leto") + 
  ylab("Višina plače(€)")+
  ggtitle("Primerjava plače v javnem in zasebnem sektorju")

javnisektor_izobrazba <- javnisektor %>%
  filter(sektor=="11 Javni sektor - SKUPAJ")
zasebnisektor_izobrazba <- javnisektor %>%
  filter(sektor=="12 Zasebni sektor - SKUPAJ")

graf3 <- ggplot(javnisektor_izobrazba ,aes(x=leto, y=placa, fill=factor(izobrazba))) + 
  geom_col(position="dodge")  + 
  coord_flip() +
  guides(fill=guide_legend("Izobrazba")) +
  xlab("Leto") + 
  ylab("Višina plače(€)")+
  ggtitle("Primerjava plače v zasebnem sektorju glede na izobrazbo")

graf4 <- ggplot(zasebnisektor_izobrazba ,aes(x=leto, y=placa, fill=factor(izobrazba))) + 
  geom_col(position="dodge")  + 
  coord_flip() +
  guides(fill=guide_legend("Izobrazba")) +
  xlab("Leto") + 
  ylab("Višina plače(€)")+
  ggtitle("Primerjava plače v javnem sektorju glede na izobrazbo")

#Minimalna in maksimalna placa glede na izobrazbo
maksimum <- data.frame(javnisektor %>%
                       group_by(sektor, izobrazba, spol) %>%
                       summarise(maksimum = max(placa)))
minimum <- data.frame(javnisektor %>% 
                      group_by(sektor, izobrazba, spol) %>%
                      summarise(minimum = min(placa)))
max_min <- merge(maksimum,minimum,by=c("spol", "izobrazba", "sektor"))

placa_javnisektor_moski <- max_min %>%
  filter(sektor=="11 Javni sektor - SKUPAJ", spol== "Moški")
placa_zasebnisektor_moski <- max_min %>%
  filter(sektor=="12 Zasebni sektor - SKUPAJ", spol== "Moški") 
placa_javnisektor_zenske <- max_min %>%
  filter(sektor=="11 Javni sektor - SKUPAJ", spol== "Ženske")
placa_zasebnisektor_zenske <- max_min %>%
  filter(sektor=="12 Zasebni sektor - SKUPAJ", spol== "Ženske")

#ANALIZA PLAČE GLEDE NA GOSPODARSKO DEJAVNOST
#Sprememba plače od leta 2010 do 2018
#sprememba place v % = ((placa 2018 - placa 2010)/ placa 2010) * 100
gd_2010_m <- gospodarskadejavnost2 %>%
  filter(leto == "2010", spol == "Moški")
gd_2018_m <- gospodarskadejavnost2 %>%
  filter(leto == "2018", spol == "Moški")
gd_moski <- merge(gd_2010_m, gd_2018_m, by=c("gospodarska.dejavnost","izobrazba", "spol"))
sprememba <- round(((gd_moski$placa.y - gd_moski$placa.x)/gd_moski$placa.x) * 100, digits=2)
gd_moski$sprememba <- sprememba

gd_2010_z <- gospodarskadejavnost2 %>%
  filter(leto == "2010", spol == "Ženske")
gd_2018_z <- gospodarskadejavnost2 %>%
  filter(leto == "2018", spol == "Ženske")
gd_zenske <- merge(gd_2010_z, gd_2018_z, by=c("gospodarska.dejavnost","izobrazba", "spol"))
sprememba <- round(((gd_zenske$placa.y - gd_zenske$placa.x)/gd_zenske$placa.x) * 100, digits=2)
gd_zenske$sprememba <- sprememba

gd_sprememba <- rbind(gd_moski, gd_zenske)

graf1 <- ggplot(gd_sprememba ,aes(x=str_wrap(gospodarska.dejavnost,45), y=sprememba, fill=factor(spol))) + 
  geom_col(position="dodge")  + 
  coord_flip() +
  guides(fill=guide_legend("Spol")) +
  xlab("Gospodarska dejavnost") + 
  ylab("Sprememba(%)")+
  ggtitle("Sprememba plače(gospodarska dejavnost)")

#Najvišja in najnižja plača v vsaki panogi glede na izobrazbo in spol
maksimum <- data.frame(gospodarskadejavnost %>% 
                         group_by(dejavnost, izobrazba, spol) %>%
                         summarise(maksimum = max(placa)))
minimum <- data.frame(gospodarskadejavnost %>% 
                        group_by(dejavnost, izobrazba, spol) %>%
                        summarise(minimum = min(placa)))
max_min <- merge(maksimum,minimum,by=c("spol", "izobrazba", "dejavnost"))


#ANALIZA PLAČE LETA 2008 IN LETA 2020, TER PRIMERJAVA OBEH LET
kriza_2008 <- kriza %>%
  filter(leto=="2008")
kriza_2020 <- kriza %>%
  filter(leto=="2020")
kriza_08_20 <- rbind(kriza_2008, kriza_2020)

graf5 <- ggplot(kriza_08_20 ,aes(x=placa, y=mesec, fill=factor(leto))) + 
  geom_col(position="dodge")  + 
  coord_flip() +
  guides(fill=guide_legend("Leto")) +
  xlab("Višina plače(€)") + 
  ylab("Mesec")+
  ggtitle("Primerjava plače v letu 2008 z letom 2020")

#Sprememba plače iz leta 2008 do leta 2020: 33,3%
#Sprememba plače v letu 2008: 9,9%
#Sprememba plače v letu 2020: 0,8%
#Sprememba plače v letu 2007: 7,4%

