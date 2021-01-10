# 4. faza: Analiza podatkov

require(ggplot2)
require(dplyr)

#NAPOVED RASTI BRUTO MINIMALNE PLAČE V SLOVENIJI
napoved_slo <- min_place %>%
  filter(Drzava=="Slovenia")
odstopanje <- lm(data = napoved_slo, Placa ~ as.numeric(Leto))
g <- data.frame(Leto=seq(2016, 2024, 1))
napoved <- g %>% mutate(Placa=predict(odstopanje, .))

graf_odstopanje <- ggplot(napoved_slo, aes(x=Leto, y=Placa)) + 
  geom_point(shape=1) + 
  geom_smooth(method=lm, formula = x~y, color = 'red') + 
  geom_point(data=napoved, aes(x=Leto, y=Placa), color='dark green', size=2) +
  ggtitle("Napoved nadaljne rasti minimalne bruto plače v Sloveniji") +
  ylab("Plača")

#Realna min. plača v 2018:  842,79€
#Realna min. plača v 2019: 886,63€

  