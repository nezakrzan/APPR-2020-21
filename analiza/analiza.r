# 4. faza: Analiza podatkov

require(ggplot2)
require(dplyr)

#NAPOVED RASTI BRUTO MINIMALNE PLAČE V SLOVENIJI
napoved_slo <- min_place %>%
  filter(Drzava=="Slovenia")
odstopanje <- lm(data=napoved_slo, formula=Placa ~ as.numeric(Leto))
g <- data.frame(Leto=seq(2016, 2024, 1))
napoved <- mutate(g, Placa=predict(odstopanje, g))

graf_odstopanje <- ggplot(napoved_slo, aes(x=Leto, y=Placa)) + 
  geom_point() + 
  geom_smooth(method=lm, fullrange = TRUE, color="red") + 
  geom_point(data=napoved, aes(x=Leto, y=Placa), color="dark green", size=2) +
  ggtitle("Napoved nadaljne rasti minimalne bruto plače v Sloveniji") +
  ylab("Plača")

