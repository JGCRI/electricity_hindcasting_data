library(energy.markets)
library(magrittr)
library(dplyr)
library(ggplot2)

fuel.markets <- marketshares(form860processed, mapping, "fuel")
fuel.markets.new <- marketshares(form860processed, mapping, "fuel", additions=TRUE)
ggsave("fueladditions.png")
write.csv(fuel.markets.new, "fueladditions.csv", row.names=FALSE)


tech.markets <- marketshares(form860processed, mapping, "tech")
tech.markets.new <- marketshares(form860processed, mapping, "tech", additions=TRUE)
ggsave("techadditions.png")
write.csv(tech.markets.new, "techadditions.csv", row.names=FALSE)
