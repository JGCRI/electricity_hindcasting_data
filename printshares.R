library(energy.markets)
library(magrittr)
library(dplyr)
library(ggplot2)

fuel.markets <- marketshares(generators, mapping, "fuel")
fuel.markets.new <- marketshares(generators, mapping, "fuel", additions=TRUE)
ggsave("fueladditions.png")
write.csv(fuel.markets.new, "fueladditions.csv", row.names=FALSE)


tech.markets <- marketshares(generators, mapping, "tech")
tech.markets.new <- marketshares(generators, mapping, "tech", additions=TRUE)
ggsave("techadditions.png")
write.csv(tech.markets.new, "techadditions.csv", row.names=FALSE)
