prep.fuelprices <- function(energyprices, uraniumprices, gdpdeflator)
{
  ## DATA
  energy.prices <- read.delim(energyprices) %>%
    dplyr::rename(fuel.general = fuel_1_general)
  uranium.prices <- read.delim(uraniumprices) %>%
    select(year, fuel.price=weighted.avg.price.nominal) %>%
    mutate(fuel.general='uranium')

  ## COMBINE
  fuelprices <- rbind(energy.prices, uranium.prices) %>%
    rename(cost.year=year) # assumption

  ## VALUE ADJUSTMENT
  fuelprices <- fuelprices %>%
    inner_join(gdpdeflator) %>%
    mutate(fuel.price = deflator * fuel.price) %>%
    mutate(fuel.price = fuel.price/1e6) %>% # $/1e6Btu -> $/Btu
    select( -deflator) %>%
    rename(year=cost.year)
}

