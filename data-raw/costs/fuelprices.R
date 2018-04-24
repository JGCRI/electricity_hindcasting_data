prep.fuelprices <- function(energyprices, uraniumprices, gdpdeflator)
{
  ## DATA
  energy.prices <- read.delim(energyprices) %>%
    dplyr::rename(fuel.general = fuel_1_general)
  uranium.prices <- read.delim(uraniumprices) %>%
    select(year, fuel.price=weighted.avg.price.nominal) %>%
    mutate(fuel.general='uranium')

  ## COMBINE, ADJUST VALUE
  fuelprices <- rbind(energy.prices, uranium.prices) %>%
    rename(cost.yr=year) %>% # assumed that prices are reported in nominal $
    mutate(fuel.general = as.character(fuel.general)) %>%
    inner_join(gdpdeflator, by="cost.yr") %>%
    mutate(fuel.price = deflator * fuel.price) %>% # $/Btu
    select( -deflator) %>%
    rename(yr=cost.yr)
}

