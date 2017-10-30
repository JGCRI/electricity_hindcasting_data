energy.prices <- read.delim("data-raw/energy.prices.txt.gz") %>%
  dplyr::rename(fuel_general = fuel_1_general)
uranium.prices <- read.delim("data-raw/uranium.prices.txt.gz") %>%
  select(year, fuel.price=weighted.avg.price.nominal) %>%
  mutate(fuel_general='uranium')

fuel.prices <- rbind(energy.prices, uranium.prices)


# Save data ---------------------------------------------------------------
devtools::use_data(fuel.prices, overwrite=TRUE)
