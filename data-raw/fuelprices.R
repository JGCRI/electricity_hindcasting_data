
# Data --------------------------------------------------------------------

energy.prices <- read.delim("data-raw/energy.prices.txt.gz") %>%
  dplyr::rename(fuel_general = fuel_1_general)
uranium.prices <- read.delim("data-raw/uranium.prices.txt.gz") %>%
  select(year, fuel.price=weighted.avg.price.nominal) %>%
  mutate(fuel_general='uranium')

fuelprices <- rbind(energy.prices, uranium.prices) %>%
  rename(cost.year=year) # assumption


gdpdef <- read.csv("data-raw/GDPDEF.csv")

# value adjustment --------------------------------------------------------
gdpdef$year <- ymd(gdpdef$DATE) %>% year()
gdpdef <- gdpdef %>%
  select(year, GDPDEF) %>%
  group_by(year) %>%
  summarise(GDPDEF = mean(GDPDEF)) %>%
  data.frame()
row.names(gdpdef) <- gdpdef$year
reference_deflation <- gdpdef['1975','GDPDEF']
gdpdef <- gdpdef %>%
  mutate(deflator75 = reference_deflation / GDPDEF) %>%
  select(-GDPDEF) %>%
  dplyr::rename(cost.year = year)

fuelprices <- fuelprices %>%
  inner_join(gdpdef) %>%
  mutate(fuel.price = deflator75 * fuel.price) %>%
  mutate(fuel.price = fuel.price/1e6) %>% # $/1e6Btu -> $/Btu
  select( -deflator75) %>%
  rename(year=cost.year)


# Save data ---------------------------------------------------------------
devtools::use_data(fuelprices, overwrite=TRUE)
