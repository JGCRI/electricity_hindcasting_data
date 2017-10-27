
# To-Do -------------------------------------------------------------------

## Rename online as year. Does this assumption make sense
# Makes sense in order tor costs to be associated with new generators from Form 860 (in_service==year)


# Data --------------------------------------------------------------------
# Industry-level
overnight <- read.delim("data-raw/overnight.cost.tsv") %>%
  select(year, cost.year, overnight_category, base.overnight, variable.o.m, fixed.o.m) %>%
  arrange(year, cost.year, overnight_category)

# GDPDEF
gdpdef <- read.csv("data-raw/GDPDEF.csv")

# string manipulation -----------------------------------------------------
overnight$overnight_category <- gsub('_', ' ', overnight$overnight_category)


# unit conversions --------------------------------------------------------
overnight <- overnight %>%
  mutate(fixed.o.m = fixed.o.m/1000/8760,# /kWyr -> /MWh
         base.overnight = base.overnight/1000) # /kW -> MW

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

capitalcosts <- overnight %>%
  inner_join(gdpdef) %>%
  mutate(base.overnight = deflator75 * base.overnight,
         variable.o.m = deflator75 * variable.o.m,
         fixed.o.m = deflator75 * fixed.o.m) %>%
  select(-cost.year, -deflator75)

# save file ---------------------------------------------------------------
devtools::use_data(capitalcosts, overwrite=TRUE)

