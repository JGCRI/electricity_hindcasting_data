## Generate all data for the package
## Source this from the top level of a development copy of the package.

# year & in_service columns
source('data-raw/generators/form860processed.R')
# summer_capacity ~ MW
# heat_rate ~ BTU/ kWh
form860processed <- prep.form860processed("data-raw/generators/form860raw.tsv", "data-raw/generators/form860retirement.txt")
devtools::use_data(form860processed, overwrite=TRUE)

source('data-raw/mappingfiles/mapping.R')
mapping <- prep.mapping('data-raw/mappingfiles/form860fuels.tsv', 'data-raw/mappingfiles/overnight_categories.tsv')
devtools::use_data(mapping, overwrite=TRUE)

source('data-raw/costs/gdpdeflator.R')
gdpdeflator <- calc.gdpdeflator("data-raw/GDPDEF.csv", "1975") # reference year

source('data-raw/costs/fuelprices.R')
# fuel.price ~ $/BTU
fuelprices <- prep.fuelprices("data-raw/energy.prices.txt.gz", "data-raw/uranium.prices.txt.gz", gdpdeflator)
devtools::use_data(fuelprices, overwrite=TRUE)

source('data-raw/costs/marginalcosts.R')
# marginal.cost ~ $/MWh
marginalcosts <- prep.marginalcosts(mapping, form860processed, fuelprices) # can include maximumheatrates by adding as argument
devtools::use_data(marginalcosts, overwrite=TRUE)

## Should use projected online year (1996-2022) instead of year of report (1997-2015)
source('data-raw/costs/capitalcosts.R')
# base.overnight ~ $/MW
# variable.o.m, fixed.o.m ~ $/MWh
capitalcosts <- prep.capitalcosts("data-raw/costs/overnight.cost.tsv", gdpdeflator)
devtools::use_data(capitalcosts, overwrite=TRUE)

master <- form860processed %>%
  select(year, utility_code, plant_code, generator_code, prime_mover, fuel, heat_rate) %>%
  left_join(mapping, by=c('prime_mover', 'fuel')) %>%
  select(-prime_mover, -fuel) %>%
  left_join(fuelprices, by=c("year", "fuel_general")) %>%
  ## NO HEATRATE REGRESSION, CALCULATE MARGINALCOSTS DIRECTLY
  mutate(marginal.cost = fuel.price*heat_rate) %>% # $/Btu * Btu/Kwh = $/Kwh
  mutate(marginal.cost = marginal.cost*1e3) %>% # $/kwh -> $/Mwh
  left_join(capitalcosts, by=c("year", "overnight_category")) %>%
  arrange(utility_code, plant_code, generator_code, overnight_category, fuel_general, year,
          heat_rate, fuel.price, marginal.cost, base.overnight, variable.o.m, fixed.o.m)

write.csv(master, 'master.csv', row.names=F)

# couldn't get to work, saved in long format
spread(key=year, value=heat_rate)
dcast(overnight_category+fuel_general+utility_code+plant_code+generator_code~ year, value.var = "heat_rate")

