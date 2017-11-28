## Generate all data for the package
## Source this from the top level of a development copy of the package.
library(magrittr)
library(dplyr)
library(lubridate)
library(stringr)
library(readxl)
library(rebus)

# Generator Capacity ------------------------------------------------------
# YEAR VS. IN_SERVICE
source('data-raw/generators/form860processed.R')
# data: https://www.eia.gov/electricity/data/eia860/
# summer_capacity ~ MW
# heat_rate ~ BTU/ kWh
form860processed <- prep.form860processed("data-raw/generators/form860raw.tsv", "data-raw/generators/form860retirement.txt")
devtools::use_data(form860processed, overwrite=TRUE)

# Mapping file ------------------------------------------------------------
source('data-raw/mappingfiles/mapping.R')
# data: fuel_general and overnight_categories constructed from native fuel and prime_mover codes
mapping <- prep.mapping('data-raw/mappingfiles/form860fuels.tsv', 'data-raw/mappingfiles/overnight_categories.tsv')
devtools::use_data(mapping, overwrite=TRUE)


# Inflation Adjustment ----------------------------------------------------
source('data-raw/costs/gdpdeflator.R')
# data: https://fred.stlouisfed.org/series/GDPDEF
gdpdeflator <- calc.gdpdeflator("data-raw/costs/GDPDEF.csv", "1975") # reference year


# Fuel Prices -------------------------------------------------------------
source('data-raw/costs/fuelprices.R')
# fossil fuels data: https://www.eia.gov/electricity/monthly/backissues.html
# issues: June 1996 - Table 26, p39(55), January 2010 - Table 4.2, p73(81), May 2016 - Table 4.2, p73(94)
# uranium data: ????
# update w/ : https://www.eia.gov/electricity/data/eia423/
# fuel.price ~ $/BTU
fuelprices <- prep.fuelprices("data-raw/costs/energy.prices.txt.gz", "data-raw/costs/uranium.prices.txt.gz", gdpdeflator)
devtools::use_data(fuelprices, overwrite=TRUE)


# Marginal Costs ----------------------------------------------------------
source('data-raw/costs/marginalcosts.R')
# data: heatrate [Btu/kwH] * fuelprice [$/BTU]
# marginal.cost ~ $/MWh (converted internally)
# marginalcosts <- prep.marginalcosts(mapping, form860processed, fuelprices)
# can include maximumheatrates by adding as argument
heatrates <- read.csv("data-raw/costs/avghr.csv") %>%
  mutate(oc = gsub("_", " ", oc)) %>%
  mutate(oc = ifelse(oc == "combined cycle", "conventional combined cycle", oc)) %>%
  mutate(oc = ifelse(oc == "combustion turbine", "conventional combustion turbine", oc)) %>%
  mutate(fuel = gsub("_", " ", fuel)) %>%
  rename(overnight_category = oc,
         fuel_general = fuel,
         year = t,
         heatrate = Val)

marginalcosts <- heatrates %>%
  left_join(fuelprices, by=c("year", "fuel_general")) %>%
  mutate(marginal.cost = fuel.price*heatrate) %>% # $/Btu * Btu/Kwh = $/Kwh
  mutate(marginal.cost = marginal.cost*1e3) %>% # $/MWh
  filter(! is.na(marginal.cost)) %>%
  select(year, overnight_category, fuel_general, marginal.cost)

devtools::use_data(marginalcosts, overwrite=TRUE)

# Capital Costs -----------------------------------------------------------
## Should use projected online year (1996-2022) instead of year of report (1997-2015)
source('data-raw/costs/capitalcosts.R')
# data:  https://www.eia.gov/outlooks/aeo/archive.php -- 'Assumptions'
# table: 'Cost and Performance Characteristics of New Central Station Electricity Generating Technologies'
# base.overnight ~ $/MW
# variable.o.m, fixed.o.m ~ $/MWh (fixed converted internally)
capitalcosts <- prep.capitalcosts("data-raw/costs/overnight.cost.tsv", gdpdeflator)
devtools::use_data(capitalcosts, overwrite=TRUE)


# Generation & Consumption ------------------------------------------------
source('data-raw/generation/1990to2000_utilities.R')
# data: https://www.eia.gov/electricity/data/eia923/eia906u.html
# generation ~ MWh
# consumption ~ physical quantity of fuel, specific to type of fuel
generation.90to00 <- prep.generation.90to00("data-raw/generation/1990-2000/")

source('data-raw/generation/2001to2016_utilities.R')
# data: https://www.eia.gov/electricity/data/eia923/
# generation ~ MWh
# consumption ~ Btu
generation.01to16 <- prep.generation.01to16("data-raw/generation/2001-2016/")

generation <- rbind(generation.90to00, generation.01to16)
devtools::use_data(generation, overwrite=TRUE)

# Capacity Factors ---------------------------------------------------------
source('data-raw/costs/capacityfactors.R')
# data: form860 generator capacities & forms759/906/920/923 plant generation output
capacityfactors <- calc.capacityfactors(form860processed, generation)
devtools::use_data(capacityfactors, overwrite=TRUE)

# Levelized Capital Costs -------------------------------------------------
source('data-raw/costs/levelize.R')
# data: plant-level capacity factors for mover-fuel combos are assigned capitalcosts via overnight_category
levelizedcosts <- calc.levelizedcosts(capacityfactors, capitalcosts, mapping, 0.13, generation)
# constant fixed charge rate of 0.13 from GCAM
devtools::use_data(levelizedcosts, overwrite=TRUE)


# Full Cost ---------------------------------------------------------------
# source('data-raw/costs/full.R')
# data: marginalcosts (endogenous to GCAM) plus levelizedcosts
fullcosts <- levelizedcosts %>%
  left_join(mapping, by=c("prime_mover", "fuel")) %>%
  filter(! is.na(fuel_general)) %>% # need fuel for marginalcost
  left_join(marginalcosts, by=c("year", "overnight_category", "fuel_general"))
fullcosts$marginal.cost[is.na(fullcosts$marginal.cost)] <- 0
# renewables w/o marginalcost
# (except conv.comb.cycle&coal, which doesn't have heatrate)
fullcosts$fullcost <- fullcosts$levcost + fullcosts$marginal.cost

save <- fullcosts %>%
  select(year, utility_code, plant_code, prime_mover, fuel, overnight_category, fuel_general, levcost, marginal.cost, fullcost)
write.csv(save, "fullcost.csv")
