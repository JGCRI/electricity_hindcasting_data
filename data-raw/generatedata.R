## Generate all data for the package
## Source this from the top level of a development copy of the package.
library(magrittr)
library(dplyr)
library(lubridate)
library(stringr)
library(readxl)
library(rebus)

# Generator Capacity ------------------------------------------------------
# data: https://www.eia.gov/electricity/data/eia860/
# nameplate, summer, winter ~ MW
# heatrate ~ BTU/ kWh

source('data-raw/generators/1990to2000_utilities.R')
generators.90to00 <- prep.generators.90to00("data-raw/generators/1990-2000/")

source('data-raw/generators/2001to2016_utilities.R')
generators.01to16 <- prep.generators.01to16("data-raw/generators/2001-2016/")

# fix code errors
generators <- rbind(generators.90to00, generators.01to16) %>%
  mutate(fuel = ifelse(fuel=="BL", "BLQ", fuel),
         fuel = ifelse(fuel=="WOC", "WC", fuel) )

# See Capacity Factors cell for use_data()

# Mapping file ------------------------------------------------------------
source('data-raw/mappingfiles/mapping.R')
# data: fuel_general and overnight_categories constructed from native fuel and prime_mover codes
mapping <- prep.mapping('data-raw/generators/fuels.csv', 'data-raw/generators/overnightcategories.csv')
devtools::use_data(mapping, overwrite=TRUE)

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

generation <- rbind(generation.90to00, generation.01to16) %>%
  group_by(yr, utilcode, plntcode, primemover, fuel) %>%
  summarise(generation=sum(generation),
            consumption=sum(consumption)) %>%
  ungroup()

# See Capacity Factors cell for use_data()


# Capacity Factors ---------------------------------------------------------
source('data-raw/costs/capacityfactors.R')
# data: form860 generator capacities & forms759/906/920/923 plant generation output
# carries original capacity and generation as well (for weighting capital costs)

# map to oc-fg instead of pm-f
# filter empty oc | fg
swapids <- function(df, mapping) {
  df.swap <- df %>%
    left_join(mapping, by=c("primemover", "fuel")) %>%
    filter(overnightcategory != "" | fuel.general != "") %>%
    select(-primemover, -fuel)
}

# save full generators dataset
generators <- swapids(generators, mapping) %>%
  dplyr::rename(vintage=startyr) %>%
  group_by(yr, utilcode, plntcode, overnightcategory, fuel.general, vintage) %>%
  summarise(nameplate=sum(nameplate)) %>%
  ungroup()
devtools::use_data(generators, overwrite=TRUE)

# mapping to oc-fg creates duplicate rows bc reported (plant x primemover x fuel)
# sum 'em up!
generation <- swapids(generation, mapping) %>%
  group_by(yr, utilcode, plntcode, overnightcategory, fuel.general) %>%
  summarise(generation=sum(generation)) %>%
  ungroup()
devtools::use_data(generation, overwrite=TRUE)

# calculate and save capacityfactors
cf <- calc.capacityfactors(generators, generation)
capacityfactors <- cf$cf
capacityfactors.unfilt <- cf$cf.unfilt
devtools::use_data(capacityfactors, overwrite=TRUE)
devtools::use_data(capacityfactors.unfilt, overwrite=TRUE)

# save generators that contribute to capacity factor less than 1
generators.cfl1 <- generators %>%
  inner_join(capacityfactors, by=c("yr", "utilcode", "plntcode", "overnightcategory", "fuel.general")) %>%
  group_by(yr, utilcode, plntcode, overnightcategory, fuel.general, vintage) %>%
  summarise(nameplate=sum(nameplate)) %>%
  ungroup()
devtools::use_data(generators.cfl1, overwrite=TRUE)

# Inflation Adjustment ----------------------------------------------------
source('data-raw/costs/gdpdeflator.R')
# data: https://fred.stlouisfed.org/series/GDPDEF
gdpdeflator <- calc.gdpdeflator("data-raw/costs/GDPDEF.csv", "2010") # reference year


# Fuel Prices -------------------------------------------------------------
source('data-raw/costs/fuelprices.R')
# fossil fuels data: https://www.eia.gov/electricity/monthly/backissues.html
# issues: June 1996 - Table 26, p39(55), January 2010 - Table 4.2, p73(81), May 2016 - Table 4.2, p73(94)
# uranium data: ????
# update w/ : https://www.eia.gov/electricity/data/eia423/
# fuel.price ~ $/BTU
fuelprices <- prep.fuelprices("data-raw/costs/fuel/energy.prices.tsv",
                              "data-raw/costs/fuel/uranium.prices.tsv",
                              gdpdeflator)
devtools::use_data(fuelprices, overwrite=TRUE)


# Marginal Costs ----------------------------------------------------------
source('data-raw/costs/marginalcosts.R')
# data: heatrate [Btu/kwH] * fuelprice [$/BTU]
# marginal.cost ~ $/MWh (converted internally)
# marginalcosts <- prep.marginalcosts(mapping, form860processed, fuelprices)
# can include maximumheatrates by adding as argument
heatrates <- read.csv("data-raw/costs/fuel/avghr.csv") %>%
  mutate(oc = gsub("_", " ", oc)) %>%
  mutate(oc = ifelse(oc == "combined cycle", "conventional combined cycle", oc)) %>%
  mutate(oc = ifelse(oc == "combustion turbine", "conventional combustion turbine", oc)) %>%
  mutate(fuel = gsub("_", " ", fuel)) %>%
  rename(overnightcategory = oc,
         fuel.general = fuel,
         yr = t,
         heatrate = Val)

marginalcosts <- heatrates %>%
  mutate(efficiency = 3412/heatrate) %>%
  left_join(fuelprices, by=c("yr", "fuel.general")) %>%
  mutate(marginal.cost = fuel.price/efficiency) %>% # $/Btu * Btu/Kwh = $/Kwh
  mutate(marginal.cost = marginal.cost*1e3) %>% # $/MWh
  filter(! is.na(marginal.cost)) %>%
  select(yr, overnightcategory, fuel.general, marginal.cost)

devtools::use_data(marginalcosts, overwrite=TRUE)


# Capital Costs -----------------------------------------------------------
## Should use projected online year (1996-2022) instead of year of report (1997-2015)
source('data-raw/costs/capitalcosts.R')
# data:  https://www.eia.gov/outlooks/aeo/archive.php -- 'Assumptions'
# table: 'Cost and Performance Characteristics of New Central Station Electricity Generating Technologies'
# overnight, om.fixed ~ $/kW
# om.var ~ $/MWh (native units)
capitalcosts <- prep.capitalcosts("data-raw/costs/AEO/assumptions.final.csv",
                                  "data-raw/costs/AEO/tech-oc.csv",
                                  gdpdeflator)
devtools::use_data(capitalcosts, overwrite=TRUE)



# Levelized Capital Costs -------------------------------------------------
source('data-raw/costs/levelize.R')
# equation: See data-raw/costs/gcam/Electricity Generation Assumptions.pdf for equation
# constant fixed charge rate of 0.13 from GCAM
# units: overnight.lev, om.fixed.lev, om.var ~ $/MWh
levelizedcosts <- calc.levelizedcosts(capacityfactors, capitalcosts, mapping, 0.13)
devtools::use_data(levelizedcosts, overwrite=TRUE)



# Full Cost ---------------------------------------------------------------
# source('data-raw/costs/full.R')
# data: marginalcosts (endogenous to GCAM) + levelizedcosts + generation
modelinput <- levelizedcosts %>%
  left_join(marginalcosts, by=c("yr", "overnightcategory", "fuel.general")) %>%
  mutate(marginal.cost = ifelse(is.na(marginal.cost), 0, marginal.cost)) %>%  # renewables aren't assigned marginalcost
  left_join(generation, by=c("yr", "utilcode", "plntcode", "overnightcategory", "fuel.general"))
devtools::use_data(modelinput, overwrite=TRUE)

cost.comp <- fullcosts %>%
  mutate(om = om.fixed.lev + om.var) %>%
  select(-om.fixed.lev, -om.var) %>%
  group_by(yr, fuel.general, overnightcategory) %>%
  summarise(marginal.cost = mean(marginal.cost)/1000,
            overnight.lev = mean(overnight.lev)/1000,
            om = mean(om)/1000) %>%
  ungroup()
write.csv(cost.comp, "data-raw/costs/gcam/comp.csv", row.names=F)
