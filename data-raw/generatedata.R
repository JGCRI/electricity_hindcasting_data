## Generate all data for the package
## Source this from the top level of a development copy of the package.
library(magrittr)
library(dplyr)
library(lubridate)
library(stringr)
library(readxl)
library(rebus)
csv <- TRUE

# Generator Capacity ------------------------------------------------------
# data: https://www.eia.gov/electricity/data/eia860/
# nameplate, summer, winter ~ MW
# heatrate ~ BTU/ kWh

source('data-raw/generators/1990to2000_utilities.R') # generator-level
capacity.90to00 <- prep.generators.90to00("data-raw/generators/1990-2000/")

source('data-raw/generators/2001to2016_utilities.R') # generator-level
capacity.01to16 <- prep.generators.01to16("data-raw/generators/2001-2016/")

# save unmapped data
capacity.unmapped <- rbind(capacity.90to00, capacity.01to16) %>%
  mutate(fuel = ifelse(fuel=="BL", "BLQ", fuel), # fix code errors
         fuel = ifelse(fuel=="WOC", "WC", fuel) ) %>%
  dplyr::rename(vintage=startyr) %>% # use vintage instead of startyr
  group_by(yr, utilcode, plntcode, primemover, fuel, vintage) %>% # aggregate to plant-level
  summarise(nameplate=sum(nameplate)) %>%
  ungroup()
devtools::use_data(capacity.unmapped, overwrite=TRUE)
if (csv) {
  write.csv(capacity.unmapped, "CSV/capacity.unmapped.csv", row.names=FALSE)
}

# Mapping file ------------------------------------------------------------
source('data-raw/mappingfiles/mapping.R')
# data: fuel_general and overnight_categories constructed from native fuel and prime_mover codes
mapping <- prep.mapping("data-raw/mappingfiles/fuel_gen.csv", "data-raw/mappingfiles/overnight_c.csv") %>%
  rename(overnightcategory = overnight_c)
devtools::use_data(mapping, overwrite=TRUE)
if(csv) {
  write.csv(mapping, "CSV/mapping.csv", row.names=FALSE)
}

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
generation.01to16 <- prep.generation.01to16("data-raw/generation/2001-2016/") %>%
  mutate(NAD="")

generation.unmapped <- rbind(generation.90to00, generation.01to16) %>%
  group_by(yr, utilcode, plntcode, primemover, fuel) %>%
  summarise(generation=sum(generation),
            consumption=sum(consumption)) %>%
  ungroup()
devtools::use_data(generation.unmapped, overwrite=TRUE)
if (csv) {
  write.csv(generation.unmapped, "CSV/generation.unmapped.csv", row.names=FALSE)
}
# Capacity Factors ---------------------------------------------------------
source('data-raw/costs/capacityfactors.R')
# data: form860 generator capacities & forms759/906/920/923 plant generation output
# carries original capacity and generation as well (for weighting capital costs)

# join capacity.unmapped and generation.unmapped
cap.gen.joined <- join.cap.gen(capacity.unmapped, generation.unmapped)
devtools::use_data(cap.gen.joined, overwrite=TRUE)
if (csv) {
  write.csv(cap.gen.joined, "CSV/cap.gen.joined.csv", row.names=FALSE)
}

# calculate capacityfactors
cf <- calc.capacityfactors(cap.gen.joined)

# grab capacityfactors as calculated
capacityfactors <- cf$cf
devtools::use_data(capacityfactors, overwrite=TRUE)
if (csv) {
  write.csv(capacityfactors, "CSV/capacityfactors.csv", row.names=FALSE)
}

# grab capacityfactors clamped to 1
capacityfactors.clamp <- cf$cf.clamp
devtools::use_data(capacityfactors.clamp, overwrite=TRUE)
if (csv) {
  write.csv(capacityfactors.clamp, "CSV/capacityfactors.clamp.csv", row.names=FALSE)
}

# Inflation Adjustment ----------------------------------------------------
source('data-raw/costs/gdpdeflator.R')
# data: https://fred.stlouisfed.org/series/GDPDEF
gdpdeflator <- calc.gdpdeflator("data-raw/costs/GDPDEF.csv", "2010") # reference year
devtools::use_data(gdpdeflator, overwrite=TRUE)
if (csv) {
  write.csv(gdpdeflator, "CSV/gdpdeflator.csv", row.names=FALSE)
}

# Fuel Prices -------------------------------------------------------------
source('data-raw/costs/fuelprices.R')
# fossil fuels data: https://www.eia.gov/electricity/monthly/backissues.html
# issues: June 1996 - Table 26, p39(55), January 2010 - Table 4.2, p73(81), May 2016 - Table 4.2, p73(94)
# uranium data: ????
# update w/ : https://www.eia.gov/electricity/data/eia423/
# fuel.price ~ $/BTU
fuelprices <- prep.fuelprices("data-raw/costs/fuel/energy.prices.tsv",
                              "data-raw/costs/fuel/uranium.prices.tsv",
                              gdpdeflator) %>%
  mutate(fuel.general = ifelse(fuel.general == "natural gas", "gas", fuel.general),
         fuel.general = ifelse(fuel.general == "oil", "petroleum", fuel.general),
         fuel.general = ifelse(fuel.general == "uranium", "nuclear", fuel.general))
devtools::use_data(fuelprices, overwrite=TRUE)
if (csv) {
  write.csv(fuelprices, "CSV/fuelprices.csv", row.names=FALSE)
}

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
if (csv) {
  write.csv(marginalcosts, "CSV/marginalcosts.csv", row.names=FALSE)
}

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
if (csv) {
  write.csv(capitalcosts, "CSV/capitalcosts.csv", row.names=FALSE)
}


# Levelized Capital Costs -------------------------------------------------
source('data-raw/costs/levelize.R')
# equation: See data-raw/costs/gcam/Electricity Generation Assumptions.pdf for equation
# constant fixed charge rate of 0.13 from GCAM
# units: overnight.lev, om.fixed.lev, om.var ~ $/MWh
levelizedcosts <- calc.levelizedcosts(capacityfactors.clamp, capitalcosts, mapping, 0.13)
devtools::use_data(levelizedcosts, overwrite=TRUE)
if (csv) {
  write.csv(levelizedcosts, "CSV/levelizedcosts.csv", row.names=FALSE)
}


# Full Cost ---------------------------------------------------------------
# source('data-raw/costs/full.R')
# data: marginalcosts (endogenous to GCAM) + levelizedcosts + generation
master <- levelizedcosts %>%
  left_join(marginalcosts, by=c("yr", "overnightcategory", "fuel.general")) %>%
  mutate(marginal.cost = ifelse(is.na(marginal.cost), 0, marginal.cost)) %>%  # renewables aren't assigned marginalcost
  left_join(cap.gen.joined, by=c("yr", "plntcode", "overnightcategory", "fuel.general"))
devtools::use_data(master, overwrite=TRUE)
if (csv) {
  write.csv(master, "CSV/master.csv", row.names=FALSE)
}

cost.comp <- fullcosts %>%
  mutate(om = om.fixed.lev + om.var) %>%
  select(-om.fixed.lev, -om.var) %>%
  group_by(yr, fuel.general, overnightcategory) %>%
  summarise(marginal.cost = mean(marginal.cost)/1000,
            overnight.lev = mean(overnight.lev)/1000,
            om = mean(om)/1000) %>%
  ungroup()
write.csv(cost.comp, "data-raw/costs/gcam/comp.csv", row.names=F)
