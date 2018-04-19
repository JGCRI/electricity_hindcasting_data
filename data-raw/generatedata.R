## Generate all data for the package
## Source this from the top level of a development copy of the package.
library(magrittr)
library(dplyr)
library(lubridate)
library(stringr)
library(readxl)
library(rebus)
csv <- TRUE


# ORIG CAP ----------------------------------------------------------------
# data: https://www.eia.gov/electricity/data/eia860/
# nameplate, summer, winter ~ MW
# heatrate ~ BTU/ kWh

source('data-raw/generators/1990to2000_utilities.R') # generator-level
capacity.90to00 <- prep.generators.90to00("data-raw/generators/1990-2000/")

source('data-raw/generators/2001to2016_utilities.R') # generator-level
capacity.01to16 <- prep.generators.01to16("data-raw/generators/2001-2016/")

# save unmapped data
generators <- rbind(capacity.90to00, capacity.01to16) %>%
  dplyr::rename(vintage=startyr) %>% # use vintage instead of startyr
  mutate(fuel = ifelse(fuel=="BL", "BLQ", fuel), # fix code errors
         fuel = ifelse(fuel=="WOC", "WC", fuel),
         primemover = ifelse(primemover=="HC", "HY", primemover)) %>%
  rename(capacity = nameplate)
devtools::use_data(generators, overwrite=TRUE)
if (csv) {
  write.csv(generators, "CSV/generators.csv", row.names=FALSE)
}

capacity.unmapped <- generators %>%
  group_by(yr, utilcode, plntcode, primemover, fuel, vintage) %>% # aggregate to plant-level

  # NOTE:
  # this dataset is at plant-level, but we are dropping utilcode as a useful key due to
  # inconsistencies between capacity and generation datasets.
  #
  # these two datasets are joined using c("yr", "plntcode", "primemover", "fuel"),
  # except for yr %in% c(2000, 2001), where we use c("yr", "plntcode", "fuel")

  summarise(capacity=sum(capacity)) %>% # rename nameplate as capacity
  ungroup()
devtools::use_data(capacity.unmapped, overwrite=TRUE)
if (csv) {
  write.csv(capacity.unmapped, "CSV/capacity.unmapped.csv", row.names=FALSE)
}


# ORIG GEN ----------------------------------------------------------------
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
  filter(! is.na(utilcode)) %>%
  filter(generation >= 0) %>%
  group_by(yr, utilcode, plntcode, primemover, fuel) %>%
  summarise(generation=sum(generation),
            consumption=sum(consumption)) %>%
  ungroup()

generation.unmapped.na <- generation.unmapped %>%
  filter( is.na(primemover) )
generation.unmapped.notna <- generation.unmapped %>%
  filter( !is.na(primemover) )

devtools::use_data(generation.unmapped, overwrite=TRUE)
if (csv) {
  write.csv(generation.unmapped, "CSV/generation.unmapped.csv", row.names=FALSE)
}
# Mapping (save mapped CAP & GEN) -----------------------------------------

source('data-raw/mappingfiles/mapping.R')
# data: fuel_general and overnight_categories constructed from native fuel and prime_mover codes
mapping <- prep.mapping("data-raw/mappingfiles/mapping_final.csv") %>%
  rename(fuel.general = fuel_general)
devtools::use_data(mapping, overwrite=TRUE)
if(csv) {
  write.csv(mapping, "CSV/mapping.csv", row.names=FALSE)
}


# MAP CAP & GEN -----------------------------------------------------------

# takes dataset df and column of interest (capacity or generation)
# maps df from native (yr-pm-f-plntcode) keys to (yr-oc-fg-plntcode) keys
# after mapping, must aggregate b/c multiple (pm-f) map to same (oc-fg)
map <- function(df, column) {
  column <- enquo(column)

  df <- df %>%
    left_join(mapping, by=c("primemover", "fuel")) %>% # map datasets to oc-fg
    select(-primemover, -fuel) %>% # aggregate redundant mappings (pm, f) -> (oc, fg)
    # no longer grouping by utilcode b/c two versions (.x, .y)
    group_by(yr, plntcode, overnightcategory, fuel.general) %>%
    summarise(!!quo_name(column) := sum(!!column)) %>%
    ungroup()
}

capacity <- map(capacity.unmapped, capacity)
devtools::use_data(capacity, overwrite=TRUE)
if(csv) {
  write.csv(capacity, "CSV/capacity.csv", row.names=FALSE)
}

generation <- map(generation.unmapped, generation)
devtools::use_data(generation, overwrite=TRUE)
if (csv) {
  write.csv(generation, "CSV/generation.csv", row.names=FALSE)
}


# JOIN.CAP.GEN ------------------------------------------------------------
source('data-raw/costs/capacityfactors.R')

# join capacity.unmapped and generation.unmapped
join.na.unmapped <- join.cap.gen(capacity.unmapped, generation.unmapped.na, na.case=TRUE)

join.notna.unmapped <- join.cap.gen(capacity.unmapped, generation.unmapped.notna, na.case=FALSE)

# save cap.gen.joined.unmapped (native pm-f codes)
cap.gen.joined.unmapped <- rbind(join.na.unmapped, join.notna.unmapped)
devtools::use_data(cap.gen.joined.unmapped, overwrite=TRUE)
if (csv) {
  write.csv(cap.gen.joined.unmapped, "CSV/cap.gen.joined.unmapped.csv", row.names=FALSE)
}

v1 <- cap.gen.joined.unmapped %>%
  inner_join(generators, by=c("yr", "plntcode", "primemover", "fuel")) %>%
  select(-capacity.y) %>%
  rename(capacity = capacity.x) %>%
  filter(!is.na(vintage))
v2 <- v1 %>%
  left_join(mapping, by=c("primemover", "fuel")) %>%
  filter(overnight_c != "OTH") %>%
  filter(overnight_d != ".") %>%
  filter(primemover != "WS") %>%
  select(-matches("status"), -winter, -summer, -multigen)



  select(-primemover, -fuel) %>%
  group_by(yr, utilcode, plntcode, gencode, vintage, fuel.general, overnightcategory) %>%
  summarise(capacity = sum(capacity),
            generatin = sum(generation)) %>%
  ungroup()



  select(-status1, -summer, -winter)

# can't use cap.gen.joined as mapped version b/c missing pm-f from 'generators'
v2 <- v1 %>%
  select(-primemover, -fuel) %>%
  group_by(yr, utilcode, plntcode, gencode, )

# Capacity Factors ---------------------------------------------------------
source('data-raw/costs/capacityfactors.R')
# data: form860 generator capacities & forms759/906/920/923 plant generation output
# carries original capacity and generation as well (for weighting capital costs)

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
  mutate(oc = ifelse(oc == "combined_cycle", "CC", oc)) %>%
  mutate(oc = ifelse(oc == "combustion_turbine", "CT", oc)) %>%
  mutate(fuel = ifelse(fuel == "oil", "petroleum", fuel)) %>%
  mutate(fuel = ifelse(fuel == "natural_gas", "gas", fuel)) %>%
  mutate(fuel = ifelse(fuel == "steam_turbine", "ST", fuel)) %>%
  mutate(fuel = ifelse(fuel == "uranium", "nuclear", fuel)) %>%
  mutate(fuel = ifelse(fuel == "photovoltaic", "PV", fuel)) %>%
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
capitalcosts <- read_excel("data-raw/costs/aeo_capital_costs.xlsx",
                           sheet = "Data4GCAM",
                           skip = 3)
names(capitalcosts) <- c("yr", "reference.yr", "overnightcategory",
                         "overnight", "om.fixed", "om.var", "heatrate")
capitalcosts$yr <- as.numeric(capitalcosts$yr)
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
  left_join(cap.gen.joined, by=c("yr", "plntcode", "overnightcategory", "fuel.general")) %>%
  rename(capacity = nameplate)
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
