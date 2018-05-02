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
  filter(generation > 0) %>%
  group_by(yr, utilcode, plntcode, primemover, fuel) %>%
  summarise(generation=sum(generation),
            consumption=sum(consumption)) %>%
  ungroup()
devtools::use_data(generation.unmapped, overwrite=TRUE)
if (csv) {
  write.csv(generation.unmapped, "CSV/generation.unmapped.csv", row.names=FALSE)
}


# OC-FG Mapping -----------------------------------------------------------
source('data-raw/mappingfiles/mapping.R')
# data: fuel_general and overnight_categories constructed from native fuel and prime_mover codes
mapping <- prep.mapping("data-raw/mappingfiles/mapping_final.csv") %>%
  rename(fuel.general = fuel_general)
devtools::use_data(mapping, overwrite=TRUE)
if(csv) {
  write.csv(mapping, "CSV/mapping.csv", row.names=FALSE)
}


# JOIN.CAP.GEN.UNMAPPED ---------------------------------------------------

source('data-raw/costs/capacityfactors.R')

# join capacity.unmapped and generation.unmapped

generation.unmapped.na <- generation.unmapped %>%
  filter( is.na(primemover) )
generation.unmapped.notna <- generation.unmapped %>%
  filter( !is.na(primemover) )

join.na.unmapped <- join.cap.gen(capacity.unmapped, generation.unmapped.na, na.case=TRUE)

join.notna.unmapped <- join.cap.gen(capacity.unmapped, generation.unmapped.notna, na.case=FALSE)

# save cap.gen.joined.unmapped (native pm-f codes)
cap.gen.joined.unmapped <- rbind(join.na.unmapped, join.notna.unmapped)
devtools::use_data(cap.gen.joined.unmapped, overwrite=TRUE)
if (csv) {
  write.csv(cap.gen.joined.unmapped, "CSV/cap.gen.joined.unmapped.csv", row.names=FALSE)
}

# CAP.GEN.JOINED ----------------------------------------------------------

# attach oc-fg clumns
# to make oc-fg unique ID's we need to do an aggregation, but after assigning vintage yr
cap.gen.joined.mapped <- cap.gen.joined.unmapped %>%
  filter(yr != 2000) %>%
  filter(!is.na(vintage)) %>%
  left_join(mapping, by=c("primemover", "fuel")) %>%
  filter(overnight_c != "OTH") %>%
  filter(overnight_d != ".") %>%
  filter(primemover != "WS") %>%
  select(-overnight_d) %>%
  rename(overnightcategory = overnight_c)

weights <- cap.gen.joined.mapped %>%
  # aggregate over vintage to get total capacity by {yr, plnt, fg, oc}
  group_by(plntcode, overnightcategory, fuel.general) %>%
  summarise(cap.total = sum(capacity) ) %>%
  ungroup() %>%
  # calculate start year's share of total capacity (w/in plant)
  right_join(cap.gen.joined.mapped, by = c("plntcode", "overnightcategory", "fuel.general")) %>%
  mutate(wt=capacity/cap.total) %>%
  # cap.total is degenerate under yr
  select(yr, plntcode, overnightcategory, fuel.general, wt)

# calculate startyr for a given {plant, oc, fg}
cap.gen.joined.mapped.weighted <- cap.gen.joined.mapped %>%
  left_join(weights, by=c("yr", "plntcode", "overnightcategory", "fuel.general")) %>%
  group_by(plntcode, overnightcategory, fuel.general) %>%
  summarise(startyr=stats::weighted.mean(vintage, wt) %>% round()) %>%
  ungroup()

# aggregate over degenerate pm-f -> oc-fg mappings
# pass on a pcode-oc-fg row's startyr to each yr in original joined data
cap.gen.joined <- cap.gen.joined.mapped %>%
  left_join(cap.gen.joined.mapped.weighted, by = c("plntcode", "fuel.general", "overnightcategory")) %>%
  group_by(yr, startyr, plntcode, fuel.general, overnightcategory) %>%
  summarise(capacity=sum(capacity), # sum over pre-mapped pm-f
            generation=sum(generation)) %>%  # sum over pre-mapped pm-f
  ungroup()

# save cap.gen.joined (oc-fg codes)
devtools::use_data(cap.gen.joined, overwrite=TRUE)
if (csv) {
  write.csv(cap.gen.joined, "CSV/cap.gen.joined.csv", row.names=FALSE)
}



# master set --------------------------------------------------------------
v1 <- cap.gen.joined.unmapped %>%
  # aggregate over vintage
  group_by(yr, plntcode, primemover, fuel) %>%
  summarise(capacity = sum(capacity),
            generation = sum(generation)) %>%
  ungroup() %>%
  inner_join(generators, by=c("yr", "plntcode", "primemover", "fuel")) %>%
  select(-capacity.y) %>% # drop GEN capacity #'s
  rename(capacity = capacity.x) %>% # use JOIN capacity #'s (plant-level)
  filter(!is.na(vintage))

v2 <- v1 %>%
  left_join(mapping, by=c("primemover", "fuel")) %>%
  filter(overnight_c != "OTH") %>%
  filter(overnight_d != ".") %>%
  filter(primemover != "WS") %>%
  select(-matches("status"), -winter, -summer, -multigen) %>%
  select(yr, utilcode, plntcode, gencode, vintage, endyr, fuel, primemover, fuel.general, overnight_c, overnight_d, capacity, generation, heatrate)



# Capacity Factors --------------------------------------------------------
source('data-raw/costs/capacityfactors.R')
# data: form860 generator capacities & forms759/906/920/923 plant generation output
# carries original capacity and generation as well (for weighting capital costs)

# calculate capacityfactors
capacityfactors <- calc.capacityfactors(cap.gen.joined, "data-raw/costs/epm2017.csv")

# CF as calculated from form data
capacityfactors.data <- capacityfactors$data
devtools::use_data(capacityfactors.data, overwrite=TRUE)
if (csv) {
  write.csv(capacityfactors.data, "CSV/capacityfactors.data.csv", row.names=FALSE)
}

# supplemental CF for missing data (from electrical power monthly)
capacityfactors.sup <- capacityfactors$epm
devtools::use_data(capacityfactors.sup, overwrite=TRUE)
if (csv) {
  write.csv(capacityfactors.sup, "CSV/capacityfactors.sup.csv", row.names=FALSE)
}


# AEO Capital Costs -------------------------------------------------------
capitalcosts <- read_excel("data-raw/costs/aeo_capital_costs.xlsx",
                            sheet = "Fill_in_Missing_Tech_category",
                            skip = 2)
names(capitalcosts) <- c("yr", "reference.yr", "technology", "yr.available",
                          "size", "overnight.foak", "overnight", "om.fixed", "om.var",
                          "heatrate.foak", "heatrate", "construction")
capitalcosts <- capitalcosts %>%
  filter(! technology %in% c("Distributed Generation (base)",
                           "Distributed Generation (peak)",
                           "IGCC w/CCS",
                           "Advanced CC",
                           "Advanced CC w/CCS",
                           "Advanced Coal (IGCC") ) %>%
  select(yr, technology, overnight, om.fixed, om.var, heatrate)

devtools::use_data(capitalcosts, overwrite=TRUE)
if (csv) {
  write.csv(capitalcosts, "CSV/capitalcosts.csv", row.names=FALSE)
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


# LCOE --------------------------------------------------------------------
source('data-raw/costs/levelize.R')
# equation: See data-raw/costs/gcam/Electricity Generation Assumptions.pdf for equation
# constant fixed charge rate of 0.13 from GCAM
# units: overnight.lev, om.fixed.lev, om.var ~ $/MWh
techmap <- read.csv("data-raw/costs/aeo-tech.csv", stringsAsFactors=F)
levelizedcosts <- calc.levelizedcosts(capitalcosts, capacityfactors.data, 0.13, fuelprices, techmap)
devtools::use_data(levelizedcosts, overwrite=TRUE)
if (csv) {
  write.csv(levelizedcosts, "CSV/levelizedcosts.csv", row.names=FALSE)
}
