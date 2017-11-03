
# To-Do -------------------------------------------------------------------
# Clarify heatrate regression
# Are we ever assigning heatrates to generators just using average in order to model for all years
# then calculate/assign marginal cost to generator according to year & category/fuel?


# Data --------------------------------------------------------------------
# mover, fuel, category, fuel_general
mapping <- mapping

# Generator-level data
generators <- form860CAsupplemented %>%
  select(year, generator_code, fuel, prime_mover, heat_rate)

# industry-level data
industry <- read.delim("data-raw/overnight.cost.tsv") %>%
  select(year=online, overnight_category, AEO=heat.rate) %>%
  filter(overnight_category %in% unique(mapping$overnight_category)) %>%
  group_by(year, overnight_category) %>%
  summarise(AEO=mean(AEO))

# maximum heatrates - necessary
maximumheatrates <- maximumheatrates

# fuel prices
fuelprices <- fuelprices


# process generator data --------------------------------------------------
generators <- generators %>%
  left_join(mapping, by=c('prime_mover', 'fuel')) %>%
  filter(! is.na(overnight_category)) %>%
  filter(! is.na(fuel_general)) %>%
  group_by( overnight_category, fuel_general, year) %>%
  summarize(heatrate = mean(heat_rate, na.rm=TRUE)) %>%
  ungroup() %>%
  # if no data for an entire year, avg == NaN
  mutate(heatrate = ifelse(is.nan(heatrate), NA, heatrate))

# combine generator and industry data -------------------------------------
comb <- generators %>%
  full_join(industry, by=c('year', 'overnight_category')) %>%
  melt(id.vars=c('year', 'overnight_category'),
       measure.vars=c('form860', 'AEO'),
       variable.name='source') %>%
  rename(heatrate=value)

# bind maximum heat_rates for 2050, 2100 ----------------------------------
generators <- generators %>%
  rbind(maximumheatrates)

# heat rate linear regression ---------------------------------------------
hr.model <- generators %>%
  filter(heatrate > 0) %>%
  group_by(overnight_category, fuel_general) %>%
  complete(year = seq(from = 1990, to = 2100, by = 1)) %>% # 2100 if including maximumheatrates
  ungroup() %>%
  mutate(time = year - 1989) %>%
  mutate(time_sq = time^2) %>%
  group_by(overnight_category, fuel_general) %>%
  do({mod <- lm(heatrate ~ time + time_sq, data = .)
  # The model is the same as heat_rate ~ time + time_sq + time*overnight_category + time_sq*overnight_category because of the group_by
  heatrate.pred <- predict(mod, newdata = .[c("time", "time_sq")])
  data.frame(., heatrate.pred)}) %>%
  select( overnight_category, fuel_general, year, heatrate, heatrate.pred)


# calculate marginal costs ------------------------------------------------

marginalcosts <- hr.model %>%
  left_join(fuelprices, by = c('fuel_general','year')) %>%
  mutate(marginal.cost = fuel.price*heatrate.pred) %>% # $/Btu * Btu/Kwh = $/Kwh
  mutate(marginal.cost = marginal.cost*1e3) %>% # $/kwh -> $/Mwh
  select(year, fuel_general, marginal.cost)

# save files --------------------------------------------------------------
devtools::use_data(marginalcosts, overwrite=TRUE)
