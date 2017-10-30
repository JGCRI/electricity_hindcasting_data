
# To-Do -------------------------------------------------------------------
# Clarify heatrate regression
# Are we ever assigning heatrates to generators just using average in order to model for all years
# then calculate/assign marginal cost to generator according to year & category/fuel?

#

# Data --------------------------------------------------------------------
# Generator-level data
generators <- form860CAsupplemented %>%
  select(year, generator_code, fuel, prime_mover, heat_rate)

# summary
mapping <- skeleton

# maximum heatrates - necessary
maximumheatrates <- maximumheatrates

# fuel prices
fuelprices <- fuelprices

# map fuel_general & overnight_category onto generator data ---------------
generators <- generators %>%
  left_join(mapping, by=c('prime_mover', 'fuel')) %>%
  filter(! is.na(fuel_general)) %>%
  filter(! is.na(overnight_category))

# average heat rate for (overnight, fuel, year) ---------------------------
hr <- generators %>%
  group_by(overnight_category, fuel_general, year) %>%
  summarize(heat.rate.avg = mean(heat_rate, na.rm=TRUE)) %>%
  ungroup() %>%
  # if no data for an entire year, avg == NaN
  mutate(heat.rate.avg = ifelse(is.nan(heat.rate.avg), NA, heat.rate.avg))

# bind maximum heat_rates for 2050, 2100 ----------------------------------
hr <- hr %>%
  rbind(maximumheatrates)

# heat rate linear regression ---------------------------------------------
h.r.model <- hr %>%
  filter(heat.rate.avg > 0) %>%
  group_by(overnight_category, fuel_general) %>%
  complete(year = seq(from = 1990, to = 2014, by = 1)) %>% # 2100 if including maximumheatrates
  ungroup() %>%
  mutate(time = year - 1989) %>%
  mutate(time_sq = time^2) %>%
  group_by(overnight_category, fuel_general) %>%
  do({mod <- lm(heat.rate.avg ~ time + time_sq, data = .)
  # The model is the same as heat_rate ~ time + time_sq + time*overnight_category + time_sq*overnight_category because of the group_by
  pred.heat.rate.avg <- predict(mod, newdata = .[c("time", "time_sq")])
  data.frame(., pred.heat.rate.avg)}) %>%
  select(overnight_category, fuel_general, year, heat.rate.avg, pred.heat.rate.avg)

# fuel price --------------------------------------------------------------
# adjust for dollar of transaction?


marginalcosts <- summary.hr %>%
  left_join(fuel.prices, by = c('fuel_general','year')) %>%
  mutate(marginal.cost = (fuel.price/10^6)*pred.heat.rate) %>% # $/MBtu * Btu/Kwh = $/Kwh
  mutate(marginal.cost = marginal.cost*1000) # $/kwh -> $/Mwh

# save files --------------------------------------------------------------
devtools::use_data(marginalcosts, overwrite=TRUE)
