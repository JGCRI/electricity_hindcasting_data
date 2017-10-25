
# Data --------------------------------------------------------------------
# Generator-level data
generators <- form860CAsupplemented %>%
  select(year, plant_code, overnight_category, fuel, heat_rate)

# summary
skeleton <- skeleton

# fuel_general mapping
fuels <- read.csv('data-raw/fuels.csv') %>%
  select(fuel, fuel_general)

# maximum heatrates
maximumheatrates <- maximumheatrates

# map fuel_general onto generator data ------------------------------------
generators <- generators %>%
  left_join(fuels, by=c('fuel')) %>%
  select(-fuel)

# average heat rate for (overnight, fuel, year) ---------------------------
hr <- generators %>%
  group_by(overnight_category, fuel_general, year) %>%
  summarize(heat.rate.avg = mean(heat_rate, na.rm=TRUE)) %>%
  ungroup() %>%
  # if no data for an entire year, avg == NaN
  mutate(heat.rate.avg = ifelse(is.nan(heat.rate.avg), NA, heat.rate.avg))

# summary -----------------------------------------------------------------
summary.hr <- skeleton %>%
  left_join(hr, by=c('overnight_category','fuel_general','year')) %>%
  # attach heat.rate.avg and capacity.factor.avg to overnight-fuel capacity
  arrange(overnight_category, fuel_general, year)

# bind maximum heat_rates for 2050, 2100 ----------------------------------
summary.hr <- summary.hr %>%
  rbind(maximumheatrates)

# heat rate linear regression ---------------------------------------------

h.r.model <- summary.hr %>%
  filter(heat.rate.avg > 0) %>%
  group_by(overnight_category, fuel_general) %>%
  complete(year = seq(from = 1990, to = 2100, by = 1)) %>%
  ungroup() %>%
  mutate(time = year - 1989) %>%
  mutate(time_sq = time^2) %>%
  group_by(overnight_category, fuel_general) %>%
  do({mod <- lm(heat.rate.avg ~ time + time_sq, data = .)
  # The model is the same as heat_rate ~ time + time_sq + time*overnight_category + time_sq*overnight_category because of the group_by
  pred.heat.rate <- predict(mod, newdata = .[c("time", "time_sq")])
  data.frame(., pred.heat.rate)}) %>%
  select(overnight_category, fuel_general, year, pred.heat.rate) %>%
  mutate(pred.heat.rate = replace(pred.heat.rate, fuel_general=="solar", NA)) %>%
  mutate(pred.heat.rate = replace(pred.heat.rate, fuel_general=="wind", NA))
# We still have overnight category, general fuel combinations with low sample sizes (even after implementing the aggregation measures).

summary.hr <- summary.hr %>%
  left_join(h.r.model, by = c("overnight_category", "fuel_general", "year")) %>%
  arrange(overnight_category, fuel_general, year)

# fuel price --------------------------------------------------------------
# adjust for dollar of transaction?
energy.prices <- read.delim("data-raw/energy.prices.txt.gz") %>%
  dplyr::rename(fuel_general = fuel_1_general)
uranium.prices <- read.delim("data-raw/uranium.prices.txt.gz") %>%
  select(year, fuel.price=weighted.avg.price.nominal) %>%
  mutate(fuel_general='uranium')

energy.prices <- rbind(energy.prices, uranium.prices)

marginalcosts <- summary.hr %>%
  left_join(energy.prices, by = c('fuel_general','year')) %>%
  mutate(marginal.cost = (fuel.price/10^6)*pred.heat.rate) %>% # $/MBtu * Btu/Kwh = $/Kwh
  mutate(marginal.cost = marginal.cost*1000) # $/kwh -> $/Mwh

# save files --------------------------------------------------------------
devtools::use_data(marginalcosts, overwrite=TRUE)
