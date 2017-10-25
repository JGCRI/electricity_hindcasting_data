# update EIA plant information w/ CA heat rate data -----------------------
form860CAsupplemented <- form860processed %>%
  left_join(ca.elec.almanac, by=c("plant_code", "year")) %>%
  # join CA almanac heat rates data to mapping_860_overnight_R.tsv data
  # multiple plants w/in same year ? 5005 original heat.rates, yet pasted 11243 times in final cap.eia dataframe
  mutate(heat_rate = ifelse(heat_rate==0, NA, heat_rate)) %>%
  mutate(heat_rate = ifelse(is.na(heat_rate), heat.rate, heat_rate))
# where 860 heat_rate data exists, leave. where NA, replace with CA heat.rate data
# select(year, plant_code, summer_capacity)

# how many plants' heat_rate is replaced with ca.elec.almanac?
test <- form860processed %>%
  left_join(ca.elec.almanac, by=c("plant_code", "year")) %>%
  mutate(heat_rate = ifelse(heat_rate==0, NA, heat_rate)) %>%
  filter(is.na(heat_rate) & !is.na(heat.rate) & heat.rate!=0)
nrow(test)

# save file ---------------------------------------------------------------
devtools::use_data(form860CAsupplemented, overwrite=TRUE)
