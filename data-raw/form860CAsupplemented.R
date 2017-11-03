# update EIA plant information w/ CA heat rate data -----------------------
form860CAsupplemented <- form860processed %>%
  left_join(ca.elec.almanac, by=c("plant_code", "year")) %>%
  # join CA almanac heat rates data to mapping_860_overnight_R.tsv data
  # multiple plants w/in same year ? 5005 original heat.rates, yet pasted 11243 times in final cap.eia dataframe
  mutate(heat_rate = ifelse(is.na(heat_rate) & !is.na(heat.rate), heat.rate, heat_rate)) %>% # where missing, replace with CA heatrate
  select(-heat.rate)
# save file ---------------------------------------------------------------
devtools::use_data(form860CAsupplemented, overwrite=TRUE)
