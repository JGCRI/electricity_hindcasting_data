# update EIA plant information w/ CA heat rate data -----------------------
form860CAsupplemented <- form860processed %>%
  left_join(ca.elec.almanac, by=c("plant_code", "year")) %>%
  # join CA almanac heat rates data to mapping_860_overnight_R.tsv data
  # multiple plants w/in same year ? 5005 original heat.rates, yet pasted 11243 times in final cap.eia dataframe
  mutate(heat_rate = ifelse(heat_rate==0, NA, heat_rate)) %>% # heat_rate is either non-zero or missing
  mutate(heat_rate = ifelse(is.na(heat_rate), heat.rate, heat_rate)) # where missing, replace with CA heatrate

# save file ---------------------------------------------------------------
devtools::use_data(form860CAsupplemented, overwrite=TRUE)
