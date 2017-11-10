
write.csv(form860CAsupplemented, 'form860CAsupplemented.csv', row.names=F)
write.csv(marginalcosts, 'marginalcosts.csv', row.names=F)
write.csv(capitalcosts, 'capitalcosts.csv', row.names=F)
write.csv(mapping, 'mapping.csv', row.names=F)

truncate <- form860processed %>%
  select(year, utility_code, plant_code, generator_code, prime_mover, fuel, heat_rate) %>%
  left_join(mapping, by=c('prime_mover', 'fuel')) %>%
  select(-prime_mover, -fuel) %>%
  select(overnight_category, fuel_general, utility_code, plant_code, generator_code, heat_rate, year)
  # couldn't get to work, saved in long format
  spread(key=year, value=heat_rate)
  dcast(overnight_category+fuel_general+utility_code+plant_code+generator_code~ year, value.var = "heat_rate")

write.csv(truncate, 'truncate.csv', row.names=F)
