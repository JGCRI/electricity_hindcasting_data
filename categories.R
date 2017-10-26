capcats <- capitalcosts %>%
  group_by(overnight_category) %>%
  summarise(AEOmin=min(year), AEOmax = max(year))

overnightmapping <- read.delim('data-raw/overnight_categories.csv') %>%
  select(prime_mover, fuel, overnight_category=overnight_category.updated)
eiacats <- form860CAsupplemented %>%
  left_join(overnightmapping, by=c('prime_mover', 'fuel')) %>%
  group_by(overnight_category) %>%
  summarise(EIAmin=min(year), EIAmax=max(year), totalcapacity = sum(summer_capacity))

timelines <- full_join(capcats, eiacats, by='overnight_category')

