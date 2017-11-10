# CA Elec Supplement --------------------------------------------------------

# diff(860==NA, 860CA==NA) should equal filter(join(860, CA), 860heat==NA, CAheat!=NA)
sum(is.na(form860processed$heat_rate)) - sum(is.na(form860CAsupplemented$heat_rate))
# 327,924 - 316,771 = 11,153

test <- form860processed %>%
  left_join(ca.elec.almanac, by=c("plant_code", "year")) %>%
  mutate(heat.rate = ifelse(heat.rate==0, NA, heat.rate)) %>%
  filter(is.na(heat_rate) & !is.na(heat.rate))
nrow(test) # 11,261


# Compare AEO and 860 -----------------------------------------------------

# years present in each group
numdatagen <- form860CAsupplemented %>%
  select(year, generator_code, fuel, prime_mover, heat_rate) %>%
  left_join(mapping, by=c('prime_mover', 'fuel')) %>%
  group_by(fuel_general) %>%
  summarise(start860=min(year), end860=max(year), percent=100 * n()/nrow(form860CAsupplemented))

numdataind <- industry %>%
  group_by(overnight_category) %>%
  summarise(startAEO=min(year), endAEO=max(year), numAEO=n())

full_join(numdatagen, numdataind, by='overnight_category') %>%
  select(overnight_category, start860, startAEO, end860, endAEO, num860, numAEO) %>%
  View()


# Count entries in each category ------------------------------------------

# num of pairings
gen.pairs <- generators %>%
  group_by(overnight_category, fuel_general) %>%
  summarise(min=min(year), max=max(year), num=n())
model.pairs <- hr.model %>%
  filter(!is.na(heatrate)) %>%
  group_by(overnight_category, fuel_general) %>%
  summarise(min=min(year), max=max(year), num=n())

# dropped bc no heatrate data (although generators appear with these combinations)
dropped <- gen.pairs %>%
  anti_join(model.pairs, by=c('overnight_category', 'fuel_general')) %>%
  select(overnight_category, fuel_general) %>%
  inner_join(generators, by=c('overnight_category', 'fuel_general'))

