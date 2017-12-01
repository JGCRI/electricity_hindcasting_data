# when grouped by ID's (year, util, plnt, oc, fg), levcst has duplicate rows
# this is from multiple pm-f choices mapping to the same oc
# must collapse these duplicate rows by taking weighted average across multiple instances of oc-fg choice
# weight by share of total generation (for ID's year, util, plnt, oc, fg)
# generation.weight = generation/generation.total

# generation (using oc-fg ID's)
# see capacityfactors.R for documentation of wonky numbers from that calculation.
# we only want to use generation weights that contributed to acceptable capacityfactors,
# so we take the 'generation' col from the calculated capacityfactors df
gen <- capfactor %>%
  select(year, utility_code, plant_code, prime_mover, fuel, generation) %>%
  left_join(map, by=c("prime_mover", "fuel")) %>%
  select(-prime_mover, -fuel)

# generation.total (using oc-fg ID's)
totalgen <- gen %>%
  group_by(year, utility_code, plant_code, overnight_category, fuel_general) %>%
  summarise(generation.total = sum(generation)) %>%
  ungroup()

# generation.weight
levcst.weighted <- levcst %>%
  left_join(gen, by=c("year", "utility_code", "plant_code", "overnight_category", "fuel_general") ) %>%
  left_join(totalgen, by=c("year", "utility_code", "plant_code", "overnight_category", "fuel_general")) %>%
  mutate(generation.weight =  generation/generation.total) %>%
  # collapse duplicate rows for ID's (year, util, plnt, oc, fg)
  group_by(year, utility_code, plant_code, overnight_category, fuel_general) %>%
  summarise(cost.lev = weighted.mean(levcst, generation.weight)) %>%
  ungroup()
