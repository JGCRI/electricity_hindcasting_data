util <- read.csv(paste(startingDir, "aggregation_70_00.csv", sep="/")) %>%
  left_join(mapping, by=c("prime_mover", "fuel")) %>%
  select(-prime_mover, -fuel)

year.totals <- util %>%
  group_by(YEAR) %>%
  summarise(total.gen = sum(generation),
            total.cons = sum(consumption))

choice.totals <- util %>%
  group_by(overnight_category, fuel_general, YEAR) %>%
  summarise(choice.gen = sum(generation),
            choice.cons = sum(consumption) )

marketshares <- choice.totals %>%
  left_join(year.totals, by=c("YEAR") ) %>%
  mutate(share.gen = 100 * choice.gen/total.gen,
         share.cons = 100 * choice.cons/total.cons) %>%
  select(-total.gen, -total.cons, -choice.gen, -choice.cons)

