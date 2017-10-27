new <- hr %>%
  group_by(overnight_category, fuel_general) %>%
  summarise(start=min(year), end=max(year))

new1 <- h.r.model %>%
  group_by(overnight_category, fuel_general) %>%
  summarise(modelstart=min(year), modelend=max(year))


fullmap <- mapping %>%
  select(overnight_category, fuel_general) %>%
  distinct()

justcat <- generators %>%
  filter(! is.na(overnight_category))  %>%
  select(overnight_category,fuel_general) %>%
  distinct()

justfuel <- generators %>%
  filter(! is.na(fuel_general))  %>%
  select(overnight_category,fuel_general) %>%
  distinct()

both <- generators %>%
  filter(! is.na(overnight_category))  %>%
  filter(! is.na(fuel_general))  %>%
  select(overnight_category,fuel_general) %>%
  distinct()

dim(right_join(generators, both, by=c('overnight_category', 'fuel_general')))



summary <- hr %>%
  group_by(overnight_category, fuel_general) %>%
  summarise(n=n())
View(summary)
