

# find num gen ------------------------------------------------------------

form860CAsupplemented %>%
  select(generator_code) %>%
  distinct() %>%
  dim() # 4,488

form860CAsupplemented %>%
  select(generator_code, plant_code) %>%
  distinct() %>%
  dim() # 29,565

form860CAsupplemented %>%
  select(generator_code, plant_code, utility_code) %>%
  distinct() %>%
  dim() # 36,533


# capacity-heatrate spread ------------------------------------------------

overnightmapping <- read.delim('data-raw/overnight_categories.tsv') %>%
  select(prime_mover, fuel, overnight_category=overnight_category.updated) %>%
  arrange(prime_mover,fuel)

proc <- form860CAsupplemented %>%
  filter( !(year==2004 & generator_code=='gen1' & plant_code=='50602'& utility_code=='25757') ) %>%
  left_join(overnightmapping, by=c('prime_mover', 'fuel')) %>%
  filter(overnight_category %in% c('combined cycle', 'combustion turbine')) %>%
  group_by(utility_code, plant_code, generator_code, overnight_category) %>%
  summarise(summer_capacity=mean(summer_capacity, na.rm=T),
            heat_rate = mean(heat_rate, na.rm=T)) %>%
  ungroup()

ggplot(proc, aes(x=heat_rate, y=summer_capacity)) +
  geom_point() +
  # xlim(0, 1.25e5) +
  #ylim(0,1e2) +
  facet_wrap(~overnight_category) +
  labs(title=paste("all years:", nrow(proc), " gens"))

  labs(title=paste("grouped by util, plant, gen codes + category, avg'd over years:", nrow(proc), " gens"))





# histogram and scatter ---------------------------------------------------

tit <- 'form860processed'
df <- form860processed
df %>%
  left_join(overnightmapping, by=c('prime_mover', 'fuel')) %>%
  ggplot( aes(heat_rate) ) +
  geom_histogram() +
  labs(title=tit)

df %>%
  left_join(overnightmapping, by=c('prime_mover', 'fuel')) %>%
  ggplot(aes(x=summer_capacity, y=heat_rate)) +
  geom_point() +
  labs(title=tit)



# k-means -----------------------------------------------------------------
category <- 'combined cycle'
sub <- form860CAsupplemented %>%
  filter( !(year==2004 & generator_code=='gen1' & plant_code=='50602'& utility_code=='25757') ) %>%
  left_join(overnightmapping, by=c('prime_mover', 'fuel')) %>%
  filter(overnight_category %in% category) %>%
  select(heat_rate, summer_capacity) %>%
  filter(complete.cases(.)) %>%
  mutate(summer_capacity = summer_capacity/10)

km.out <- kmeans(sub, centers=2, nstart=20)

plot(sub, col=km.out$cluster, main=category)


# efficiency spread -------------------------------------------------------
# advanced combustion turbines achieve greater than 65% efficiency (LHV, nbatural gas benchmark)
# ngcc: greater than 60%?

# mover, fuel, category, fuel_general
mapping <- mapping

# Generator-level data
generators <- form860CAsupplemented %>%
  dplyr::rename(heatrate=heat_rate)

generators <- generators %>%
  left_join(mapping, by=c('prime_mover', 'fuel')) %>%
  mutate(heatrate = ifelse(is.nan(heatrate) | heatrate==0, NA, heatrate)) %>%
  mutate(efficiency = 3412/heatrate)

bad <- generators %>%
  filter(efficiency > 1)

cat <- c('combustion turbine', 'combined cycle')
generators %>%
  filter(overnight_category %in% cat) %>%
  ggplot( aes(efficiency) ) +
  geom_histogram() +
  xlim(0,1) +
  #ggtitle('All')
  facet_wrap(~overnight_category)


