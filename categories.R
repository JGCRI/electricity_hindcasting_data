
# To-Do -------------------------------------------------------------------

# How to filter form860 data so that generation is presented alongside its corresponding investment (construction delay)
# Look at investment vs capacity factor
# Other parameters to illuminate relationship between investment and performance characteristics?


# overnight categories ----------------------------------------------------
capcats <- capitalcosts %>%
  group_by(overnight_category) %>%
  summarise(AEOmin=min(year), AEOmax = max(year), AEOyears=n(),
            avgovernight = sum(base.overnight) )

overnightmapping <- read.delim('data-raw/overnight_categories.tsv') %>%
  select(prime_mover, fuel, overnight_category=overnight_category.updated) %>%
  arrange(prime_mover,fuel)
eiacats <- form860CAsupplemented %>%
  left_join(overnightmapping, by=c('prime_mover', 'fuel')) %>%
  group_by(overnight_category) %>%
  summarise(EIAmin=min(year), EIAmax=max(year), EIAyears=length(unique(year)),
            totalcapacity = sum(summer_capacity, na.rm=TRUE), avgheatrate = mean(heat_rate, na.rm=TRUE),
            generators= 100 * n()/nrow(form860CAsupplemented) )

timelines <- full_join(capcats, eiacats, by='overnight_category')

# capacity vs numgenerators
ggplot(na.omit(timelines), aes(generators, totalcapacity)) +
  geom_point(aes(colour=avgheatrate))

# capacities of conv.cc turbines
form860CAsupplemented %>%
  left_join(overnightmapping, by=c('prime_mover', 'fuel')) %>%
  filter(overnight_category=='conventional combined cycle') %>%
  ggplot( aes(summer_capacity)) +
  geom_histogram() +
  labs(title='conventional combined cycle')
# capacities of all categories
form860CAsupplemented %>%
  left_join(overnightmapping, by=c('prime_mover', 'fuel')) %>%
  ggplot( aes(summer_capacity) ) +
  geom_histogram() +
  facet_wrap(~overnight_category)
# capacities of all categories
form860CAsupplemented %>%
  left_join(overnightmapping, by=c('prime_mover', 'fuel')) %>%
  ggplot( aes( summer_capacity, colour=overnight_category) ) +
  geom_freqpoly()

# movers ------------------------------------------------------------------
movers <- read.delim('data-raw/form860movers.csv')
moverslist <- as.character(movers$prime_mover) # 38
moversused <- as.character(unique(form860CAsupplemented$prime_mover)) # 34
mappingmovers <- as.character(unique(overnightmapping$prime_mover)) # matches movers used -- good!

# fuels -------------------------------------------------------------------
# used and unused
fuels <- read.delim('data-raw/form860fuels.csv') %>%
  select(fuel, fuel_general=corrections.from.write.up)
fuelslist <- as.character(fuels$fuel) # 77
fuelsused <- as.character(unique(form860CAsupplemented$fuel)) # 64
mappingfuels <- as.character(unique(overnightmapping$fuel)) # matches fuelsused -- good!
# fuelsnotused <- fuelslist[! fuelslist %in% fuelsused]

# fuel_general mapping
renewables <- c('GEO', 'GST', 'SUN', 'WAT', 'WND')
biomass <- c('AB', 'OBS', 'REF', 'WD', 'WDS', 'WDL') # WDL originally unassigned
naturalgas <- c('LFG', 'MTE', 'NG', 'OBS', 'SNG', 'SG', 'SGC', 'SGP', 'GAS') # GAS originally ignored
oil <- c('DFO', 'FO1', 'FO2', 'FO4', 'FO5', 'FO6', 'OBL', 'RFO', 'WO', 'PET') # PET originally ignored
kerosene <- c('JF', 'KER')
coal <- c('ANT', 'BIT', 'COL', 'LIG', 'PC', 'SUB', 'WC', 'RC', 'SC')
uranium <- c('NUC', 'UR') # also plutonium and thorium but zero generators
propane <- 'PG'
unassigned <- c('MF', 'MSW', 'BLQ', 'LPG', 'STM', 'WH', 'PUR', 'SLW', 'TDF', 'BDF', 'OG', 'RG') # unclear mapping
ignored <- c('BIO', 'RRO', 'TOP', 'FO3', 'COM', 'PRO', 'CWM', 'PL', 'TH', 'COG', 'LNG', 'MTH') # zero generators
undefined <- c('OO', 'SU', 'UNK', 'WOC')



# fuel_general ------------------------------------------------------------
fuelstime <- fuel.prices %>%
  group_by(fuel_general) %>%
  summarise(min=min(year) , max=max(year))
