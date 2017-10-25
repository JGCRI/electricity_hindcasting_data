
# Data --------------------------------------------------------------------
fuels <- read.csv('data-raw/fuels.csv') %>%
  select(fuel, fuel_general)

skeleton <- form860supplemented %>%
  left_join(fuels, by='fuel') %>%
  select(year, overnight_category, fuel_general)


# Summary -----------------------------------------------------------------

skeleton <- skeleton %>%
  filter(overnight_category != 'undefined' & !is.na(overnight_category)) %>%
  filter(! is.na(fuel_general)) %>%
  filter(! is.na(year)) %>%
  filter(year >= 1990) %>%
  distinct(overnight_category, fuel_general, year) %>%
  group_by(overnight_category, fuel_general) %>%
  complete(year=seq(from=1990, to=2014, by=1)) %>%
  ungroup()


# Save --------------------------------------------------------------------

use_data(skeleton, overwrite=TRUE)
