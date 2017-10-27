
# Data --------------------------------------------------------------------
fuels <- read.delim('data-raw/form860fuels.csv') %>%
  select(fuel, fuel_general=corrections.from.write.up)

categories <- read.delim('data-raw/overnight_categories.csv') %>%
  select(prime_mover, fuel, overnight_category = overnight_category.updated)


# Combine -----------------------------------------------------------------

skeleton <- left_join(categories, fuels, by='fuel')


# Save --------------------------------------------------------------------

use_data(skeleton, overwrite=TRUE)
