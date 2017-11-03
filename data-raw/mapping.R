
# Data --------------------------------------------------------------------
fuels <- read.delim('data-raw/form860fuels.tsv') %>%
  select(fuel, fuel_general=corrections.from.write.up)

categories <- read.delim('data-raw/overnight_categories.tsv') %>%
  select(prime_mover, fuel, overnight_category = overnight_category.updated)


# Combine -----------------------------------------------------------------

mapping <- left_join(categories, fuels, by='fuel')


# Save --------------------------------------------------------------------

use_data(mapping, overwrite=TRUE)
