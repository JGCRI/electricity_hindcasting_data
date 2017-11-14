
prep.mapping <- function(fuelsfile, categoriesfile)
{
## DATA
fuels <- read.delim(fuelsfile) %>%
  select(fuel, fuel_general)

categories <- read.delim(categoriesfile) %>%
  select(prime_mover, fuel, overnight_category)

## COMBINE
mapping <- left_join(categories, fuels, by='fuel')

}
