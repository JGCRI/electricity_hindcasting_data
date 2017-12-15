
prep.mapping <- function(fuelsfile, categoriesfile)
{
  ## DATA
  fuels <- read.csv(fuelsfile) %>%
    select(fuel, fuel.general) %>%
    mutate(fuel = as.character(fuel),
           fuel.general = as.character(fuel.general) )

  categories <- read.csv(categoriesfile) %>%
    select(primemover, fuel, overnightcategory) %>%
    mutate(primemover = as.character(primemover),
           fuel = as.character(fuel),
           overnightcategory = as.character(overnightcategory) )

  ## COMBINE
  mapping <- full_join(categories, fuels, by='fuel')
  mapping

}
