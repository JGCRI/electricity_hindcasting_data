
prep.mapping <- function(df.cap, fuelsfile, moversfile)
{
  ## combos that need mapping
  combos <- df.cap %>%
    select(primemover, fuel) %>%
    distinct()

  ## mapping files
  fuels <- read.csv(fuelsfile, stringsAsFactors=F) %>%
    select(fuel, fuel.general)

  tech <- read.csv(moversfile, stringsAsFactors=F) %>%
    select(primemover, tech)

  ## COMBINE
  mapping <- combos %>%
    left_join(fuels, by="fuel") %>%
    left_join(tech, by="primemover") %>%
    mutate(tech = paste0(fuel.general, " (", tech, ")"))
  mapping

}
