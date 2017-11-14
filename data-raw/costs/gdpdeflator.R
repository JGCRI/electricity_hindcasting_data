
calc.gdpdeflator <- function(deflatorfile, referenceyear)
{

  gdpdef <- read.csv(deflatorfile)

  gdpdef$year <- ymd(gdpdef$DATE) %>% year()
  gdpdef <- gdpdef %>%
    select(year, GDPDEF) %>%
    group_by(year) %>%
    summarise(GDPDEF = mean(GDPDEF)) %>%
    data.frame()
  row.names(gdpdef) <- gdpdef$year
  reference_deflation <- gdpdef[referenceyear,'GDPDEF']
  gdpdef <- gdpdef %>%
    mutate(deflator = reference_deflation / GDPDEF) %>%
    select(-GDPDEF) %>%
    dplyr::rename(cost.year = year)

  gdpdef

}
