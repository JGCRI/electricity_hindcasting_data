calc.capacityfactors <- function(cap.gen.joined)
{
  cf <- cap.gen.joined %>%
    filter(generation > 0) %>% # drop generators that used more energy than they produced
    mutate(potentialgeneration = nameplate * 8760) %>%
    mutate(capacityfactor = generation/potentialgeneration) %>%
    select(yr, plntcode, overnightcategory, fuel.general, capacityfactor)

  cf.clamp <- cf %>%
    mutate(capacityfactor = ifelse(capacityfactor > 1, 1, capacityfactor))
  # CF > 1 is data error. See figs/filterbyCF for analysis how filter(CF < 1) affected data

  data <- list(cf, cf.clamp)
  names(data) <- c("cf", "cf.clamp")
  # cf.clamp used in later calculations

  data
}
