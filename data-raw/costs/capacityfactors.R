calc.capacityfactors <- function(generators, plantgeneration)
{
  plantcapacity <- generators %>%
    select(yr, nameplate, utilcode, plntcode, overnightcategory, fuel.general) %>%
    rename(capacity = nameplate) %>%
    group_by(yr, utilcode, plntcode, overnightcategory, fuel.general) %>%
    summarise(capacity=sum(capacity)) %>%
    ungroup()

  plantgeneration <- plantgeneration %>%
    select(yr, generation, utilcode, plntcode, overnightcategory, fuel.general) %>%
    filter(generation > 0) # drop generators that used more energy than they produced

  cf.unfilt <- plantcapacity %>%
    inner_join(plantgeneration, by=c("yr", "utilcode", "plntcode", "overnightcategory", "fuel.general") ) %>%
    mutate(potentialgeneration = capacity * 8760) %>%
    mutate(capacityfactor = generation/potentialgeneration) %>%
    select(yr, utilcode, plntcode, overnightcategory, fuel.general, capacityfactor)

  cf <- cf.unfilt %>%
    mutate(capacityfactor = ifelse(capacityfactor > 1, 1, capacityfactor))
  # CF > 1 is data error. See figs/filterbyCF for analysis how filter(CF < 1) affected data

  data <- list(cf.unfilt, cf)
  names(data) <- c("cf.unfilt", "cf")
  # cf used in later calculations

  data
}
