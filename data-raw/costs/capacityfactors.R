calc.capacityfactors <- function(generators, plantgeneration)
{
  plantcapacity <- generators %>%
    select(yr, nameplate, utilcode, plntcode, gencode, overnightcategory, fuel.general) %>%
    rename(capacity = nameplate) %>%
    group_by(yr, utilcode, plntcode, overnightcategory, fuel.general) %>%
    summarise(capacity=sum(capacity)) %>%
    ungroup()

  plantgeneration <- plantgeneration %>%
    select(yr, generation, utilcode, plntcode, overnightcategory, fuel.general) %>%
    filter(generation > 0) # drop generators that used more energy than they produced

  capacityfactors <- plantcapacity %>%
    inner_join(plantgeneration, by=c("yr", "utilcode", "plntcode", "overnightcategory", "fuel.general") ) %>%
    mutate(potentialgeneration = capacity * 8760) %>%
    mutate(capacityfactor = generation/potentialgeneration) %>%
    filter(capacityfactor < 1) %>% # this could be erroneous records or the fact that we're using summercapacity instead of nameplate
    select(yr, utilcode, plntcode, overnightcategory, fuel.general, capacityfactor)

  capacityfactors

}
