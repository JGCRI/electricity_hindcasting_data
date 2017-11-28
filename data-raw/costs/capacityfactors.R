calc.capacityfactors <- function(generators, plantgeneration)
{
  plantcapacity <- generators %>%
    select(year, summer_capacity, utility_code, plant_code, generator_code, prime_mover, fuel) %>%
    rename(capacity = summer_capacity) %>%
    group_by(year, utility_code, plant_code, prime_mover, fuel) %>%
    summarise(capacity=sum(capacity)) %>%
    ungroup()

  plantgeneration <- plantgeneration %>%
    select(year, generation, utility_code, plant_code, prime_mover, fuel) %>%
    filter(generation > 0) # drop generators that used more energy than they produced

  capacityfactors <- plantcapacity %>%
    inner_join(plantgeneration, by=c("year", "utility_code", "plant_code", "prime_mover", "fuel") ) %>%
    mutate(potentialgeneration = capacity * 8760) %>%
    mutate(capacityfactor = generation/potentialgeneration) %>%
    filter(capacityfactor < 1) # this could be erroneous records or the fact that we're using summercapacity instead of nameplate

  capacityfactors

}
