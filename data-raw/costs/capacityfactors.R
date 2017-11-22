calc.capacityfactors <- function(generators, plantgeneration)
{
  plantcapacity <- generators %>%
    select(year, summer_capacity, utility_code, plant_code, generator_code, prime_mover, fuel) %>%
    rename(capacity = summer_capacity) %>%
    group_by(year, utility_code, plant_code, prime_mover, fuel) %>%
    summarise(capacity=sum(capacity))

  plantgeneration <- plantgeneration %>%
    select(-consumption)

  capacityfactors <- plantcapacity %>%
    inner_join(plantgeneration, by=c("year", "utility_code", "plant_code", "prime_mover", "fuel") ) %>%
    mutate(potentialgeneration = capacity * 8760) %>%
    mutate(capacityfactor = generation/potentialgeneration) %>%
    filter(capacityfactor > 0 & capacityfactor < 1) %>%
    select(year, utility_code, plant_code, prime_mover, fuel, capacityfactor)

}
