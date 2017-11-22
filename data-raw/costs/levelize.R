calc.levelizedcosts <- function(capfactor, capcosts, map, fcr)
{
  map <- map %>%
    select(prime_mover, fuel, overnight_category)

  levcst <- capfactor %>%
    inner_join(map, by=c("prime_mover", "fuel")) %>%
    inner_join(capcosts, by=c("year", "overnight_category")) %>%
    mutate(overnight = fcr * overnight / capacityfactor,
           om.fixed = om.fixed / capacityfactor,
           levcost = overnight + om.fixed + om.var) %>%
    select(year, utility_code, plant_code, prime_mover, fuel, levcost)

}
