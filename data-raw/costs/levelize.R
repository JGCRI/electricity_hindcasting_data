calc.levelizedcosts <- function(capfactor, capcosts, map, fcr)
{

  levcst <- capfactor %>%
    inner_join(map, by=c("prime_mover", "fuel")) %>%  # attach overnight_category and fuel_general
    inner_join(capcosts, by=c("year", "overnight_category")) %>% # attach three capital costs
    # levelize capital costs
    mutate(overnight.lev = fcr * overnight / (8760 * capacityfactor),
           om.fixed.lev = om.fixed / (8760 * capacityfactor),
           cost.lev = overnight + om.fixed + om.var) %>%
    select(year, utility_code, plant_code, prime_mover, fuel, overnight_category, fuel_general,
           overnight.lev, om.fixed.lev, om.var, cost.lev) %>%
    group_by(year, utility_code, plant_code, overnight_category, fuel_general) %>%
    summarise(overnight.lev = mean(overnight.lev),
              om.fixed.lev = mean(om.fixed.lev),
              om.var = mean(om.var),
              cost.lev = mean(cost.lev)) %>%
    ungroup()

  levcst
}
