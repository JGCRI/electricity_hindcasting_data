calc.levelizedcosts <- function(capfactor, capcosts, map, fcr, gen)
{

  levcst <- capfactor %>%
    inner_join(map, by=c("prime_mover", "fuel")) %>%  # attach overnight_category and fuel_general
    inner_join(capcosts, by=c("year", "overnight_category")) %>% # attach three capital costs
    # levelize capital costs
    mutate(overnight = fcr * overnight / capacityfactor,
           om.fixed = om.fixed / capacityfactor,
           levcost = overnight + om.fixed + om.var) %>%
    select(-capacityfactor, -overnight, -om.var, -om.fixed)

  # when grouped by ID's (year, util, plnt, oc, fg), levcst has duplicate rows
  # this is from multiple pm-f choices mapping to the same oc
  # must collapse these dupicate rows by taking weighted average across multiple instances of oc-fg choice
  # weight by share of total generation (for year, util, plnt, oc, fg ID's)
  # generation.weight = generation/generation.total

  # generation (using oc-fg ID's)
  # due to inconsistent/erroneous recordkeeping, some plants generated more than their recorded capacity allows.
  # in other cases, generators produced negative energy (used power to stay online but weren't utilized)
  # both instances were filtered out in producing the capacityfactors data, therefore we will use only generation data
  # consistent with the capacityfactors data. this is what left_join(capfactor, gen) accomplishes
  gen <- capfactor %>%
    left_join(gen, by=c("year", "utility_code" ,"plant_code", "prime_mover", "fuel")) %>%
    left_join(map, by=c("prime_mover", "fuel")) %>%
    select(year, utility_code, plant_code, overnight_category, fuel_general, generation)

  # generation.total (using oc-fg ID's)
  totalgen <- gen %>%
    group_by(year, utility_code, plant_code, overnight_category, fuel_general) %>%
    summarise(generation.total = sum(generation)) %>%
    ungroup()

  # generation.weight
  levcst <- levcst %>%
    left_join(gen, by=c("year", "utility_code", "plant_code", "overnight_category", "fuel_general") ) %>%
    left_join(totalgen, by=c("year", "utility_code", "plant_code", "overnight_category", "fuel_general")) %>%
    mutate(generation.weight =  generation/generation.total) %>%
    # collapse duplicate rows for ID's (year, util, plnt, oc, fg)
    group_by(year, utility_code, plant_code, overnight_category, fuel_general) %>%
    summarise(levcost = weighted.mean(levcost, generation.weight)) %>%
    ungroup() %>%
    select(year, utility_code, plant_code, overnight_category, fuel_general, levcost)

  levcst
}
