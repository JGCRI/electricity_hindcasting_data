calc.levelizedcosts <- function(capcosts, cf.data, fcr, fuelprices, techmap) {

  # map capital costs technology labels to overnightcategory
  capcosts.oc <- capcosts %>%
    inner_join(techmap, by="technology") %>%  # attach overnightcategory & fuel.general
    select(-technology)

  # take plant-level capacity factor data
  levcst <- cf.data %>%
    inner_join(capcosts.oc, by=c("yr", "overnightcategory", "fuel.general")) %>% # attach $$$ data to plant-level capacity factor data
    mutate(LCOE_Capital = fcr * (1000 * overnight) / (8760 * capacityfactor), # $/MWh
           LCOE_FOM = (1000 * om.fixed) / (8760 * capacityfactor),
           LCOE_VOM = om.var,
           LCOE_wo_Fuel = LCOE_Capital + LCOE_FOM + LCOE_VOM) %>%  # $/MWh
    select(-overnight, -om.fixed, -om.var, -capacityfactor)

  levcst.fuel <- levcst %>%
    left_join(fuelprices, by=c("yr", "fuel.general")) %>%
    mutate(fuel.price = ifelse(is.na(fuel.price), 0, fuel.price),
           LCOE_Fuel = fuel.price / (3412 / heatrate),
           LCOE = LCOE_wo_Fuel + LCOE_Fuel) %>%  # adjust fuel.price to fuel efficiency = 3412 / heatrate
    select(-fuel.price, -heatrate)

}
