calc.levelizedcosts <- function(capcosts, cf.data, fcr, fuelprices, techmap) {
  # levelize capital costs

  # take capital costs data
  levcst <- capcosts %>%
    full_join(techmap, by="technology") %>% # attach overnightcategory & fuel.general
    select(-technology) %>%
    inner_join(cf.data, by=c("yr", "overnightcategory", "fuel.general")) %>% # attach $$$ data to plant-level capacity factor data
    mutate(LCOE_Capital = fcr * (1000 * overnight) / (8760 * capacityfactor), # $/MWh
           LCOE_FOM = (1000 * om.fixed) / (8760 * capacityfactor),
           LCOE_VOM = om.var,
           LCOE_wo_Fuel = LCOE_Capital + LCOE_FOM + LCOE_VOM) %>%  # $/MWh
    select(-overnight, -capacityfactor, -om.fixed, -om.var)

  levcst.fuel <- levcst %>%
    left_join(fuelprices, by=c("yr", "fuel.general")) %>%
    mutate(fuel.price = ifelse(is.na(fuel.price), 0, fuel.price),
           LCOE_Fuel = fuel.price / (3412 / heatrate),
           LCOE = LCOE_wo_Fuel + LCOE_Fuel) %>%  # adjust fuel.price to fuel efficiency = 3412 / heatrate
    select(-fuel.price, -heatrate)

}
