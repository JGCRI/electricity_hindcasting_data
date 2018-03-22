calc.levelizedcosts <- function(capfactor, capcosts, map, fcr)
{
  # levelize capital costs
  levcst <- capfactor %>%
    inner_join(capcosts, by=c("yr", "overnightcategory")) %>% # attach overnight, om.var, om.fixed
    mutate(overnight.lev = fcr * (1000 * overnight) / (8760 * capacityfactor), # $/MWh
           om.fixed.lev = (1000 * om.fixed) / (8760 * capacityfactor),
           lcoe = overnight.lev + om.fixed.lev + om.var) %>%  # $/MWh
    select(-overnight, -om.fixed, -capacityfactor)

  levcst
}
