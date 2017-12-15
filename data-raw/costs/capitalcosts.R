
prep.capitalcosts <- function(aeofile, tech.oc.file, gdpdeflator )
{
  ## DATA
  overnight <- read.csv(aeofile) %>%
    select(Technology, yr, cost.yr, overnight.base, variable.o.m, fixed.o.m)
  # native units: overnight.base ~ $/kW ; fixed.o.m ~ $/kW/yr ; variable.o.m ~ $/MWh

  ## NORMALIZE CATEGORIES TEXT
  tech.oc.map <- read.csv(tech.oc.file)
  overnight.oc <- overnight %>%
    left_join(tech.oc.map, by=c("Technology")) %>%
    filter(overnightcategory != "") %>%
    mutate(overnightcategory = as.character(overnightcategory)) %>%
    select(-Technology)


  ## VALUE ADJUSTMENT
  capitalcosts <- overnight.oc %>%
    inner_join(gdpdeflator, by="cost.yr") %>%
    mutate(overnight = deflator * overnight.base,
           om.var = deflator * variable.o.m,
           om.fixed = deflator * fixed.o.m) %>%
    select(yr, overnightcategory, overnight, om.var, om.fixed)

  capitalcosts

}
