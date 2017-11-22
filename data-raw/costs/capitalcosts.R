
prep.capitalcosts <- function(aeofile, gdpdeflator)
{

  ## DATA
  overnight <- read.delim(aeofile) %>%
    select(year, cost.year, overnight_category, base.overnight, variable.o.m, fixed.o.m) %>%
    arrange(year, cost.year, overnight_category)

  ## NORMALIZE CATEGORIES TEXT
  overnight$overnight_category <- gsub('_', ' ', overnight$overnight_category)

  ## UNIT CONVERSIONS
  overnight <- overnight %>%
    mutate(fixed.o.m = fixed.o.m/1000,# /kW*yr -> /MW*yr
           base.overnight = base.overnight/1000) # /kW -> /Mw

  ## VALUE ADJUSTMENT
  capitalcosts <- overnight %>%
    inner_join(gdpdeflator) %>%
    mutate(overnight = deflator * base.overnight,
           om.var = deflator * variable.o.m,
           om.fixed = deflator * fixed.o.m) %>%
    select(year, overnight_category, overnight, om.var, om.fixed)

  capitalcosts

}
