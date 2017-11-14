
prep.capitalcosts <- function(aeofile, gdpdeflator)
{


  # To-Do -------------------------------------------------------------------


  ## DATA
  overnight <- read.delim(aeofile) %>%
    select(year, cost.year, overnight_category, base.overnight, variable.o.m, fixed.o.m) %>%
    arrange(year, cost.year, overnight_category)

  ## NORMALIZE CATEGORIES TEXT
  overnight$overnight_category <- gsub('_', ' ', overnight$overnight_category)

  ## UNIT CONVERSIONS
  overnight <- overnight %>%
    mutate(fixed.o.m = fixed.o.m/1000/8760,# /kWyr -> /MWh
           base.overnight = base.overnight/1000) # /kW -> MW

  ## VALUE ADJUSTMENT
  capitalcosts <- overnight %>%
    inner_join(gdpdeflator) %>%
    mutate(base.overnight = deflator * base.overnight,
           variable.o.m = deflator * variable.o.m,
           fixed.o.m = deflator * fixed.o.m) %>%
    select(-cost.year, -deflator)

}
