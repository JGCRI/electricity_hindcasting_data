library(ggplot2)

ggplot(marginalcosts) +
  geom_line(aes(x = year, y = pred.heat.rate)) +
  geom_point(aes(x = year, y = heat.rate.avg)) +
  facet_wrap(~overnight_category + fuel_general) +
  labs(x='year', y='Btu/MWh', title='Average/Predicted Heat Rate ')


o.m <- capitalcosts %>%
  select(-base.overnight) %>%
  melt( id.vars=c('year', 'overnight_category'),
        measure.vars=c('variable.o.m', 'fixed.o.m'),
        variable.name='cost')

ggplot(o.m) +
  geom_point(aes(x=year, y=value, colour=cost)) +
  facet_wrap(~overnight_category) +
  labs(x='year', y='1975$/MWh', title='O&M Costs (1997-2015)')

select(capitalcosts, year, overnight_category,base.overnight) %>%
  ggplot() +
  geom_point(aes(x=year,y=base.overnight)) +
  facet_wrap(~overnight_category) +
  labs(x='year', y='1975$/MW', title='Overnight Costs (1997-2015)')
