library(ggplot2)

# heat rates --------------------------------------------------------------

# generators grouped by category and fuel
hr.model %>%
  filter( ! overnight_category %in% c('fuel cell', 'photovoltaic' ) )%>%
  filter(ifelse(fuel_general=='oil', ! overnight_category %in% c('steam turbine', 'conventional combined cycle'), overnight_category!='')) %>%
  #group_by(year, overnight_category) %>%
  #summarise(heatrate=mean(heatrate, na.rm=TRUE),
  #          heatrate.pred=mean(heatrate.pred, na.rm=TRUE)) %>%
  #ungroup() %>%
  ggplot(aes(x=year)) +
  geom_point(aes(y = heatrate)) +
  geom_line(aes(y = heatrate.pred)) +
  facet_wrap(~overnight_category+fuel_general, scales='free_y') +
  labs(x='year', y='Btu/MWh', title='Heat Rate ~ Category & Fuel')

hr.model %>%
  filter(overnight_category=='conventional combined cycle', fuel_general=='oil') %>%
  ggplot(aes(x=year)) +
  geom_point(aes(y = heatrate)) +
  geom_line(aes(y = heatrate.pred)) +
  #facet_wrap(~overnight_category+fuel_general) +
  labs(x='year', y='Btu/MWh', title='Heat Rate ~ Category & Fuel')

hr.model %>%
  ggplot(aes(x=year)) +
  geom_point(aes(y = heatrate)) +
  geom_line(aes(y = heatrate.pred)) +
  facet_wrap(~overnight_category+fuel_general, scales='free_y') +
  labs(x='year', y='Btu/MWh', title='Heat Rate ~ Category & Fuel')


# generators grouped by fuel
ggplot(hr.model, aes(x=year)) +
  geom_point(aes(y = heatrate)) +
  geom_line(aes(y = heatrate.pred)) +
  facet_wrap(~fuel_general) +
  labs(x='year', y='Btu/MWh', title='Heat Rate ~ Fuel')


# marginal costs ----------------------------------------------------------

ggplot(marginalcosts, aes(x=year)) +
  geom_line(aes(y=marginal.cost, color=fuel_general)) +
  labs(x='year', y='$/MWh', title='Marginal Cost by Fuel')


# capital costs -----------------------------------------------------------

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
