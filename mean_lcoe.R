library(ElectricityHindcastingData)
library(ggplot2)


# CF ----------------------------------------------------------------------
cf <- capacityfactors.data %>%
  group_by(yr, fuel.general, overnightcategory) %>%
  summarise(avg = mean(capacityfactor),
            min = min(capacityfactor),
            max = max(capacityfactor)) %>%
  ungroup() %>%
  arrange(fuel.general, overnightcategory, yr) %>%
  mutate(tech = paste0(fuel.general, "-", overnightcategory)) %>%
  select(-fuel.general, -overnightcategory) %>%
  tidyr::gather(key=param, value=CF, -yr, -tech )

ggplot(cf, aes(x=yr, y=CF, color=param)) +
  geom_line() +
  facet_wrap(~tech)

capacityfactors.data %>%
  mutate(tech = paste0(fuel.general, "-", overnightcategory)) %>%
  ggplot(., aes(x=capacityfactor)) +
  geom_histogram(binwidth=0.01) +
  facet_wrap(~tech, scales="free")


# LCOE --------------------------------------------------------------------
levelizedcosts %>%
  mutate(tech = paste0(fuel.general, "-", overnightcategory)) %>%
  ggplot(., aes(x=capacityfactor, y=LCOE_wo_Fuel)) +
  geom_point() +
  facet_wrap(~tech, scales="free")

levelizedcosts %>%
  mutate(tech = paste0(fuel.general, "-", overnightcategory)) %>%
  ggplot(., aes(x=yr, y=LCOE_wo_Fuel)) +
  geom_point() +
  facet_wrap(~tech, scales="free")






# final_plant_level.xlsx --------------------------------------------------
final_plant_lcoe <- levelizedcosts %>%
  group_by(startyr, plntcode, fuel.general, overnightcategory) %>%
  summarise(lcoe = mean(LCOE)) %>%
  ungroup() %>%
  filter( startyr %in% 1995:2015)


# fuel-tech market shares -------------------------------------------------

market <- cap.gen.joined %>%
  group_by(yr) %>%
  summarise(cap.total=sum(capacity)) %>%
  ungroup()

mshares <- cap.gen.joined %>%
  group_by(yr, fuel.general, overnightcategory ) %>%
  summarise(capacity = sum(capacity)) %>%
  ungroup() %>%
  right_join(market, by = "yr") %>%
  mutate(mshare = capacity/cap.total) %>%
  select(-matches("cap"))

mshares %>% tidyr::spread(key=yr, value=mshare) %>% View()

# weighted industry-level LCOE --------------------------------------------

# by plant-level capacity shares of total fuel-tech capacity

lcoe_avg <- levelizedcosts %>%
  group_by(yr, fuel.general, overnightcategory) %>%
  summarise(LCOE_avg = mean(LCOE)) %>%
  ungroup()












