aeo <- read.csv("data-raw/costs/gcam/AEO-raw.csv") # assumptions.final.csv mapped to oc's
nameplate <- read.csv("data-raw/costs/gcam/nameplate.csv") %>% # nameplate values
  mutate_at(vars(ends_with("code")), as.character) %>%
  mutate_if(is.factor, as.character)
heatrates <- read.csv("data-raw/costs/fuel/avghr.csv") %>%
  mutate(oc = gsub("_", " ", oc)) %>%
  mutate(oc = ifelse(oc == "combined cycle", "conventional combined cycle", oc)) %>%
  mutate(oc = ifelse(oc == "combustion turbine", "conventional combustion turbine", oc)) %>%
  mutate(fuel = gsub("_", " ", fuel)) %>%
  rename(overnightcategory = oc,
         fuel.general = fuel,
         yr = t,
         hr = Val)# 2010 values of capitalcosts (input to levelization)

fullinput <- aeo %>% # nominal costs
  full_join(capitalcosts, by=c("yr", "overnightcategory")) %>% # 2010-adjusted values
  inner_join(capacityfactors, by=c("yr", "overnightcategory")) %>% # full input to levelization calc
  full_join(marginalcosts, by=c("yr", "overnightcategory", "fuel.general")) %>%  # heatrates and fuel prices
  full_join(gdpdeflator, by=c("cost.yr")) %>%  # deflator to convert nominal values
  left_join(nameplate, by=c("yr", "utilcode", "plntcode", "overnightcategory", "fuel.general")) %>%
  left_join(heatrates, by=c("yr", "overnightcategory", "fuel.general")) %>%
  select(-heatrate)


write.csv(fullinput, "data-raw/costs/gcam/fullinput.csv", row.names=FALSE)
