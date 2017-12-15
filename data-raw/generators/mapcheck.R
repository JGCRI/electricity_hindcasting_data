aeo <- read_excel("data-raw/costs/AEO/test.xlsx", "combined") %>%
  mutate(yr.available = as.numeric(yr.available),
         size = as.numeric(size),
         leadtime = as.numeric(leadtime),
         overnight.base = as.numeric(overnight.base),
         variable.o.m = as.numeric(variable.o.m),
         fixed.o.m = as.numeric(fixed.o.m),
         heatrate.1 = as.numeric(heatrate.1),
         heatrate.n = as.numeric(heatrate.n))
summary.tech <- aeo %>%
  group_by(Technology) %>%
  summarise(strt.aeo=min(yr), end.aeo=max(yr),
            size.avg = mean(size, na.rm=T),
            overnight.avg = mean(overnight.base, na.rm=T)) %>%
  ungroup()

summary.oc <- generators %>%
  left_join(mapping, by=c("primemover", "fuel")) %>%
  group_by(overnightcategory) %>%
  summarise(strt.860=min(yr), end.860=max(yr), n=n()/nrow(generators),
            size.avg = mean(nameplate, na.rm=T)) %>%
  ungroup()

tech.oc.map <- read_excel("data-raw/costs/AEO/test.xlsx", "categories")

combine <- summary.tech %>%
  inner_join(tech.oc.map, by="Technology") %>%
  right_join(summary.oc, by="overnightcategory") %>%
  #select(overnightcategory, starts_with("size.avg")) %>%
  dplyr::rename(aeo.pred = size.avg.x,
                eia.form = size.avg.y)
