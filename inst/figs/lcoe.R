library(ElectricityHindcastingData)
library(tidyr)
library(ggplot2)
library(ggridges)
data(master, mapping)
master <- master %>%
  mutate(cshare = capacity / sum(capacity),
         LCOE = overnight.lev + om.fixed.lev + om.var) %>%
  select(-overnight.lev, -om.fixed.lev, -om.var, -marginal.cost, -generation)


# industry-wide summary calculation ---------------------------------------

summary <- master %>%
  group_by(yr, overnightcategory, fuel.general) %>%
  summarise(LCOE = weighted.mean(LCOE, capacity)) %>%
  ungroup()

# grab 21 (oc, fg) combos we have a mapping for
mapping <- mapping %>%
  select(overnightcategory, fuel.general) %>%
  distinct() %>%
  mutate_all(as.character)

# force row for all possible (oc, fg, yr) combos: 130 possible
# keep only those (oc, fg) combos that appear in the mapping: 16 observed in data
summary.complete <- summary %>%
  complete(overnightcategory, fuel.general, yr) %>%
  inner_join(mapping, by=c("overnightcategory", "fuel.general"))

# make summary.complete have two time series:
## one with actual LCOE data
## the other holds mean(LCOE) wherever there is missing data
summary.complete.avg <- summary.complete %>%
  group_by(fuel.general, overnightcategory) %>%
  summarise(LCOE = mean(LCOE, na.rm=TRUE)) %>%
  ungroup()
summary.complete.na <- summary.complete %>%
  filter(is.na(LCOE)) %>%
  select(-LCOE) %>%
  left_join(summary.complete.avg, by=c("fuel.general", "overnightcategory")) %>%
  rename(missing=LCOE)
summary.complete <- summary.complete %>%
  left_join(summary.complete.na, by=c("yr", "fuel.general", "overnightcategory"))

# n=16 for all years indicates all oc-fg combos appear in each yr
summary.yr <- summary.complete %>%
  group_by(yr) %>%
  summarise(n=n()) %>%
  ungroup()


# industry-wide plot ------------------------------------------------------

ggplot(summary.complete, aes(x=yr)) +
  geom_line(aes(y = LCOE)) +
  geom_point(aes(y = missing)) +
  facet_wrap(~fuel.general + overnightcategory, scales="free") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ylab("2010$/MWh") +
  ggtitle("average levelized cost of energy")
ggsave("inst/figs/lcoe.avg.png", device="png",
       width = 8.5, height = 8.5, units = "in")

# histograms --------------------------------------------------------------

ggplot(master, aes(x=LCOE)) + xlab("2010$/MWh") +
  geom_histogram(aes(fill = ..count..)) +
  facet_wrap(~fuel.general+overnightcategory, scales="free") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave("inst/figs/lcoe-hists/industry.png", device="png",
       width=8.5, height=8.5, units="in")

ggplot(master, aes(x=log10(LCOE))) + xlab("log10(2010$/MWh)") +
  geom_histogram(aes(fill = ..count..)) +
  facet_wrap(~fuel.general+overnightcategory, scales="free")
ggsave("inst/figs/lcoe-hists/industry(logx).png",device="png",
       width=8.5, height=8.5, units="in")
