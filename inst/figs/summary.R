library(ElectricityHindcastingData)
library(tidyr)
library(ggplot2)
library(ggridges)
library(magrittr)
library(dplyr)
library(rlang)
data(master, mapping)



# total sector capacity ---------------------------------------------------

ggplot(master, aes(x=yr, y=capacity)) + ylab("MW") + ggtitle("total capacity") +
  geom_bar(stat="identity") +
  facet_wrap(~fuel.general + overnightcategory, scales="free") +
  scale_y_continuous(labels = scales::scientific) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave("inst/figs/capacity.png", device="png",
       width = 8.5, height = 8.5, units = "in")

# stacked barchart
ggplot(master, aes(x=yr, y=capacity)) + ylab("MW") + ggtitle("total capacity") +
  geom_bar(aes(fill=fuel.general), stat="identity") +
  scale_y_continuous(labels = scales::scientific) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave("inst/figs/capacity.stacked.png", device="png",
       width = 8.5, height = 8.5, units = "in")




# average summary time-series ---------------------------------------------

capacity.summary <- summaryCalc(master, capacity)
summaryPlot(capacity.summary, capacity, "MW")
ggsave("inst/figs/capacity.avg.png", device="png",
       width = 8.5, height = 8.5, units = "in")

generation.summary <- summaryCalc(master, generation)
summaryPlot(generation.summary, generation, "MWh")
ggsave("inst/figs/generation.avg.png", device="png",
       width = 8.5, height = 8.5, units = "in")

lcoe.summary <- summaryCalc(master, lcoe)
summaryPlot(lcoe.summary, lcoe, "2010$/MWh")
ggsave("inst/figs/lcoe.avg.png", device="png",
       width = 8.5, height = 8.5, units = "in")



# unmapped CAP/GEN --------------------------------------------------------

data(generation.unmapped, capacity.unmapped, mapping)
map <- function(df, column) {
  column <- enquo(column)

  df <- df %>%
    left_join(mapping, by=c("primemover", "fuel")) %>% # map datasets to oc-fg
    select(-primemover, -fuel) %>% # aggregate redundant mappings (pm, f) -> (oc, fg)
    # no longer grouping by utilcode b/c two versions (.x, .y)
    group_by(yr, plntcode, overnightcategory, fuel.general) %>%
    summarise(!!quo_name(column) := sum(!!column)) %>%
    ungroup()
}

gen <- map(generation.unmapped, generation)
gen.summary <- summaryCalc(gen, generation)
summaryPlot(gen.summary, generation, "MWh")
ggsave("inst/figs/GEN.avg.png", device="png",
       width = 8.5, height = 8.5, units = "in")

cap <- capacity.unmapped %>%
  rename(capacity = nameplate) %>%
  map(., capacity)
cap.summary <- summaryCalc(cap, capacity)
summaryPlot(cap.summary, capacity, "MW")
ggsave("inst/figs/CAP.avg.png", device="png",
       width = 8.5, height = 8.5, units = "in")

# 00-01 missing data ------------------------------------------------------

master.missing <- full_join(capacity.summary, generation.summary,
                            by = c("overnightcategory", "fuel.general", "yr"))

# master dataset is only missing data for yr = 2000
master.missing %>% filter(yr %in% c(2000, 2001)) %>% View("Master (00-01")

orig.missing <- full_join(cap.summary, gen.summary,
                          by = c("overnightcategory", "fuel.general", "yr"))

orig.missing %>% filter(yr %in% c(2000, 2001)) %>% View("ORIG (00-01)")
