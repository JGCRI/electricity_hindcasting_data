library(ElectricityHindcastingData)
library(tidyr)
library(ggplot2)
library(ggridges)
library(magrittr)
library(dplyr)
library(rlang)

# ORIG CAP/GEN summary time-series ----------------------------------------
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

orig.cap.summary <- capacity.unmapped %>% map(capacity) %>% summaryCalc(capacity)
summaryPlot(orig.cap.summary, capacity, "MW")
ggsave("inst/figs/ORIG.CAP.avg.png", device="png",
       width = 8.5, height = 8.5, units = "in")

orig.gen.summary <- generation.unmapped %>% map(generation) %>% summaryCalc(generation)
summaryPlot(orig.gen.summary, generation, "MWh")
ggsave("inst/figs/ORIG.GEN.avg.png", device="png",
       width = 8.5, height = 8.5, units = "in")


# JOIN.CAP.GEN summary time-series ----------------------------------------
data(cap.gen.joined)

join.cap.summary <- cap.gen.joined %>% summaryCalc(capacity)
summaryPlot(join.cap.summary, capacity, "MW")
ggsave("inst/figs/JOIN.CAP.avg.png", device="png",
       width = 8.5, height = 8.5, units = "in")

join.gen.summary <- cap.gen.joined %>% summaryCalc(generation)
summaryPlot(join.gen.summary, generation, "MWh")
ggsave("inst/figs/JOIN.GEN.avg.png", device="png",
       width = 8.5, height = 8.5, units = "in")



# total sector capacity ---------------------------------------------------
data(master)

ggplot(master, aes(x=yr, y=capacity)) + ylab("MW") + ggtitle("total capacity") +
  geom_bar(stat="identity") +
  facet_wrap(~fuel.general + overnightcategory, scales="free") +
  scale_y_continuous(labels = scales::scientific) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave("inst/figs/FINAL.CAP.total.png", device="png",
       width = 8.5, height = 8.5, units = "in")

# stacked barchart
ggplot(master, aes(x=yr, y=capacity)) + ylab("MW") + ggtitle("total capacity") +
  geom_bar(aes(fill=fuel.general), stat="identity") +
  scale_y_continuous(labels = scales::scientific) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave("inst/figs/FINAL.CAP.total.stacked.png", device="png",
       width = 8.5, height = 8.5, units = "in")





# master summary time-series ----------------------------------------------
data(master)

capacity.summary <- summaryCalc(master, capacity)
summaryPlot(capacity.summary, capacity, "MW")
ggsave("inst/figs/FINAL.CAP.avg.png", device="png",
       width = 8.5, height = 8.5, units = "in")

generation.summary <- summaryCalc(master, generation)
summaryPlot(generation.summary, generation, "MWh")
ggsave("inst/figs/FINAL.GEN.avg.png", device="png",
       width = 8.5, height = 8.5, units = "in")

lcoe.summary <- summaryCalc(master, lcoe)
summaryPlot(lcoe.summary, lcoe, "2010$/MWh")
ggsave("inst/figs/FINAL.LCOE.avg.png", device="png",
       width = 8.5, height = 8.5, units = "in")












