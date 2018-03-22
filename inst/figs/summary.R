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
