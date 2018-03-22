library(ElectricityHindcastingData)
library(tidyr)
library(ggplot2)
library(ggridges)
library(magrittr)
library(dplyr)
library(rlang)
data(master, mapping)


# average plant capacity --------------------------------------------------
capacity.summary <- summaryCalc(master, capacity)

# barchart (w/ points on period average for missing data)
ggplot(capacity.summary, aes(x=yr)) + ylab("MW") + ggtitle("average plant capacity") +
  geom_bar(aes(y = capacity), stat="identity") +
  geom_point(aes(y = missing)) +
  facet_wrap(~fuel.general + overnightcategory, scales="free") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave("inst/figs/capacity.avg.png", device="png",
       width = 8.5, height = 8.5, units = "in")

# stacked barchart
ggplot(capacity.summary, aes(x=yr, y=capacity)) + ylab("MW") + ggtitle("average capacity") +
  geom_bar(aes(fill=fuel.general), stat="identity") +
  scale_y_continuous(labels = scales::scientific) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave("inst/figs/capacity.avg.stacked.png", device="png",
       width = 8.5, height = 8.5, units = "in")

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


# average plant lcoe  -----------------------------------------------------
lcoe.summary <- summaryCalc(master, lcoe)

ggplot(lcoe.summary, aes(x=yr)) +
  geom_line(aes(y = lcoe)) +
  geom_point(aes(y = missing)) +
  facet_wrap(~fuel.general + overnightcategory, scales="free") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ylab("2010$/MWh") +
  ggtitle("average levelized cost of energy")
ggsave("inst/figs/lcoe.avg.png", device="png",
       width = 8.5, height = 8.5, units = "in")
