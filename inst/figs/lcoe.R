library(ElectricityHindcastingData)
library(tidyr)
library(ggplot2)
library(ggridges)
data(master, mapping)

# industry-wide -----------------------------------------------------------
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

# histograms --------------------------------------------------------------

ggplot(master, aes(x=lcoe)) + xlab("2010$/MWh") +
  geom_histogram(aes(fill = ..count..)) +
  facet_wrap(~fuel.general+overnightcategory, scales="free") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave("inst/figs/lcoe-hists/industry.png", device="png",
       width=8.5, height=8.5, units="in")

ggplot(master, aes(x=log10(lcoe))) + xlab("log10(2010$/MWh)") +
  geom_histogram(aes(fill = ..count..)) +
  facet_wrap(~fuel.general+overnightcategory, scales="free")
ggsave("inst/figs/lcoe-hists/industry(logx).png",device="png",
       width=8.5, height=8.5, units="in")
