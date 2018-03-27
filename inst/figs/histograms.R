library(ElectricityHindcastingData)
library(tidyr)
library(ggplot2)
library(ggridges)
library(magrittr)
library(dplyr)
library(rlang)
data(master)



# capacity ----------------------------------------------------------------

ggplot(master, aes(x=capacity)) + xlab("MW") +
  geom_histogram(aes(fill = ..count..)) +
  facet_wrap(~fuel.general + overnightcategory, scales="free")
ggsave("inst/figs/capacity-hists/industry.png", device="png",
       width=8.5, height=8.5, units="in")

ggplot(master, aes(x=log10(capacity))) + xlab("log10(MW)") +
  geom_histogram(aes(fill = ..count..)) +
  facet_wrap(~fuel.general + overnightcategory, scales="free")
ggsave("inst/figs/capacity-hists/industry(logx).png",device="png",
       width=8.5, height=8.5, units="in")

fuels <- unique(master$fuel.general)
lapply(fuels, function(fuel) { # 1 fuel, 1 plot: faceted by year
  print(paste0("Plot: ", fuel))

  df <- filter(master, fuel.general == fuel)

  p <- ggplot(df, aes(x=nameplate)) + xlab("MW") +
    geom_histogram(aes(fill = ..count..)) +
    facet_grid(yr~., scales="fixed") +
    scale_y_continuous(labels = scales::scientific) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    ggtitle(fuel)
  fn <- paste0("inst/figs/capacity-hists/by year/", fuel, ".png")
  ggsave(fn, plot=p, device="png", width=8.5, height=17, units="in")

  p.log <- ggplot(df, aes(x=log10(nameplate))) + xlab("log10(MW)") +
    geom_histogram(aes(fill = ..count..)) +
    facet_grid(yr~., scales="fixed") +
    scale_y_continuous(labels = scales::scientific) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    ggtitle(fuel)
  fn <- paste0("inst/figs/capacity-hists/by year (logx)/", fuel, ".png")
  ggsave(fn, plot=p.log, device="png", width=8.5, height=17, units="in")

})




# lcoe --------------------------------------------------------------------

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
