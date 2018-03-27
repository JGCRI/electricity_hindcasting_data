library(ElectricityHindcastingData)
library(tidyr)
library(ggplot2)
library(ggridges)
library(magrittr)
library(dplyr)
library(rlang)
data(master)



# spread by fuel (1996-2016) ----------------------------------------------

# capacity
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

# lcoe
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


# spread by year ----------------------------------------------------------
spread_by_year <- function(fuel, df) { # 1 fuel, 1 plot: faceted by year
  df <- df %>%
    filter(fuel.general==fuel) %>%
    mutate(yr = as.factor(yr))
  print(paste0("Plot: ", fuel))

  p <- ggplot(df, aes(x=capacity)) + xlab("MW") +
    geom_histogram(aes(fill = ..count..)) +
    facet_grid(yr~., scales="fixed") +
    scale_y_continuous(labels = scales::scientific) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    ggtitle(fuel)
  fn <- paste0("inst/figs/capacity-hists/by year/", fuel, ".png")
  ggsave(fn, plot=p, device="png", width=8.5, height=17, units="in")

  p.log <- ggplot(df, aes(x=log10(capacity))) + xlab("log10(MW)") +
    geom_histogram(aes(fill = ..count..)) +
    facet_grid(yr~., scales="fixed") +
    scale_y_continuous(labels = scales::scientific) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    ggtitle(fuel)
  fn <- paste0("inst/figs/capacity-hists/by year (logx)/", fuel, " (logx).png")
  ggsave(fn, plot=p.log, device="png", width=8.5, height=17, units="in")

}
fuels <- unique(master$fuel.general)
lapply(fuels, spread_by_year, master)
