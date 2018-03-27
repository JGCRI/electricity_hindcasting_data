library(ElectricityHindcastingData)
library(tidyr)
library(ggplot2)
library(ggridges)
library(magrittr)
library(dplyr)
library(rlang)
data(master)


# spread by fuel (1996-2016) ----------------------------------------------

ggplot(master, aes(x=capacity, y=fuel.general, fill=fuel.general)) + xlab("MW") +
  geom_density_ridges() +
  theme_ridges()
ggsave("inst/figs/capacity-ridges/industry.png", device="png")

ggplot(master, aes(x=log10(capacity), y=fuel.general, fill=fuel.general)) + xlab("log10(MW)") +
  geom_density_ridges() +
  theme_ridges()
ggsave("inst/figs/capacity-ridges/industry(logx).png", device="png")

# spread by year ----------------------------------------------------------
spread_by_year <- function(fuel, df) { # 1 fuel, 1 plot: faceted by year
  df <- df %>%
    filter(fuel.general==fuel) %>%
    mutate(yr = as.factor(yr))
  print(paste0("Plot: ", fuel))

  p <- ggplot(df, aes(x=capacity, y=yr)) + xlab("MW") +
    geom_density_ridges() +
    theme_ridges() +
    ggtitle(fuel)
  fn <- paste0("inst/figs/capacity-ridges/by year/", fuel, ".png")
  ggsave(fn, plot=p, device="png", width=8.5, height=17, units="in")

  p.log <- ggplot(df, aes(x=log10(capacity), y=yr)) + xlab("log10(MW)") +
    geom_density_ridges() +
    theme_ridges() +
    ggtitle(fuel)
  fn <- paste0("inst/figs/capacity-ridges/by year (logx)/", fuel, " (logx).png")
  ggsave(fn, plot=p.log, device="png", width=8.5, height=17, units="in")
}
fuels <- unique(master$fuel.general)
lapply(fuels, spread_by_year, master)
