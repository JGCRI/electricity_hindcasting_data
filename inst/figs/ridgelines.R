library(ElectricityHindcastingData)
library(tidyr)
library(ggplot2)
library(ggridges)
library(magrittr)
library(dplyr)
library(rlang)
data(master, mapping)


# ridgeline plots (faceted by year) (by fuel) -----------------------------

ggplot(master, aes(x=nameplate, y=fuel.general, fill=fuel.general)) + xlab("MW") +
  geom_density_ridges() +
  theme_ridges()
ggsave("inst/figs/capacity-ridges/industry.png", device="png")

ggplot(master, aes(x=log10(nameplate), y=fuel.general, fill=fuel.general)) + xlab("log10(MW)") +
  geom_density_ridges() +
  theme_ridges()
ggsave("inst/figs/capacity-ridges/industry(logx).png", device="png")

lapply(fuels, function(fuel) { # 1 fuel, 1 plot: faceted by year
  df <- master %>%
    filter(fuel.general==fuel) %>%
    mutate(yr = as.factor(yr))

  print(paste0("Plot: ", fuel))

  p <- ggplot(df, aes(x=nameplate, y=yr)) + xlab("MW") +
    geom_density_ridges() +
    theme_ridges() +
    ggtitle(fuel)
  fn <- paste0("inst/figs/capacity-ridges/by year/", fuel, ".png")
  ggsave(fn, plot=p, device="png")

  p.log <- ggplot(df, aes(x=log10(nameplate), y=yr)) + xlab("log10(MW)") +
    geom_density_ridges() +
    theme_ridges() +
    ggtitle(fuel)
  fn <- paste0("inst/figs/capacity-ridges/by year (logx)/", fuel, ".png")
  ggsave(fn, plot=p, device="png")
})
