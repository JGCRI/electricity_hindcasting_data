library(ElectricityHindcastingData)
library(tidyr)
library(ggplot2)
library(ggridges)
library(magrittr)
library(dplyr)
library(rlang)
data(master, mapping)

# industry-wide summary ---------------------------------------------------

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

# total industry capacity over the years ----------------------------------
ggplot(master, aes(x=yr, y=nameplate)) + ylab("MW") + ggtitle("total capacity") +
  geom_bar(stat="identity") +
  facet_wrap(~fuel.general + overnightcategory, scales="free") +
  scale_y_continuous(labels = scales::scientific) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave("inst/figs/capacity.png", device="png",
       width = 8.5, height = 8.5, units = "in")

# stacked barchart
ggplot(master, aes(x=yr, y=nameplate)) + ylab("MW") + ggtitle("total capacity") +
  geom_bar(aes(fill=fuel.general), stat="identity") +
  scale_y_continuous(labels = scales::scientific) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave("inst/figs/capacity.stacked.png", device="png",
       width = 8.5, height = 8.5, units = "in")



# histograms over the years (by fuel) -------------------------------------


ggplot(master, aes(x=nameplate)) + xlab("MW") +
  geom_histogram(aes(fill = ..count..)) +
  facet_wrap(~fuel.general + overnightcategory, scales="free")
ggsave("inst/figs/capacity-hists/industry.png", device="png",
       width=8.5, height=8.5, units="in")

ggplot(master, aes(x=log10(nameplate))) + xlab("log10(MW)") +
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
