library(energy.markets)
library(dplyr)
library(magrittr)

data(modelinput, generators.new, mapping)

mi <- modelinput %>%
  mutate(choice = paste(fuel.general, overnightcategory, sep="-")) %>%
  select(-fuel.general, -overnightcategory) %>%
  group_by(yr, choice) %>%
  summarise(cost = mean(overnight.lev + om.fixed.lev + om.var + marginal.cost)) %>%
  ungroup()


# individual plot
plot.all <- function(df, fold) {
  p <- ggplot(df, aes(x=yr, y=nameplate)) +
    geom_line(aes(col=choice))
  fn <- paste0(fold, "/all.png")
  ggsave(filename=fn, plot=p, device="png", width=11, height=8, units="in")
}

# break out by fuel, facetign by overnight category
plot.ind <- function(df, fold) {
  lapply(unique(df$fuel.general), function(fg, df, fold) {
    df2 <- df %>%
      filter(fuel.general==fg)
    p <- ggplot(df2, aes(x=yr, y=nameplate)) +
      geom_line() +
      facet_grid(fuel.general~overnightcategory) +
      ylab("MW")
    fn <- paste0(fold, "/", fg, ".png")
    ggsave(filename=fn, plot=p, device="png", width=11, height=8, units="in")
  }, gn, fold)
}

folder <- "figs/capacityaddns/"
gn <- generators.new %>%
  left_join(mapping, by=c("primemover", "fuel")) %>%
  select(-primemover, -fuel) %>%
  filter(fuel.general != "" & overnightcategory != "") %>%
  group_by(yr) %>%
  #group_by(yr, fuel.general, overnightcategory) %>%
  summarise(nameplate=sum(nameplate)) %>%
  ungroup()
  #mutate(choice=paste(fuel.general, overnightcategory, sep="-"))
plot.all(gn, folder)
plot.ind(gn, folder)


folder <- "figs/capacity/"
g <- generators %>%
  left_join(mapping, by=c("primemover", "fuel")) %>%
  select(-primemover, -fuel) %>%
  filter(fuel.general != "" & overnightcategory != "") %>%
  group_by(yr) %>%
  #group_by(yr, fuel.general, overnightcategory) %>%
  summarise(nameplate=sum(nameplate)) %>%
  ungroup()
  #mutate(choice=paste(fuel.general, overnightcategory, sep="-"))
plot.all(g, folder)
plot.ind(g, folder)


