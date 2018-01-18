library(magrittr)
library(dplyr)
library(ggplot2)
library(energy.markets)

data(capacityfactors)

# display & save stacked bar plot
plot <- function(df, ptitle) {

  p <- ggplot(df, aes(x=yr, y=capfac, group=1)) +
    ggtitle(ptitle) +
    theme(axis.text.x = element_text(angle = 45, hjust=1)) +
    ylab("CF")

  # geom_bar()
  if (grepl("fuel", ptitle)) {
    p <- p +
      geom_line(aes(color=fuel.general, group=fuel.general)) +
      geom_point(aes(color=fuel.general))
  } else if (grepl("overnight", ptitle)) {
    p <- p +
      geom_line(aes(color=overnightcategory))
  }
  # save
  fn <- paste0("figs/", ptitle, ".png")
  print(paste0("Saving to ", fn))
  ggsave(filename=fn, plot=p, device="png", width=11, height=8, units="in")

  p
}

# aggregate by fuel/overnight, then plot
plot.agg <- function(df, ptitle, group) {
  df.grp <- df %>%
    group_by_at(vars(yr, matches(group))) %>%
    summarise(capfac=mean(capfac)) %>%
    ungroup()
  ptitle <- paste0(ptitle, " by ", group)
  plot(df.grp, ptitle)
}

cf <- capacityfactors %>%
  group_by(yr, overnightcategory, fuel.general) %>%
  summarise(capfac = mean(capacityfactor)) %>%
  ungroup() %>%
  mutate(yr=as.factor(yr))
plot.agg(cf, "CF", "fuel")

