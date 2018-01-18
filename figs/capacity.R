library(dplyr)
library(magrittr)
library(ggplot2)

data(generators, generators.cfl1)

# display & save stacked bar plot
plot <- function(df, ptitle) {

  p <- ggplot(df, aes(x=yr, y=nameplate)) +
    ggtitle(ptitle) +
    theme(axis.text.x = element_text(angle = 45, hjust=1)) +
    ylab("MW")

  # geom_bar()
  if (grepl("fuel", ptitle)) {
    p <- p +
      geom_bar(aes(fill=fuel.general), stat="identity")
  } else if (grepl("overnight", ptitle)) {
    p <- p +
      geom_bar(aes(fill=overnightcategory), stat="identity")
  }

  # save
  fn <- paste0("figs/", ptitle, ".png")
  ggsave(filename=fn, plot=p, device="png", width=11, height=8, units="in")

  p
}

# aggregate by fuel/overnight, then plot
plot.agg <- function(df, ptitle, group) {
  df.grp <- df %>%
    group_by_at(vars(yr, matches(group))) %>%
    summarise(nameplate=sum(nameplate)) %>%
    ungroup()
  ptitle <- paste0(ptitle, " by ", group)
  plot(df.grp, ptitle)
}



# CFL1 Dataset ------------------------------------------------------------

# new additions
cfl1.new <- generators.cfl1 %>%
  filter(yr == vintage) %>%
  mutate(yr = as.factor(yr))
plot.agg(cfl1.new, "CFL1 Additions",  "fuel")
plot.agg(cfl1.new, "CFL1 Additions", "overnight")

# full fleet
cfl1 <- generators.cfl1 %>%
  mutate(yr = as.factor(yr))
plot.agg(cfl1, "CFL1 Fleet", "fuel")
plot.agg(cfl1, "CFL1 Fleet", "overnight")



# Original Dataset --------------------------------------------------------
# new additions
orig.new <- generators %>%
  filter(yr == vintage) %>%
  mutate(yr = as.factor(yr))
plot.agg(orig.new, "ORIG Additions",  "fuel")
plot.agg(orig.new, "ORIG Additions", "overnight")

# full fleet
orig <- generators %>%
  mutate(yr = as.factor(yr))
plot.agg(orig, "ORIG Fleet", "fuel")
plot.agg(orig, "ORIG Fleet", "overnight")


