library(dplyr)
library(magrittr)
library(ggplot2)
library(tidyr)

# functions ---------------------------------------------------------------

# display & save stacked bar plot
plot <- function(df, ptitle) {

  p <- ggplot(df, aes(x=yr, y=generation)) +
    ggtitle(ptitle) +
    theme(axis.text.x = element_text(angle = 45, hjust=1)) +
    ylab("MWh") +
    scale_y_continuous(labels = scales::scientific)

  # geom_bar()
  if (grepl("fuel", ptitle)) {
    p <- p +
      geom_bar(aes(fill=fuel), stat="identity")
  } else if (grepl("overnight", ptitle)) {
    p <- p +
      geom_bar(aes(fill=overnight), stat="identity")
  }

  # save
  # fn <- paste0("figs/", ptitle, ".png")
  # ggsave(filename=fn, plot=p, device="png", width=11, height=8, units="in")

  return(p)
}

# aggregate by fuel/overnight, then plot
plot.agg <- function(df, ptitle, group) {
  df.grp <- df %>%
    group_by_at(vars(yr, matches(group))) %>%
    summarise(generation=sum(generation)) %>%
    ungroup()
  ptitle <- paste0(ptitle, " by ", group)
  p <- plot(df.grp, ptitle)
  return(p)
}

# multiplot w/ one legend function
source("figs/sharedlegend.R")
# 1 2
# 3 4

# multiplot w/ multiple legends
source("figs/multiplot.R")
# 1 3
# 2 4


# Original GEN Dataset ----------------------------------------------------

data(generation.unmapped, mapping)

# full fleet
gen <- generation.unmapped %>%
  left_join(mapping, by=c("primemover", "fuel")) %>%
  group_by(yr, fuel.general, overnightcategory) %>%
  summarise(generation=sum(generation)) %>%
  ungroup() %>%
  dplyr::rename(fuel = fuel.general,
                overnight = overnightcategory) %>%
  mutate(yr = as.factor(yr))

# get plots
gen.fuel <- plot.agg(gen, "GEN", "fuel")


# CAP Potential Generation Data -------------------------------------------
data(capacity.unmapped, mapping)

# full fleet
potential.cap <- capacity.unmapped %>%
  left_join(mapping, by=c("primemover", "fuel")) %>%
  group_by(yr, fuel.general, overnightcategory) %>%
  summarise(nameplate=sum(nameplate)) %>%
  ungroup() %>%
  dplyr::rename(fuel = fuel.general,
                overnight = overnightcategory) %>%
  mutate(yr = as.factor(yr),
         generation = 8760*nameplate) %>%
  select(-nameplate)

potential.cap.fuel <- plot.agg(potential.cap, "CAP Potential Output", "fuel")


# joined dataset ----------------------------------------------------------
data(cap.gen.joined)

# full fleet
join <- cap.gen.joined %>%
  group_by(yr, fuel.general, overnightcategory) %>%
  summarise(generation = sum(generation)) %>%
  ungroup() %>%
  dplyr::rename(fuel = fuel.general,
                overnight = overnightcategory) %>%
  mutate(yr = as.factor(yr))


# get plots
join.fuel <- plot.agg(join, "JOIN", "fuel")

# PLOT --------------------------------------------------------------------

fn <- "figs/Output by fuel.png"
print(paste0("Saving ", fn))
png(fn, width=8.5, height=11, units="in", res=250)
grid_arrange_shared_legend(potential.cap.fuel, gen.fuel, join.fuel,
                           position="right", ncol=1, nrow=3)
dev.off()

