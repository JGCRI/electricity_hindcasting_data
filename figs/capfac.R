library(magrittr)
library(dplyr)
library(ggplot2)
library(energy.markets)

data(capacityfactors, capacityfactors.unfilt)

# display & save stacked bar plot
plot <- function(df, ptitle) {

  p <- ggplot(df, aes(x=yr, y=capfac, group=1)) +
    ggtitle(ptitle) +
    theme(axis.text.x = element_text(angle = 45, hjust=1)) +
    ylab("CF")

  # geom_bar()
  if (grepl("fuel", ptitle)) {
    p <- p +
      geom_line(aes(color=fuel, group=fuel)) +
      geom_point(aes(color=fuel)) +
      facet_grid(fuel~.)
  } else if (grepl("overnight", ptitle)) {
    p <- p +
      geom_line(aes(color=overnight, group=overnight)) +
      geom_point(aes(color=overnight))
  }
  # save
  # fn <- paste0("figs/", ptitle, ".png")
  # print(paste0("Saving to ", fn))
  # ggsave(filename=fn, plot=p, device="png", width=11, height=8, units="in")

  return(p)
}

# aggregate by fuel/overnight, then plot
plot.agg <- function(df, ptitle, group) {
  df.grp <- df %>%
    group_by_at(vars(yr, matches(group))) %>%
    summarise(capfac=mean(capfac)) %>%
    ungroup()

  df.grp$yr <- factor(df.grp$yr, levels=seq(from=1990, to=2016, by=1))
  if (group=="fuel") {
    df.grp <- df.grp %>%
      group_by(yr, fuel) %>%
      complete(fuel, yr, fill=list(capfac=0))
  } else if (group=="overnight") {
    df.grp <- df.grp %>%
      group_by(yr, overnight) %>%
      complete(overnight, yr, fill=list(capfac=0))
  } else {
    stop("Group not recognized!")
  }



  ptitle <- paste0(ptitle, " by ", group)
  p <- plot(df.grp, ptitle)
  return(p)
}

# multiplot and share legend function
source("figs/sharedlegend.R")

# Capacity Factors Datasets -----------------------------------------------

cf <- capacityfactors %>%
  dplyr::rename(fuel = fuel.general,
                overnight = overnightcategory) %>%
  mutate(overnight = gsub("conventional ", "", overnight)) %>%
  group_by(yr, overnight, fuel) %>%
  summarise(capfac = mean(capacityfactor)) %>%
  ungroup()

cf.fuel <- plot.agg(cf, "CFL1", "fuel")

cf.unfilt <- capacityfactors.unfilt %>%
  dplyr::rename(fuel = fuel.general,
                overnight = overnightcategory) %>%
  mutate(overnight = gsub("conventional ", "", overnight)) %>%
  filter(capacityfactor > 1) %>%
  group_by(yr, overnight, fuel) %>%
  summarise(capfac = mean(capacityfactor)) %>%
  ungroup()
cf.unfilt.fuel <- plot.agg(cf.unfilt, "CFG1", "fuel")


# PLOTS -------------------------------------------------------------------
fn <- "figs/CF by fuel.png"
print(paste0("Saving ", fn))
png(fn, width=11, height=11, units="in", res=250)
grid_arrange_shared_legend(cf.fuel, cf.unfilt.fuel,
                           position="right", ncol=2, nrow=1)
dev.off()


