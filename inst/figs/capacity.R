library(energy.markets)
library(dplyr)
library(magrittr)
library(ggplot2)
library(tidyr)

# Functions ---------------------------------------------------------------
elec_tech_colors <- c( "coal" = "#a0237c",
                       "b Coal w/CCS" = "#dab4c7",
                       "gas" = "#25a9e0",
                       "d Gas w/CCS" = "#84e7f9",
                       "petroleum" = "#d01c2a",
                       "f Oil w/CCS" = "#f7988f",
                       "biomass" = "#00931d",
                       "h Biomass w/CCS" = "#88c892",
                       "nuclear" = "#ef8e27",
                       "geothermal" = "#ad440c",
                       "water" = "#fdfa28",
                       "wind" = "#3d86f9",
                       "solar" = "#fdd67b",
                       "MSW" = "#507fab", # n CHP
                       "other" = "#92a75d", # o Battery
                       "energy reduction" = "grey")
# display & save stacked bar plot
plot <- function(df, ptitle) {

  p <- ggplot(df, aes(x=yr, y=nameplate)) +
    ggtitle(ptitle) +
    theme(axis.text.x = element_text(angle = 45, hjust=1)) +
    ylab("MW") +
    scale_y_continuous(labels = scales::scientific) +
    scale_fill_manual(values=elec_tech_colors)


  # geom_bar()
  if (grepl("fuel", ptitle)) {
    p <- p +
      geom_bar(aes(fill=fuel), stat="identity")
  } else if (grepl("overnight", ptitle)) {
    p <- p +
      geom_bar(aes(fill=overnight), stat="identity")
  }

  return(p)
}

# aggregate by fuel/overnight, then plot
plot.agg <- function(df, ptitle, group) {
  df.grp <- df %>%
    group_by_at(vars(yr, matches(group))) %>%
    summarise(nameplate=sum(nameplate)) %>%
    ungroup()

  df.grp$yr <- factor(df.grp$yr, levels=seq(from=1990, to=2016, by=1))
  if (group=="fuel") {
    df.grp <- df.grp %>%
      group_by(yr, fuel) %>%
      complete(fuel, yr, fill=list(nameplate=0))
  } else if (group=="overnight") {
    df.grp <- df.grp %>%
      group_by(yr, overnight) %>%
      complete(overnight, yr, fill=list(nameplate=0))
  } else {
    stop("Group not recognized!")
  }

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



# Original CAP Dataset --------------------------------------------------------
data(capacity.unmapped, mapping)

# full fleet
cap <- capacity.unmapped %>%
  left_join(mapping, by=c("primemover", "fuel")) %>%
  group_by(yr, fuel.general, overnightcategory) %>%
  summarise(nameplate=sum(nameplate)) %>%
  ungroup() %>%
  dplyr::rename(fuel = fuel.general,
                overnight = overnightcategory) %>%
  mutate(yr = as.factor(yr))

# get plots
cap.fuel <- plot.agg(cap, "CAP", "fuel")


# joined dataset ----------------------------------------------------------
data(cap.gen.joined)

# full fleet
join <- cap.gen.joined %>%
  group_by(yr, fuel.general, overnightcategory) %>%
  summarise(nameplate = sum(nameplate)) %>%
  ungroup() %>%
  dplyr::rename(fuel = fuel.general,
                overnight = overnightcategory) %>%
  mutate(yr = as.factor(yr))


# get plots
join.fuel <- plot.agg(join, "JOIN", "fuel")


# PLOTS -------------------------------------------------------------------

## SAVE FUEL PLOT
fn <- "figs/Capacity by fuel.png"
print(paste0("saving ", fn))
png(fn, width=11, height=17, units="in", res=250)
grid_arrange_shared_legend(orig.fuel, join.fuel,
                           position="right", ncol=2, nrow=1)
dev.off()

