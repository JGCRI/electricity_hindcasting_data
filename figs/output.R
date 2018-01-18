library(dplyr)
library(magrittr)
library(ggplot2)

data(generation, generators)

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

# Generation Data ---------------------------------------------------------

output <- generation %>%
  dplyr::rename(fuel = fuel.general,
                overnight = overnightcategory) %>%
  mutate(overnight = gsub("conventional ", "", overnight)) %>%
  group_by(yr, overnight, fuel) %>%
  summarise(generation=sum(generation)) %>%
  ungroup() %>%
  mutate(yr=as.factor(yr))

output.fuel <- plot.agg(output, "Output", "fuel")
output.oc <- plot.agg(output, "Output", "overnight")



# Potential Generation Data -----------------------------------------------
potential <- generators %>%
  dplyr::rename(fuel=fuel.general,
                overnight=overnightcategory) %>%
  mutate(overnight = gsub("conventional ", "", overnight)) %>%
  group_by(yr, overnight, fuel) %>%
  summarise(nameplate=sum(nameplate)) %>%
  ungroup() %>%
  mutate(generation = 8760*nameplate,
         yr=as.factor(yr))

potential.fuel <- plot.agg(potential, "Potential Output", "fuel")
potential.oc <- plot.agg(potential, "Potential Output", "overnight")

# PLOT --------------------------------------------------------------------

# png("figs/Output.png", width=11, height=5, units="in", res=250)
# multiplot(output.fuel, output.oc, cols=2)
# dev.off()

png("figs/Output.png", width=11, height=8.5, units="in", res=250)
grid_arrange_shared_legend(output.fuel, potential.fuel,
                           position="right", ncol=2, nrow=1)
dev.off()
