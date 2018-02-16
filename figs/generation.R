library(dplyr)
library(magrittr)
library(ggplot2)

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

# Generation Data ---------------------------------------------------------
data(generation)

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


# Unmapped Generation Data ------------------------------------------------
data(generation.unmapped)

output.unmapped <- generation.unmapped %>% 
  group_by(yr, primemover, fuel) %>% 
  summarise(generation=sum(generation)) %>% 
  ungroup() %>% 
  mutate(yr=as.factor(yr)) 
  
output.unmapped.fuel <- plot.agg(output.unmapped, "Unmapped Output", "fuel")

# ORIG Potential Generation Data -------------------------------------------
data(capacity)
potential.orig <- capacity %>%
  dplyr::rename(fuel=fuel.general,
                overnight=overnightcategory) %>%
  mutate(overnight = gsub("conventional ", "", overnight)) %>%
  group_by(yr, overnight, fuel) %>%
  summarise(nameplate=sum(nameplate)) %>%
  ungroup() %>%
  mutate(generation = 8760*nameplate,
         yr=as.factor(yr))

potential.orig.fuel <- plot.agg(potential.orig, "ORIG Potential Output", "fuel")
potential.orig.oc <- plot.agg(potential.orig, "ORIG Potential Output", "overnight")

# merged output data ------------------------------------------------------
data(mapping)
merged <- read.delim("C:/Users/guti220/Desktop/merged.tsv") %>% 
  left_join(mapping, by=c("primemover", "fuel")) %>% 
  select(-primemover, -fuel) %>%
  dplyr::rename(fuel = fuel.general,
                overnight = overnightcategory,
                vintage = startyr) %>%
  mutate(overnight = gsub("conventional ", "", overnight)) %>%
  mutate(yr = as.factor(yr)) %>%
  filter(!is.na(generation)) %>%
  group_by(yr, utilcode, plntcode, overnight, fuel) %>%
  summarise(generation = sum(generation)) %>%
  ungroup()

merged.fuel <- plot.agg(merged, "MERGED", "fuel")
# PLOT --------------------------------------------------------------------

# png("figs/Output.png", width=11, height=5, units="in", res=250)
# multiplot(output.fuel, output.oc, cols=2)
# dev.off()

fn <- "figs/Output by fuel.png"
print(paste0("Saving ", fn))
png(fn, width=8.5, height=11, units="in", res=250)
grid_arrange_shared_legend(potential.orig.fuel, output.fuel, merged.fuel,
                           position="right", ncol=1, nrow=3)
dev.off()
