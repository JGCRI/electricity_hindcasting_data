library(dplyr)
library(magrittr)
library(ggplot2)

data(generators, generators.cfl1)

# display & save stacked bar plot
plot <- function(df, ptitle) {

  p <- ggplot(df, aes(x=yr, y=nameplate)) +
    ggtitle(ptitle) +
    theme(axis.text.x = element_text(angle = 45, hjust=1)) +
    ylab("MW") +
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
    summarise(nameplate=sum(nameplate)) %>%
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


# CFL1 Dataset ------------------------------------------------------------

# new additions
cfl1.new <- generators.cfl1 %>%
  dplyr::rename(fuel = fuel.general,
                overnight = overnightcategory) %>%
  mutate(overnight = gsub("conventional ", "", overnight)) %>%
  filter(yr == vintage) %>%
  mutate(yr = as.factor(yr))

cfl1.new.fuel <- plot.agg(cfl1.new, "CFL1 Additions",  "fuel")
cfl1.new.oc <- plot.agg(cfl1.new, "CFL1 Additions", "overnight")

# full fleet
cfl1 <- generators.cfl1 %>%
  dplyr::rename(fuel = fuel.general,
                overnight = overnightcategory) %>%
  mutate(overnight = gsub("conventional ", "", overnight)) %>%
  mutate(yr = as.factor(yr))

cfl1.fuel <- plot.agg(cfl1, "CFL1 Fleet", "fuel")
cfl1.oc <- plot.agg(cfl1, "CFL1 Fleet", "overnight")

# Original Dataset --------------------------------------------------------
# new additions
orig.new <- generators %>%
  dplyr::rename(fuel = fuel.general,
                overnight = overnightcategory) %>%
  mutate(overnight = gsub("conventional ", "", overnight)) %>%
  mutate(yr = as.factor(yr)) %>%
  filter(yr == vintage)

orig.new.fuel <- plot.agg(orig.new, "ORIG Additions",  "fuel")
orig.new.oc <- plot.agg(orig.new, "ORIG Additions", "overnight")

# full fleet
orig <- generators %>%
  dplyr::rename(fuel = fuel.general,
                overnight = overnightcategory) %>%
  mutate(overnight = gsub("conventional ", "", overnight)) %>%
  mutate(yr = as.factor(yr))

orig.fuel <- plot.agg(orig, "ORIG Fleet", "fuel")
orig.oc <- plot.agg(orig, "ORIG Fleet", "overnight")


# PLOTS -------------------------------------------------------------------

## SAVE CFL1 PLOT
png("figs/CFL1.png", width=11, height=8.5, units="in", res=250)
multiplot(cfl1.new.fuel,cfl1.fuel, cfl1.new.oc,cfl1.oc, cols=2)
dev.off()

## SAVE ORIG PLOT
png("figs/ORIG.png", width=11, height=8.5, units="in", res=250)
multiplot(orig.new.fuel, orig.fuel, orig.new.oc, orig.oc, cols=2)
dev.off()

## SAVE FUEL PLOT
png("figs/Capacity by fuel.png", width=11, height=8.5, units="in", res=250)
grid_arrange_shared_legend(cfl1.new.fuel, cfl1.fuel, orig.new.fuel, orig.fuel,
                           position="right", ncol=2, nrow=2)
dev.off()

## SAVE OVERNIGHT PLOT
png("figs/Capacity by overnight.png", width=11, height=8.5, units="in", res=250)
grid_arrange_shared_legend(cfl1.new.oc, cfl1.oc, orig.new.oc, orig.oc,
                           position="right", ncol=2, nrow=2)
dev.off()



# XOR (Original, CFL1) ----------------------------------------------------
data(capacityfactors)
xor <- generators %>%
  anti_join(capacityfactors, by=c("yr", "utilcode", "plntcode", "overnightcategory", "fuel.general")) %>%
  dplyr::rename(fuel = fuel.general,
                overnight = overnightcategory) %>%
  mutate(overnight = gsub("conventional ", "", overnight)) %>%
  mutate(yr = as.factor(yr))
xor.fuel <- plot.agg(xor, "XOR Fleet", "fuel")

png("figs/xor by fuel.png", width=11, height=8.5, units="in", res=250)
print(xor.fuel)
dev.off()

