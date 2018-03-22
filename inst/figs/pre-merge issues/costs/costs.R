library(energy.markets)
library(dplyr)
library(magrittr)
library(ggplot2)
library(tidyr)
# multiplot w/ one legend function
source("figs/sharedlegend.R")
# 1 2
# 3 4



# base capital costs ------------------------------------------------------
data(capitalcosts)
cptlcsts.melt <- capitalcosts %>%
  select(yr, overnightcategory, overnight, om.fixed, om.var) %>%
  gather(key="cost", value="value", -yr, -overnightcategory)


cost.base.fixed.tech <- cptlcsts.melt %>%
  filter(cost == "om.fixed") %>%
  ggplot(aes(x=yr, y=value, color=overnightcategory)) +
  ggtitle("om.fixed") +
  ylab("$/kW") +
  theme(axis.text.x = element_text(angle = 45, hjust=1)) +
  geom_line()+
  geom_point() +
  facet_grid(.~overnightcategory, scale="free")

cost.base.var.tech <- cptlcsts.melt %>%
  filter(cost == "om.var") %>%
  ggplot(aes(x=yr, y=value, color=overnightcategory)) +
  ggtitle("om.var") +
  ylab("$/thous. kW") +
  theme(axis.text.x = element_text(angle = 45, hjust=1)) +
  geom_line()+
  geom_point() +
  facet_grid(.~overnightcategory, scale="free")

cost.base.over.tech <- cptlcsts.melt %>%
  filter(cost == "overnight") %>%
  ggplot(aes(x=yr, y=value, color=overnightcategory)) +
  ggtitle("overnight") +
  ylab("$/kW") +
  theme(axis.text.x = element_text(angle = 45, hjust=1)) +
  geom_line()+
  geom_point() +
  facet_grid(.~overnightcategory, scale="free")

# levelized capital costs -------------------------------------------------
data(levelizedcosts)

levcsts <- levelizedcosts %>%
  group_by(yr, fuel.general) %>%
  summarise(overnight.lev = mean(overnight.lev, na.rm=TRUE),
            om.fixed.lev = mean(om.fixed.lev, na.rm=TRUE),
            om.var = mean(om.var, na.rm=TRUE)) %>%
  ungroup()
levcsts.melt <- levcsts %>%
  gather(key="cost", value="value", -yr, -fuel.general)

cost.lev.fuel <- levcsts.melt %>%
  ggplot(aes(x=yr, y=value, color=fuel.general)) +
  ylab("$/MWh") +
  geom_line()+
  geom_point() +
  facet_grid(cost~fuel.general, scale="free")

# fuel prices -------------------------------------------------------------
data(fuelprices)
price.fuel <- fuelprices %>%
  ggplot(aes(x=yr, y=fuel.price)) +
  geom_line() +
  geom_point() +
  facet_grid(fuel.general~., scale="free") +
  ylab("USD$2010/BTU")



# Save plots --------------------------------------------------------------

## SAVE base costs by tech PLOT
fn <- "figs/Costs.base by tech.png"
print(paste0("saving ", fn))
png(fn, width=11, height=17, units="in", res=250)
grid_arrange_shared_legend(cost.base.fixed.tech, cost.base.var.tech, cost.base.over.tech,
                           position="right", ncol=1, nrow=3)
dev.off()

## SAVE costs by fuel PLOT
fn <- "figs/Costs.lev by fuel.png"
print(paste0("saving ", fn))
png(fn, width=11, height=17, units="in", res=250)
cost.lev.fuel
dev.off()

## SAVE fuel price by fuel PLOT
fn <- "figs/Fuel prices.png"
print(paste0("saving ", fn))
png(fn, width=11, height=17, units="in", res=250)
price.fuel
dev.off()


# boilerplate -------------------------------------------------------------

# scatter plot of plants' costs (by fuel)
filter(levcsts.melt, cost == "om.fixed.lev") %>%
  levcsts.melt %>%
  ggplot(aes(x=yr, y=value, color=fuel.general)) +
  ylab("$/MWh") +
  geom_point() +
  facet_grid(cost~fuel.general, scale="free")

# histogram of plants' costs (by fuel)
levcsts.melt %>%
  filter(fuel.general == "nuclear") %>%
  ggplot(aes(value)) +
  geom_histogram() +
  facet_grid(cost~fuel.general, scale="free")
