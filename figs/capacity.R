library(dplyr)
library(magrittr)
library(ggplot2)
library(tidyr)

data(generators)#, generators.cf)


# Functions ---------------------------------------------------------------

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


# cf Dataset ------------------------------------------------------------

# new additions
cf.new <- generators.cf %>%
  dplyr::rename(fuel = fuel.general,
                overnight = overnightcategory) %>%
  mutate(overnight = gsub("conventional ", "", overnight)) %>%
  filter(yr == vintage) %>%
  mutate(yr = as.factor(yr))

cf.new.fuel <- plot.agg(cf.new, "CF Additions",  "fuel")
cf.new.oc <- plot.agg(cf.new, "CF Additions", "overnight")

# full fleet
cf <- generators.cf %>%
  dplyr::rename(fuel = fuel.general,
                overnight = overnightcategory) %>%
  mutate(overnight = gsub("conventional ", "", overnight)) %>%
  mutate(yr = as.factor(yr))

cf.fuel <- plot.agg(cf, "CF Fleet", "fuel")
cf.oc <- plot.agg(cf, "CF Fleet", "overnight")

# Original Dataset --------------------------------------------------------
# new additions
orig <- generators %>%
  dplyr::rename(fuel = fuel.general,
                overnight = overnightcategory) %>%
  mutate(overnight = gsub("conventional ", "", overnight)) %>%
  mutate(yr = as.factor(yr))

orig.new <- orig %>%
  filter(yr == vintage)

orig.new.fuel <- plot.agg(orig.new, "ORIG Additions",  "fuel")
orig.new.oc <- plot.agg(orig.new, "ORIG Additions", "overnight")

# full fleet
orig.fuel <- plot.agg(orig, "ORIG Fleet", "fuel")
orig.oc <- plot.agg(orig, "ORIG Fleet", "overnight")


# XOR (Original, cf) ----------------------------------------------------

# refactor and drop vintage
gen <- generators %>%
  dplyr::rename(fuel = fuel.general,
                overnight = overnightcategory) %>%
  mutate(overnight = gsub("conventional ", "", overnight)) %>%
  mutate(yr = as.factor(yr)) %>%
  group_by(yr, utilcode, plntcode, overnight, fuel) %>%
  summarise(nameplate=sum(nameplate)) %>%
  ungroup()
test <- out %>%
  group_by(plntcode, overnight, fuel) %>%
  summarise(UTIL = paste0(unique(utilcode), collapse = ", "),
            n = length(unique(utilcode))) %>%
  ungroup()

View(filter(test, n>1))

gen.cf <- generators.cf %>%
  dplyr::rename(fuel = fuel.general,
                overnight = overnightcategory) %>%
  mutate(overnight = gsub("conventional ", "", overnight)) %>%
  mutate(yr = as.factor(yr)) %>%
  group_by(yr, utilcode, plntcode, overnight, fuel) %>%
  summarise(nameplate=sum(nameplate)) %>%
  ungroup()

# CAPACITY IN ORIG BUT NOT IN CF
xor.cf <- anti_join(gen, gen.cf, by=c("yr", "utilcode", "plntcode", "overnight", "fuel"))
xor.cf.fuel <- plot.agg(xor.cf, "XOR.CF Fleet", "fuel")

# CAPACITY IN ORIG BUT NOT IN GENERATION
data(generation)
out <- generation %>%
  dplyr::rename(fuel = fuel.general,
                               overnight = overnightcategory) %>%
  mutate(overnight = gsub("conventional ", "", overnight)) %>%
  mutate(yr = as.factor(yr))

xor.out <- anti_join(gen, out, by=c("yr", "utilcode", "plntcode", "overnight", "fuel"))
xor.out.fuel <- plot.agg(xor.out, "XOR.OUT Fleet", "fuel")

xor.out2 <- anti_join(out, gen, by=c("yr", "utilcode", "plntcode", "overnight", "fuel")) %>%
  mutate(nameplate = generation/8760)
xor.out.fuel2 <- plot.agg(xor.out2, "XOR.OUT2 Fleet", "fuel")

and.out <- inner_join(gen, out, by=c("yr", "utilcode", "plntcode", "overnight", "fuel")) %>%
  filter(generation>0)
and.out.fuel <- plot.agg(and.out, "AND.OUT Fleet", "fuel")


# merged cap data ---------------------------------------------------------
merged <- read.csv("C:/Users/guti220/Desktop/energy.markets/merged.map.csv", stringsAsFactors=FALSE) %>%
  select(-primemover, -fuel) %>%
  dplyr::rename(fuel = fuel.general,
                overnight = tech,
                vintage = startyr) %>%
  mutate(yr = as.factor(yr)) %>%
  filter(!is.na(generation)) %>%
  group_by(yr, utilcode, plntcode, overnight, fuel, vintage) %>%
  summarise(nameplate = sum(nameplate)) %>%
  ungroup()

# new additions
mer.new <- merged %>%
  filter(yr == vintage)

mer.new.fuel <- plot.agg(mer.new, "MERGED Additions",  "fuel")
mer.new.oc <- plot.agg(mer.new, "MERGED Additions", "overnight")

# full fleet
mer <- merged %>% # aggregate over vintage
  group_by(yr, utilcode, plntcode, overnight, fuel) %>%
  summarise(nameplate = sum(nameplate)) %>%
  ungroup() #%>%
  # filter(! yr %in% c(1998, 1999, 2000))
mer.fuel <- plot.agg(mer, "MERGED Fleet", "fuel")
mer.oc <- plot.agg(mer, "MERGED Fleet", "overnight")


# PLOTS -------------------------------------------------------------------

## SAVE cf PLOT
fn <- "figs/cf.png"
print(paste0("saving ", fn))
png(fn, width=11, height=8.5, units="in", res=250)
multiplot(cf.new.fuel,cf.fuel, cf.new.oc,cf.oc, cols=2)
dev.off()

## SAVE ORIG PLOT
fn <- "figs/ORIG.png"
print(paste0("saving ", fn))
png(fn, width=11, height=8.5, units="in", res=250)
multiplot(orig.new.fuel, orig.fuel, orig.new.oc, orig.oc, cols=2)
dev.off()

## SAVE FUEL PLOT
fn <- "figs/Capacity by fuel.png"
print(paste0("saving ", fn))
png(fn, width=11, height=8.5, units="in", res=250)
grid_arrange_shared_legend(cf.new.fuel, cf.fuel, orig.new.fuel, orig.fuel,
                           position="right", ncol=2, nrow=2)
dev.off()

## SAVE OVERNIGHT PLOT
fn <- "figs/Capacity by overnight.png"
print(paste0("saving ", fn))
png(fn, width=11, height=8.5, units="in", res=250)
grid_arrange_shared_legend(cf.new.oc, cf.oc, orig.new.oc, orig.oc,
                           position="right", ncol=2, nrow=2)
dev.off()

## SAVE XOR PLOT
fn <- "figs/xor by fuel.png"
print(paste0("Saving ", fn))
png(fn, width=11, height=8.5, units="in", res=250)
print(xor.fuel)
dev.off()

## SAVE MERGED.FUEL PLOT
fn <- "merged.fuel.png"
print(paste0("Saving ", fn))
png(fn, width=11, height=8.5, units="in", res=250)
grid_arrange_shared_legend(mer.fuel, mer.new.fuel, ncol=1, nrow=2)
dev.off()

## SAVE ORIG.FUEL PLOT
fn <- "orig.fuel.png"
print(paste0("Saving ", fn))
png(fn, width=11, height=8.5, units="in", res=250)
grid_arrange_shared_legend(orig.fuel, orig.new.fuel, ncol=1, nrow=2)
dev.off()
