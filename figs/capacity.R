library(energy.markets)
library(dplyr)
library(magrittr)
library(ggplot2)
library(tidyr)
# Functions ---------------------------------------------------------------
elec_tech_colors <- c( "coal" = "#a0237c",
                       "b Coal w/CCS" = "#dab4c7",
                       "natural gas" = "#25a9e0",
                       "d Gas w/CCS" = "#84e7f9",
                       "oil" = "#d01c2a",
                       "f Oil w/CCS" = "#f7988f",
                       "biomass" = "#00931d",
                       "h Biomass w/CCS" = "#88c892",
                       "uranium" = "#ef8e27",
                       "geothermal" = "#ad440c",
                       "water" = "#fdfa28",
                       "wind" = "#3d86f9",
                       "solar" = "#fdd67b",
                       "n CHP" = "#507fab",
                       "o Battery" = "#92a75d",
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



# Original Dataset --------------------------------------------------------
data(capacity)
# full fleet
orig <- capacity %>%
  dplyr::rename(fuel = fuel.general,
                overnight = overnightcategory) %>%
  mutate(overnight = gsub("conventional ", "", overnight)) %>%
  mutate(yr = as.factor(yr))

# aggregate over vintage for full fleet
orig.fleet <- orig %>%
  group_by(yr, utilcode, plntcode, overnight, fuel) %>%
  summarise(nameplate=sum(nameplate)) %>%
  ungroup()

# filter for new additions only
orig.new <- orig %>%
  filter(yr == vintage)

# get plots
orig.fuel <- plot.agg(orig.fleet, "ORIG Fleet", "fuel")
orig.new.fuel <- plot.agg(orig.new, "ORIG Additions",  "fuel")
orig.oc <- plot.agg(orig.fleet, "ORIG Fleet", "overnight")
orig.new.oc <- plot.agg(orig.new, "ORIG Additions", "overnight")



# yr.utilcode.plntcode.oc.fg ----------------------------------------------
data(capacity, generation)

mer1 <- inner_join( capacity, generation,
                      by = c("yr", "utilcode", "plntcode", "overnightcategory", "fuel.general") ) %>%
  rename(overnight = overnightcategory,
         fuel = fuel.general)

# aggregate over vintage for full fleet
mer1.fleet <- mer1 %>%
  group_by(yr, utilcode, plntcode, overnight, fuel) %>%
  summarise(nameplate = sum(nameplate)) %>%
  ungroup()

# filter for new additions only
mer1.new <- mer1 %>%
  filter(yr == vintage)

# get plots
mer1.fuel <- plot.agg(mer1.fleet, "MER1 Fleet", "fuel")
mer1.new.fuel <- plot.agg(mer1.new, "MER1 Additions",  "fuel")
mer1.oc <- plot.agg(mer1.fleet, "MER1 Fleet", "overnight")
mer1.new.oc <- plot.agg(mer1.new, "MER1 Additions", "overnight")



# yr.utilcode.plntcode.pm.f -----------------------------------------------
data(capacity.unmapped, generation.unmapped, mapping)

# merge
mer2 <- inner_join( capacity.unmapped, generation.unmapped, # join unmapped datasets
                            by = c("yr", "utilcode", "plntcode", "primemover", "fuel") ) %>%
  left_join(mapping, by=c("primemover", "fuel")) %>% # map datasets to oc-fg
  select(-primemover, -fuel) %>% # aggregate redundant mappings (pm, f) -> (oc, fg)
  group_by(yr, utilcode, plntcode, vintage, overnightcategory, fuel.general) %>%
  summarise(nameplate = sum(nameplate) ) %>%
  ungroup() %>%
  rename(overnight = overnightcategory, # rename for plotting functions
         fuel = fuel.general)

# aggregate over vintage for full fleet
mer2.fleet <- mer2 %>%
  group_by(yr, utilcode, plntcode, overnight, fuel) %>%
  summarise(nameplate = sum(nameplate)) %>%
  ungroup()

# filter for new additions only
mer2.new <- mer2 %>%
  filter(yr == vintage)

mer2.fuel <- plot.agg(mer2.fleet, "MER2 Fleet", "fuel")
mer2.new.fuel <- plot.agg(mer2.new, "MER2 Additions",  "fuel")
mer2.oc <- plot.agg(mer2.fleet, "MER2 Fleet", "overnight")
mer2.new.oc <- plot.agg(mer2.new, "MER2 Additions", "overnight")


# yr.plntcode.pm.f --------------------------------------------------------
data(capacity.unmapped, generation.unmapped, mapping)

# # merge
# mer3 <- inner_join( capacity.unmapped, generation.unmapped, # join unmapped datasets
#                     by = c("yr", "plntcode", "primemover", "fuel") ) %>%
#   left_join(mapping, by=c("primemover", "fuel")) %>% # map datasets to oc-fg
#   select(-primemover, -fuel) %>% # aggregate redundant mappings (pm, f) -> (oc, fg)
#   group_by(yr, plntcode, vintage, overnightcategory, fuel.general) %>% # no longer grouping by utilcode b/c two versions (.x, .y)
#   summarise(nameplate = sum(nameplate) ) %>%
#   ungroup() %>%
#   rename(overnight = overnightcategory, # rename for plotting functions
#          fuel = fuel.general)

# merge for ! yr %in% c(2001, 2002)
# join includes primemover
mer3.rest <- generation.unmapped %>%
  filter(! yr %in% c(2001, 2002)) %>%
  inner_join( capacity.unmapped, ., # join unmapped datasets W/O UTILCODE
              by = c("yr", "plntcode", "primemover", "fuel") )

# merge for yr %in% c(2001, 2002)
# join excludes primemover, so we opt to use CAP primemover column in later aggregation
mer3.012 <- generation.unmapped %>%
  filter( yr %in% c(2001, 2002)) %>%
  inner_join( capacity.unmapped, ., # join unmapped datasets by DROPPING PRIMEMOVER
              by = c("yr", "plntcode", "fuel") ) %>%
  dplyr::rename(primemover = primemover.x) %>%  #use CAP pm column
  select(-primemover.y) # drop GEN pm column


mer3 <- rbind(mer3.rest, mer3.012) %>%
  left_join(mapping, by=c("primemover", "fuel")) %>% # map datasets to oc-fg
  select(-primemover, -fuel) %>% # aggregate redundant mappings (pm, f) -> (oc, fg)
  group_by(yr, plntcode, vintage, overnightcategory, fuel.general) %>% # no longer grouping by utilcode b/c two versions (.x, .y)
  summarise(nameplate = sum(nameplate),
            generation = sum(generation)) %>%
  ungroup() %>%
  rename(overnight = overnightcategory, # rename for plotting functions
         fuel = fuel.general)


# aggregate over vintage for full fleet
mer3.fleet <- mer3 %>%
  group_by(yr, plntcode, overnight, fuel) %>%
  summarise(nameplate = sum(nameplate)) %>%
  ungroup()

# filter for new additions only
mer3.new <- mer3 %>%
  filter(yr == vintage)

mer3.fuel <- plot.agg(mer3.fleet, "MER3 Fleet", "fuel")
mer3.new.fuel <- plot.agg(mer3.new, "MER3 Additions",  "fuel")
mer3.oc <- plot.agg(mer3.fleet, "MER3 Fleet", "overnight")
mer3.new.oc <- plot.agg(mer3.new, "MER3 Additions", "overnight")



# Wonjun's 'merged' Dataset ---------------------------------------------------------
data(mapping)
mer4 <- read.delim("C:/Users/guti220/Desktop/merged.tsv") %>%
  left_join(mapping, by=c("primemover", "fuel")) %>% # (pm, f) -> (oc, fg)
  select(-primemover, -fuel) %>%
  dplyr::rename(fuel = fuel.general,
                overnight = overnightcategory,
                vintage = startyr) %>%
  mutate(overnight = gsub("conventional ", "", overnight)) %>%
  mutate(yr = as.factor(yr)) %>%
  filter(!is.na(generation)) %>% # merged weird, includes NA generation
  group_by(yr, utilcode, plntcode, overnight, fuel, vintage) %>%
  summarise(nameplate = sum(nameplate)) %>%
  ungroup()


# aggregate over vintage for full fleet
mer4.fleet <- mer4 %>%
  group_by(yr, utilcode, plntcode, overnight, fuel) %>%
  summarise(nameplate = sum(nameplate)) %>%
  ungroup()

# filter for new additions only
mer4.new <- mer4 %>%
  filter(yr == vintage)

# get plots
mer4.fuel <- plot.agg(mer4.fleet, "MER4 Fleet", "fuel")
mer4.new.fuel <- plot.agg(mer4.new, "MER4 Additions",  "fuel")
mer4.oc <- plot.agg(mer4.fleet, "MER4 Fleet", "overnight")
mer4.new.oc <- plot.agg(mer4.new, "MER4 Additions", "overnight")
# figs/outfile.txt --------------------------------------------------------

sink("figs/outfile.txt") # log

cat("Mapped Capacity Dataset : orig.fleet \n")
cat("DF size (agg'd over vintage): ", dim(orig.fleet), " \n")
cat("Cols: ", names(orig), "\n")
cat("\n")

cat("Mapped Generation Dataset \n")
cat("DF size: ", dim(generation), "\n")
cat("Cols: ", names(generation), "\n")
cat("\n")

fulljoin <- full_join(capacity, generation,
                      by = c("yr", "utilcode", "plntcode", "overnightcategory", "fuel.general")) %>%
  select(yr, utilcode, plntcode, overnightcategory, fuel.general) %>%
  distinct()
cat("Mapped fulljoin \n")
cat("DF size (agg'd over vintage): ", dim(fulljoin), "\n")
cat("Cols: ", names(fulljoin), "\n")
cat("\n")

cat("Merge on yr.utilcode.plntcode.oc.fg: mer1.fleet \n")
cat("Merged DF size (agg'd over vintage): ",dim(mer1.fleet), "\n")
cat("Retention (fulljoin): ", 100 * nrow(mer1.fleet)/ nrow(fulljoin), "% \n" )
cat("Retention (orig.fleet): ", 100 * nrow(mer1.fleet)/ nrow(orig.fleet), "% \n" )
cat("\n\n\n")

# orig.unmapped.fleet <- capacity.unmapped %>%
#   select(yr, utilcode, plntcode, primemover, fuel) %>%
#   distinct()
# cat("Unmapped Capacity Dataset \n")
# cat("DF size (agg'd over vintage): ", dim(orig.unmapped.fleet), "\n")
# cat(names(orig.unmapped.fleet), "\n")
# cat("\n")
#
# cat("Unmapped Generation Dataset \n")
# cat("DF size: ", dim(generation.unmapped), "\n")
# cat(names(generation.unmapped), "\n")
# cat("\n")
#
# fulljoin.unmapped <- full_join(capacity.unmapped, generation.unmapped,
#                                by = c("yr", "utilcode", "plntcode", "primemover", "fuel")) %>%
#   select(yr, utilcode, plntcode, primemover, fuel ) %>%
#   distinct()
# cat("Unmapped fulljoin")
# cat("DF size (agg'd over vintage): ", dim(fulljoin.unmapped), "\n")
# cat(names(fulljoin.unmapped), "\n")
# cat("\n")
cat("NEXT TWO OPTIONS MERGE *THEN* MAP, SO RETENTION IS COMPARED TO SAME 'FULLJOIN' AND 'ORIG.FLEET' DF'S AS ABOVE\n\n")

cat("Merge on yr.utilcode.plntcode.pm.f: mer2.fleet \n")
cat("Merged DF size (agg over vintage): ",dim(mer2.fleet), "\n")
cat("Retention (fulljoin): ", 100 * nrow(mer2.fleet)/ nrow(fulljoin), "% \n" )
cat("Retention (orig.fleet): ", 100 * nrow(mer2.fleet)/ nrow(orig.fleet), "% \n" )
cat("\n")

cat("Merge on yr.plntcode.pm.f: mer3 \n")
cat("Merged df size (agg over vintage): ",dim(mer3.fleet), "\n")
cat("Retention (fulljoin): ", 100 * nrow(mer3.fleet)/ nrow(fulljoin), "% \n" )
cat("Retention (orig.fleet): ", 100 * nrow(mer3.fleet)/ nrow(orig.fleet), "% \n" )

cat("\n\n")

cat("Wonjun's Merged Dataset: mer4.fleet", "\n")
cat("Merged DF size (agg'd over vintage): ", dim(mer4.fleet), "\n")
cat("Retention: unsure \n")
cat("\n\n")

sink() # end logging
print("saving figs/outfile.txt")

# PLOTS -------------------------------------------------------------------

## SAVE ORIG PLOT
fn <- "figs/ORIG.png"
print(paste0("saving ", fn))
png(fn, width=11, height=8.5, units="in", res=250)
multiplot(orig.new.fuel, orig.fuel, orig.new.oc, orig.oc, cols=2)
dev.off()

## SAVE FUEL PLOT
fn <- "figs/Capacity by fuel.png"
print(paste0("saving ", fn))
png(fn, width=11, height=17, units="in", res=250)
grid_arrange_shared_legend(orig.fuel, orig.new.fuel,
                           mer1.fuel, mer1.new.fuel,
                           mer2.fuel, mer2.new.fuel,
                           mer3.fuel, mer3.new.fuel,
                           mer4.fuel, mer4.new.fuel,
                           position="right", ncol=2, nrow=5)
dev.off()



