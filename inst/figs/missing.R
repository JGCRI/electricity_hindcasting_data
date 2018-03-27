##### track missing 00-01 data in ORIG GEN data #####

# the missing data originally appears in generation.unmapped 2000-2001
# see summary time-series @ inst/figs/GEN.avg.png
# this missing data isn't reflected in capacity.unmapped.
# See summary time-series @ inst/figs/CAP.avg.png
#
# Because capacity factor calculation requires capacity & generation data at the
#   yr-plant-mover-fuel level, data missing in either source (ORIG CAP, ORIG GEN) should appear
#   downstream
#
# After ORIG, I save MAP CAP & MAP GEN, which should independently retain their unique profiles
#   of existing/missing data.
#
# After MAP, I save JOIN.CAP.GEN. Its summary time-series should have missing data reflecting the
#   union of missing data in ORIG CAP & ORIG GEN (also MAP CAP & MAP GEN, because those two sets of
#   datasets should have the same missing data)
#
# After JOIN, I calculate CF and attach relevant price data. This means that the master (final)
#   dataset should contain the same missing data as JOIN.

library(ElectricityHindcastingData)
library(tidyr)
library(ggplot2)
library(ggridges)
library(magrittr)
library(dplyr)
library(rlang)

data(capacity.unmapped, generation.unmapped, mapping,
     capacity, generation,
     cap.gen.joined,
     master)

map <- function(df, column) {
  column <- enquo(column)

  df <- df %>%
    left_join(mapping, by=c("primemover", "fuel")) %>% # map datasets to oc-fg
    select(-primemover, -fuel) %>% # aggregate redundant mappings (pm, f) -> (oc, fg)
    # no longer grouping by utilcode b/c two versions (.x, .y)
    group_by(yr, plntcode, overnightcategory, fuel.general) %>%
    summarise(!!quo_name(column) := sum(!!column)) %>%
    ungroup()
}

my_full_join <- function(df1, df2) {
  full_join(df1, df2, by = c("fuel.general", "overnightcategory", "yr"))
}

select_missing <- function(df) {
  df %>%
    filter(yr >= 1996) %>% # capital cost data begins @ 96
    filter(!is.na(missing.x) | ! is.na(missing.y)) %>%
    arrange(fuel.general, overnightcategory, yr)
}

# get summary time-series  ------------------------------------------------

# get capacity & generation summary time-series for each data source
# sources: ORIG CAP/GEN , MAP CAP/GEN, JOIN.CAP.MAP, MASTER

orig.cap.summary <- capacity.unmapped %>% map(capacity) %>% summaryCalc(capacity)
orig.gen.summary <- generation.unmapped %>% map(generation) %>% summaryCalc(generation)

map.cap.summary <- capacity %>% summaryCalc(capacity)
map.gen.summary <- generation %>% summaryCalc(generation)

join.cap.summary <- cap.gen.joined %>% summaryCalc(capacity)
join.gen.summary <- cap.gen.joined %>% summaryCalc(generation)

master.cap.summary  <- master %>% summaryCalc(capacity)
master.gen.summary <- master %>% summaryCalc(generation)




# bind CAP/GEN summary-time series  ---------------------------------------

# easier to compare locations of missing values in single table
orig.summary <- my_full_join(orig.cap.summary, orig.gen.summary) %>% select_missing()
map.summary <- my_full_join(map.cap.summary, map.gen.summary) %>% select_missing()
join.summary <- my_full_join(join.cap.summary, join.gen.summary) %>% select_missing()
master.summary <- my_full_join(master.cap.summary, master.gen.summary) %>% select_missing()



# inspect missing data ----------------------------------------------------

# these fg-oc pairs are missing data just for 2001 & 2002
orig.summary %>%
  group_by(fuel.general, overnightcategory) %>%
  filter(any(unique(yr) %in% c(2000, 2001, 2002))) %>%
  View("ORIG")

# the same pairs appear in the mapped version of ORIG
map.summary %>%
  group_by(fuel.general, overnightcategory) %>%
  filter(any(unique(yr) %in% c(2000, 2001, 2002))) %>%
  View("MAP")

# JOIN dataset somehow finds missing 2001 GEN data (but not 2000...)
# ^ what did this mean?

join.summary %>%
  # group_by(fuel.general, overnightcategory) %>%
  # filter(unique(yr) == c(2001, 2002)) %>%
  filter(length(unique(yr)) == 2) %>%
  View("JOIN")

# downstream effect
master.summary %>% View("Master (00-01)")



