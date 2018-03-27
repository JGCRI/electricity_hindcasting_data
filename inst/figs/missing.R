##### track missing data #####

# there are two types of "missing' data
#
# the data pipeline goes:
# ORIG CAP/GEN (f-pm) -> JOIN.CAP.GEN (f-pm) -> JOIN.CAP.GEN (fg-oc) -> FINAL (fg-oc)
#
# the first missing data originally appears in generation.unmapped 2000-2001
# see summary time-series @ inst/figs/ORIG.GEN.avg.png. The 01-02 appear missing because
# the ORIG GEN is missing the native pm codes (check raw data files).
#
# JOIN.CAP.GEN (f-pm) does two types of joins. For 01-02, the join uses
# by = c("yr", "plntcode", "fuel"), which excludes the pm codes. ORIG CAP pm codes are
# used b/c they are left out of ORIG GEN. For all other years, the join uses
# by = c("yr", "plntcode", "primemover", "fuel").
#
# the second missing data was identified in JOIN.CAP.GEN (fg-oc). b/c this is a join, the
# missing data appears for plots of both CAP and GEN plots. See inst/figs/JOINED.XX.avg.png
# for discontinuous time-series plots for fuels biomass and petrolum.
#
# The discontinuity occurs for the year 2000. It appears that this year should be joined
# using by = c("yr", "plntcode", "primemover").
#
# Which dataset's fuel code should we use, ORIG CAP or ORIG GEN?

library(ElectricityHindcastingData)
library(tidyr)
library(ggplot2)
library(ggridges)
library(magrittr)
library(dplyr)
library(rlang)

data(capacity.unmapped, generation.unmapped, mapping,
     capacity, generation,
     cap.gen.joined.unmapped,
     cap.gen.joined,
     master)

map_and_aggregate <- function(df, column) {
  column <- enquo(column)

  df <- df %>%
    left_join(mapping, by=c("primemover", "fuel")) %>% # map datasets to oc-fg
    select(-primemover, -fuel) %>% # aggregate redundant mappings (pm, f) -> (oc, fg)
    # no longer grouping by utilcode b/c two versions (.x, .y)
    group_by(yr, plntcode, overnightcategory, fuel.general) %>%
    summarise(!!quo_name(column) := sum(!!column)) %>%
    ungroup()
}

map_and_attach <- function(df) {

  df <- df %>%
    left_join(mapping, by=c("primemover", "fuel"))
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


# inspect unmapped JOIN.CAP.GEN -------------------------------------------


join.unmapped <- cap.gen.joined.unmapped %>% map_and_attach(capacity)

join.unmapped %>%
  filter(yr == 2000) %>%
  select(primemover, fuel, fuel.general, overnightcategory) %>%
  distinct() %>%
  arrange(fuel.general, overnightcategory) %>%
  View("JOIN 2000")

# biomass & petroleum only fuels that have continuous data for all years except 2000
join.unmapped %>%
  filter(fuel.general %in% c("biomass", "petroleum")) %>%
  select(yr, primemover, fuel, fuel.general, overnightcategory) %>%
  distinct() %>%
  arrange(fuel.general, overnightcategory, yr) %>%
  View("JOIN Bio/Oil Permutations")


# inspect mapped ORIG CAP/GEN  --------------------------------------------

# anti_join between cap.2000 and gen.2000 is what's dropped on the basis of
# mismatched keys. But this only shows fg-oc keys that already have sparse time-series

cap.2000 <- capacity %>%
  filter(yr == 2000) %>%
  select(fuel.general, overnightcategory) %>%
  distinct() %>%
  arrange(fuel.general, overnightcategory)

cap.2000 %>% View("ORIG CAP 2000")

gen.2000 <- generation %>%
  filter(yr == 2000) %>%
  select(fuel.general, overnightcategory) %>%
  distinct() %>%
  arrange(fuel.general, overnightcategory)

gen.2000 %>% View("ORIG GEN 2000")

anti_join(cap.2000, gen.2000) %>% View("ANTIJOIN 2000")

# inspect join(ORIG CAP, ORIG GEN) using fewer keys -----------------------

# fg = c(biomass, petroleum) have consistent time-series except for yr=2000, and
# don't appear in the above anti_join, meaning there is a mismatch in either the
# plant code or native fuel code

# join by yr-pcode-pm-f
# no bio/oil

# join by yr-pcode-f (current package implementation)
# no bio/oil

# join by yr-pcode-pm
join.00.pm <- generation.unmapped %>%
  filter( yr == 2000) %>%
  inner_join( capacity.unmapped, ., # join unmapped datasets
              by = c("yr", "plntcode", "primemover") ) %>%
  select(-starts_with("utilcode"), -vintage, -capacity, -generation, -consumption)

# we struck oil!!!

# attach (fg-oc) using common pm codes, ORIG CAP fuel codes
join.00.pm %>%
  left_join(mapping, by = c("fuel.x"="fuel", "primemover")) %>%
  select(yr, plntcode, fuel.general, overnightcategory) %>%
  distinct() %>%
  # filter(fuel.general %in% c("biomass", "petroleum")) %>%
  View("Using ORIG CAP fuel codes")

# attch (fg-oc) using common pm codes, ORIG GEN fuel codes
join.00.pm %>%
  left_join(mapping, by = c("fuel.y"="fuel", "primemover")) %>%
  select(yr, plntcode, fuel.general, overnightcategory) %>%
  distinct() %>%
  # filter(fuel.general %in% c("biomass", "petroleum")) %>%
  View("Using ORIG GEN fuel codes")

# CAP and GEN map to different things?
join.00.pm %>%
  #select(-vintage, -capacity, -generation, -consumption) %>%
  select(-plntcode) %>%
  distinct() %>%
  # replace (f,pm).x with (fg,oc).x
  left_join(mapping, by=c("fuel.x"="fuel", "primemover.x"="primemover")) %>%
  select(-fuel.x, -primemover.x) %>%
  rename(fg.x = fuel.general,
         oc.x = overnightcategory) %>%
  # replace (f,pm).y with (fg,oc).y
  left_join(mapping, by=c("fuel.y"="fuel", "primemover.y"="primemover")) %>%
  select(-fuel.y, -primemover.y) %>%
  rename(fg.y = fuel.general,
         oc.y = overnightcategory) %>%
  distinct() %>%
  filter(fg.x != fg.y | oc.x != oc.y)


# get summary time-series  ------------------------------------------------

# get capacity & generation summary time-series for each data source
# sources: ORIG CAP/GEN , MAP CAP/GEN, JOIN.CAP.MAP, MASTER

orig.cap.summary <- capacity.unmapped %>% map_and_aggregate(capacity) %>% summaryCalc(capacity)
orig.gen.summary <- generation.unmapped %>% map_and_aggregate(generation) %>% summaryCalc(generation)

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



