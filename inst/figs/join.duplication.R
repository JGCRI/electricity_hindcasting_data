##### track degenerate matches in JOIN.CAP.GEN #####
#
# 3 types of join:
#     yr-pcode-pm (2000)
#     yr-pcode-f (2001-02)
#     yr-pcode-pm-f (all else)
#
# for 2000-02, the two types of join allow for degenerate matches (matching on the 3 keys but failing to disambiguate between distinct values for the
# 4th key).
#
# For example, in 2000, in a set of rows that share the same yr-pcode-pm codes, the rows distinguished by fuel code will be collapsed.
# These rows will have columns yr-pcode-pm-f.x-f.y-cap-gen, meaning a choice between f.x and f.y will have to be made.
#
#

data(capacity.unmapped, generation.unmapped,
     mapping)

# aggregate over vintage (unnecessary variable)
capacity.unmapped <- capacity.unmapped %>%
  group_by(yr, utilcode, plntcode, primemover, fuel) %>%
  summarise(capacity = sum(capacity)) %>%
  ungroup()

# set of uniquely-identifying keys if 0
capacity.unmapped %>% select(yr, utilcode, plntcode, primemover, fuel) %>% duplicated () %>% sum()


# 2000 --------------------------------------------------------------------

# need f to uniquely identify correct gen-value to assign to a cap-value in ORIG CAP
capacity.unmapped %>%
  select(yr, utilcode, plntcode, primemover, fuel) %>%
  duplicated() %>%
  sum()
generation.unmapped %>%
  select(yr, utilcode, plntcode, primemover) %>%
  duplicated() %>%
  sum()

mapping.fuel <- mapping %>% select(fuel, fuel.general)

# join by yr-pcode-pm
join.00 <- generation.unmapped %>%
  inner_join( capacity.unmapped, ., # join unmapped datasets
              by = c("yr", "plntcode", "primemover", "fuel") ) %>%
  select(-starts_with("utilcode")) %>% # not used in join, end up with .x and .y version
  filter( yr == 2000) %>% # join method specific to this year
  select(-capacity, -generation, -consumption) # only want to look at keys for now

# attach 2x fuel.general info (according to fuel.x & fuel.y)
join.00.map <- join.00[,] %>%
  left_join(mapping.fuel, by=c("fuel.x"="fuel")) %>%
  rename(fg.x = fuel.general)  %>%
  left_join(mapping[,c("fuel", "fuel.general")], by=c("fuel.y"="fuel")) %>%
  rename(fg.y = fuel.general) %>%
  distinct()

join.00.map %>%
  filter(fg.x != fg.y) %>%
  View("Map-Duplication (2000)")

join.00.map %>%
  filter(fg.x == fg.y) %>%
  View("Unique Mapping (2000)")

# 2001-02 --------------------------------------------------------------------

# for years 2001-02, only set of primemovers available are from ORIG CAP
capacity.unmapped %>% filter(yr %in% c(2001, 2002)) %>% select(primemover) %>% distinct()
generation.unmapped %>% filter(yr %in% c(2001, 2002)) %>% select(primemover) %>% distinct()

# join by yr-pcode-pm
join.012 <- generation.unmapped %>%
  inner_join( capacity.unmapped, ., # join unmapped datasets
              by = c("yr", "plntcode", "fuel") ) %>%
  select(-starts_with("utilcode")) %>% # not used in join, end up with .x and .y version
  filter( yr %in% c(2001, 2002)) %>% # join method specific to this year
  select(-capacity, -generation, -consumption) %>% # only want to look at keys for now
  arrange(yr, plntcode, primemover.x, primemover.y, fuel)

# attach 2x fuel.general info (according to fuel.x & fuel.y)
join.012.map <- join.012[,] %>%
  left_join(mapping[,c("fuel", "primemover", "overnightcategory")], by=c("fuel", "primemover.x"="primemover")) %>%
  rename(oc.x = overnightcategory)  %>%
  left_join(mapping[,c("fuel", "primemover", "overnightcategory")], by=c("fuel", "primemover.y"="primemover")) %>%
  rename(oc.y = overnightcategory) %>%
  distinct()

# only one set of mappings b/c no ORIG GEN pm-codes!!
join.012.map %>%
  filter(oc.x != oc.y) %>% # different mappings
  View("Map-Duplication (2001-02)")
join.012.map %>%
  filter(oc.x == oc.y) %>% # same mappings
  View("Unique Mapping (2001-02)")

# how many plants report multiple prime movers?
dup.inds <- join.012 %>% select(yr, plntcode, fuel) %>% duplicated()
sum(dup.inds) # 872
join.012[dup.inds,] %>% arrange(yr, plntcode, fuel) %>% View("Same fuel-mult.movers")
# how many plants report multiple fuels?
dup.inds <- join.012 %>% select(yr, plntcode, primemover.x) %>% duplicated()
sum(dup.inds) # 872
join.012[dup.inds,] %>% arrange(yr, plntcode, primemover.x) %>% View("Same mover-mult.fuels")


dup.inds <- generation.unmapped %>%
  filter(yr %in% c(2001, 2002)) %>%
  select(yr, plntcode, fuel) %>%
  duplicated()
sum(dup.inds) # 17
generation.unmapped[dup.inds,] %>% arrange(yr, plntcode, primemover, fuel) %>% View("ORIG GEN DUPs") # mystery plant anyway..

dup.inds <- capacity.unmapped %>%
  filter(yr %in% 2001, 2002) %>%
  select(yr, plntcode, primemover, fuel) %>%
  duplicated()
sum(dup.inds) # 0
