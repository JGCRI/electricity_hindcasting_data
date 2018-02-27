library(energy.markets)
library(dplyr)
library(magrittr)

# (generation.unmapped, mapping) ------------------------------------------
data(generation.unmapped, mapping)

gen.mapped <- inner_join(mapping, generation.unmapped, by=c("primemover", "fuel"))
# don't include 2001-2002

gen.mapped.summary <- gen.mapped %>%
  group_by(primemover, fuel, overnightcategory, fuel.general) %>%
  summarise(share = sum(generation) / sum(generation.unmapped$generation)) %>%
  ungroup()
# only 84.8% of all generation included in mapping

# these are mappings not used by generation.unmapped
gen.unmapped <- anti_join(mapping, generation.unmapped, by=c("primemover", "fuel"))

# these are code pairs used by generation.unmapped but not mapped
gen.unmapped <- anti_join(generation.unmapped, mapping, by=c("primemover", "fuel"))
# dropped share of total generation across all years primarily from 2001-02

gen.unmapped.summary <- gen.unmapped %>%
  group_by(primemover, fuel, yr) %>%
  summarise(share = sum(generation) / sum(generation.unmapped$generation)) %>%
  ungroup()
# this has the lost 15.2%
# 2001-02 has exclusively NA for primemover....b/c that col is empty in the raw data files

df <- filter(generation.unmapped, yr %in% c(2001, 2002))


# split merging -----------------------------------------------------------

data(capacity.unmapped, generation.unmapped, mapping)

# merge for ! yr %in% c(2001, 2002)
mer3.rest <- generation.unmapped %>%
  filter(! yr %in% c(2001, 2002)) %>%
  inner_join( capacity.unmapped, ., # join unmapped datasets W/O UTILCODE
                    by = c("yr", "plntcode", "primemover", "fuel") ) %>%
  left_join(mapping, by=c("primemover", "fuel")) %>% # map datasets to oc-fg
  select(-primemover, -fuel) %>% # aggregate redundant mappings (pm, f) -> (oc, fg)
  group_by(yr, plntcode, vintage, overnightcategory, fuel.general) %>% # no longer grouping by utilcode b/c two versions (.x, .y)
  summarise(nameplate = sum(nameplate) ) %>%
  ungroup() %>%
  rename(overnight = overnightcategory, # rename for plotting functions
         fuel = fuel.general)

# merge for yr %in% c(2001, 2002)
mer3.012 <- generation.unmapped %>%
  filter( yr %in% c(2001, 2002)) %>%
  inner_join( capacity.unmapped, ., # join unmapped datasets by DROPPING PRIMEMOVER
              by = c("yr", "plntcode", "fuel") ) %>%
  dplyr::rename(primemover = primemover.x) %>% # use CAP pm column
  left_join(mapping, by=c("primemover", "fuel")) %>% # map datasets to oc-fg
  #select(-primemover, -fuel) %>% # aggregate redundant mappings (pm, f) -> (oc, fg)
  group_by(yr, plntcode, vintage, primemover, fuel, overnightcategory, fuel.general) %>% # no longer grouping by utilcode b/c two versions (.x, .y)
  summarise(nameplate = sum(nameplate),
            generation = sum(generation)) %>%
  ungroup()

gen.012.summary <- mer3.012 %>%
  group_by(yr, primemover, fuel, overnightcategory, fuel.general) %>%
  summarise(share = sum(generation) / sum(generation.unmapped$generation)) %>%
  ungroup()


# combine merging ---------------------------------------------------------
data(capacity.unmapped, generation.unmapped, mapping)

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
  ungroup()

mer3.summary <- mer3 %>%
  group_by(yr) %>%
  summarise(share.gen = round(100 * sum(generation) / sum(mer3$generation), 2),
            share.cap = round(100 * sum(nameplate) / sum(mer3$nameplate), 2)) %>%
  ungroup()

# summary of generation/capacity
ggplot(mer3.summary, aes(x=yr, y=share.gen)) +
  geom_line() +
  geom_point() +
  ylab("% Total Generation, 1990-2016 (MWh)")

ggplot(mer3.summary, aes(x=yr, y=share.cap)) +
  geom_line() +
  geom_point() +
  ylab("% Total Capacity, 1990-2016, (MW)")

ggplot(mer3.summary, aes(x=share.cap, y=share.gen)) +
  geom_line() +
  geom_point() +
  xlab("% Total Capacity, 1990-2016, (MW)") +
  ylab("% Total Generation, 1990-2016 (MWh)")

