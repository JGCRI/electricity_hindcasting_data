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
  inner_join( capacity.unmapped, ., # join unmapped datasets
                    by = c("yr", "plntcode", "primemover", "fuel") ) %>% 
  left_join(mapping, by=c("primemover", "fuel")) %>% # map datasets to oc-fg
  select(-primemover, -fuel) %>% # aggregate redundant mappings (pm, f) -> (oc, fg)
  group_by(yr, plntcode, vintage, overnightcategory, fuel.general) %>% # no longer grouping by utilcode b/c two versions (.x, .y)
  summarise(nameplate = sum(nameplate) ) %>%
  ungroup() %>% 
  rename(overnight = overnightcategory, # rename for plotting functions
         fuel = fuel.general) 

mer3.012 <- mer3.rest <- generation.unmapped %>% 
  filter( yr %in% c(2001, 2002)) %>% 
  inner_join( capacity.unmapped, ., # join unmapped datasets
              by = c("yr", "plntcode", "primemover", "fuel") )

%>% 
  left_join(mapping, by=c("primemover", "fuel")) %>% # map datasets to oc-fg
  select(-primemover, -fuel) %>% # aggregate redundant mappings (pm, f) -> (oc, fg)
  group_by(yr, plntcode, vintage, overnightcategory, fuel.general) %>% # no longer grouping by utilcode b/c two versions (.x, .y)
  summarise(nameplate = sum(nameplate) ) %>%
  ungroup() %>% 
  rename(overnight = overnightcategory, # rename for plotting functions
         fuel = fuel.general) 
# aggregate over vintage for full fleet
mer3.fleet <- mer3 %>% 
  group_by(yr, plntcode, overnight, fuel) %>%
  summarise(nameplate = sum(nameplate)) %>%
  ungroup() 
