
# match CA plant codes w/ EIA plant codes ---------------------------------

ca.mapping <- read.delim("data-raw/ca.mapping.tsv")
# Source: http://www.energy.ca.gov/almanac/electricity_data/web_qfer/, 'Power Plant ID Cross Reference Table.xls'
# Dataset that maps the unique identifier in the CA dataset to the Form 860 Plant Code #'s
ca.mapping <- ca.mapping %>%
  filter(is.na(plant_code)==FALSE) %>%
  mutate(plant.id = tolower(plant.id)) %>%
  mutate(plant.id = gsub(" ", "", plant.id)) %>%
  distinct(plant.id, plant_code, .keep_all = TRUE) %>%
  select(plant.id, plant_code) #CA Energy Commision ID && EIA Plant Code #'s



# store plant heat rates --------------------------------------------------

ca.elec.almanac <- read.delim("data-raw/ca.elec.almanac.tsv")
# source: http://www.energy.ca.gov/almanac/electricity_data/web_qfer/Heat_Rates.php
# Plant-level heat rate, fuel consumption, energy production data
ca.elec.almanac <- ca.elec.almanac %>%
  filter(heat.rate != 0) %>%
  mutate(plant.id = gsub(" ", "", plant.id)) %>%
  mutate(plant.id = tolower(plant.id)) %>%
  left_join(ca.mapping, by = "plant.id") %>% # join ca.mapping (which contains EIA ID's) to ca.elec.almanac
  mutate(heat.rate = 1000*heat.rate) %>%
  # Converts units from MMbtu/MWh to btu/kWh (units favored by the EIA)
  select(plant_code, year, heat.rate) #EIA Plant Code #'s, Year, & Heat Rate



# Save data ---------------------------------------------------------------
devtools::use_data(ca.elec.almanac, overwrite=TRUE)
