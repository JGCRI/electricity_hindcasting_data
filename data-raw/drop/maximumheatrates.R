
# Data --------------------------------------------------------------------

efficiencies <- read.csv('data-raw/maximum-efficiencies.csv') %>%
  select(-best, -rate, -Technology)
# Source: The data is from Table 5 of 'Cost of power or power of cost: a US modeling perspective' by Muratori, Ledna, McJeon, et al.
# We can use this data to put some upper bounds on the heat rate gains by power plant type


# Reshape & Convert -------------------------------------------------------

efficiencies <- melt(efficiencies, id.vars = c("overnight_category", "fuel_general"))
colnames(efficiencies) <- c("overnight_category", "fuel_general", "year", "efficiency")
maximumheatrates <- efficiencies %>%
  filter(!is.na(overnight_category) & !is.na(fuel_general)) %>%
  mutate(year = gsub('year.', '', year)) %>%
  filter(year != 2015) %>%
  mutate(heatrate = 3412/efficiency) %>% # btu/kWh = 3412
  select(-efficiency) %>%
  mutate(year = as.numeric(year))

devtools::use_data(maximumheatrates, overwrite=TRUE)
