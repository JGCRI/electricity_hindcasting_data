prep.form860processed <- function(form860rawfile, retirementfile)
{
  ## LOAD DATA
  form860raw <- read.delim(form860rawfile) %>%
    select(-fuel_2, -fuel_3) %>%
    dplyr::rename(fuel = fuel_1) %>%
    mutate(generator_code = tolower(generator_code)) %>%
    mutate(generator_code = gsub(" ","", generator_code)) %>%
    mutate(generator_code = gsub("[^a-zA-Z0-9]","", generator_code)) %>%
    mutate(summer_capacity = as.numeric(summer_capacity),
           prime_mover = as.character(prime_mover),
           status_code_1 = as.character(status_code_1),
           status_code_2 = as.character(status_code_2),
           fuel = as.character(fuel))

  retirement <- read.delim(retirementfile) %>%
    mutate(year = as.character(year)) %>%
    mutate(generator_code = tolower(generator_code)) %>%
    mutate(generator_code = gsub(" ","", generator_code)) %>%
    mutate(generator_code = gsub("[^a-zA-Z0-9]","", generator_code)) %>%
    mutate(year = as.numeric(year),
           retirement = as.character(retirement))

  ## DATA ERRORS
  form860processed <- form860raw %>%
    left_join(retirement, by = c('utility_code', 'plant_code', 'generator_code', 'year')) %>%
    mutate(retirement = ifelse(retirement %in% c('', ' ', '.', 0, 1, 8000, 9999), NA, retirement)) %>%
    mutate(retirement = ifelse(retirement == 95, 1995, retirement)) %>%
    mutate(retirement = ifelse(retirement == 96, 1996, retirement)) %>%
    mutate(retirement = ifelse(retirement == 97, 1997, retirement)) %>%
    # if no retirement data but status=retirement, mark retired as of 1989 (data starts @ 1990)
    mutate(retirement = ifelse(is.na(retirement) & status_code_1=='RE', 1989, retirement)) %>%
    mutate(retirement = ifelse(is.na(retirement) & status_code_2 == 'RE', 1989, retirement)) %>%
    # If the retirement year is greater than the actual year then this is a planned retirement year. If it actually retires then we'll care otherwise not.
    mutate(retirement = ifelse(retirement > year, NA, retirement)) %>%
    mutate(in_service = ifelse(in_service==0, NA, in_service)) %>%
    filter(summer_capacity != 0) %>%
    mutate(summer_capacity = ifelse(summer_capacity < 0, summer_capacity*-1, summer_capacity)) %>% # can't have negative capacity
    mutate(summer_capacity = ifelse(year >= 1990 & year <= 2000, summer_capacity/1000, summer_capacity)) %>% # kW -> Mw
    mutate(fuel = ifelse(fuel=='BL', 'BLQ', fuel)) %>% # assume BL is mistake
    mutate(fuel = ifelse(fuel=='', NA, fuel)) %>%
    mutate(prime_mover = ifelse(prime_mover=='', NA, prime_mover)) %>%
    mutate(prime_mover = toupper(prime_mover)) %>% # one case of ic (instead of IC)
    mutate(heat_rate = ifelse(heat_rate==0, NA, heat_rate))


  ## FILTER STATUS/RETIREMENT
  form860processed <- form860processed %>%
    filter(year <= retirement | is.na(retirement)) %>%
    filter(status_code_1 != "RE" & status_code_2 != "RE") %>%
    filter(status_code_2 != "RA") %>%
    filter(status_code_2 != "CN") %>%
    filter(status_code_2 != "PL" & status_code_2 != "P" & status_code_2 != "IP") %>%
    filter(status_code_2 != "CO" & status_code_2 != "L" & status_code_2 != "T") %>%
    filter(status_code_2 != "U" & status_code_2 != "V" & status_code_2 != "TS" & status_code_1 != "TS") %>%
    filter(status_code_2 != "LE")

  form860processed
}

