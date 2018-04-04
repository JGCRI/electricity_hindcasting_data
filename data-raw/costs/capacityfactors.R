join.cap.gen <- function(cap.vintage, gen)
{
  # aggregate over vintage (unique to capacity dataset)
  cap <- cap.vintage %>%
    group_by(yr, utilcode, plntcode, primemover, fuel) %>%
    summarise(capacity = sum(capacity)) %>%
    ungroup()

  # merge for ! yr %in% c(2001, 2002)
  # join includes primemover
  join.rest <- gen %>%
    filter(! yr %in% c(2001, 2002)) %>%
    inner_join( cap, ., # join unmapped datasets W/O UTILCODE
                by = c("yr", "plntcode", "primemover", "fuel") ) %>%
    select(-starts_with("utilcode"), -consumption) # drop ORIG CAP/GEN utilcodes, & consumption

  # merge for yr %in% c(2001, 2002)
  # join excludes primemover, so we opt to use CAP pm column in later aggregation
  join.01.02 <- gen %>%
    filter( yr %in% c(2001, 2002)) %>%
    inner_join( cap, ., # join unmapped datasets by DROPPING PRIMEMOVER
                by = c("yr", "plntcode", "fuel") ) %>%
    dplyr::rename(primemover = primemover.x) %>%  #use ORIG CAP pm column
    select(-primemover.y, -starts_with("utilcode"), -consumption) # drop ORIG GEN pm column & consumption, ORIG CAP/GEN utilcodes

  # bind joined data sets together
  join.unmapped <- rbind(join.rest, join.01.02)

  # map to oc-fg, and aggregate
  join.mapped <- join.unmapped %>%
    left_join(mapping, by=c("primemover", "fuel")) %>% # map datasets to oc-fg
    select(-primemover, -fuel) %>% # aggregate redundant mappings (pm, f) -> (oc, fg)
    # no longer grouping by utilcode b/c two versions (.x, .y)
    group_by(yr, plntcode, overnightcategory, fuel.general) %>%
    summarise(capacity = sum(capacity),
              generation = sum(generation)) %>%
    ungroup()

  join <- list(join.unmapped, join.mapped)
  names(join) <- c("unmapped", "mapped")
  # join.mapped used in later calculations

  join
}

calc.capacityfactors <- function(cap.gen.joined)
{
  cf <- cap.gen.joined %>%
    filter(generation > 0) %>% # drop generators that used more energy than they produced
    mutate(potentialgeneration = capacity * 8760) %>%
    mutate(capacityfactor = generation/potentialgeneration) %>%
    select(yr, plntcode, overnightcategory, fuel.general, capacityfactor)

  cf.clamp <- cf %>%
    mutate(capacityfactor = ifelse(capacityfactor > 1, 1, capacityfactor))
  # CF > 1 is data error. See figs/filterbyCF for analysis how filter(CF < 1) affected data

  data <- list(cf, cf.clamp)
  names(data) <- c("cf", "cf.clamp")
  # cf.clamp used in later calculations

  data
}
