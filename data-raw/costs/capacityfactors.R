join.cap.gen <- function(cap, gen, na.case) {


  if (na.case) {
    # merge for yr %in% c(2001, 2002)
    # join excludes primemover, so we opt to use CAP pm column in later aggregation
    join.unmapped <- gen %>%
      inner_join( cap, ., # join unmapped datasets by DROPPING PRIMEMOVER
                  by = c("yr", "plntcode", "fuel") ) %>%
      dplyr::rename(primemover = primemover.x) %>%  #use ORIG CAP pm column
      select(-primemover.y, -starts_with("utilcode"), -consumption) # drop ORIG GEN pm column & consumption, ORIG CAP/GEN utilcodes

  } else {
    # merge for ! yr %in% c(2001, 2002)
    # join includes primemover
    join.unmapped <- gen %>%
      inner_join( cap, ., # join unmapped datasets W/O UTILCODE
                  by = c("yr", "plntcode", "primemover", "fuel") ) %>%
      select(-starts_with("utilcode"), -consumption) # drop ORIG CAP/GEN utilcodes, & consumption
  }

  join.unmapped
}

calc.capacityfactors <- function(merged, supFile) {
  data <- merged %>%
    mutate(potentialgeneration = capacity * 8760,
           capacityfactor.raw = generation / potentialgeneration ) %>%
    # CF > 1 is data error. See figs/filterbyCF for analysis how filter(CF < 1) affected data
    mutate(capacityfactor = ifelse(capacityfactor.raw > 1, 1, capacityfactor.raw)) %>%
    filter(capacityfactor >= 0.1) %>%
    select(yr, plntcode, overnightcategory, fuel.general, capacityfactor)

  epm <- read.csv(supFile) %>%
    select(overnightcategory, capacityfactor)

  out <- list(data, epm)
  names(out) <- c("data", "epm")
  return(out)
}
