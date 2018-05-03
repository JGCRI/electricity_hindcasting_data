join.cap.gen <- function(cap, gen, na.case) {

  # joined unmapped CAP & GEN datasets
  # neither case includes utilcode in the join b/c of inconsistent utilcodes between data sets

  if (na.case) {
    # merge CAP w/ GEN data that contains NA in primemover (this occurs exclusively for 2001-2002)
    join.unmapped <- inner_join( cap, gen,
                  # in order to match, join must exclude primemover
                  by = c("yr", "plntcode", "fuel") ) %>%
      # use CAP pm column
      dplyr::rename(primemover = primemover.x) %>%
      # drop GEN pm column & both utilcode cols
      select(-primemover.y, -starts_with("utilcode"))

  } else {
    # merge CAP w/ GEN that doesn't contain NA in primemover
    join.unmapped <- gen %>%
      inner_join( cap, .,
                  by = c("yr", "plntcode", "primemover", "fuel") ) %>%
      # drop both utilcode cols
      select(-starts_with("utilcode"))
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
    select(-capacity, -generation, -potentialgeneration, -capacityfactor.raw)

  epm <- read.csv(supFile) %>%
    select(overnightcategory, capacityfactor)

  out <- list(data, epm)
  names(out) <- c("data", "epm")
  return(out)
}
