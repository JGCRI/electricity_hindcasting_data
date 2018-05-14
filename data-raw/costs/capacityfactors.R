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
    select(-capacity, -generation, -potentialgeneration, -capacityfactor.raw) %>%
    mutate(fill = 0) # signals CF doesn't need to be filled

  epm <- read.csv(supFile) %>%
    select(overnightcategory, fuel.general, EPM=capacityfactor) %>%
    mutate(fill = 1) # used to join to rows that need CF filled (see complete() below)

  data.fill <- data %>%
    # for each startyr in data, we want an entry for all native combinations of fg-oc
    # missing entries are filled with values determined by parameter fill
    complete(startyr, nesting(fuel.general, overnightcategory),
             fill = list(plntcode = NA,
                         fill = 1)) %>%
    left_join(epm, by=c("overnightcategory", "fuel.general", "fill")) %>%
    mutate(capacityfactor = ifelse(fill==1, EPM, capacityfactor))

  out <- list(data.fill, epm)
  names(out) <- c("data.fill", "epm")
  return(out)
}
