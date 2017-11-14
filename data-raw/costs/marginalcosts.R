prep.marginalcosts <- function(mapping, formdata, fuelprices, maximumheatrates=NULL)
{
  ## DATA
  generators <- formdata %>%
    select(year, generator_code, fuel, prime_mover, heat_rate) %>%
    left_join(mapping, by=c('prime_mover', 'fuel')) %>%
    filter(! is.na(overnight_category)) %>%
    filter(! is.na(fuel_general)) %>%
    group_by( overnight_category, fuel_general, year) %>%
    summarize(heatrate = mean(heat_rate, na.rm=TRUE)) %>%
    ungroup() %>%
    # if no data for an entire year, avg == NaN
    mutate(heatrate = ifelse(is.nan(heatrate), NA, heatrate))

  ## COMBINE HR
  if (!is.null(maximumheatrates)) {
    generators <- rbind(generators, maximumheatrates)
  }

  ## HR LINEAR REGRESSION
  hr.model <- generators %>%
    filter(heatrate > 0) %>%
    group_by(overnight_category, fuel_general) %>%
    tidyr::complete(year = seq(from = min(generators$year), to = max(generators$year), by = 1)) %>%
    ungroup() %>%
    mutate(time = year - (min(generators$year) - 1)) %>%
    mutate(time_sq = time^2) %>%
    group_by(overnight_category, fuel_general) %>%
    do({mod <- lm(heatrate ~ time + time_sq, data = .)
    # The model is the same as heat_rate ~ time + time_sq + time*overnight_category + time_sq*overnight_category because of the group_by
    heatrate.pred <- predict(mod, newdata = .[c("time", "time_sq")])
    data.frame(., heatrate.pred)}) %>%
    ungroup() %>%
    select( overnight_category, fuel_general, year, heatrate, heatrate.pred)


  ## CALCULATE MARGINAL COSTS
  marginalcosts <- hr.model %>%
    left_join(fuelprices, by = c('fuel_general','year')) %>%
    mutate(marginal.cost = fuel.price*heatrate.pred) %>% # $/Btu * Btu/Kwh = $/Kwh
    mutate(marginal.cost = marginal.cost*1e3) %>% # $/kwh -> $/Mwh
    select(year, fuel_general, marginal.cost)

}
