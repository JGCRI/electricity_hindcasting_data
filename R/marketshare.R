marketshares <- function(generators, mapping, type, additions=FALSE)
{
  generators <- generators %>%
    inner_join(mapping, by=c("prime_mover", "fuel") ) %>%
    select(year, in_service, summer_capacity, overnight_category, fuel_general) %>%
    filter(complete.cases(.))

  ## NUCLEAR new additions in 1990-1996 work at fractions of MW, but buncha generators from 60s/70s reach ~8000 capacity
  if (additions) {
    generators <- generators %>%
      filter(year == in_service)
  }

  ## CALCULATE SHARES
  
  # given electricity market, choice of fuel
  if (type=='fuel') {
    market <- generators %>%
      group_by(year) %>%
      summarise(fulladd = sum(summer_capacity) )
    choice <- generators %>%
      group_by(year, fuel_general) %>%
      summarise(choiceadd = sum(summer_capacity) )
    # calculate choice's share of market capacity
    share <- choice %>%
      left_join(market, by="year" ) %>%
      mutate(share = choiceadd/fulladd) %>%
      select(year, fuel_general, share)
  }

  # given fuel market, choice of tech/overnight_category
  if (type == 'tech') {
    market <- generators %>%
      group_by(year, fuel_general) %>%
      summarise(fulladd = sum(summer_capacity) )
    choice <- generators %>%
      group_by(year, fuel_general, overnight_category) %>%
      summarise(choiceadd = sum(summer_capacity) )
    # calculate choice's share of market capacity
    share <- choice %>%
      left_join(market, by=c("year", "fuel_general") ) %>%
      mutate(share = choiceadd/fulladd) %>%
      select(year, fuel_general, overnight_category, share) %>%
      arrange(year, fuel_general, overnight_category, share)
  }

  ## PLOT 
  if (type == "fuel") {
    plot <- share %>%
      ggplot(aes(x=year, y=share)) +
      geom_col( aes(fill=fuel_general) )
      title <- "Fuel shares within electricity market"
  }

  if (type == "tech") {
    plot <- share %>%
    ggplot(aes(x=year, y=share)) +
      geom_col( aes(fill=overnight_category) ) +
      facet_wrap(~fuel_general)
      title <- "Tech shares given choice of fuel"

  }

  if (additions) {
    title <- paste(title, "(Capacity Additions Only)", sep=" ")
  }


  plot <- plot +
    ggtitle(title) +
    theme(axis.text.x=element_text(angle=45, hjust=1))
  print(plot)
  return(share)

}
