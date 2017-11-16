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


  # define market nesting by grouping
  if (type=='total') {
    market <- generators %>%
      group_by(year)
    join ='year'
  }
  if (type == 'fuel') {
    market <- generators %>%
      group_by(year, fuel_general)
    join = c("year", "fuel_general")
  }
  if (type == 'tech') {
    market <- generators %>%
      group_by(year, overnight_category)
    join = c("year", "overnight_category")
  }

  # calculate full market capacity
  market <- market %>%
    summarise(fulladd = sum(summer_capacity) )

  # calculate choice's (fuel, tech, or both) capacity
  choice <- generators %>%
    group_by(year, overnight_category, fuel_general) %>%
    summarise(choiceadd = sum(summer_capacity) )

  # calculate choice's share of capacity
  share <- choice %>%
    left_join(market, by=join ) %>%
    mutate(share = choiceadd/fulladd) %>%
    select(year, overnight_category, fuel_general, share)


  ## PLOT
  if (type == "total") {
    plot <- share %>%
      mutate(choice=paste(overnight_category, fuel_general, sep=" - ")) %>%
      ggplot(aes(x=year, y=share)) +
      geom_col( aes(fill=choice) ) +
      ggtitle("Electricity Generation Market Shares") +
      theme(axis.text.x=element_text(angle=45, hjust=1))
  }

  if (type == "fuel") {
    plot <- share %>%
      ggplot(aes(x=year, y=share)) +
      geom_col( aes(fill=overnight_category) ) +
      facet_wrap(~fuel_general) +
      ggtitle("Nested Tech Shares given Fuel") +
      theme(axis.text.x=element_text(angle=45, hjust=1))

  }

  if (type == "tech") {
    plot <- share %>%
    ggplot(aes(x=year, y=share)) +
      geom_col( aes(fill=fuel_general) ) +
      facet_wrap(~overnight_category) +
      ggtitle("Nested Fuel Shares given Tech") +
      theme(axis.text.x=element_text(angle=45, hjust=1))

  }

  print(plot)
  return(share)

}
