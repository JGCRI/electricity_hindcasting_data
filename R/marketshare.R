#' Calculate electricity-generating capacity market shares
#'
#' @param generators Generator-level data holding capacity numbers
#' @param mapping Map overnightcategory and fuel.general onto primemover and fuel
#' @param type "Fuel" market (top-level) or "tech" market (nested)
#' @param additions Logical indicating whether to calculate share of capacity additions for each year
#'
#' @return
#' @export
marketshares <- function(generators, mapping, type, additions=FALSE)
{
  generators <- generators %>%
    inner_join(mapping, by=c("primemover", "fuel") ) %>%
    select(yr, startyr, nameplate, overnightcategory, fuel.general) %>%
    mutate(overnightcategory = ifelse(overnightcategory=="", NA, overnightcategory),
           fuel.general = ifelse(fuel.general=="", NA, fuel.general) ) %>%
    filter(complete.cases(.)) %>%
    dplyr::rename(capacity=nameplate)

  ## NUCLEAR new additions in 1990-1996 work at fractions of MW, but buncha generators from 60s/70s reach ~8000 capacity
  if (additions) {
    generators <- generators %>%
      filter(yr == startyr)
  }

  ## CALCULATE SHARES

  # given electricity market, choice of fuel
  if (type=='fuel') {
    market <- generators %>%
      group_by(yr) %>%
      summarise(fulladd = sum(capacity) )
    choice <- generators %>%
      group_by(yr, fuel.general) %>%
      summarise(choiceadd = sum(capacity) )
    # calculate choice's share of market capacity
    share <- choice %>%
      left_join(market, by="yr" ) %>%
      mutate(share = choiceadd/fulladd) %>%
      select(yr, fuel.general, share)
  }

  # given fuel market, choice of tech/overnightcategory
  if (type == 'tech') {
    market <- generators %>%
      group_by(yr, fuel.general) %>%
      summarise(fulladd = sum(capacity) )
    choice <- generators %>%
      group_by(yr, fuel.general, overnightcategory) %>%
      summarise(choiceadd = sum(capacity) )
    # calculate choice's share of market capacity
    share <- choice %>%
      left_join(market, by=c("yr", "fuel.general") ) %>%
      mutate(share = choiceadd/fulladd) %>%
      select(yr, fuel.general, overnightcategory, share) %>%
      arrange(yr, fuel.general, overnightcategory, share)
  }

  ## PLOT
  if (type == "fuel") {
    plot <- share %>%
      ggplot(aes(x=yr, y=share)) +
      geom_col( aes(fill=fuel.general) )
      title <- "Fuel shares within electricity market"
  }

  if (type == "tech") {
    plot <- share %>%
    ggplot(aes(x=yr, y=share)) +
      geom_col( aes(fill=overnightcategory) ) +
      facet_wrap(~fuel.general)
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
