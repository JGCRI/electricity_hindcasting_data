#' Calculate summary time-series of master dataset for variable of interest
#'
#' @param master data.frame, provided in package data
#' @param column unquoted column name, c(capacity, generation, lcoe)
#' @param funcn
#' @return data.frame using keys (oc, fg, yr), reporting yearly averages for column-variable and ultimate averages for missing (oc, fg, yr) rows
#' @export

summaryCalc <- function(master, column, weights=NULL) {
  column <- enquo(column) # convert unquote column name to quosure


  # average value given (oc, fg) for each year
  # only lcoe is weighted (by capacity)
  # if unweighted, provide vector of weights=1. vector must be same length as group
  # if weighted, use plant-capacity to weight (oc, fg, yr)-avg
  summary.avg.yr <- master %>%
    group_by(yr, overnightcategory, fuel.general) %>%
    summarise(column.avg.yr = ifelse(quo_name(column) != "lcoe",
                                     stats::weighted.mean((!!column), rep(1, n())),
                                     stats::weighted.mean((!!column), capacity))) %>%
    ungroup()

  # grab 21 (oc, fg) combos we have a mapping for
  mapping <- mapping %>%
    select(overnightcategory, fuel.general) %>%
    distinct() %>%
    mutate_all(as.character)

  # force row for all possible (oc, fg, yr) combos: 130 possible -> some have NA and need to be filled with (oc, fg)-avg
  # keep only those (oc, fg) combos that appear in the mapping: 16 observed in data
  summary.complete.yr <- summary.avg.yr %>%
    complete(overnightcategory, fuel.general, yr) %>%
    inner_join(mapping, by=c("overnightcategory", "fuel.general"))

  # make summary.complete.yr have two time series:
  ## one with actual LCOE data
  ## the other holds mean(LCOE) wherever there is missing data
  oc.fg.avg <- summary.complete.yr %>%
    group_by(fuel.general, overnightcategory) %>%
    summarise(column.avg = mean(column.avg.yr, na.rm=TRUE)) %>%
    ungroup()
  summary.complete.yr.na <- summary.complete.yr %>%
    filter(is.na(column.avg.yr)) %>%
    select(-column.avg.yr) %>%
    left_join(oc.fg.avg, by=c("fuel.general", "overnightcategory")) %>%
    rename(missing=column.avg)
  summary.complete.yr <- summary.complete.yr %>%
    left_join(summary.complete.yr.na, by=c("yr", "fuel.general", "overnightcategory")) %>%
    rename(!!quo_name(column) := column.avg.yr) # report average values under colname indicated as input
}

#' Plot the summary time-series for variable of interest
#'
#' @param df data.frame, summary data produced by summaryCalc()
#' @param column unquoted column name
#' @param units string, Units of variable
#' @return time-series plot faceted by oc & fg
#' @export
summaryPlot <- function(df, column, units) {

  column <- enquo(column)

  # barchart (w/ points on period average for missing data)
  ggplot(df, aes(x=yr)) + ylab(units) + ggtitle(paste0("average plant ", quo_name(column))) +
    geom_line(aes_string(y = quo_text(column))) +
    geom_point(aes(y = missing)) +
    facet_wrap(~fuel.general + overnightcategory, scales="free") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}

