
prep.mapping <- function(mappingFile)
{
  mapping <-  read.csv(mappingFile, stringsAsFactors=F)
  mapping <- mapping %>%
    select(primemover, fuel, fuel_general, matches("overnight"))

  mapping

}
