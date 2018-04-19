
prep.mapping <- function(mappingFile)
{
  mapping <- mappingFile %>%
    read.csv()
  mapping <- mapping %>%
    select(primemover, fuel, fuel_general, matches("overnight"))

  mapping

}
