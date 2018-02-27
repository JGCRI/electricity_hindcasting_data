
prep.mapping <- function(fuelfile, techfile)
{
  fuel <- read.csv(fuelfile)
  tech <- read.csv(techfile)
  mapping <- full_join(fuel, tech, by=c("primemover", "fuel"))
  mapping

}
