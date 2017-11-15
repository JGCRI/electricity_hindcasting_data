library(xlsx)
library(stringr)

# 1970-2000 Utilities Generation/Consumption ------------------------------
startingDir<- "data-raw/Generation&Consumption/Utilities (1970-2000)"
filez<-list.files(startingDir,pattern=".xls")
newCols <- list()

for (i in seq(1:length(filez)) ) {
  #i <- 7
  file <- filez[i]
  year <- str_extract(file, "[1|2][0-9]{3}" )
  print(year)

  data.raw <- read.xlsx2(paste(startingDir, file, sep="/"), 1) %>%
    mutate(YEAR = as.numeric(year) ) %>%     # modify in place (originally 2 digits)
    filter(STATUS %in% c("", "A") ) %>%     # active (" ") or new addition ("A")
    select(-starts_with("STK"))
  if (i == 1 ) {canonicalCols <- names(data.raw)}
  if (any(! names(data.raw) %in% canonicalCols)) {newCols[[file]] <- names(data.raw)[! names(data.raw) %in% canonicalCols]}

  ## ELECTRICITY GENERATION
  if (year >= 1996) {
    # after 1996, some had monthly reporting req's -- others annual. the monthly reports use estimates
    # for those with annual reporting reqs. for this reason, we just use the annual reports after 1996.
    # for annual reports, the "generation" col is named differently
    gencols <- "NETGEN"
  } else {
    # sum over 12 GEN## columns (12 monthly reports contained in one workbook)
    gencols <- "GEN"
  }
  data.raw <- mutate_at(data.raw, vars(starts_with(gencols)), as.numeric )
  data.raw$total.generation <- data.raw %>%
    select( starts_with(gencols) ) %>%
    rowSums(.)
  data.raw <- data.raw %>%
    select(-starts_with(gencols) ) %>%
    rename(generation=total.generation)

  ## FUEL CONSUMPTION
  # "consumption" col isn't renamed after 1996, so starts_with() call works over 1970-2000 period
  data.raw <- mutate_at(data.raw, vars(starts_with("CON")), as.numeric )
  data.raw$total.consumption <- data.raw %>%
    select( starts_with("CON") ) %>%
    rowSums(.)
  data.raw <- data.raw %>%
    select(-starts_with("CON") ) %>%
    rename(consumption=total.consumption)

  ## TRUNCATE
  
  data.filter <- data.raw %>%
    select(PMOVER, FUELTYP, YEAR, generation, consumption ) 
  ## MAPPING
  mapping.pm <- data.raw %>%
    select(PMOVER, PMDESC) %>%
    group_by(PMOVER, PMDESC) %>%
    summarise(n=n()) %>%
    arrange(PMOVER)
  mapping.fuel <- data.raw %>%
    select(FUELTYP, FUELDESC) %>%
    group_by(FUELTYP, FUELDESC) %>%
    summarise(n=n()) %>%
    arrange(FUELTYP)

  ## SAVE/APPEND
  if (i == 1) {
    write.table(data.filter, file=paste(startingDir, "aggregation_70_00.csv", sep="/"), sep=",",  append=FALSE, row.names=FALSE)
    write.table(mapping.pm, file=paste(startingDir, "movermismapping.csv", sep="/"), sep=",",  append=FALSE, row.names=FALSE)
    write.table(mapping.fuel, file=paste(startingDir, "fuelmismapping.csv", sep="/"), sep=",",  append=FALSE, row.names=FALSE)
  } else {
    write.table(data.filter, file=paste(startingDir, "aggregation_70_00.csv", sep="/"), sep=",", append=TRUE, row.names=FALSE, col.names= FALSE)
    write.table(mapping.pm, file=paste(startingDir, "movermismapping.csv", sep="/"), sep=",", append=TRUE, row.names=FALSE, col.names= FALSE )
    write.table(mapping.fuel, file=paste(startingDir, "fuelmismapping.csv", sep="/"), sep=",", append=TRUE, row.names=FALSE, col.names= FALSE )
  }
  ## DISTINCT MAPPING CODES
  if (file == filez[length(filez)]) {
    # prime mover
    read.csv(paste(startingDir, "movermismapping.csv", sep="/")) %>%
      group_by(PMOVER, PMDESC) %>%
      summarise(n=sum(n)) %>%
      write.csv(file=paste(startingDir, "movermismapping.csv", sep="/"), row.names=FALSE)
    # fuel
    read.csv(paste(startingDir, "fuelmismapping.csv", sep="/")) %>%
      group_by(FUELTYP, FUELDESC) %>%
      summarise(n=n()) %>%
      write.csv(file=paste(startingDir, "fuelmismapping.csv", sep="/"), row.names=FALSE)
  }
}



