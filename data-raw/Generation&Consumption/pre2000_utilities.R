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
    select(PMOVER, PMDESC, FUELTYP, FUELDESC, YEAR, generation, consumption ) %>%
    mutate(PMOVER = as.integer(PMOVER)) %>%
    mutate(PMDESC = as.character(PMDESC)) %>%
    mutate(FUELTYP = as.character(FUELTYP)) %>%
    mutate(FUELDESC = as.character(FUELDESC))

  ## MAP FROM FORM759 TO FORM860 CODES
  # many entries have a mismatch between the two native cols used to identify PM and FUEL
  # inner_join() filters through only rows where both cols match the mapping given in the form's documentation
  # then replaces the two cols with a col whose codes correspond to the form860 mapping files
  mapping.pm <- read.csv(paste(startingDir, "movermapping.csv", sep="/")) %>%
    select(PMOVER, PMDESC, prime_mover) %>%
    mutate(PMDESC = as.character(PMDESC))
  mapping.fuel <- read.csv(paste(startingDir, "fuelmapping.csv", sep="/")) %>%
    select(FUELTYP, FUELDESC, fuel) %>%
    mutate(FUELTYP = as.character(FUELTYP)) %>%
    mutate(FUELDESC = as.character(FUELDESC))
  data.filter <- data.filter %>%
    inner_join( mapping.pm, by=c("PMOVER", "PMDESC") ) %>%
    select(-PMOVER, -PMDESC) %>%
    inner_join( mapping.fuel, by=c("FUELTYP", "FUELDESC")) %>%
    select(-FUELTYP, -FUELDESC)

  ## RECORD NATIVE MISMAPPING
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
  ## DISTINCT MAPPING CODES ON LAST RUN
  if (file == filez[length(filez)]) {
    # prime mover
    read.csv(paste(startingDir, "movermismapping.csv", sep="/")) %>%
      group_by(PMOVER, PMDESC) %>%
      summarise(n=sum(n)) %>%
      write.csv(file=paste(startingDir, "movermismapping.csv", sep="/"), row.names=FALSE)
    # fuel
    read.csv(paste(startingDir, "fuelmismapping.csv", sep="/")) %>%
      group_by(FUELTYP, FUELDESC) %>%
      summarise(n=sum(n)) %>%
      write.csv(file=paste(startingDir, "fuelmismapping.csv", sep="/"), row.names=FALSE)
  }
}

util <- read.csv(paste(startingDir, "aggregation_70_00.csv", sep="/")) %>%
  left_join(mapping, by=c("prime_mover", "fuel")) %>%
  select(-prime_mover, -fuel)

year.totals <- util %>%
  group_by(YEAR) %>%
  summarise(total.gen = sum(generation),
            total.cons = sum(consumption))

choice.totals <- util %>%
  group_by(overnight_category, fuel_general, YEAR) %>%
  summarise(choice.gen = sum(generation),
            choice.cons = sum(consumption) )

marketshares <- choice.totals %>%
  left_join(year.totals, by=c("YEAR") ) %>%
  mutate(share.gen = 100 * choice.gen/total.gen,
         share.cons = 100 * choice.cons/total.cons) %>%
  select(-total.gen, -total.cons, -choice.gen, -choice.cons)

