prep.generators.90to00 <- function(startingDir, filt, all)
  {
  dirs<-list.dirs(startingDir, full.names=FALSE)
  cols <- list()

  # each year has files stored in its own directory underneath startingDir
  for (i in seq(1:length(dirs)) ) {
    dir <- dirs[i]
    year <- str_extract(dir, "[1|2][0-9]{3}" )
    if (is.na(year)) {next}

    # get that year's files. naming convention differs
    filez <- list.files(paste(startingDir, dir, sep="/"))
    group1 <- c(1990, 1991)
    group2 <- c(1992, 1993, 1994, 1995, 1996)
    group3 <- c(1997)
    group4 <- c(1998, 1999, 2000)
    if (year %in% group1 | year %in% group2) {file <- "TYPE3"}
    if (year %in% group3) {file <- "GENERTOR"}
    if (year %in% group4) {file <- "ExistingGenerators"}
    genfile <- str_subset(filez, file)

    data.raw <- read_excel(paste(startingDir, dir, genfile, sep="/"), sheet=1)
    names(data.raw) <- names(data.raw) %>%
      toupper() %>%
      gsub("AND", "&", .) %>%
      gsub("[_\\.]", "", .)

    if (filt) {
      data.filt <- data.raw %>%
        filter(matches("MULTIGEN") == "") %>%
        select(UTILITYCODE, PLANTCODE, GENERATORCODE, PRIMEMOVER, ENERGYSOURCE1, # ID info
               NAMEPLATECAPACITY, SUMMERCAPABILITY, WINTERCAPABILITY, HEATRATE, # system values
               SERVICETYPE, STATUSCODE, STATUSCODE1, NOTES, SUMMERCAPABILITYFLAG, WINTERCAPABILITYFLAG,# descriptors
               INSERVICEYEAR, STARTOPERATIONYEAR) # years
      # 90 NAMEPLATECAPACITY in kW
    }

    if (all) {
      data.allfilt <- data.raw %>%
        filter(matches("MULTIGEN") == "") %>%
        select(matches("UTIL"), PLANTCODE )
    }

    # store data cols
    cols[[genfile]] <- names(data.raw)
  }
  cols
}
