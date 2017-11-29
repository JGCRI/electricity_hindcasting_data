prep.generators.90to00 <- function(startingDir, ran)
{
  dirs<-list.dirs(startingDir, full.names=FALSE)
  cols <- list()

  # each year has files stored in its own directory underneath startingDir
  for (i in seq(1:length(dirs)) ) {
    dir <- dirs[i]
    # extract year from directory
    year <- str_extract(dir, "[1|2][0-9]{3}" )
    if (is.na(year)) {next}
    if (! year %in% ran) {next}

    # get that year's generator-level data file
    filez <- list.files(paste(startingDir, dir, sep="/"))
    if (year %in% seq(1990,1996) ) {
      file <- "TYPE3"
    } else if (year == 1997) {
      file <- "GENERTOR"
    } else if (year %in% seq(1998, 2000) ) {
      file <- "ExistingGenerators"
    }
    genfile <- str_subset(filez, file)

    # read in data
    path <- paste(startingDir, dir, genfile, sep="/")
    if (year == 1996) {
      data.raw <- read.csv(path)
    } else if (year %in% seq(1998,2000)) {
      data.raw <- read_excel(path, sheet=2)
    } else {
      data.raw <- read_excel(path, sheet=1)
    }

    # normalize column names
    names(data.raw) <- names(data.raw) %>%
      toupper() %>%
      gsub("AND", "&", .) %>%
      gsub(" ", "", .) %>%
      gsub("[-_\\.]", "", .) %>%
      tolower()

    # subset cols according to year (format changes)
    if (year %in% seq(1990, 1996)) {

      colinds <- c(2,3,4,5,
                   6,7,9,
                   15,16,18,
                   19,20,22,24,39)
      colnames <- c("multigen", "utilcode", "plntcode", "gencode",
                    "primemover", "nameplate", "status1",
                    "fuel1", "fuel2", "heatrate",
                    "summer", "winter", "status2", "endyr", "startyr")

    } else if (year == 1997) {

      colinds <- c(1,2,3,4,
                   5,6,8,10,
                   13,14,15,16,18,
                   20,37)
      colnames <- c("multigen", "utilcode", "plntcode", "gencode",
                    "primemover", "nameplate", "status1", "startyr",
                    "fuel1", "fuel2", "summer", "winter", "status2",
                    "endyr", "heatrate")

    } else if (year %in% seq(1998, 1999)) {

      colinds <- c(2,3,4,
                   6,7,8,9,
                   10,12,14,16,18,19,22,36)
      colnames <- c("utilcode", "plntcode", "gencode",
                    "primemover", "nameplate", "summer", "winter",
                    "fuel1", "fuel2", "status1", "startyr", "endyr", "multigen", "status2", "heatrate")

    } else if (year == 2000) {

      colinds <- c(2,3,4,
                   6,7,8,9,
                   10,12,14,
                   16,18,19,22,23)
      colnames <- c("utilcode", "plntcode", "gencode",
                    "primemover", "nameplate", "summer", "winter",
                    "fuel1", "fuel2", "status1",
                    "startyr", "endyr", "multigen", "status2", "heatrate")

    }


    # subset/rename cols
    data.sub <- data.raw[,colinds]
    #names(data.sub) <- colnames

    # store data cols - original or renamed?
    cols[[genfile]] <- names(data.sub)
  }
  cols
}
