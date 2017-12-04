prep.generators.90to00 <- function(startingDir)
{
  dirs<-list.dirs(startingDir, full.names=FALSE)
  generators.90to00 <- data.frame()

  # each year has files stored in its own directory underneath startingDir
  for (i in seq(1:length(dirs)) ) {
    dir <- dirs[i]

    # extract year from directory name
    year <- str_extract(dir, "[1|2][0-9]{3}" )
    if (is.na(year)) {next}

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
    data.raw <- readdata(path, year)

    # subset desired cols (index changes with year)
    data.sub <- subdata(data.raw, year) %>%
      mutate(yr = year) %>%
      select(yr, utilcode, plntcode, gencode, multigen,
             primemover, fuel1, fuel2,
             nameplate, summer, winter, heatrate,
             status1, status2, startyr, endyr)

    # convert capacities & heatrate to MW and BTU/MWH
    data.conv <- data.sub %>%
      mutate(nameplate = nameplate/1000,
             summer = summer/1000,
             winter = winter/1000,
             heatrate = heatrate*1000)

    # filter by nameplate, status, and retirement (only nameplate implemented currently)
    data.filt <- filtdata(data.conv)

    # collapse duplicates -- usually when multiple changes planned, therefore same entry w/ different status2!
    data.remdup <- remdup(data.filt)

    # append to master dataframe (outside for loop)
    generators.90to00 <- rbind(generators.90to00, data.remdup)
  }

  generators.90to00
}


readdata <- function(path, year)
{
  if (year == 1996) {
    data.raw <- read.csv(path)
  } else if (year %in% seq(1998,2000)) {
    data.raw <- read_excel(path, sheet=2)
  } else {
    data.raw <- read_excel(path, sheet=1)
  }
  data.raw
}

subdata <- function(df, year)
{

  # normalize column names
  names(df) <- names(df) %>%
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
  data.sub <- df[,colinds]
  names(data.sub) <- colnames

  data.sub
}

filtdata <- function(df, year)
{
  data.filt <- df %>%
    mutate(
      primemover = ifelse(is.na(primemover), "", primemover),
      fuel1 = ifelse(is.na(fuel1), "", fuel1),
      fuel2 = ifelse(is.na(fuel2), "", fuel2),
      status1 = ifelse(is.na(status1), "", status1),
      status2 = ifelse(is.na(status2), "", status2),
      nameplate = ifelse(nameplate == 0, NA, nameplate),
      summer = ifelse(summer == 0, NA, summer),
      winter = ifelse(winter == 0, NA, winter),
      heatrate = ifelse(heatrate == 0, NA, heatrate)
      ) %>%
    filter( status1 %in% c("OP", "SB") ) %>%
    filter( nameplate != 0 )

  data.filt
}

remdup <- function(df)
{
  data.remdup <- df %>%
    group_by(yr, utilcode, plntcode, gencode, primemover, fuel1, fuel2, nameplate, summer, winter, heatrate,
             status1, startyr, endyr) %>%
    summarise(status2 = paste(status2, sep=", "))

  data.remdup
}
