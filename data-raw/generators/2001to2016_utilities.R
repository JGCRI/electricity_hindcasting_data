prep.generators.01to16 <- function(startingDir)
{
  dirs<-list.dirs(startingDir, full.names=FALSE)
  generators.01to16 <- data.frame()

  # each year has files stored in its own directory underneath startingDir
  for (dir in dirs) {
    # extract year from directory name
    yr.ind <- str_extract(dir, "[2][0-9]{3}" ) %>% as.numeric()
    if (is.na(yr.ind)) {next}

    # get that year's generator-level data file
    filez <- list.files(paste(startingDir, dir, sep="/")) # %>% tolower()
    if (yr.ind %in% seq(2001,2008) ) {
      file <- "^G[Ee][Nn]Y0[0-9].xls$"
    } else if (yr.ind %in% seq(2009, 2012) ) {
      file <- "^Generator[s]?Y[2]?[0]?\\d\\d.xls[x]?$"
    } else if (yr.ind %in% seq(2013, 2016) ) {
      file <- "^3_1_Generator_Y20\\d\\d.xlsx$"
    }
    genfile <- str_subset(filez, file)

    # read in data
    path <- paste(startingDir, dir, genfile, sep="/")
    data.raw <- readdata(path, yr.ind)

    # subset desired cols (index changes with year)
    data.sub <- subdata(data.raw, yr.ind) %>%
      mutate(yr = yr.ind,
             heatrate = NA,
             status2 = "") %>% # not included in 01to16 but present in 90to00
      select(yr, utilcode, plntcode, gencode, multigen,
             primemover, fuel, fuel2,
             nameplate, summer, winter, heatrate,
             status1, status2, startyr, endyr)

    # normalize datatype of cols, then filter by nameplate and status
    data.filt <- filtdata(data.sub)

    # append to master dataframe (outside for loop)
    if (nrow(generators.01to16) == 0 ) {
      generators.01to16 <- data.filt
    } else {
      generators.01to16 <- rbind(generators.01to16, data.filt)
    }
  }

  generators.01to16
}

readdata <- function(path, year)
{
  if (year %in% seq(2001, 2010) ) {
    data.raw <- read_excel(path, sheet=1, skip=0)
  } else if (year %in% seq(2011,2016)) {
    data.raw <- read_excel(path, sheet=1, skip=1)
  }
  data.raw
}

subdata <- function(df, year)
{
  names(df) <- names(df) %>%
    tolower() %>%
    gsub(" ", "", .) %>%
    gsub("[-_\\.\\(\\)]", "", .) %>%
    tolower()

  ut <- "utili?t?y?(?:id|code)"
  pl <- "pl[a]?ntcode"
  gen <- "gene?r?a?t?o?r?(?:id|code)"
  mgen <- "(?:multigen|unitcode)"
  pm <- "^primemover$"
  f1 <- "^ene?r?g?y?source1$"
  f2 <- "^ene?r?g?y?source2$"
  np <- "^nameplatec?a?p?a?c?i?t?y?m?w?$"
  sum <- "^summe?r?cap"
  win <- "^winte?r?cap"
  s1 <- "status"
  syr <- "(?:insv|operating)year"
  eyr <- "(?:^plannedretirementyear$|retireyear)" # future
  eyr2 <- "^retirem?e?n?t?year$" # already happened

  df.sub <- df %>%
    select(utilcode = matches(ut),
           plntcode = matches(pl),
           gencode = matches(gen),
           multigen = matches(mgen),
           primemover = matches(pm),
           fuel = matches(f1),
           fuel2 = matches(f2),
           nameplate = matches(np),
           summer = matches(sum),
           winter = matches(win),
           status1 = matches(s1),
           startyr = matches(syr),
           endyr = matches(eyr) ) %>%
    mutate(utilcode = toupper(as.character(utilcode)),
           plntcode = toupper(as.character(plntcode)),
           gencode = toupper(as.character(gencode)),
           multigen = toupper(as.character(multigen)),
           primemover = toupper(as.character(primemover)),
           nameplate = as.numeric(nameplate),
           summer = as.numeric(summer),
           winter = as.numeric(winter),
           fuel = toupper(as.character(fuel)),
           fuel2 = toupper(as.character(fuel2)),
           status1 = toupper(as.character(status1)),
           startyr = as.numeric(startyr),
           endyr = as.numeric(endyr) )

  df.sub
}

filtdata <- function(df, year)
{
  data.filt <- df %>%
    mutate(
      primemover = ifelse(is.na(primemover), "", primemover),
      fuel = ifelse(is.na(fuel), "", fuel),
      fuel2 = ifelse(is.na(fuel2), "", fuel2),
      status1 = ifelse(is.na(status1), "", status1),
      status2 = ifelse(is.na(status2), "", status2),
      nameplate = ifelse(nameplate == 0, NA, nameplate),
      summer = ifelse(summer == 0, NA, summer),
      winter = ifelse(winter == 0, NA, winter),
      heatrate = ifelse(heatrate == 0, NA, heatrate)
    ) %>%
    filter( status1 %in% c("OP", "SB") ) %>%
    filter( !is.na(nameplate))

  data.filt
}
