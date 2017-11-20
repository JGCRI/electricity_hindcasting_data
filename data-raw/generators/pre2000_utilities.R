# 1990-2000 Utilities Form860 ------------------------------
library(stringr)
library(xlsx)
startingDir<- "data-raw/generators/Utilities (1990-2000)"
dirs<-list.dirs(startingDir, full.names=FALSE)
cols <- list()

for (i in seq(1:length(dirs)) ) {
  #i <- 7
  dir <- dirs[i]
  year <- str_extract(dir, "[1|2][0-9]{3}" )
  if (is.na(year)) {next}

  filez <- list.files(paste(startingDir, dir, sep="/"))
  group1 <- c(1990, 1991)
  group2 <- c(1992, 1993, 1994, 1995, 1996)
  group3 <- c(1997)
  group4 <- c(1998, 1999, 2000)
  if (year %in% group1 | year %in% group2) {pattern <- "TYPE3"}
  if (year %in% group3) {pattern <- "GENERTOR"}
  if (year %in% group4) {pattern <- "ExistingGenerators"}
  genfile <- str_subset(filez, pattern)

  data.raw <- read.xlsx2(paste(startingDir, dir, genfile, sep="/"), 1)
  if (year %in% group1) {
    cols[[genfile]] <- names(data.raw)
  }

  else(break)
}

if (year == 1990) {cols <- append(cols, names(data.raw)) %>% unlist()}
else if (year %in% group1) {
  if(any(!names(data.raw) %in% cols)) {
    print(year)
    print(names(data.raw)[!names(data.raw) %in% cols])
  }
}
else (break)
