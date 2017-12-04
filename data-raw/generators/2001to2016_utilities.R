prep.generators.01to16 <- function(startingDir)
{
  dirs<-list.dirs(startingDir, full.names=FALSE)
  fil <- list()
  for (dir in dirs) {
    filez <- list.files(paste(startingDir, dir, sep="/"))
    fil[[dir]] <- filez
  }

  fil
}
