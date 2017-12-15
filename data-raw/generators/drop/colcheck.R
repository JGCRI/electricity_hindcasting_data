allcols <- or(ut, pl, gen, pm, f1, f2, np, sum, win, s1, syr, eyr)

getcols <- function(pat)
{
  lapply(cols, str_subset, pat)
}

no <- list()
for (i in seq(1:length(cols))) {
  set <- cols[[i]]
  set.yes <- str_detect(set, allcols)
  no[[names(cols)[i]]] <- set[!set.yes]
}

