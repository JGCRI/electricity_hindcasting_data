peakfinder <- function(d){
  dh <- hist(d,plot=FALSE)
  ins <- dh[["intensities"]]
  nbins <- length(ins)
  ss <- which(rank(ins)%in%seq(from=nbins-2,to=nbins)) ## pick the top 3 intensities
  dh[["mids"]][ss]
}

peaks <- peakfinder(form860CAsupplemented$summer_capacity)

hist(data)
sapply(peaks,function(x) abline(v=x,col="red"))