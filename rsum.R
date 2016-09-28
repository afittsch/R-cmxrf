#
# sliding average of a vector

rsum <- function(ar, n=2) {
  arsum <- cumsum(ar)
  (arsum[(n+1):length(arsum)] - arsum[1:(length(arsum)-n)])/n
}

# numeric derivation x[n]-x[n-1]...
# 
nderiv <- function(ar) {
  ar[2:length(ar)]-ar[1:(length(ar)-1)]
}


fwhm <- function(spectrum) {
  xmax <- max(spectrum, na.rm = TRUE)
  idx <- match(xmax, spectrum)
  x <- 1:length(spectrum)
  x1 <- d$x[d$x < xmax][which.min(abs(d$y[d$x < xmax]-max(d$y)/2))]
  x2 <- d$x[d$x > xmax][which.min(abs(d$y[d$x > xmax]-max(d$y)/2))]
  
}