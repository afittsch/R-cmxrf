# Scripts to calculate beamwith with of a Knife-Edge-Scan
#
# Example:
# PyMCAData contains a PyMCA Result-File (Batchfitting, dat-file)
#knifeEdge(PyMCAData$Fe.Ka,width=2)
#In this example the stepsize of the data-points is 2um (width=2).

library(smoother)


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

#calculate fwhm from a curve (one maximum, positive) 
fwhm <- function(spectrum) {
  spec <-spectrum$y
  xmax <- which.max(spec)
  ymax <- spec[xmax]
  ymax2 <-ymax/2
  
  
  ind <- seq_along(spec)
  x1 <- ind[ind<xmax][which.min((spec[ind<xmax]-ymax2)<0)-1]
  x2 <- ind[ind>xmax][which.min((spec[ind>xmax]-ymax2)>0)-1]
  
  x1=spectrum$x[x1]+(spectrum$x[x1+1]-spectrum$x[x1])*(ymax2-spec[x1])/(spec[x1+1]-spec[x1])
  x2=spectrum$x[x2]+(spectrum$x[x2+1]-spectrum$x[x2])*(ymax2-spec[x2])/(spec[x2+1]-spec[x2])
  list(FWHM=x2-x1,FWHM.coord=list(x=c(x1,x2), y=c(ymax2,ymax2)),FWHM.plot=spec)
}

#calculate (and plot) the width of a beam observed with a knife edge.
knifeEdge <- function(scan,width,window=0.15,plotit=TRUE) {
  smthSpect <- smth(scan,window = window)
  ndr <-nderiv(smthSpect)
  sp <- list(x=seq_along(ndr)*width,y=ndr)
  bw <- fwhm(sp)
  bw$smoothSpec <- smthSpect
  #print (bw)
  #print(sp)
  if (plotit) {
    plot(sp,
         xlab=expression("Travel" ~ group("[",mu*m,"]")),
         ylab=expression(Delta*Counts/mu*m))
    
    lines(sp)
    lines(bw$FWHM.coord)
    text(sum(bw$FWHM.coord$x)/2,bw$FWHM.coord$y[1],bquote(.(round(bw$FWHM,1)) ~ mu*m),adj=c(0.5,0))
  }
  bw
}