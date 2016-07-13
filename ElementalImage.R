library(tcltk)
library(fields)

# File filter for file.choose
filters <- matrix(c("dat-files",".dat","All","*"),2,2,byrow = TRUE)

# Wrapper for filechooser in windows vs. linux/mac world
filechooser<-function(default = "", caption = "Select files", multi = TRUE, 
                      filters = NULL, index = 1) {
	if(Sys.info()["sysname"] != "Windows") tk_choose.files(default,caption, multi, filters, index)
	else choose.files()
}
	
# Wrapper for dirchooser in windows vs. linux/mac world
dirchooser<-function(default = "", caption = "Select directory") {
	if(Sys.info()["sysname"] == "Linux") tk_choose.dir(default,caption)
	else choose.files()
}
	
#read PyMCAData once per session only
if (length(ls(pattern="^PyMCAData$"))==0) {
	filename <- filechooser(caption="Select Data File", multi = FALSE, filters = filters)
	PyMCAData <- read.table(filename, header=TRUE)
	# the following attributes need to be set by the user (until the instrument writes a spec file)
	# [3] Matrix dimensions (x,y,z) # elements
	attr(PyMCAData, "Matrix") <- c(1,410,430)
	# [3] Stepsize in um (x,y,z)
	attr(PyMCAData, "Stepsize") <- c(0,10,10)
	# [4] clear text name of tube, device # (to be able to distinguish different instruments, currently =1001), KV, mA
	attr(PyMCAData, "Tube") <- c("XOS Fittschen",1001,50,1)
	# [2] Detector Nmae, device # device # (to be able to distinguish different instruments, currently =2001)
	attr(PyMCAData, "Detector") <- c("Vortex Fittschen",2001)
}

#helper to detect element and emission-lines only
stringFilter <- function(s){!(length(grep("^((s.+\\..+)|([^.]*))$",s))>0)}

#list of elements contained in the dat file
elements <- data.frame(matrix(unlist(strsplit(Filter(stringFilter, names(PyMCAData)),"[.]")),ncol=2,byrow=TRUE))

#periodicTable <- read.csv(tk_choose.files(),header=TRUE)
 
#helper to translate variable names to correct greek letters(Ka,Kb) and indices(L1,L2..)
translateElementLine <- function(name) {
  if (is.symbol(name)) {
    ns = as.character(name)
    n1 <- sub("[.]([LM])(.)"," ~ \\1[\\2]",fixed=FALSE,
              sub("[.]Ka"," ~ K * alpha",
                  sub("[.]Kb"," ~ K * beta",name)))
    exp <- parse(text = n1)
    if (length(exp)==1) return(exp[[1]])
  } 
  name
}

translateExpression <- function(e) {
  if (is.symbol(e)) return(translateElementLine(e))
  if (is.call(e)) {
    if(length(e) >= 2) {
      return(as.call(c(e[[1]], lapply(e[-1],translateExpression))))
    }
  }
  e
}

correlationPlot <- function(x,y, data=PyMCAData) {
  m=match.call()
  xname<-translateExpression(substitute(x ~ group("[",count,"]"),list(x=m$x)))
  yname<-translateExpression(substitute(y ~ group("[",count,"]"),list(y=m$y)))
  x <- eval(substitute(x),data,parent.frame())
  y <- eval(substitute(y),data,parent.frame())
  plot(x,y,xlab=xname,ylab=yname)
}

countMap <- function(x,data=PyMCAData) {
  m=match.call()
  xname<-translateExpression(call("*",m$x,' [counts]'))
  x <- eval(substitute(x),data,parent.frame())
  mat <- attr(data,"Matrix")
  dimension <- attr(data,"Stepsize")
  xmax <- mat[[2]]*dimension[[2]]
  ymax <- mat[[3]]*dimension[[3]]
  xs <- seq(0,xmax,by=dimension[[2]])
  ys <- seq(0,ymax,by=dimension[[3]])
  zs <- matrix(x,ncol = mat[[3]])
  image.plot(x=xs, y=ys, zs,
             xlab=quote(group("[",mu * m,"]")),ylab=quote(group("[",mu * m,"]")), asp=1)
  title(xname)
}
