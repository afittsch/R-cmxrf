library(tcltk)
library(fields)



filters <- matrix(c("dat-files",".dat","All","*"),2,2,byrow = TRUE)

filechooser<-function(default = "", caption = "Select files", multi = TRUE, 
                      filters = NULL, index = 1) {
	if(Sys.info()["sysname"] != "Windows") tk_choose.files(default,caption, multi, filters, index)
	else choose.files()
}
	

dirchooser<-function(default = "", caption = "Select directory") {
	if(Sys.info()["sysname"] == "Linux") tk_choose.dir(default,caption)
	else choose.files()
}
	

if (length(ls(pattern="^PyMCAData$"))==0) {
	filename <- filechooser(caption="Select Data File", multi = FALSE, filters = filters)
	PyMCAData <- read.table(filename, header=TRUE)
}

stringFilter <- function(s){!(length(grep("^((s.+\\..+)|([^.]*))$",s))>0)}


elements <- data.frame(matrix(unlist(strsplit(Filter(stringFilter, names(PyMCAData)),"[.]")),ncol=2,byrow=TRUE))

#periodicTable <- read.csv(tk_choose.files(),header=TRUE)

translateElementLine <- function(name) {
  n1 <- sub("[.]([LM])(.)"," ~ \\1[\\2]",fixed=FALSE,
            sub("[.]Ka"," ~ K * alpha",
                sub("[.]Kb"," ~ K * beta",name)))
  parse(text = n1)
}

correlationPlot <- function(x,y, data=PyMCAData) {
  m=match.call()
  xname<-translateElementLine(paste(m$x,"~ '[counts]'"))
  yname<-translateElementLine(paste(m$y,"~ '[counts]'"))
  x <- eval(substitute(x),data,parent.frame())
  y <- eval(substitute(y),data,parent.frame())
  plot(x,y,xlab=xname,ylab=yname)
}

