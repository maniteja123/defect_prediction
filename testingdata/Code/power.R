library(VGAM)
library(fitdistrplus)
library(actuar)
setwd("D:\\Studies\\Project\\BugPrediction\\Softwares\\testingdata")
nextReleaseDates = data.frame( 
  tomcat3.3="2002-09-06",
  tomcat4.1="2003-12-03",
  tomcat5="2007-02-28",
  tomcat6="2011-01-14",
  tomcat7="2014-06-25" )
files = c("tomcat 3.3","tomcat 4.1","tomcat 5","tomcat 6")

plotPower <- function(file) {
  r <- paste(file,"csv",sep = ".")
  data <- read.csv(r)
  data <- data[,c(1,9)]
  nextRelease <- as.Date(nextReleaseDates[file][1,],format = "%Y-%m-%d")
  data$Opened <- as.Date(as.POSIXct(data$Opened,format = "%d-%m-%y"))
  data <- data[data$Opened < nextRelease,]
  
  rownames(data) <- NULL
  
  fre <- table(format(data$Opened,"%Y-%m"))
  fre <- as.data.frame(fre)
  y <- fre$Freq
  x = 1:length(y)
  
  
  fit <-  fitdist(y, "power", start=list(shape = 1, scale = 500))
  
  file <- paste(file,"power fitdist",sep=" ")
  r <- paste(file,"pdf",sep = ".")
  pdf(r, width = 10, height = 10)
  plot(fit)
  dev.off()
  
  return(summary(fit))
}

r <- paste(file,"csv",sep = ".")
data <- read.csv(r)
data <- data[,c(1,9)]
nextRelease <- as.Date(nextReleaseDates[file][1,],format = "%Y-%m-%d")
data$Opened <- as.Date(as.POSIXct(data$Opened,format = "%d-%m-%y"))
data <- data[data$Opened < nextRelease,]

rownames(data) <- NULL

fre <- table(format(data$Opened,"%Y-%m"))
fre <- as.data.frame(fre)
y <- fre$Freq
x = 1:length(y)


fit <-  fitdist(y, "pow", start=list(shape = 1, scale = 500))
