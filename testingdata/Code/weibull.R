library(VGAM)
library(fitdistrplus)
library(actuar)
setwd("D:\\Studies\\Project\\BugPrediction\\Softwares\\testingdata\\new")
file = "tomcat 5"
plotWeibull <- function(file) {
  r <- paste(file,"csv",sep = ".")
  data <- read.csv(r)
  data <- data[,c(1,9)]
  
  data$Opened <- as.Date(as.POSIXct(data$Opened))
  #releaseDate <- as.Date("2002-09-06")
  #nextRelease <- as.Date("2003-12-03")
  #data <- data[data$Opened > releaseData & data$Opened < nextRelease,]
  rownames(data) <- NULL
  
  #days <- data$Opened - releaseDate
  #days<-as.integer(days/30)
  #data <- cbind(data,days)
    
  #data$days<-as.factor(data$days)
  #a <- summary(data$days)
  #plot(a)
  
  fre <- table(format(data$Opened,"%Y-%m"))
  fre <- as.data.frame(fre)
  y <- fre$Freq / sum(fre$Freq)
  x = 1:length(y)
  fit <-
    nls(y ~ ((a / b) * ((x / b) ^ (a - 1)) * exp(-(x / b) ^ a)),start = list(a = 2,b = 11))
  #fit <-  fitdist(y, "weibull", start=list(shape = 1, scale = 500))
  file <- paste(file,"weibull",sep=" ")
  r <- paste(file,"pdf",sep = ".")
  pdf(r, width = 10, height = 10)
  plot(x,y)
  #plot(fit)
  lines(x,predict(fit))
  dev.off()
  
  return(AIC(fit))
  #for 3.3 1
  #nls(y ~ ((a/b) * ((x/b)^(a-1)) * exp(- (x/b)^a)),start = list(a=2,b=11))
  #a = 4.19;b=15.44
}
input = "tomcat 5"
plotWeibull(input)