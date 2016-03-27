library(VGAM)
library(fitdistrplus)
library(actuar)
library(MLmetrics)
library(PMCMR)
library(scmamp)
library(Hmisc)
setwd("D:\\Studies\\Project\\BugPrediction\\Softwares\\testingdata")

releaseDates = data.frame( 
  tomcat3.3="2001-03-06",
  tomcat4.1="2002-09-06",
  tomcat5="2003-12-03",
  tomcat6 = "2007-02-28",
  tomcat7 = "2011-01-14",
  tomcat8 = "2014-06-25",
  firefox1="2004-11-09",
  firefox2="2006-08-24",
  firefox3="2008-06-17",
  firefox4="2011-03-22",
  firefox5="2011-06-21",
  libre3 = "2010-09-28",
  libre4 ="2013-02-07",
  freebsd2 = "1994-11-01",
  freebsd3 = "1998-10-01",
  freebsd4 = "2000-03-01",
  netbeans4 = "2004-12-01",
  netbeans5 = "2006-01-01",
  netbeans6 = "2007-12-03",
  netbeans7 ="2011-04-19",
  netbeans8 ="2014-03-18",
  wireshark1.0 = "2008-03-31",
  wireshark1.4 = "2010-07-30",
  wireshark1.6 = "2011-06-07",
  wireshark1.8 = "2012-06-21")

nextReleaseDates = data.frame( 
  tomcat3.3="2002-09-06",
  tomcat4.1="2003-12-03",
  tomcat5="2005-02-28",
  tomcat6="2011-01-14",
  tomcat7="2014-06-25",
  firefox1="2006-10-24",
  firefox2="2008-06-17",
  firefox3="2011-03-22",
  firefox4="2011-06-21",
  firefox5="2011-08-11" ,
  libre3 = "2013-02-07",
  libre4 = "2015-07-01",
  freebsd2 = "1998-10-01",
  freebsd3 = "2000-03-01",
  freebsd4 = "2003-01-01",
  netbeans4 = "2005-05-01",
  netbeans5 = "2006-10-30",
  netbeans6 = "2008-04-28",
  netbeans7 ="2012-01-05",
  netbeans8 ="2015-11-04")

dists = c("pareto","weibull","exp","gamma","lnorm")

parse <- function(file) {
  r <- paste(file,"csv",sep = ".")
  data <- read.csv(r)
  data <- data[,c(1,5)]
  data$Opened <- as.Date(as.POSIXct(data$Opened,format = "%Y-%m-%d")) #others
  #data$Opened <- as.Date(as.POSIXct(data$Opened,format = "%d-%m-%y")) #libre,tomcat 3- 6,eclipse2.0.1
  #View(data)
  release <- as.Date(releaseDates[file][1,],format = "%Y-%m-%d")
  #nextRelease <- as.Date(nextReleaseDates[file][1,],format = "%Y-%m-%d")
  #data <- data[data$Opened > release & data$Opened < nextRelease,]
  data <- data[data$Opened > release,] #firefox4, tomact 6,7,8, wireshark
  #data <- data[ data$Opened < nextRelease,]
  rownames(data) <- NULL
  #View(data)
  fre <- table(format(data$Opened,"%Y-%m"))
  fre <- as.data.frame(fre)
  #View(fre)
  y <- fre$Freq
  return(y) 
  return (head(y,10)) #freebsd , netbeans3, 8, tomcat 6,7,8
  return (head(y,20)) # eclipse1
}

plotDist <- function(file, model) {
  y <- parse(file)
  #y <- head(y,10)
  #View(y)
  x = 1:length(y)
  
  if(model=="pareto") {
    fit <-  fitdist(y, "pareto", start=list(shape = 1, scale = 500))
    file <- paste(file,"pareto fitdist",sep=" ")
  } else if(model=="weibull"){
    fit <-  fitdist(y, "weibull", start=list(shape = 1, scale = 500))
    file <- paste(file,"weibull fitdist",sep=" ")
  } else if(model=="gamma") {
    fit <-  fitdist(y, "gamma", start=list(shape = 1, scale = 500))
    file <- paste(file,"gamma fitdist",sep=" ")
  } else if(model=="exp") {
    fit <-  fitdist(y, "exp")
    file <- paste(file,"exp fitdist",sep=" ")
  } else if(model=="gpd") {
    fit <-  fitdist(y, "gpd", start=list(shape = 1, scale = 5000))
    file <- paste(file,"gpd fitdist",sep=" ")
  } else if(model=="lnorm") {
    fit <-  fitdist(y, "lnorm")
    file <- paste(file,"lnorm fitdist",sep=" ")
  }
  
  #r <- paste(file,"pdf",sep = ".")
  #pdf(r, width = 10, height = 10)
  #plot(fit)
  #dev.off()
  
  return(fit)
}

getError <- function(model) {
  data <- model$data
  y <- as.data.frame(data)
  x <- 1:length(data)
  y$sum <- cumsum(data)/sum(data)
  if(model$distname=="pareto") {
    y$cdf <- ppareto(x,model$estimate[1],model$estimate[2])
  } else if(model$distname=="weibull"){
    y$cdf <- pweibull(x,model$estimate[1],model$estimate[2])
  } else if(model$distname=="gamma") {
    y$cdf <- ppareto(x,model$estimate[1],model$estimate[2])
  } else if(model$distname=="exp") {
    y$cdf <- pexp(x,model$estimate[1])
  } else if(model$distname=="gpd") {
    y$cdf <- pgpd(x,model$estimate[1],model$estimate[2]) 
  } else if(model$distname=="lnorm") {
    y$cdf <- plnorm(x,model$estimate[1],model$estimate[2])
  }
  se = de = tv = pv = 0
  mean = mean(y$sum)
  
  for(i in 1:model$n){
    se = se + (y$cdf[i] - y$sum[i])^2
    de = de + (y$sum[i] - mean)^2
    tv = tv + (y$sum[i])^2
    pv = pv + (y$cdf[i])^2
  }
  
  rmse = sqrt(se/model$n)
  rrse = sqrt(se/de)
  r2 = 1 - se/de
  #rmse = RMSE(y$sum,y$cdf)
  #rrse = RRSE(y$sum,y$cdf))
  #r2 = R2_score(y$sum,y$cdf)
  theil1 = rmse / (sqrt(tv/model$n) + sqrt(pv/model$n))
  theil2 = rmse / sqrt(tv/model$n)
  return (c(rmse,rrse,theil1,theil2,r2))
}

getStats <- function(model) {
  summ <- summary(model)
  summary <- as.data.frame(as.array(summ[1:length(summ)]))
  colnames(summary) <- c("val")
  return(summary)
}

getGof <- function(model) {
  gof <- gofstat(model)
  gof <- as.data.frame(as.array(gof[1:length(gof)]))
  colnames(gof) <- c("val")
  return(gof)
}


getResults <- function(fit) {
  #gof <- gofstat(fit)
  #gof <- as.data.frame(as.array(gof[1:length(gof)]))
  #ks <- gof[[1]]$ks[[1]]
  #ad <- gof[[1]]$ad[[1]]
  #return(c(fit$aic,fit$bic,getError(fit),ks,ad))
  return(c(fit$aic,fit$bic,getError(fit)))
}

getFinal <- function(file){
  #nodata <- data.frame(aic= numeric(0), bic= numeric(0), rmse = numeric(0), rrse = numeric(0) ,theil1 =numeric(0), theil2 = numeric(0), r2 = numeric(0), ks = numeric(0), ad = numeric(0))
  nodata <- data.frame(aic= numeric(0), bic= numeric(0), rmse = numeric(0), rrse = numeric(0) ,theil1 =numeric(0), theil2 = numeric(0), r2 = numeric(0))
  p<-plotDist(file,"pareto")
  nodata[1,] <- getResults(p)
  p<-plotDist(file,"weibull")
  nodata[2,] <- getResults(p)
  p<-plotDist(file,"exp")
  nodata[3,] <- getResults(p)
  p<-plotDist(file,"gamma")
  nodata[4,] <- getResults(p)
  tryCatch ({
    p<-plotDist(file,"gpd")
    nodata[5,] <- getResults(p)
      },error = function(e){
        nodata[5,] <- c(0,0,0,0,0,0,0)
      })
  p<-plotDist(file,"lnorm")
  nodata[6,] <- getResults(p)
  rownames(nodata) <- c("pareto","weibull","exp","gamma","gpd","lnorm")
  return(nodata)
}

friedman <- function(metric) {
  A = as.data.frame(metric)
  A <- A[,2:ncol(metric)]
  A <- as.matrix(A)
  #K <- as.numeric(ncol(A))
  #N <- as.numeric(nrow(A))
  #cd <- sqrt((N*(N+1))/(12*K))
  ranks <- A
  fri <- friedman.test(A)
  #print(friedmanTest(A))
  if (fri$p.value < 0.05){
    print("null hypothesis rejected")
    #nem <- posthoc.friedman.nemenyi.test(A)
    #plotCD(A)
    nem <- nemenyiTest(A)
    for(i in 1:nrow(A)){
      ranks[i,] <- rank(A[i,])
    }
    mean <- colMeans(ranks)
    cd <- nem$statistic
    print(mean)
    print(cd)
    #plot(mean, xaxt = "n", xlab='Distribution')
    errbar(1:5,mean,mean+cd,mean,xaxt = "n",xlab='Distribution')
    axis(1, at=1:5, labels=dists)
  } else {
    print("null hypothesis accepted")
  }
  
}

getBestFit <- function() {
  aic <- read.csv("D:/Studies/Project/BugPrediction/Softwares/testingdata/aic.csv")
  #bic <- read.csv("D:/Studies/Project/BugPrediction/Softwares/testingdata/bic.csv")
  #rrse <- read.csv("D:/Studies/Project/BugPrediction/Softwares/testingdata/rrse.csv")
  #rms <- read.csv("D:/Studies/Project/BugPrediction/Softwares/testingdata/rms.csv")
  #theil1 <- read.csv("D:/Studies/Project/BugPrediction/Softwares/testingdata/theil1.csv")
  #theil2 <- read.csv("D:/Studies/Project/BugPrediction/Softwares/testingdata/theil2.csv")
  friedman(aic);
  #nemneyiPlot(aic);
}