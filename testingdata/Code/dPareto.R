ddpar <- function(x, theta, beta) {
  print (paste0("d ",theta," ",beta))
  ifelse(x<beta,return ((theta/(2*beta))*((x/beta)**(theta-1))),return ((theta/(2*beta))*((beta/x)**(theta+1))))
}

pdpar <- function(x, theta, beta) {
  print (paste0("p ",theta," ",beta))
  ifelse(x<beta, return (0.5*(x/beta)**theta),return (1-(0.5*(beta/x)**theta)))
}