repel <-
function(y,log=TRUE){
  n <- length(y)
  if(n==1) return(y)
  k <- ceiling(0.1+n/2)
  r <- rank(y)
  yo <- sort(y)
  if(log){
    for(i in (k+1):n) yo[i] <- max(yo[i],1.35*yo[i-1])
    for(i in (k-1):1) yo[i] <- min(yo[i],yo[i+1]/1.35)
  }else{
    d <- 0.35*yo[1]
    for(i in (k+1):n) yo[i] <- max(yo[i],yo[i-1]+d)
    for(i in (k-1):1) yo[i] <- min(yo[i],yo[i+1]-d)
  }  
  yo[r]
}
