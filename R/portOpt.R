portOpt <-
function(mu,vol,beta,indexVol,interest)
{
  muP <- seq(0,max(mu),length.out=40)
  sdP <- numeric(40)
  for(i in 1:40){
    sdP[i] <- pOpt(mu,vol,beta,indexVol,muP[i])$vol
  }
  iOpt <- which.max((muP-interest)/sdP)
  wOpt <- pOpt(mu,vol,beta,indexVol,muP[iOpt])$w
  return(list(mu=muP[iOpt],sd=sdP[iOpt],w=wOpt))
}
