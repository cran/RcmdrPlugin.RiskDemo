returns.sim <-
function(volvec,indexvol,beta,nSim=10000)
{
  covmat<-indexvol^2*beta%*%rbind(beta)
  diag(covmat)<-volvec^2
  p <- length(beta)
  R <- matrix(rnorm(p*nSim),nrow=nSim,ncol=p)%*%chol(covmat)
  exp(R)-1
}
