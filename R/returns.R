returns <-
function(volvec,indexvol,beta)
{
  covmat<-indexvol^2*beta%*%rbind(beta)
  diag(covmat)<-volvec^2
  tuotod <- exp(0.5*volvec^2)-1
  tuotcov<-exp(0.5*outer(volvec^2,volvec^2,"+"))*(exp(covmat)-1)
  list(mean=tuotod,cov=tuotcov)
}
