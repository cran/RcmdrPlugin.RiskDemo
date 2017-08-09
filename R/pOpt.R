pOpt <-
function(mu,vol,beta,indexVol,C)
{
   #Special variances
   delta <- vol^2-indexVol^2*beta^2
   #Inverse of the covariance matrix
   SigmaI <- invertSpecial(indexVol^2,beta,delta)
   Mu <- cbind(1,mu)
   w <- SigmaI%*%Mu%*%solve(t(Mu)%*%SigmaI%*%Mu)%*%c(1,C)
   vol <- sqrt((indexVol*sum(w*beta))^2+sum(w^2*delta))
   return(list(w=w,vol=vol))
}
