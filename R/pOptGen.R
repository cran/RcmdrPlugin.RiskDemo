pOptGen <-
function(mu,Sigma,C)
{
   SigmaI <- solve(Sigma)
   Mu <- cbind(1,mu)
   w <- c(SigmaI%*%Mu%*%solve(t(Mu)%*%SigmaI%*%Mu)%*%c(1,C))
   vol <- sqrt(w%*%Sigma%*%w)
   return(list(w=w,vol=vol))
}
