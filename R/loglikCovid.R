loglikCovid <-
function(y,par,it=TRUE){
  #Seuraan notaatiossa suunnilleen Hamiltonia
  a <- 2
  y <- as.numeric(y)
  y <- log(y+a)
  k <- 8
  n <- length(y)
  ll <- numeric(n)
  r <- exp(par[3])


  
  Ff <- matrix(0,nrow=k,ncol=k)
  Ff[1:2,1:2] <- c(1,0,1,1)
  Ff[3,3:8] <- -1
  Ff[4:8,3:7] <- diag(5)
  h <- c(1,0,1,0,0,0,0,0)
  
  xi <- numeric(8)
  xi[2] <- (y[8]-y[1])/7
  xi[1] <- mean(y[1:7])-3*xi[2]
  
  #xi[1:2] <- par[5:6]
   
  
  Qq <- matrix(0,nrow=k,ncol=k)
  Qq[1,1] <- max(exp(xi[1])-a,0.1)/exp(xi[1])^2
  Qq[2:3,2:3] <- diag(exp(par[1:2])) 
  #Qq[2,2] <- 0.0001
  #tilavektorin ei-ehdollinen kovarianssimatriisi
  #P <- matrix(solve(diag(k*k)-kronecker(Ff,Ff))%*%c(Qq),nc=k)
  P <- Qq
  P[4,4] <- P[5,5] <- P[6,6] <- P[7,7] <- P[8,8] <- exp(par[2])
  P <- 10*P
  Xi <- Xif <- matrix(NA,nrow=n,ncol=k)
  Pfmat <- array(NA,dim=c(n,k,k))
  for(t in 1:n){
    Xi[t,] <- xi
    innVar <- c(h%*%P%*%h + r) #innovation variance
    e <- c(y[t] - h%*%xi) 
    ll[t] <- e^2/innVar+log(innVar)
    K0 <- P%*%h/innVar
    xif <- Xif[t,] <- xi+K0*e
    Pf <- P-K0%*%t(h)%*%P
    Pfmat[t,,] <- Pf
    xi <- Ff%*%xif
    Qq[1,1] <- max(exp(xi[1])-a,0.1)/exp(xi[1])^2
    P <- Ff%*%Pf%*%t(Ff)+Qq
  }
  loglik <- sum(ll)
  loglik <- loglik+(par[1]+9)^2 #penalized log likelihood
  if(it) return(loglik) else return(list(loglik=loglik,ll=ll,Xi=Xi,Xif=Xif,Pfmat=Pfmat,Q=Qq))
}
