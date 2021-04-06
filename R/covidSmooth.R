covidSmooth <-
function(par,y){
  #tasoitettu versio
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
  P <- Qq
  P[4,4] <- P[5,5] <- P[6,6] <- P[7,7] <- P[8,8] <- exp(par[2])
  P <- 10*P
  Xi <- matrix(NA,nrow=n,ncol=k) #1-step predicted values
  Xif <- matrix(NA,nrow=n,ncol=k) #filtered values
  Xis <- matrix(NA,nrow=n,ncol=k) #smoothed values
  Pmat <- Pfmat <- Psmat <- Jmat <- array(NA,dim=c(n,k,k))
  
  for(t in 1:n){
    Xi[t,] <- xi
    Pmat[t,,] <- P
    innVar <- c(h%*%P%*%h + r) #innovation variance
    e <- c(y[t] - h%*%xi) 
    K0 <- P%*%h/innVar
    xif <- xi + K0*e
    Xif[t,] <- xif
    Pf <- P-K0%*%t(h)%*%P
    Pfmat[t,,] <- Pf
    
    xi <- Ff%*%xif
    Qq[1,1] <- max(exp(xi[1])-a,0.1)/exp(xi[1])^2
    P <- Ff%*%Pf%*%t(Ff)+Qq
    Jmat[t,,] <- Pf%*%t(Ff)%*%solve(P)
  }
  Xis[n,] <- xif
  Psmat[n,,] <- Pfmat[n,,] 
  for(t in (n-1):1){
    Xis[t,] <- Xif[t,]+Jmat[t,,]%*%(Xis[t+1,]-Xi[t+1,])
    Psmat[t,,] <- Pfmat[t,,]+Jmat[t,,]%*%(Psmat[t+1,,]-Pmat[t+1,,])%*%t(Jmat[t,,]) 
  }
  list(Xif=Xif,Xis=Xis,Pmat=Pmat,Pfmat=Pfmat,Psmat=Psmat)
}
