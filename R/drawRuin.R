drawRuin <-
function(nsim=10,Tup=10,U0=1000,theta=0.01,lambda=100,alpha=1,beta=0.1){
    P <- (1+theta)*lambda*alpha/beta
    intervals <- 200
    X <- matrix(rpois(nsim*intervals,lambda*Tup/intervals),nrow=intervals,ncol=nsim)
    npos <- sum(X>0)
    X[X>0] <- rgamma(npos,alpha*X[X>0],beta)
    X <- rbind(U0,P*Tup/intervals-X)
    X <- apply(X,2,cumsum)
    t <- seq(0,Tup,length=intervals+1)
    plot(t,X[,1],xlim=c(0,Tup),ylim=c(min(X,0),max(X)),type="n",ylab="Insurer's captial (\u20AC)",
               xlab="Time (years)",main="Surplus process",xaxs="i")
    for(j in 1:nsim)
       lines(t,X[,j])
    abline(h=0,col="red")
}
