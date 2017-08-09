computeRuinFinite <-
function(T0,U0=NULL,theta=NULL,eps=NULL,lambda,alpha,beta){
     cs <- function(s,lambda,alpha,beta,P){
        lambda*((beta/(beta-s))^alpha-1)-s*P
     }
     cDiff <- function(s,lambda,alpha,beta,P){
        lambda*alpha*beta^alpha/(beta-s)^(alpha+1)-P
     }
     snu <- function(nu,lambda,alpha,beta,P){
        beta-(lambda*alpha*beta^alpha/(nu+P))^(1/(alpha+1))
     }
     ccNu <- function(nu,lambda,alpha,beta,P){
         nu*snu(nu,lambda,alpha,beta,P)-cs(snu(nu,lambda,
                    alpha,beta,P=P),lambda,alpha,beta,P)
     }
     ccNuY <- function(nu,lambda,alpha,beta,P,y){
         nu*snu(nu,lambda,alpha,beta,P)-cs(snu(nu,lambda,alpha,beta,P),lambda,alpha,beta,P)-y
     }


     if(!is.null(theta)){
         P <- (1+theta)*lambda*alpha/beta
         R <- solveLund(alpha,beta,theta)
         if(R < 1e-6) return("Safety loading is too small")
         muTinv <- cDiff(R,lambda=lambda,alpha=alpha,beta=beta,P=P)
         if(is.null(eps)){
             x <- min(T0/U0,1/muTinv)
             eps <- exp(-x*ccNu(1/x,lambda,alpha,beta,P)*U0)
         }else if(is.null(U0)){
             y <- -log(eps)/T0
             xInv <- uniroot(ccNuY,lower=muTinv,upper=100*muTinv,extendInt="upX",lambda=lambda,alpha=alpha,
                   beta=beta,P=P,y=y)$root
             U0 <- T0*xInv
         }
     }else{
             R <- -log(eps)/U0
             Pup <- lambda/R*((beta/(beta-R))^alpha-1)
             #Plow <- lambda*alpha/beta-U0/T0
             Plow <- lambda*alpha/beta
             muTinv <- cDiff(R,lambda=lambda,alpha=alpha,beta=beta,P=Pup)
             x <- T0/U0
             if(x>=1/muTinv){P <- Pup}else{
               y <- -log(eps)/T0
               if(ccNuY(Plow,nu=1/x,lambda=lambda,alpha=alpha,beta=beta,y=y)>0)
                 return("No positive solution for risk loading")
               P <- uniroot(ccNuY,nu=1/x,lower=Plow,upper=Pup,lambda=lambda,alpha=alpha,beta=beta,y=y)$root
             }
             theta <- beta*P/(alpha*lambda)-1
     }
     list(LundbergExp=R,initialCapital=U0,safetyLoading=theta,ruinProb = eps)
}
