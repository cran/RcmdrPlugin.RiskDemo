solveLund <-
function(alpha,beta,theta){
       #Function to determine Lundberg's exponent when the loss distribution is
       #Gamma(alpha,beta) and the safety loading theta
       eps <- 1e-12
       f <- function(t,alpha,beta,theta){
           (beta/(beta-t))^alpha-1-alpha*(1+theta)*t/beta
       }
       uniroot(f,lower=eps,upper=beta-eps,alpha=alpha,beta=beta,theta=theta)$root
}
