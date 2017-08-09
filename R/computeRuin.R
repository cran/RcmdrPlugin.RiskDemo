computeRuin <-
function(U0=NULL,theta=NULL,eps=NULL,alpha,beta){
    #Computes U0, theta or eps when the two other values are given
    #Uses Lundberg's exponential bound
    #U0: initial capital
    #theta: safety loading
    #eps: probability of ruin
    if(!is.null(theta)){
       R <- solveLund(alpha,beta,theta)
       if(R < 1e-6) return("Safety loading is too small")
       if(is.null(U0)){
          U0 <- -log(eps)/R
       }else if(is.null(eps)){
           eps <- exp(-R*U0)}
    }else{
       R <- -log(eps)/U0
       theta <- beta/(alpha*R)*((beta/(beta-R))^alpha-1)-1
    }
    list(LundbergExp=R,initialCapital=U0,safetyLoading=theta,ruinProb = eps)
}
