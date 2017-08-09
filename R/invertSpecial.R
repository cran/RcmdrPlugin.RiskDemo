invertSpecial <-
function(alpha,beta,delta){
   #Inverts a special matrix alpha*beta%*%rbind(beta)+diag(delta)
   #alpha: scalar
   #beta: p-vector
   #delta: p-vector
   DeltaI <- diag(1/delta)
   gamma <- DeltaI%*%beta
   DeltaI-gamma%*%t(gamma)/(sum(gamma*beta)+1/alpha)
}
