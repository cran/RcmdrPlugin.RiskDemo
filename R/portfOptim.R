portfOptim <-
function(i,symbol,yield,vol,beta,indexVol=0.2,nStocks=7,total=1,balanceInt=1,C=0.05,riskProportion=1,riskfreeRate=0,sim=FALSE){
  #Funktio hakee optimisalkun
  #indexVol: vertailuindeksin volatiliteetti
  #balanceInt: salkun tasapainotusv?li (vuosina)
  #nStocks: osakkeiden maksimim??r? salkussa
  #total: total investment 
  #C: expected portfolio return
  #riskProportion: proportion of risky investments
  #riskfreeRate: risk-free rate (not as percentage!)
  #i: the chosen stocks
  volvec <- sqrt(balanceInt)*vol
  indexvol <- sqrt(balanceInt)*indexVol
  RET <- returns(volvec,indexvol,beta)
  divYield <- balanceInt*yield 
  mu <- RET$mean+divYield
  #mu <- divYield
  V <- RET$cov
  
  if(nStocks==1){
     w <- 1
     portfolio <- total
     names(portfolio)<- symbol[i]
     tuotOd <- total*mu[i]
     tuotHaj <- total*sqrt(V[i,i])
     #return(list(portfolio=portfolio,returnExpectation=tuotOd,returnDeviation=tuotHaj))
  } else{  
    mu <- mu[i]
    V <- RET$cov[i,i]
    b <- pOptGen(mu,V,C)
    w <- b$w
    tuotOd <- total*round(C,4)
    tuotHaj <- total*round(b$vol,4)
    portfolio <- total*round(w,3)
  }
  if(sim){ 
      SIM <- total*(returns.sim(volvec[i],indexvol,beta[i])%*%w + sum(divYield[i]*w))
    }
  if(riskProportion != 1){
    mu0 <- total*balanceInt*riskfreeRate
    tuotOd <- riskProportion*tuotOd+(1-riskProportion)*mu0
    tuotHaj <- riskProportion*tuotHaj
    portfolio <- c(total*(1-riskProportion),riskProportion*portfolio)
    names(portfolio)<-c("Risk-free",symbol[i])
    if(sim){ SIM <- riskProportion*SIM+(1-riskProportion)*mu0}
  }else{  
    names(portfolio)<-symbol[i]    
  }
  if(sim){
     VaR <- quantile(SIM,probs=c(0.005,0.01,0.05,0.1,0.5))
     dev.new()
     aa <- density(SIM)
     plot(aa,main="Predictive return distribution",xlab="")
     i <- max(which(aa$x<0))
     xx <- c(min(aa$x),aa$x[1:i],aa$x[i])
     yy <- c(0,aa$y[1:i],0)
     polygon(xx,yy,col="red")
     nn <- length(aa$x)
     xx <- c(aa$x[i+1],aa$x[(i+1):nn],aa$x[nn])
     yy <- c(0,aa$y[(i+1):nn],0)
     polygon(xx,yy,col="green")
  }
  else{
     VaR <- NULL}   
  list(portfolio=portfolio,returnExpectation=tuotOd,returnDeviation=tuotHaj,VaR=VaR)
}
