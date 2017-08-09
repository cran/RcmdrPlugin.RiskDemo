solveYield <-
function(buyDate,matDate,rateCoupon,bondPr,nPay){
  f <- function(buyDate,matDate,rateCoupon,x,nPay){
    return(bondPrice(buyDate,matDate,rateCoupon,yieldToMat=x,nPay)$flatPrice-bondPr)
  }
  yieldToMat <- uniroot(f,lower=0.001,upper=0.2,buyDate=buyDate,matDate=matDate,rateCoupon=rateCoupon,nPay=nPay)$root
  bondPrice(buyDate,matDate,rateCoupon,yieldToMat,nPay)
}
