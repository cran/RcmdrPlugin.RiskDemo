bondFigure <-
function(buyDate,matDate,rateCoupon,yieldToMat=NULL,bondPr=NULL,nPay){
   if(!is.null(yieldToMat)){
     bP <- bondPrice(buyDate,matDate,rateCoupon,yieldToMat,nPay)$flatPrice} else{
     a <- solveYield(buyDate,matDate,rateCoupon,bondPr,nPay)
     bP <- a$flatPrice
     yieldToMat <- a$yieldToMat
   }

   x <- seq(0.001,0.2,by=0.001)
   y <- numeric(length(x))
   for(i in 1:length(x))
      y[i] <- bondPrice(buyDate,matDate,rateCoupon,yieldToMat=x[i],nPay)$flatPrice
   plot(100*x,y,type="l",xlab="Interest Rate (%)",ylab="Bond Price (\u20AC)",xaxs="i")
   lines(c(0,100*rateCoupon),c(100,100),lty=2)
   lines(c(100*rateCoupon,100*rateCoupon),c(0,100),lty=2)
   lines(c(0,100*yieldToMat),c(bP,bP),lty=2)
   lines(c(100*yieldToMat,100*yieldToMat),c(0,bP),lty=2)
}
