bondCurve <-
function(date1,date2=NULL,yield=TRUE,forward=TRUE,AAA=TRUE,all=TRUE,params){
   date <- params[,"date"]
   param1 <- as.numeric(params[date==date1,-1])
   if(is.na(param1[1]))
     return("Data for this date are unavailable")

   t <- seq(0,30,by=0.1)
   c0 <- param1[7];c1 <- param1[8]; c2 <- param1[9]; c3 <- param1[10]; d1 <- param1[11]; d2 <- param1[12]
   ea1 <- exp(-t/d1); ea2 <- exp(-t/d2)
   ya <- c0+c1*(1-ea1)/(t/d1)+c2*((1-ea1)/(t/d1)-ea1)+c3*((1-ea2)/(t/d2)-ea2)
   fa <- c0+c1*ea1+c2*(t/d1)*ea1+c3*(t/d2)*ea2
   yMax <- max(fa)
   yMin <- min(fa)


   if(AAA){
      b0 <- param1[1];b1 <- param1[2]; b2 <- param1[3]; b3 <- param1[4]; t1 <- param1[5]; t2 <- param1[6]

      e1 <- exp(-t/t1); e2 <- exp(-t/t2)
      y <- b0+b1*(1-e1)/(t/t1)+b2*((1-e1)/(t/t1)-e1)+b3*((1-e2)/(t/t2)-e2)
      f <- b0+b1*e1+b2*(t/t1)*e1+b3*(t/t2)*e2
      yMax <- max(yMax,f)
      yMin <- min(yMin,f)
   }
   if(!is.null(date2)){
       param2 <- as.numeric(params[date==date2,-1])
       if(is.na(param2[1])) date2 <- NULL
   }
   if(!is.null(date2)){
      if(all){
         c0 <- param2[7];c1 <- param2[8]; c2 <- param2[9]; c3 <- param2[10]; d1 <- param2[11]; d2 <- param2[12]
         ea1 <- exp(-t/d1); ea2 <- exp(-t/d2)
         ya2 <- c0+c1*(1-ea1)/(t/d1)+c2*((1-ea1)/(t/d1)-ea1)+c3*((1-ea2)/(t/d2)-ea2)
         fa2 <- c0+c1*ea1+c2*(t/d1)*ea1+c3*(t/d2)*ea2
         yMax <- max(yMax,fa2)
         yMin <- min(yMin,fa2)
      }
      if(AAA){
         b0 <- param2[1];b1 <- param2[2]; b2 <- param2[3]; b3 <- param2[4]; t1 <- param2[5]; t2 <- param2[6]

         e1 <- exp(-t/t1); e2 <- exp(-t/t2)
         y2 <- b0+b1*(1-e1)/(t/t1)+b2*((1-e1)/(t/t1)-e1)+b3*((1-e2)/(t/t2)-e2)
         f2 <- b0+b1*e1+b2*(t/t1)*e1+b3*(t/t2)*e2
         yMax <- max(yMax,f2)
         yMin <- min(yMin,f2)
     }
   }
   #Plotting....
   plot(t,fa,type="n",xlab="Time to maturity in years",ylab="Yield in %",ylim=c(yMin,yMax))
   if(all){
      if(yield){ lines(t,ya,col="blue")}
      if(forward){lines(t,fa,col="red")}
   }
   if(AAA){
      if(yield){ lines(t,y,col="blue",lwd=2) }
      if(forward){lines(t,f,col="red",lwd=2)}
   }
   if(!is.null(date2)){
      if(all){
         if(yield){ lines(t,ya2,col="blue",lty=2)}
         if(forward){lines(t,fa2,col="red",lty=2)}
      }
      if(AAA){
         if(yield){ lines(t,y2,col="blue",lwd=2,lty=2)}
         if(forward){lines(t,f2,col="red",lwd=2,lty=2)}
      }
   }
   if(forward&&yield){
     if(AAA&&all){
        legend("bottomright",legend=c("Forward AAA","Yield AAA","Forward all","Yield all"),
        col=c("red","blue","red","blue"),lty=c(1,1,1,1),lwd=c(2,2,1,1))}else{
        legend("bottomright",legend=c("Forward","Yield"),col=c("red","blue"),lty=c(1,1))}
   }else if(AAA&&all){
     if(forward){
        legend("bottomright",legend=c("Forward AAA","Forward all"),
        col=c("red","red"),lty=c(1,1),lwd=c(2,1))}else if(yield){
        legend("bottomright",legend=c("Yield AAA","Yield all"),
        col=c("blue","blue"),lty=c(1,1),lwd=c(2,1))
     }
   }
  if(!is.null(date2)){
    legend("topleft",legend=c(date1,date2),lty=c(1,2))
    }
}
