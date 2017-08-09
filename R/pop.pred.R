pop.pred <-
function(mort,mort.fcast){
   ages <- mort.fcast$model$y$x
   time <- mort.fcast$model$y$time
   mort <- extract.ages(extract.years(mort,years=time),ages=ages)
   sex <- names(mort.fcast$rate)[1]
   rate <- mort.fcast$rate[[sex]]
   N <- length(ages)
   Tt <- length(time)
   h <- dim(rate)[2]
   E <- matrix(NA,nrow=N,ncol=h)

   pop0 <- ts(mort$pop[[sex]][1,])
   y <- log(pop0)
   fit <- arima(y,c(0,2,2))
   pred0 <- exp(predict(fit,n.ahead=h)$pred)
   E0 <- mort$pop[[sex]][,Tt]
   E[1,] <- pred0 #forecast for 0-year old
   for(i in 2:N) {
      E[i,1] <- E0[i-1]*(1-rate[i-1,1])
      for(j in 2:h)
        E[i,j] <- E[i-1,j-1]*(1-rate[i-1,j-1])
   }
   dimnames(E) <- list(as.character(ages),as.character(time[Tt]+1:h))
   pop <- list()
   pop[[sex]] <- E
   out <- list(type="mortality",label=mort.fcast$label,lambda=0,year=mort.fcast$year,age=mort.fcast$age,pop=pop)
   class(out) <- 'demogdata'
   out
}
