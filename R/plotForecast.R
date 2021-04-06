plotForecast <-
function(data,region,start=NULL,end=NULL,np=30,predInt=0.95,log=TRUE){
  #series: zoo object
  #np: forecasting horizon
  #predInt: level of the prediction interval
  Aika <- NULL
  lower <- NULL
  upper <- NULL
  
  data$Aika <- as.Date(data$Aika)
  region <- ifelse(region=="All regions","Kaikki Alueet",region)
  series <- data[data$Alue==region,"val"]
  aika <- data[data$Alue==region,"Aika"]
  series <- window(zoo(series,order.by=aika),start=start,end=end) 
  
  i0 <- min(which(!is.na(series)))
  i1 <- max(which(!is.na(series)))
  series[is.na(series)] <- 0
  series <- series[i0:i1]
  
  a <- 2
  n <- length(series)
  n2 <- ifelse(is.null(end),n-4,n)
  timePred <- seq.Date(from=time(series)[n2]+1,by=1,length.out=np+(n-n2))
  #p0 <- log(c(2,0.00005,0.002,0.08))
  p0 <- c(-9,-7,-3.3)
  fit <- nlm(loglikCovid,p=p0,y=series[1:n2])
  out <- loglikCovid(fit$estimate,y=series[1:n2],it=FALSE)
  seasonal <- rep(c(out$Xif[n2,3:8],-sum(out$Xif[n2,3:8]))[7:1],length.out=np+(n-n2))
  pred <- out$Xif[n2,1]+(1:(np+(n-n2)))*out$Xif[n2,2]+seasonal
  pred <- ifelse(pred>log(a),pred,log(a)+0.1)
  P <- out$Pfmat[n2,,]
  varcomp <- exp(fit$estimate)
  se <- sqrt(P[1,1]+P[3,3]+2*P[1,3]+
               (1:(np+n-n2))^2*P[2,2]+cumsum((exp(pred)-a)/exp(pred)^2)+
               varcomp[2]+varcomp[3])
  cv <- qnorm(0.5*(1+predInt))#critical value
  
  pred.frame2 <- data.frame(Aika=timePred,pred=exp(pred)-a,lower=ifelse(exp(pred-cv*se)-a>0,exp(pred-cv*se)-a,0.5),upper=exp(pred+cv*se)-a)
  p <- ggplot(pred.frame2 ,aes(x=Aika,y=pred))+geom_line()+geom_point()+
    geom_ribbon(aes(x=Aika, ymin = lower, ymax = upper),alpha=0.3,fill=2) + 
    labs(title=paste0("Forecast of new cases in ",region),x="",y=paste0("Median (",100*predInt,"% interval)"))
  if(log) p <- p + scale_y_continuous(breaks=c(1,2,5,10,20,50,100,200,500,1000,2000,5000,10000,20000,50000), 
                                     trans="log")
  p
}
