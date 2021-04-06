plotR <-
function(data,region,start=NULL,end=NULL,confInt=0.95){
  Aika <- NULL
  
  #confInt: level of the confidence interval
  alpha <- 4.03
  beta <- 0.62
  
  data$Aika <- as.Date(data$Aika)
  region <- ifelse(region=="All regions","Kaikki Alueet",region)
  series <- data[data$Alue==region,"val"]
  aika <- data[data$Alue==region,"Aika"]
  series <- window(zoo(series,order.by=aika,frequency=7),start=start,end=end) 
  
  i0 <- min(which(!is.na(series)))
  i1 <- max(which(!is.na(series)))
  series[is.na(series)] <- 0
  series <- series[i0:i1]
  
  a <- 2
  n <- length(series)
  n2 <- ifelse(is.null(end),n-4,n)
  y <- series[1:n2]
  p0 <- c(-9,-7,-3.3)
  fit <- nlm(loglikCovid,p=p0,y=y)
  out <- covidSmooth(fit$estimate,y)
  
  
  cv <- qnorm(0.5*(1+confInt))#critical value
  Rseries <- (1+out$Xis[,2]/beta)^alpha
  se <- sqrt(out$Psmat[,2,2])
  Rmin <- (1+(out$Xis[,2]-cv*se)/beta)^alpha
  Rmax <- (1+(out$Xis[,2]+cv*se)/beta)^alpha
  
  R <- round(as.numeric(Rseries[n-7-(n-n2)]),1)
  R.frame <- data.frame(Aika=time(y)-7,Rseries=Rseries,Rmin=Rmin,Rmax=Rmax)[-(1:7),]
  
  
  ggplot(R.frame, aes(x=Aika,y=Rseries)) +geom_line()+ ylim(0,3.5)+
    geom_ribbon(aes(x=Aika, ymin = Rmin, ymax = Rmax),alpha=0.3,fill=2)+
    scale_x_date(name="",breaks = date_breaks("months"),labels = date_format("%b"))+
    labs(title=paste0("Effective reproduction number (R) in ",region),y=paste0("R (",100*confInt,"% CI)"))+
    geom_text(aes(x=last(Aika),y=R,label=paste("R=",R)))+
    geom_hline(yintercept=1,linetype="dashed",col="red")
  
}
