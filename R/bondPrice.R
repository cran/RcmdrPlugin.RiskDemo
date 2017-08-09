bondPrice <-
function(buyDate,matDate,rateCoupon,yieldToMat,nPay){
    buyDate <- as.Date(buyDate)
    matDate <- as.Date(matDate)
    daysMax <- c(31,28,31,30,31,30,31,31,30,31,30,31)
    #Maturity months and days
    year <-  as.numeric(substr(buyDate,1,4))
    month <- as.numeric(substr(matDate,6,7))
    day <-   as.numeric(substr(matDate,9,10))
    if(nPay==1) {months <- month}
    else if(nPay==2){
      months <- (month+c(0,6))%%12
      months[months==0] <- 12
      months <- sort(months)
    }else if(nPay==4){
      months <- (month+c(0,3,6,9))%%12
      months[months==0] <- 12
      months <- sort(months)
    }
    days <- numeric(nPay)
    for(i in 1:nPay) days[i] <- min(day,daysMax[months[i]])
    dates <- c(paste(year-1,months[nPay],days[nPay],sep="-"),paste(year,months,days,sep="-"),paste(year+1,months[1],day[1],sep="-"))
    dates <- as.Date(dates)
    nextInd <- min(which(dates-buyDate>0))
    periodLength <- dates[nextInd]-dates[nextInd-1]
    daysFromCoupon <- buyDate-dates[nextInd-1]
    k <- as.numeric(daysFromCoupon)/as.numeric(periodLength)

    Tt <- as.numeric(round(nPay*c(matDate-dates[nextInd-1])/365.25))

    #r <- (1+yieldToMat)^(1/nPay)-1
    r <- yieldToMat/nPay
    #price <- (1+r)^k*(rateCoupon/nPay*(1-1/(1+r)^Tt)/r+1/(1+r)^Tt)
    price <- (1+k*r)*(rateCoupon/nPay*(1-1/(1+r)^Tt)/r+1/(1+r)^Tt)
    accruedInterest <- k*rateCoupon/nPay
    flatPrice <- price - accruedInterest
    return(list(yieldToMaturity=yieldToMat,flatPrice=100*flatPrice,daysSinceLastCoupon=c(daysFromCoupon),
       daysInCouponPeriod=c(periodLength),accruedInterest=100*accruedInterest,
       invoicePrice=100*price))
}
