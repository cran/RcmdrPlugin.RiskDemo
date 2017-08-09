stock.price <-
function(dividend, k = NULL, g = NULL, ROE = NULL, b = NULL,
    riskFree = NULL, marketPremium = NULL, beta = NULL)
{
    if (is.null(k)) {
        riskFree <- riskFree/100
        marketPremium <- marketPremium/100
        beta <- beta/100
        k <- riskFree + beta * marketPremium
    }
    else {
        k <- k/100
    }
    if (is.null(g)) {
        b <- b/100
        ROE <- ROE/100
        g <- b * ROE
    }
    else {
        g <- g/100
    }
    n <- length(dividend)
    if (n == 1) {
        price <- dividend/(k - g)
        if(!is.null(b)){
          PVGO <- price- dividend/((1-b)*k)}
        else{PVGO <- NA}
    }
    else if (n > 1) {
        income <- dividend
        income[n] <- dividend[n] + dividend[n] * (1 + g)/(k -
            g)
        price <- sum(income/(1 + k)^(1:n))
        PVGO <- NA
    }
    return(list(dividend=dividend, k=k, g=g, PVGO=PVGO, stockPrice = price))
}
