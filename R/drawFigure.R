drawFigure <-
function(symbol, yield, vol, beta, r = 1, total = 1, indexVol = 20,
    nStocks = 7, balanceInt = 12, A = 10, riskfree = FALSE, bor = FALSE)
{
    r <- r/100
    yield <- yield/100
    vol <- vol/100
    indexVol <- indexVol/100
    beta <- beta/100
    balanceInt <- balanceInt/12
    if (nStocks == 1) {
        ii <- which.max((yield - r)/vol)
    }
    else {
        b <- portOpt(yield, vol, beta, indexVol, r)
        ii <- order(b$w, decreasing = TRUE)[1:nStocks]
    }
    if (nStocks > 1) {
        volvec <- sqrt(balanceInt) * vol
        indexvol <- sqrt(balanceInt) * indexVol
        RET <- returns(volvec, indexvol, beta)
        divYield <- balanceInt * yield
        mu <- (RET$mean + divYield)[ii]
        sdev <- sqrt(diag(RET$cov))[ii]
        muMin <- min(mu)
        muMax <- max(mu)
        Cvec <- seq(muMin, muMax, len = 40)
        x <- y <- numeric(length(Cvec))
        for (i in 1:length(Cvec)) {
            a <- portfOptim(ii, symbol, yield, vol, beta, indexVol,
                nStocks, total, balanceInt, C = Cvec[i])
            x[i] <- a$returnDeviation
            y[i] <- a$returnExpectation
        }
        duplicatedInd <- duplicated(x)
        x <- x[!duplicatedInd]
        y <- y[!duplicatedInd]
        o <- order(y)
        x <- x[o]
        y <- y[o]
        Cvec <- Cvec[o]
        i0 <- which.max(y - 0.5 * A * x^2/total)
        x0 <- x[i0]
        y0 <- y[i0]
        i2 <- which.max((y - r * total)/x)
        x2 <- x[i2]
        y2 <- y[i2]
        k <- (y2 - r * total)/x2
    }
    else {
        a <- portfOptim(ii, symbol, yield, vol, beta, indexVol,
            nStocks = 1, total, balanceInt)
        x <- a$returnDeviation
        y <- a$returnExpectation
        x0 <- x2 <- x
        sdev <- x0/total
        y0 <- y2 <- y
        mu <- y0/total
        k <- (y2 - r * total)/x2
    }
    x1 <- total * k/A
    y1 <- r * total + k * x1
    plot(x, y, xlim = c(0, 1.2 * max(x, x0, x1, total*sdev)), ylim = c(0,
        1.2 * max(y, y0, y1)), type = "n", xlab = "Standard deviation",
        ylab = "Expectation", main = "Efficient frontier of returns",xaxs="i",yaxs="i")
    points(sdev*total,mu*total,pch="*",col="red")
    if (nStocks > 1) {
        lines(x, y, lwd = 2)
    }
    if (riskfree) {
        abline(r * total, k)
        if (bor || x1 < x2) {
            points(x1, y1, pch = 1)
        }
        else {
            points(x2, y2, pch = 1, cex = 2)
        }
        points(x2, y2, pch = 19)
    }
    else {
        points(x0, y0, pch = 1)
    }


    if (bor) {
        riskProp <- x1/x2
    }
    else {
        riskProp <- min(1, x1/x2)
    }
    if (nStocks > 1) {
        if (riskfree == FALSE) {
            Cc <- Cvec[i0]
            a <- portfOptim(ii, symbol, yield, vol, beta, indexVol,
                nStocks, total, balanceInt, C = Cc, sim = TRUE)
        }
        else {
            a <- portfOptim(ii, symbol, yield, vol, beta, indexVol,
                nStocks, total, balanceInt, C = y2/total, riskProportion = riskProp,
                riskfreeRate = r, sim = TRUE)
        }
    }
    else {
        if (riskfree == FALSE) {
            a <- portfOptim(ii, symbol, yield, vol, beta, indexVol,
                nStocks = 1, total, balanceInt, riskProportion = 1,
                riskfreeRate = r, sim = TRUE)
        }
        else {
            a <- portfOptim(ii, symbol, yield, vol, beta, indexVol,
                nStocks = 1, total, balanceInt, riskProportion = riskProp,
                riskfreeRate = r, sim = TRUE)
        }
    }
    return(a)
}
