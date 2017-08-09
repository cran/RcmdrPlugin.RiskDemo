\name{portfOptim}
\alias{portfOptim}
\title{
Portfolio optimization for an index model
}
\description{
Finds an optimal portfolio for long-term investments and plots a return distribution. 
}
\usage{
portfOptim(i, symbol, yield, vol, beta, 
  indexVol = 0.2, nStocks = 7, total = 1, balanceInt = 1, C = 0.05, 
  riskProportion = 1, riskfreeRate = 0, sim = FALSE)
}
\arguments{
  \item{i}{
 vector of the indices of the included risky investments
}
  \item{symbol}{
 character vector of the symbols of the risky investments
}
  \item{yield}{
 vector of expected yields (in euros)
}
  \item{vol}{
 vector of volatilities 
}
  \item{beta}{
 vector of betas
}
  \item{indexVol}{
 portfolio index volatility
}
  \item{nStocks}{
  number of stocks in the portfolio
}
  \item{total}{
  total sum invested (in euros)
}
  \item{balanceInt}{
  balancing interval of the portfolio (in years)
}
  \item{C}{
  expected portfolio return (in euros)
}
  \item{riskProportion}{
  proportion of risky investments 
}
  \item{riskfreeRate}{
  risk-free interest rate
}
  \item{sim}{
  is the return distribution simulated and plotted (logical value)?
}
}
\details{
   The arguments vol, beta, indexVol, riskProportion and riskfreeRate are given in decimals. The portfolio is optimized by minimizing the variance of the portfolio yield for a given expected yield. The returns are assumed to be log-normally distributed. The covariance matrix is computed using the single index model and the properties of the log-normal distribution. 
}
\value{
\item{portfolio}{numeric vector of allocations to each stock (in euros)}
\item{returnExpectation}{expected value of the return distribution (in euros)}
\item{returnDeviation}{standard deviation of the return distribution (in euros)}
\item{VaR}{0.5\%,1\%,5\%,10\% and 50\% percentiles of the return distribution (in euros)}
}
\references{
Bodie, Kane, and Marcus (2014) \emph{Investments, 10th Global Edition}, McGraw-Hill Education, (see Section 7.4 The Markowitz Portfolio Optimization Model and Section 8.2 The Single-Index Model).
}
\author{
Arto Luoma <arto.luoma@wippies.com>
}
\note{
This function is usually called by drawFigure. 
}
\seealso{
\code{\link{drawFigure}}
}
\examples{
data(stockData, package="RcmdrPlugin.RiskDemo")
with(stockData,portfOptim(i=1:5,symbol=rownames(stockData),
  yield=divYield/100,vol=vol/100,beta=beta/100,total=100, sim=TRUE))
}
