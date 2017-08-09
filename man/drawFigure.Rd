\name{drawFigure}
\alias{drawFigure}
\title{
Efficient frontier and return distribution figures
}
\description{
Plots the efficient frontiers of risky investments and all investments. The optimum points corresponding to the risk aversion coefficient are indicated by dots. Further, the function plots a predictive return distribution figure.
}
\usage{
drawFigure(symbol, yield, vol, beta, r = 1, 
  total = 1, indexVol = 20, nStocks = 7, balanceInt = 12, A = 10, 
  riskfree = FALSE, bor = FALSE)
}
\arguments{
  \item{symbol}{
  character vector of the symbols of the risky investments
}
  \item{yield}{
  vector of yields (\%)
}
  \item{vol}{
  vector of volatilities (\%)
}
  \item{beta}{
  vector of betas (\%)
}
  \item{r}{
  risk-free interest rate (\%)
}
  \item{total}{
  total investment (for example in euros)
}
  \item{indexVol}{
  volatility of market portfolio (\%)
}
  \item{nStocks}{
  number of risky investments in the portfolio
}
  \item{balanceInt}{
  balancing interval of the portfolio in months
}
  \item{A}{
  risk aversion coefficient (see details)
}
  \item{riskfree}{
  is risk-free investment included in the portfolio (logical)
}
  \item{bor}{
  is borrowing (negative risk-free investment) allowed (logical) 
}
}
\details{
  The function uses the single-index model and Markovitz portfolio optimization model to find the optimum risky portfolio. The returns are assumed to be log-normally distributed. The maximized function is mu - 0.5*A*var where mu is expected return, A is risk aversion coefficient, and var is return variance. 
}
\value{
 \item{portfolio}{allocation of the total investment (in euros)}
 \item{returnExpectation}{expected portfolio return}
 \item{returnDeviation}{standard deviation of the portfolio}
}
\references{
Bodie, Kane, and Marcus (2014) \emph{Investments, 10th Global Edition}, McGraw-Hill Education, (see Section 7.4 The Markowitz Portfolio Optimization Model and Section 8.2 The Single-Index Model).
}
\author{
Arto Luoma <arto.luoma@wippies.com>
}
\seealso{
\code{\link{portfOptim}}
}

\examples{
data(stockData, package="RcmdrPlugin.RiskDemo")
with(stockData,drawFigure(symbol=rownames(stockData),yield=divYield,
  vol=vol,beta=beta,r=1,total=100,indexVol=10, 
  nStocks=5,balanceInt=12,A=10,riskfree=TRUE,bor=FALSE))
}
