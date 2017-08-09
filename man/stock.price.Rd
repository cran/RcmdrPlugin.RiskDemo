\name{stock.price}
\alias{stock.price}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Computing stock prices
}
\description{
This function computes the intrinsic stock price using the constant growth dividend discount model.
}
\usage{
stock.price(dividend, k = NULL, g = NULL, ROE = NULL, b = NULL, 
  riskFree = NULL, marketPremium = NULL, beta = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dividend}{
expected dividend(s) for the next year(s) (in euros), separated by commas
}
  \item{k}{required rate of return
}
  \item{g}{
 growth rate of dividends
}
  \item{ROE}{
return on investment
}
  \item{b}{
plowback ratio
}
  \item{riskFree}{
riskfree rate
}
  \item{marketPremium}{
market risk premium
}
  \item{beta}{
beta
}
}
\details{
All the above rates are given in percentages (except the dividends). One should provide either k or the following three: riskFree, marketPremium, beta. Further, one should provide either g or the following two: ROE and b. In the output, k and g are given in decimals.
}
\value{
  \item{dividend}{expected dividend(s) for the next year(s) (in euros)}
  \item{k}{required rate of return}
  \item{g}{growth rate of dividends}
  \item{PVGO}{present value of growths opportunities}
  \item{stockPrice}{intrinsic stock price}
}
\references{
Bodie, Kane, and Marcus (2014) \emph{Investments, 10th Global Edition}, McGraw-Hill Education, (see Dividend Discount Models in Section 18.3).
}
\author{
Arto Luoma <arto.luoma@wippies.com>
}
\examples{
  stock.price(dividend=c(1),k=12,g=10)
  stock.price(dividend=c(1),ROE=50,b=20,riskFree=5,marketPremium=8,beta=90)
}


