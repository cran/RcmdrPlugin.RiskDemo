\name{solveYield}
\alias{solveYield}
\title{
Computing bond yields
}
\description{
This function computes the yield to maturity, given the (flat) bond price.
}
\usage{
solveYield(buyDate, matDate, rateCoupon, bondPr, nPay)
}
\arguments{
  \item{buyDate}{
  settlement date (the date when the bond is bought)
}
  \item{matDate}{
  maturity date
}
  \item{rateCoupon}{
 annual coupon rate 
}
  \item{bondPr}{
bond price. The flat price without accrued interest.
}
  \item{nPay}{
 number of payments per year
}
}
\details{
 all the rates are given in decimals
}
\value{
A list with the following components:
\item{yieldToMaturity}{yield to maturity}
\item{flatPrice}{flat price}
\item{daysSinceLastCoupon}{days since previous coupon payment}
\item{daysInCouponPeriod}{days in a coupon period}
\item{accruedInterest}{accrued interest since last coupon payment}
\item{invoicePrice}{invoice price (= flat price + accrued interest)}
}
\references{
Bodie, Kane, and Marcus (2014) \emph{Investments, 10th Global Edition}, McGraw-Hill Education, (see Bond Pricing between Coupon Dates in Section 14.2).
}
\author{
Arto Luoma <arto.luoma@wippies.com>
}
\note{
With Excel function YIELD you can do the same.
}
\seealso{
\code{\link{bondPrice}}
}
\examples{
solveYield("2012-7-31","2018-7-31",0.0225,100,2)
}

