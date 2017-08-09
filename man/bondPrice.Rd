\name{bondPrice}
\alias{bondPrice}
\title{
Computing bond prices
}
\description{
This function computes the bond price, given the yield to maturity.
}
\usage{
bondPrice(buyDate, matDate, rateCoupon, yieldToMat, nPay)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{buyDate}{
the date at which the bond is bought (settlement date). 
}
  \item{matDate}{
maturity date
}
  \item{rateCoupon}{
annual coupon date
}
  \item{yieldToMat}{
yield to maturity 
}
  \item{nPay}{
number of coupon payments per day
}
}
\details{
All the rates are given in decimals.
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
With Excel functions PRICE, DATE, COUPDAYBS and COUPDAYS you can do the same. 
}
\seealso{\code{\link{solveYield}}
}
\examples{
bondPrice("2012-7-31","2018-7-31",0.0225,0.0079,2)
bondPrice("2012-7-31","2018-7-31",0.0225,0.0079,4)
bondPrice("2012-7-31","2030-5-15",0.0625,0.02117,2)
}

