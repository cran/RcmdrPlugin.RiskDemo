\name{bondFigure}
\alias{bondFigure}
\title{
Bond price as a function of interest rate.
}
\description{
This function plots the bond price as a function of interest rate. It also shows, using dotted lines, the yield to maturity rate corresponding to the face value, and the flat price corresponding to the yield to maturity. 
}
\usage{
bondFigure(buyDate, matDate, rateCoupon, yieldToMat = NULL, bondPr = NULL, nPay)
}
\arguments{
  \item{buyDate}{
the date when the coupon is bought (settlement date)
}
  \item{matDate}{
maturity date
}
  \item{rateCoupon}{
coupon rate (in decimals)
}
  \item{yieldToMat}{
yield to maturity (in decimals)
}
  \item{bondPr}{
the flat price of the bond
}
  \item{nPay}{
number of coupon payments per year
}
}
\details{
 either yieldToMat or bondPr should be given as input.
}
\value{
This function only plots a figure.
}
\references{
Bodie, Kane, and Marcus (2014) \emph{Investments, 10th Global Edition}, McGraw-Hill Education, (see Section 14.2 Bond Pricing).
}
\author{
Arto Luoma <arto.luoma@wippies.com>
}
\seealso{
\code{\link{bondPrice}}, \code{\link{solveYield}}
}
\examples{
bondFigure("2012-7-31","2018-7-31",rateCoupon=0.0225,yieldToMat=0.0079,nPay=2)
bondFigure("2012-7-31","2018-7-31",rateCoupon=0.0225,bondPr=90,nPay=2)
}

