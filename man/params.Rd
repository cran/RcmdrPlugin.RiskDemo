\name{params}
\alias{params}
\docType{data}
\title{
Yield curve parameter data
}
\description{
Yield curve parameters from the European Central Bank (ECB), downloaded on Nov 4, 2023
}
\usage{data("params")}
\format{
  A data frame with 4902 observations on the following 13 variables.
  \describe{
    \item{\code{date}}{a Date}
    \item{\code{b0}}{a numeric vector}
    \item{\code{b1}}{a numeric vector}
    \item{\code{b2}}{a numeric vector}
    \item{\code{b3}}{a numeric vector}
    \item{\code{t1}}{a numeric vector}
    \item{\code{t2}}{a numeric vector}
    \item{\code{c0}}{a numeric vector}
    \item{\code{c1}}{a numeric vector}
    \item{\code{c2}}{a numeric vector}
    \item{\code{c3}}{a numeric vector}
    \item{\code{d1}}{a numeric vector}
    \item{\code{d2}}{a numeric vector}
  }
}
\details{
The parameters b0 to b3 are the beta-parameters, and t1 and t2 the tau-parameters for AAA-rated government bonds. The parameters c0 to c3 are the beta-parameters, and d1 and d2 the tau-parameters for all government bonds.
}
\source{
https://bit.ly/2zfs0G8
}
\examples{
data(params)
bondCurve(as.Date("2004-09-06"),params=params)
}
\keyword{datasets}
