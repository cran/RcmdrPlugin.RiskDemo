\name{stockData}
\alias{stockData}
\docType{data}
\title{
Stock data
}
\description{
Stock data on large companies in Helsinki Stock Exchange, downloaded from Kauppalehti web page (www.kauppalehti.fi), on May 13, 2017
}
\usage{data("stockData")}
\format{
  A data frame with 35 observations on the following 7 variables.
  \describe{
    \item{\code{names}}{name of the firm} 
    \item{\code{abbrs}}{abbreviation of the firm}
    \item{\code{quote}}{closing quote}
    \item{\code{vol}}{volatility (\%)}
    \item{\code{beta}}{beta (\%)}
    \item{\code{div}}{dividend (eur/stock)}
    \item{\code{divYield}}{dividend yield (\%)}
  }
}
\source{
www.kauppalehti.fi
}
\examples{
data(stockData)
plot(stockData[,-(1:2)])
}

