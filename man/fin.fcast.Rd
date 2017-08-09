\name{fin.fcast}
\alias{fin.fcast}
\docType{data}
\title{
Finnish mortality forecast
}
\description{
Finnish mortality forecast 50 years ahead (2016-2065) for 0 - 100 years old. The forecast is 
based on an estimated Lee-Carter model. The kt coefficients were forecast using a random walk with drift. Fitted rates were used as the starting value. 
}
\usage{data("fin.fcast")}
\format{
 An object of class "fmforecast"; for details, see documentation of package "demography".
}
\details{
The forecast was produced using function "forecast.lca" of package "demography".
}
\examples{
data(fin.fcast)
print(fin.fcast)
plot(fin.fcast)
}

