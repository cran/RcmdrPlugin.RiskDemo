 \name{plotForecast}
\alias{plotForecast}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Forecasting new covid cases
}
\description{
This function forecasts the numbers of new covid cases using a simple linear state space model.
}
\usage{
plotForecast(data, region, start = NULL, end = NULL, np = 30, predInt = 0.95, 
    log = TRUE)
}
\arguments{
  \item{data}{
data frame including columns \code{Aika} (character string indicating the date), \code{Alue} (character string indicating the region) and  \code{val} (numeric indicating the number of new cases)
}
  \item{region}{
characters string indicating the region for which the forecast is made
}
  \item{start}{
beginning date of the observations used in the estimation of the forecasting model
}
  \item{end}{
ending date of the observations used in the estimation of the forecasting model
}
  \item{np}{
integer indicating the forecasting horizon in days
}
  \item{predInt}{
decimal indicating the probability of the forecasting interval
}
  \item{log}{
logical indicating if a log scale is used in the plot
}
}
\value{
No value.
}
\author{
Arto Luoma <arto.luoma@wippies.com>
}
\seealso{
\code{\link{plotR}}, \code{\link{dataCovidFin}}
}

\examples{
data(dataCovidFin)
plotForecast(data=dataCovidFin, region='All regions', start="2020-09-01")
}

