\name{plotR}
\alias{plotR}
\title{
Plotting the effective reproduction number (R)
}
\description{
This function plots a time series of the effective reproduction number R and its confidence interval.
}
\usage{
plotR(data, region, start = NULL, end = NULL, confInt = 0.95)
}
\arguments{
  \item{data}{
data frame including columns \code{Aika} (character string indicating the date), \code{Alue} (character string indicating the region) and  \code{val} (numeric indicating the number of new cases)
}
  \item{region}{
characters string indicating the region for which the R series is computed
}
  \item{start}{
beginning date of the time window for which the R is computed
}
  \item{end}{
ending date of the time window for which the R is computed
}
  \item{confInt}{
decimal between 0 and 1, indicating the level of the confidence interval of R
}
}
\value{
No value
}
\author{
Arto Luoma <arto.luoma@wippies.com>
}
\seealso{
\code{\link{plotForecast}}, \code{\link{dataCovidFin}}
}
\examples{
data(dataCovidFin)
plotR(data=dataCovidFin, region='All regions')
}

