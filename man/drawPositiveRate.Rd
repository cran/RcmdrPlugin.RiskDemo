\name{drawPositiveRate}
\alias{drawPositiveRate}
\title{
Plotting the positive rate of COVID-19 tests or the tests per case
}
\description{
This function plots a time series of either the positive rate of COVID-19 tests or the number of tests per case.
}
\usage{
drawPositiveRate(data, countries, start = "2020-06-01", end = "last", 
    measure = "positive_rate", curve = TRUE, bars = FALSE, log = FALSE)
}
\arguments{
  \item{data}{
data frame including columns \code{location} (character string indicating the country), \code{date} (character string) and  \code{tests_per_case, positive_rate} (numeric)
}
  \item{countries}{
vector of characters srings indicating the countries for which the selected statistic is plotted
}
  \item{start}{
beginning date of the time window for which the time series are plotted
}
  \item{end}{
ending date of the time window for which the time series are plotted
}
  \item{measure}{
statistic for which the time series are plotted
}
  \item{curve}{
logical indicating if smoothed curves are drawn
}
  \item{bars}{
logical indicating if bars are plotted
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
\code{\link{dataCovid}, \link{drawTests}}
}
\examples{
data(dataCovid)
drawPositiveRate(dataCovid,countries=c("Finland","France"))
}

