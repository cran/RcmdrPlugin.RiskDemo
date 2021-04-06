\name{drawTests}
\alias{drawTests}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Plotting time series related to COVID-19 testing
}
\description{
This function plots time series of new and total COVID-19 tests, possibly in proportion to population.
}
\usage{
drawTests(data, countries, start = "2020-06-01", end = "last", measure = "new_tests", 
    atop = TRUE, perThousand = FALSE, drawMean = TRUE, bars = TRUE, log = FALSE)
}
\arguments{
  \item{data}{
data frame similar to (or including the same columns as) \code{dataCovid}
}
  \item{countries}{
vector of characters strings indicating the countries for which the time series are plotted
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
  \item{atop}{
logical indicating if the bars of different countries are plotted on top of one another
}
  \item{perThousand}{
logical indicating if the statistic is proportioned to a population of thousand
}
  \item{drawMean}{
logical indicating if a smoothed curve is drawn 
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
\code{\link{dataCovid}, \link{drawPositiveRate}}
}
\examples{
data(dataCovid)
drawTests(dataCovid,countries=c("Finland","France"),perThousand=TRUE)
}
