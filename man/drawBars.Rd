\name{drawBars}
\alias{drawBars}
\title{
Plotting epidemic statistics
}
\description{
This function plots several epidemic statistics for selected countries.
}
\usage{
drawBars(data, countries, start = "2020-06-01", end = "last", measure = "new_cases", 
    atop = TRUE, perMillion = FALSE, drawMean = TRUE, bars = TRUE)
}
\arguments{
  \item{data}{
data frame similar to (or including the same columns as) \code{dataCovid}
}
  \item{countries}{
vector of characters srings indicating the countries for which the selected statistic is plotted
}
  \item{start}{
beginning date of the time window for which the statistic is plotted
}
  \item{end}{
ending date of the time window for which the statistic is plotted
}
  \item{measure}{
statistic to be plotted
}
  \item{atop}{
logical indicating if the bars of different countries are plotted on top of one another
}
  \item{perMillion}{
logical indicating if the statistic is proportioned to a population of million
}
  \item{drawMean}{
logical indicating if a smoothed curve is drawn 
}
  \item{bars}{
logical indicating if bars are plotted
}
}
\value{
No value.
}
\author{
Arto Luoma <arto.luoma@wippies.com>
}
\seealso{
\code{\link{drawBarsFin}}, \code{\link{dataCovid}}
}
\examples{
data(dataCovid)
drawBars(data=dataCovid, countries=c('Finland','France'),start='2020-6-1',
         measure='new_cases',perMillion=TRUE)
}

