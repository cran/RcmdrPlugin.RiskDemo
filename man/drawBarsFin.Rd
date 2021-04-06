\name{drawBarsFin}
\alias{drawBarsFin}
\title{
Plotting epidemic statistics with Finnish data
}
\description{
This function plots the new cases or total cases of an epidemic for selected regions in Finland.
}
\usage{
drawBarsFin(data, pop, regions, start = "2020-06-01", end = "last", 
    measure = "new_cases", atop = TRUE, perMillion = FALSE, drawMean = TRUE, 
    bars = TRUE)
}
\arguments{
  \item{data}{
data frame including columns \code{Aika} (character string indicating the date), \code{Alue} (character string indicating the region) and  \code{val} (numeric indicating the number of new cases)
}
  \item{pop}{
data frame including columns \code{Alue} (character string indicating the region) and \code{val} (integer indicating the population)
}
  \item{regions}{
vector of characters strings indicating the regions for which the selected statistic is plotted
}
  \item{start}{
beginning date of the time window for which the curve is plotted
}
  \item{end}{
ending date of the time window for which the curve is plotted
}
  \item{measure}{
statistic to be plotted
}
  \item{atop}{
logical indicating if the bars of different regions are plotted on top of one another
}
  \item{perMillion}{
logical indicating if the statistic is proportioned to a population of million
}
  \item{drawMean}{
logical indicating if a smoothed curve (rolling mean of 7 observations) is plotted
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
\code{\link{drawBars}}, \code{\link{dataCovidFin}}
}
\examples{
data(dataCovidFin)
data(popRegionsFin)
drawBarsFin(dataCovidFin,popRegionsFin,regions=popRegionsFin$Alue[1:7])
}

