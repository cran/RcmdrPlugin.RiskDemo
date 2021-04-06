\name{drawIncidenceFin}
\alias{drawIncidenceFin}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Plotting incidence curves of an epidemic with Finnish data
}
\description{
This function plots incidence curves of an epidemic for selected regions of Finland. The incidences are new cases per 100 000 inhabitants within one or two weeks. 
}
\usage{
drawIncidenceFin(data, pop, regions, start = "2020-06-01", end = "last", weeks = 2, 
    includeAllRegions = TRUE, log = TRUE)
}
\arguments{
  \item{data}{
data frame including columns \code{Aika} (character string indicating the date), \code{Alue} (character string indicating the region) and  \code{val} (numeric indicating the number of new cases)
}
  \item{pop}{
data frame including columns \code{Alue} (character string indicating the region) and \code{val} (integer indicating the population)
}
  \item{regions}{
vector of characters srings indicating the regions for which the curves are plotted
}
  \item{start}{
  beginning date of the time window for which the curve is plotted
}
  \item{end}{
ending date of the time window for which the curve is plotted
}
  \item{weeks}{
Integer telling  how many weeks' observations are used to calculate the incidence. Usually 1 or 2.
}
  \item{includeAllRegions}{
logical indicating if a curve for total incidence is included
}
  \item{log}{
logical indicating if a log scale is used in the plot
}
}

\value{
No value
}
\author{
Arto Luoma <arto.luoma@wippies.com>
}
\seealso{
\code{\link{drawIncidence}}, \code{\link{dataCovidFin}}
}
\examples{
data(dataCovidFin)
data(popRegionsFin)
drawIncidenceFin(data = dataCovidFin, pop = popRegionsFin, 
    regions = popRegionsFin$Alue[1:5], start = "2020-06-01", end="last", weeks=2, 
    includeAllRegions = TRUE)
}

