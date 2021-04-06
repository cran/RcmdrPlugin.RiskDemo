\name{drawIncidence}
\alias{drawIncidence}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Plotting incidence curves of an epidemic
}
\description{
This function plots incidence curves of an epidemic for selected countries. The incidences are new cases per 100 000 inhabitants within one or two weeks.   
}
\usage{
drawIncidence(data, countries, start = "2020-06-01", end = "last", weeks = 2, 
    log = TRUE)
}
\arguments{
  \item{data}{
data frame including columns \code{location} (character string indicating the country), \code{date} (character string) and  \code{new_cases_per_million} (numeric)
}
  \item{countries}{
vector of characters srings indicating the countries for which the curves are plotted
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
\code{\link{drawIncidenceFin}}, \code{\link{dataCovid}}
}
\examples{
data(dataCovid)
Europe <- c("Germany","France","United Kingdom","Italy","Spain","Poland","Romania",
    "Netherlands","Belgium","Greece")
drawIncidence(dataCovid,countries=Europe)
}

