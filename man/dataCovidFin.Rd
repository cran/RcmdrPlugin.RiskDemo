\name{dataCovidFin}
\alias{dataCovidFin}
\docType{data}
\title{
Confirmed COVID-19 cases in Finland
}
\description{
This data set provides the confirmed COVID-19 cases in 21 Finnish hospital districts, in addition to the total number.
}
\usage{data("dataCovidFin")}
\format{
  A data frame with 16082 observations on the following 3 variables.
  \describe{
    \item{\code{Aika}}{Date}
    \item{\code{Alue}}{character vector: hospital district}
    \item{\code{val}}{numeric vector: number of new confirmed cases}
  }
}
\details{
The data were downloaded on March 31, 2021, via THL's open data API.
}
\source{
https://bit.ly/2PO1DnS
}
\references{
https://bit.ly/3ryfwE4
}
\examples{
library(zoo)
data(dataCovidFin)
casesFin <- subset(dataCovidFin, subset = Alue=="Kaikki Alueet")
plot(zoo(casesFin$val,order.by=casesFin$Aika),ylab="New COVID-19 cases in Finland",xlab="")
} 

