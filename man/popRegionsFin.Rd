\name{popRegionsFin}
\alias{popRegionsFin}
\docType{data}
\title{
Population data on Finnish hospital districts
}
\description{
This data set provides the populations of the 21 hospital districts, in addition to the total Finnish population. 
}
\usage{data("popRegionsFin")}
\format{
  A data frame with 22 observations on the following 2 variables.
  \describe{
    \item{\code{Alue}}{character vector: hospital district}
    \item{\code{val}}{numeric vector: population}
  }
}
\details{
The data were downloaded on March 31, 2021, via THL's open data API.
}
\source{
https://bit.ly/39uZy7C
}
\references{
https://bit.ly/3ryfwE4
}
\examples{
data(popRegionsFin)
print(popRegionsFin)
}

