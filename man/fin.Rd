\name{fin}
\alias{fin}
\docType{data}
\title{
Mortality data for Finland
}
\description{
Mortality data for Finland
    Series: female male total
    Years: 1878 - 2015
    Ages:  0 - 110 
}
\details{
This is part of the  countries.mort data (countries.mort[[11]]).
}
\usage{data("fin")}
\format{
  object of class demogdata
}
\source{
Human Mortality Database. University of California, Berkeley (USA), and Max Planck Institute for Demographic Research (Germany). Available at www.mortality.org or www.humanmortality.de (data downloaded May 3, 2017).
}
\examples{
data(fin)
print(fin)
plot(fin)
}
\keyword{datasets}
