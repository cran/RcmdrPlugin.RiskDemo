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
HMD. Human Mortality Database. Max Planck Institute for Demographic Research (Germany), University of California, Berkeley (USA), and French Institute for Demographic Studies (France). Available at www.mortality.org. (Data downloaded Nov 13, 2023.)
}
\examples{
data(fin)
print(fin)
plot(fin)
}
\keyword{datasets}
