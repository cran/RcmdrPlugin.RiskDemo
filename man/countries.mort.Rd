\name{countries.mort}
\alias{countries.mort}
\docType{data}
\title{
Mortality data
}
\description{
Mortality data for 38 countries (period death rates and exposures) retrieved from
Human Mortality Database. Exposures are only included for the Nordic countries, China, U.S., Russia, Japan  and Germany.  
}
\usage{data("countries.mort")}
\format{
 List of objects of class demogdata.
 }
\source{
Human Mortality Database. University of California, Berkeley (USA), and Max Planck Institute for Demographic Research (Germany). Available at www.mortality.org or www.humanmortality.de (data downloaded May 3, 2017). 
}
\examples{
data(countries.mort)
plot(countries.mort[[1]])
}

