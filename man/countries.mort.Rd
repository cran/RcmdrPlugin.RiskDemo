\name{countries.mort}
\alias{countries.mort}
\docType{data}
\title{
Mortality data
}
\description{
Mortality data for 10 countries (period death rates and exposures) retrieved from
Human Mortality Database. The data are rounded to three significant digits and include the Nordic countries, China, U.S., Russia, Japan  and Germany.  
}
\usage{data("countries.mort")}
\format{
 List of objects of class demogdata.
 }
\source{
HMD. Human Mortality Database. Max Planck Institute for Demographic Research (Germany), University of California, Berkeley (USA), and French Institute for Demographic Studies (France). Available at www.mortality.org. (Data downloaded Nov 13, 2023.)
}
\examples{
data(countries.mort)
plot(countries.mort[[1]])
}

