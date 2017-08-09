\name{bondCurve}
\alias{bondCurve}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Drawing forward and yield curves
}
\description{
This function draws forward and yields curves, for AAA-rated central governement bonds and/or all central governement bonds.   
}
\usage{
bondCurve(date1, date2 = NULL, yield = TRUE, forward = TRUE, 
  AAA = TRUE, all = TRUE, params)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{date1}{
The date for which the curves are drawn
}
  \item{date2}{
Optional second date for which the curves are drawn
}
  \item{yield}{
Is the yield curve shown (TRUE/FALSE)?
}
  \item{forward}{
 Is the forward curve shown (TRUE/FALSE)? 
}
  \item{AAA}{
 Are the curves drawn for the AAA-rated bonds (TRUE/FALSE)?
}
  \item{all}{
Are the curves drawn for the bonds with all ratings (TRUE/FALSE)?
}
  \item{params}{
  The data frame of curve parameters
}
}
\value{
No value. Only a figure is produced.
}
\references{
https://www.ecb.europa.eu/stats/financial_markets_and_interest_rates/euro_area_yield_curves/html/index.en.html
}
\author{
Arto Luoma
}
\examples{
data(params)
bondCurve(as.Date("2004-09-06"),params=params)
}
